# CompileRegMemo 后端计划

## 目标

`CompileRegMemo` 作为独立于现有 `CompileMemo` 的新后端，目标是面向 ANF + join points 输入生成“固定槽位环境”的 memo CEK 代码。

核心目标：

- 支持 `Jump`、`BCont`、`BRecC`
- 在 join point 上使用显式控制流，而不是继续依赖表达式级 `push` / `pop`
- 在得到每条路径的精确 liveness 后，执行类似弦图着色的槽位分配
- 让变量、join 参数和临时值都落到固定 stack slot / env slot
- 尽量消除仅用于重排环境的 `push_env` / `pop_env`

## 总体策略

`CompileRegMemo` 采用两层 lowering：

1. `Syntax`/ANF AST -> backend-local CFG IR
2. CFG IR -> 带固定槽位布局的 CEK 发射

中间 CFG IR 至少要显式表示：

- basic block
- block 参数
- `Jump` / conditional branch / match branch
- 普通调用
- 返回
- 每个块内的顺序求值语句

`BCont` 和 `BRecC` 在这层 IR 中都应成为 block 定义：

- `BCont` -> 非递归 join block
- `BRecC` -> 一个递归 block group
- `Jump` -> 对 block 的跳转，参数对应 block 形参

语义上这很接近 SSA 中的 block 参数 / phi，而不是普通函数调用。

## 分阶段计划

### Phase 1: 建 backend-local IR

新增 `CompileRegMemo` 私有 IR，用于把一个顶层函数及其内部 join points 收集成一个 control-flow graph。

最低要求：

- 给每个局部 binder / join binder 一个唯一 id
- 收集顶层函数入口块
- 把 `Let (BCont ...)` 和 `Let (BRecC ...)` 降成块定义
- 把 `Jump` 降成块间边
- 保留 `App (GVar ...)` 作为普通函数调用节点

注意：

- 不要继续直接把 `BCont` 映射到现有 `ctx.conts`
- 顶层函数 continuation 和局部 join point 要分开处理

### Phase 2: 精确 liveness

现有 `Liveness.annotate_prog_with_liveness` 适合做语法树级自由变量分析，但不够支撑固定槽位分配。

`CompileRegMemo` 需要新增 block/edge 级 liveness：

- `live_in(block)`
- `live_out(block)`
- `live_on_edge(pred -> succ)`
- join 参数对应的 phi-use / block-param-use

特别注意：

- `Jump` 的参数使用不能按普通表达式 `fv_after` 处理
- 不同前驱进入同一 join 时，活跃变量集合可能不同，必须按边精确记录
- `match` / `if` 的各个分支末尾都要进入同一数据流求解

### Phase 3: 槽位分配

在拿到精确 liveness 后，构造 interference graph，并做固定槽位分配。

推荐策略：

- 以“一个顶层函数 + 其内部 join graph”作为单个分配单元
- 普通变量、临时值、join 参数统一参与分配
- 优先利用 ANF + block 参数的近 SSA 结构，采用 <https://www.cs.cmu.edu/~411/lectures/02-regalloc.pdf> 里的分配方法

分配产物至少包括：

- `slot_of_value : value_id -> slot`
- 每个 block 入口的参数槽位约定
- 每个 block 的 frame size
- 是否需要 scratch slot 以处理并行搬运环

### Phase 4: Jump coalescing 和 parallel copy

`Jump` 边上的参数传递不能退化成 push/pop。

需要：

- 优先让实参与 join 形参共槽
- 若共槽失败，则在边上插入 parallel copy
- 对置换环使用 scratch slot 或 cycle-breaking 方案

例如：

- `jump j(x, y)` 且 `slot(x) = slot(j.a)`、`slot(y) = slot(j.b)` 时，无需 copy
- `jump j(y, x)` 且目标槽位互换时，需要显式处理搬运环

### Phase 5: 代码生成

发射阶段按槽位读写环境，而不是按栈顶协议推进。

需要补的基础能力：

- `get_env_slot`
- `set_env_slot`
- 按 slot 批量或并行重排的 helper
- 必要时扩容并预留 scratch slot

发射策略：

- `Var` -> 直接读取固定 slot
- `Let` 绑定结果 -> 直接写入目标 slot
- `Jump` -> 执行 parallel copy 后 `goto`
- `BCont` / `BRecC` -> 编译为具备固定入口布局的 block pc
- 非尾 `App` -> 保留 continuation 机制，caller-save / restore 基于 slot 描述，而不是“压紧保留前缀”

### Phase 6: runtime 支撑

沿用当前 `State.state.e : value Dynarray.t`，runtime 不需要重写，需要新增更直接的 slot 更新接口。

最低支持项：

- 单槽覆盖写
- 多槽并行搬运
- 固定 frame 大小初始化
- 非尾调用返回时按保存描述恢复 live slots

## 关键注意事项

### 1. 不要在现有 `CompileMemo.scope` 上继续演化

`CompileMemo.scope` 的核心是：

- 名字 -> 当前前缀位置
- `env_length` 表示当前有效前缀
- `push_s` / `pop_s` / `drop_s` 通过模拟栈来维护布局

这套结构不适合 join CFG。`CompileRegMemo` 应该单独定义：

- 唯一化 value id
- `slot_env`
- `block_layout`
- `frame_layout`

### 2. join point 不是普通函数

- join point 没有独立调用栈语义
- 它们只是 CFG 汇合点
- 若把它们当普通函数，会重新引入参数构造和 caller/callee 协议，抵消固定槽位收益

### 3. `BRecC` 要按 SCC 处理

递归 join points 不应逐个独立分配槽位。需要：

- 先识别递归 continuation group
- 以 SCC 为单位求 liveness 和 frame 约定
- 保证组内所有回边都满足统一入口布局

### 4. liveness 必须是边敏感的

块级 `live_in/live_out` 仍然不够描述某些 join 边上的并行搬运需求。

必须额外保留：

- 进入每个 successor 前真正需要带过去的值
- 该值在 successor 参数中的目标槽位

否则无法正确做 copy coalescing。

### 5. 非尾调用和 join 跳转要分开

普通 `App (GVar ...)` 与 `Jump` 虽然表面都像“跳到别处并传参数”，但语义不同：

- `Jump` 不创建新的 continuation frame
- 非尾 `App` 仍然需要回到 caller

因此：

- `Jump` 使用 block 边 + slot 重排
- 非尾 `App` 使用 continuation 保存/恢复协议

### 7. validator 缺位期间要 fail fast

`CompileRegMemo` 应先只接受明确形状：

- `BCont (PVar name, Lam (...), _)`
- `BRecC` 中每项都为 `PVar = Lam`
- `Jump` target 必须是已知 join binder

否则直接报错，不要静默接受复杂形状。

## 建议的数据结构

可以从下面这组结构起步：

```ocaml
type value_id = int
type block_id = int
type slot = int

type operand =
  | local of value_id
  | global of string
  | imm_int of int
  | imm_bool of bool
  | ctor of string

type terminator =
  | Jump of block_id * operand list
  | Branch of operand * block_id * block_id
  | Match of operand * (string * block_id) list * block_id option
  | Call of string * operand list * call_cont
  | Return of operand

type stmt =
  | Bind of value_id * rhs

type block = {
  id : block_id;
  params : value_id list;
  body : stmt list;
  term : terminator;
}
```

## 落地顺序建议

建议按下面顺序实现：

1. 新建 `CompileRegMemo` 模块和 backend entry，但先保持 fail-fast
2. 建 backend-local IR
3. 打通 `BCont` / `Jump`
4. 加 liveness 与槽位分配
5. 接通 `BRecC`
6. 改写普通表达式发射到固定槽位模型

## 当前仓库内约束

- `Typing -> ANF -> Pat.compile -> Backend.compile` 已经是主流程
- `CompilePlain` / `CompileSeq` 已支持 `Jump` / `BCont` / `BRecC`
- `CompileMemo` 仍是旧模型，占位失败是预期现状

因此 `CompileRegMemo` 应作为实验性新后端推进，而不是在第一步就替换 `CompileMemo`。
