# ANF 与 Join Points 实施计划

## Summary

- 在 typed AST 上新增 `Jump` 表达式节点，形状类似 `App`，用于显式表示跳转到 join point。
- 废弃 CPS 规范化路径，改为在 `Typing.top_type_of_prog` 之后执行新的 `ANF` 变换；`typer` 不改。
- join point 绑定规则明确为：
  - 非递归 join point 使用 `BCont`
  - 递归 join point 使用 `BRecC`
  - 普通递归值绑定继续使用 `BRec`
- 不在 AST 结构中强制 ANF 参数原子性或 join-target 合法性；这些由后续独立 validator 检查。
- 调试和前端主流程已切到 `Typing -> ANF -> Pat.compile`。
- 后端现状：
  - `CompilePlain` 已支持 `Jump` / `BCont` / `BRecC`
  - `CompileSeq` 已支持 `Jump` / `BCont` / `BRecC`
  - `CompileMemo` 仍未适配新的 join-point 输入
- CLI 本轮未调整，默认 backend 仍保持现状；因此主编译路径对 ANF 的完整支持仍取决于所选 backend。
- defunc 不进入新管线，也不保留在调试入口中。

## Current Status

### Done

- `Syntax.expr` / pretty-printer / tag-map / 遍历辅助函数已支持 `Jump`。
- `Transform.anf_prog` 已作为规范化入口接入主流程。
- `Tail.ml`、`Liveness.ml`、`Pat.ml` 等依赖 AST 全匹配的模块已覆盖 `Jump`、`BCont`、`BRecC`。
- `bin/main.ml` 当前实际流程为：
  - `Typing.top_type_of_prog`
  - `Transform.anf_prog`
  - `Pat.compile`
  - `Backend.compile`
- `CompilePlain` 与 `CompileSeq` 已把 join point 当作普通局部函数来输出：
  - `Jump` 编译为调用
  - `BCont` 编译为局部/顶层 `let`
  - `BRecC` 编译为局部/顶层 `let rec ... and ...`
- 测试已从“plain/seq 遇到 ANF 节点应失败”切换为“plain/seq 成功、memo 仍失败”。

### Not Done

- `CompileMemo` 仍保留 placeholder 分支，不支持 `Jump`、`BCont`、`BRecC`。
- 还没有独立的 ANF validator 去检查参数原子性、join-target 合法性和 continuation 绑定形状。
- CLI 没有根据 backend 支持矩阵调整默认值或帮助文本。

## Key Changes

### AST / syntax

- 在 `Syntax.expr` 中加入 `Jump of 'a expr * 'a expr list * 'a`。
- 更新 `pp_expr`，让 `Jump` 有单独打印形式并与 `App` 区分。
- 同步更新 `expr_tag`、`expr_tag_map` 以及所有对 `expr` 做全匹配的辅助函数，确保 `Jump` 可被遍历和重标注。
- 保留现有 binding 类型，但明确语义：
  - `BOne` / `BRec` 用于普通值绑定
  - `BCont` / `BRecC` 用于 continuation / join point 绑定

### Transform / ANF

- 在 `Transform.ml` 中新增 `anf_expr` / `anf_prog` 主入口，替代 `cps_prog`。
- ANF 变换负责：
  - 将复杂子表达式拆为顺序 `let`
  - 在控制流汇合点引入 join point
  - 用 `Jump` 作为分支末尾或回边的显式跳转
- 控制流 lowering 规则固定为：
  - 非递归汇合点生成 `Let (BCont ..., body, ...)`
  - 需要自回边或互递归回跳的 continuation 组生成 `Let (BRecC ..., body, ...)`
  - 普通递归值定义保持 `Let (BRec ..., body, ...)`，不混入 continuation
- `if` / `match` 等多分支表达式统一采用“先建立 join，再在每个分支末尾 `Jump` 到 join”的策略。
- 普通函数调用仍使用 `App`；只有跳到 join point 时使用 `Jump`。

### Driver / debug / docs

- `bin/main.ml` 改为在 typing 后调用 `Transform.anf_prog` 作为规范化调试输出。
- 当前只保留 `--print-anf` 调试输出，不再暴露 CPS/defunc 输出。
- CLI 默认 backend 和帮助文本本轮未调整。
- 文档中的 pipeline 描述从 “CPS + defunctionalisation” 改为 “Typing 后 ANF + join points”。

### Compatibility cleanup

- `Typing.ResolveGlobal` 继续拒绝 `BCont` / `BRecC` 等 join-only 结构；因为它们只会在 typing 后出现，不需要修改 typer。
- 需要更新所有基于 AST 的遍历模块，使其至少能编译并正确穿透 `Jump`、`BCont`、`BRecC` 的语义分层。
- 任何仍默认把 `BRecC` 当未使用占位符的代码，都要改成显式支持“递归 continuation 组”。

## Public Interfaces / Types

- `Syntax.expr`
  - 新增 `Jump of 'a expr * 'a expr list * 'a`
- `Transform`
  - 新增并暴露 `anf_prog : 'a prog -> 'a prog`
- Binding semantics
  - `BRec`：普通递归值绑定
  - `BRecC`：递归 continuation / join-point 绑定
- CLI / docs
  - 调试 flag 与帮助文本改为 ANF 术语，移除 CPS/defunc 对外入口

## Test Plan

- AST coverage
  - 为 `Jump` 的 pretty-print、tag-map、遍历函数增加回归测试。
- ANF shape tests
  - 为 `if`、`match`、嵌套调用、复杂子表达式拆分分别加 golden tests，检查：
    - 非递归汇合点使用 `BCont`
    - 递归 continuation 使用 `BRecC`
    - 普通递归定义仍使用 `BRec`
    - join 跳转使用 `Jump` 而不是 `App`
- Pipeline
  - 增加 CLI/debug 快照测试，确认 typing 后输出的是 ANF，不再是 CPS/defunc。
- Backends
  - `CompilePlain` / `CompileSeq` 对 `Jump`、`BCont`、`BRecC` 的编译成功测试已加入。
  - `CompileMemo` 目前仍保留失败测试，作为未适配保护。
- Build safety
  - 全仓编译通过，确保所有 `Syntax.expr` / `binding` 的模式匹配都覆盖新语义。

## Assumptions

- join points 只在 typing 完成后的 typed AST 中引入，因此 `Typing.ml` 无需新增 `Jump`、`BCont`、`BRecC` 的类型规则。
- ANF 性质与 join-target 合法性不由 AST 结构保证，依赖后续独立 validator。
- 后端已部分适配：`plain` / `seq` 已支持，`memo` 尚未支持。
- defunctionalization 不进入新管线，也不保留在新的调试接口中。
