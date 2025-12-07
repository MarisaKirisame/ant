# Ant syntax highlighting for VS Code

Visual Studio Code lets you add lightweight syntax highlighting by contributing a TextMate grammar through an extension. The files in this folder define a minimal syntax highlighter for `.ant` sources so the ML-like constructs (`let`, `type`, `match`, variants, literals, etc.) stand out while editing.

## Installing locally

1. Install VS Code's extension packaging tool if you do not already have it:
   ```bash
   npm install -g @vscode/vsce
   ```
2. Build a VSIX from the repository root (the `NODE_OPTIONS` bit injects a `File` polyfill that `vsce` expects when running on Node 18):
   ```bash
   cd vscode-ant
   NODE_OPTIONS="--require ./scripts/polyfill-file.js" vsce package
   ```
   This produces `ant-syntax-highlighting-0.0.1.vsix` in the same directory.
3. In VS Code run the **Extensions: Install from VSIX...** command and select the generated file, or run:
   ```bash
   code --install-extension ant-syntax-highlighting-0.0.1.vsix
   ```
4. Reload VS Code. Any file ending in `.ant` will now use the Ant grammar—block comments (`(* … *)`) and the basic tokens are highlighted.

You can tweak the grammar in `syntaxes/ant.tmLanguage.json` or add more language features whenever needed.
