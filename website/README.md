# Website Documentation

This directory contains the mdBook source for the current `\Ant` language guide.

Build through the repository Makefile:

```bash
make website
```

The build expects `mdbook` on `PATH`.

The document source uses `\Ant` for the project/language name. Override the rendered name with:

```bash
ANT_DOC_NAME=MyLang make website
```

Use `make website-check` for static checks plus `mdbook build`, and `make website-serve` to run `mdbook serve` at `http://127.0.0.1:3000/`.

Override the serve address with:

```bash
MDBOOK_PORT=3001 make website-serve
```
