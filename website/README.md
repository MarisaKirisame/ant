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

When publishing under a subpath, set mdBook's site URL path:

```bash
ANT_DOC_SITE_URL=/repo-name/ make website
```

Use `make website-check` for static checks plus `mdbook build`, and `make website-serve` to run `mdbook serve` at `http://127.0.0.1:3000/`.

Override the serve address with:

```bash
MDBOOK_PORT=3001 make website-serve
```

The `Pages` GitHub Actions workflow builds the published site as `Chordata` and deploys `website/book` to the external `ant-memo/ant-memo.github.io` Pages repository from `main` pushes or manual workflow runs. The workflow expects a `PAGES_DEPLOY_TOKEN` secret with write access to that repository.
