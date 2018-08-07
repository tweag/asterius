# Haddock Autolink

A [Sphinx](www.sphinx-doc.org) extension for automatically linking to Haddocks
for Haskell packages.

## How to use the autolinker

This extension provides two autolinkers: `:haddock:` and `:haddock_short:` which
take a string of the form `package-name/Module.Name#identifier` and generate a
link to the Haddocks. `:haddock:` will have the module name and identifier in
the link text, `:haddock_short:` will only have the identifier.

You can omit the identifier to link directly to the module
(`containers/Data.Sequence`), and you can omit the module and identifier to link
to the package (`containers`).


### Implicit Haddock Links

If you are writing documentation for the `foo` haskell package (`project =
'foo'` in your ReadTheDoc's `conf.py`) then you can omit the `package-name` part
of the `package-name/Module.Name#identifier` and instead just put
`/Module.Name#identifier`.


### Examples

```
:haddock:`containers`
```
=> [containers](https://hackage.haskell.org/package/containers)

```
:haddock:`containers/Data.Sequence`
```
=> [Data.Sequence](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html)

```
:haddock:`containers/Data.Sequence#empty`
```
=> [Data.Sequence#empty](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html#v:empty)


## Installing the extension

If you are using the [Haskell Package ReadTheDocs
template](https://github.com/m-renaud/haskell-rtd-template) then everything will
already be set up for you.

If you are not using the template, we recommend you install this as a submodule
so you get any updates to the extension.

1. `cd` to the `docs/_extensions/` directory in your repo
2. Add this repo as a submodule: `git submodule add
   https://github.com/m-renaud/haddock-autolink`
3. Add the extension path in your `conf.py` file:
   `sys.path.insert(0, os.path.abspath('.') + '/_extensions/haddock-autolink')`
4. Add `'haddock-autolink'` to the `extensions` list in `conf.py`

When cloning your repo in the future use `git clone --recurse-submodules
https://github.com/YOUR_USER/YOUR_REPO.git`, the `--recurse-submodules` flag
will automatically pull down any submodules (including this one). For more info
see the [Git submodules
docs](https://git-scm.com/book/en/v2/Git-Tools-Submodules).


## Other Haddock Hosting Options

This autolinker supports linking to non-Hackage documentation, currently
Stackage and local docs are supported. You can control this by setting
environment variables when building the docs:

```shell
make -e <env_variables> html

# Hackage is the default so the following two are equivalent:
make html
make -e HADDOCK_HOST=hackage html
```

### Linking to Stackage

To link to Stackage you'll need to set two environment variables when building
the documentation:

- `HADDOCK_HOST=stackage`
- `STACKAGE_RESOLVER=<resolver>` where `<resolver>` is the Stackage resolver you
  want to link to (for example, `lts-10.0`)

```shell
make -e HADDOCK_HOST=stackage STACKAGE_RESOLVER=lts-10.0 html
```


### Linking to local docs

You can also link to locally hosted Haddocks by setting:

- `HADDOCK_HOST=local`
- `HADDOCK_DIR=<path-to-haddock-root>` (for example `file:///path/to/haddocks/`)

```shell
make -e HADDOCK_HOST=local HADDOCK_DIR=file:///usr/local/docs/haddocks/
```
