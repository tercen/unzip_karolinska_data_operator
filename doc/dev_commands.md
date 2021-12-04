## setting up

in a shell:

```
rm README.md 
mv README_template.md README.md
```

## adding packages

in R console:

```R
renv::init()
```

## managing the repo

in a shell:

```
git add -A && git commit -m "init repo"
git push
git tag -a 0.0.1 -m "++" && git push --tags
```

## commit message format

[generate-changelog](https://github.com/lob/generate-changelog) is used to generate logs on release, see [release.yml](../.github/workflows/release.yml).

To use this module, your commit messages have to be in this format:

```
type(category): description [flags]
```

Where `type` is one of the following:

* `breaking`
* `build`
* `ci`
* `chore`
* `docs`
* `feat`
* `fix`
* `other`
* `perf`
* `refactor`
* `revert`
* `style`
* `test`

Where `flags` is an optional comma-separated list of one or more of the following (must be surrounded in square brackets):

* `breaking`: alters `type` to be a breaking change

And `category` can be anything of your choice. If you use a type not found in the list (but it still follows the same format of the message), it'll be grouped under `other`.

see https://github.com/lob/generate-changelog#usage

see https://github.com/tercen/generate-changelog-action