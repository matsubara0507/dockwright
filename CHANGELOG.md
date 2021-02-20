# Changelog for dockwright

## Unreleased changes

## 1.2.0

- Refactor: update resolver to lts-17.4
    - update req package to 3.x
    - update language-docker to 9.x (sep EDSL to dockerfile-creator package)
- CI: use GitHub Actions instead of TravisCI
- Registry: use GitHub Container Registry instead of Docker Hub

## 1.1.0

- Feat: `--with-name` option for display tags commands

## 1.0.0

- Refactor: update to lts14.4 and Stack v2
    - update GHC to 8.6.5
    - update extensible to 0.6.1
    - update req to 2.1.0
    - update language-docker to 8.0.2
    - change package to githash from gitrev
- Refactor: use mix.hs and fallible
- Feat: strip prefix for release
- Feat: before/after template
- Modify: `--echo` can use lower case
- Feat: `--tags` command
- Feat: `--new-tags` command
- Feat: generate default config
- Feat: `--help` option
