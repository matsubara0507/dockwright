# dockwright

CLI for building Dockerfile from template and config yaml.

see [example](example)

## Development

build docker image

```
$ stack --docker build -j 1 Cabal # if out of memory in docker
$ stack --docker --local-bin-path=./bin install
$ docker build -t matsubara0507/dockwright . --build-arg local_bin_path=./bin
```

run

```
$ docker run --rm -v `pwd`/example:/work matsubara0507/dockwright dockwright .dockwright.yaml
```
