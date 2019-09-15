# dockwright

[![Build Status](https://travis-ci.com/matsubara0507/dockwright.svg?branch=master)](https://travis-ci.com/matsubara0507/dockwright)
[![](https://images.microbadger.com/badges/image/matsubara0507/dockwright.svg)](https://microbadger.com/images/matsubara0507/dockwright "Get your own image badge on microbadger.com")


CLI for building Dockerfile from template and config yaml.

see [example](example)

```
$ dockwright --help
dockwright [options] [config-file]
  -h  --help        Show this help text
  -v  --verbose     Enable verbose mode
      --version     Show version
  -d  --default     Dump default config
      --echo[=ENV]  Show fetched env after build
      --tags        Fetch docker image tags from DockerHub
      --new-tags    Fetch new tags from tags config
      --with-name   Append image name to display tag
```

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
