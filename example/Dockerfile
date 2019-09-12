FROM ubuntu:18.04
ENV STACK_VERSION="2.1.3"
RUN apt-get update \
 && apt-get install -y curl gcc git gnupg libc6-dev libffi-dev libgmp-dev make netbase xz-utils zlib1g-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -sSL https://get.haskellstack.org/ | sh \
 && stack upgrade --binary-version=${STACK_VERSION}
