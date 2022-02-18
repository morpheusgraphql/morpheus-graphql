FROM ubuntu:latest
RUN apt-get update
RUN apt-get install --assume-yes curl gcc libgmp-dev libpq-dev make xz-utils zlib1g-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack --version

# Install GHC.
WORKDIR /project
RUN stack exec -- ghc --version

RUN stack setup
VOLUME [ ".:/project" ]