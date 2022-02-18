FROM ubuntu:latest
RUN apt-get update
RUN apt-get install --assume-yes curl gcc libgmp-dev libpq-dev make xz-utils zlib1g-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack --version

# Install GHC.
WORKDIR /project
COPY . /project

RUN stack setup 
RUN stack exec -- ghc --version
RUN stack build --only-dependencies

# # Run project.
# ENV HOST 0.0.0.0
# ENV PORT 80
# EXPOSE 80
# CMD /usr/local/bin/counter