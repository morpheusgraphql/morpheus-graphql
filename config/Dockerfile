FROM fpco/stack-build:lts-18.20
RUN mkdir -p /project
COPY . /project
WORKDIR /project
RUN stack build --fast --skip-ghc-check --no-terminal --test --only-dependencies
