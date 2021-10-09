FROM fpco/stack-build:lts-18.13 as builder

ADD stack.yaml package.yaml ./
RUN stack -j "$(nproc)" --system-ghc build --only-dependencies

ADD ./ ./
RUN stack -j "$(nproc)" --system-ghc build --copy-bins

FROM builder as test

RUN stack test
 && stack install ormolu
 && ormolu --mode check $(find src app test -name '*.hs')

FROM debian:10.9

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp-dev \
    libtinfo5 \
    netbase \
 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /root/.local/bin/take-care /usr/local/bin/

ENTRYPOINT ["/usr/local/bin/take-care"]
