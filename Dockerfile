FROM fpco/stack-build:lts-20.2 as builder

WORKDIR /app

ADD stack.yaml package.yaml ./
RUN stack -j "$(nproc)" --system-ghc build --only-dependencies

ADD ./ ./
RUN stack -j "$(nproc)" --system-ghc build --copy-bins

RUN stack install dhall \
  && find types/zoo -type f -name '*.dhall' | xargs dhall freeze --check \
  && find types/zoo -type f -name '*.dhall' | xargs -n 1 dhall resolve --file > /dev/null

FROM builder as test

RUN stack test \
 && stack install ormolu \
 && ormolu --cabal-default-extensions --mode check $(find src app test -name '*.hs')

FROM debian:11.3

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp-dev \
    libtinfo5 \
    netbase \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /root/.local/bin/take-care /usr/local/bin/
COPY --from=builder /app/types /app/types
COPY --from=builder /root/.cache/dhall /root/.cache/dhall
COPY --from=builder /root/.cache/dhall-haskell /root/.cache/dhall-haskell

ENTRYPOINT ["/usr/local/bin/take-care"]
