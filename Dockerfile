FROM fpco/stack-build:lts-17.8 as builder

ADD stack.yaml package.yaml ./
RUN stack -j "$(nproc)" --system-ghc build --only-dependencies

ADD ./ ./
RUN stack -j "$(nproc)" --system-ghc build --test --keep-going --copy-bins

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
