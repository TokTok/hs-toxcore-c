# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["rm", "-rf", "/work/hs-toxcore-c"]
COPY --chown=builder:users . /work/hs-toxcore-c
RUN ["stack", "build", "toxcore-c"]
