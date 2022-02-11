FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-toxcore-c"]
COPY --chown=builder:users . /work/hs-toxcore-c
RUN ["stack", "install", "toxcore-c"]

FROM scratch
COPY --from=build /home/builder/.local/ /
