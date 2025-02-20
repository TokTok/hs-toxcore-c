#!/bin/sh

set -ex

sudo apt-get install ninja-build libopus-dev libsodium-dev libvpx-dev

git clone --recurse-submodules --depth=1 https://github.com/TokTok/c-toxcore
cmake \
  -B c-toxcore/_build \
  -D BOOTSTRAP_DAEMON=OFF \
  -D CMAKE_INSTALL_PREFIX:PATH="/usr" \
  -G Ninja \
  c-toxcore
cmake --build c-toxcore/_build
sudo cmake --install c-toxcore/_build
rm -rf c-toxcore
