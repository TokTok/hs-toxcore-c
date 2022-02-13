#!/bin/sh

set -ex

sudo apt-get install libmsgpack-dev libopus-dev libsodium-dev libvpx-dev

git clone --depth=1 https://github.com/TokTok/c-toxcore
cmake -Bc-toxcore/_build -Hc-toxcore -DBOOTSTRAP_DAEMON=OFF -DCMAKE_INSTALL_PREFIX:PATH="/usr"
cmake --build c-toxcore/_build --parallel "$(nproc)"
sudo cmake --build c-toxcore/_build --parallel "$(nproc)" --target install
rm -rf c-toxcore