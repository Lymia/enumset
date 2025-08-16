#!/usr/bin/env bash

set -eu

cd -- "$(dirname -- "$(readlink -- -f "$0")")"

cargo run --target thumbv7em-none-eabihf -p enumset_test_embedded --release > target/qemu-output.txt

grep " SmallEnum = A | B " target/qemu-output.txt
grep " ArrayEnum = A | C | E | X " target/qemu-output.txt
