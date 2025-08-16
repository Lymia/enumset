#! /usr/bin/env nix-shell
#! nix-shell -p bash rustup clang qemu -i bash --pure

set -eu

cargo install defmt-print

ELF_BINARY=$1
# All suitable for thumbv7em-none-eabihf
MACHINE="-cpu cortex-m4 -machine mps2-an386"
# MACHINE="-cpu cortex-m7 -machine mps2-387"
# MACHINE="-cpu cortex-m7 -machine mps2-500"
LOG_FORMAT='{[{L}]%bold} {s} {({ff}:{l:1})%dimmed}'
echo -e "ELF_BINARY=$ELF_BINARY"
shift
echo -e "Running on '$MACHINE'..."
echo -e "------------------------------------------------------------------------"
qemu-system-arm $MACHINE -semihosting-config enable=on,target=native -display none -kernel $ELF_BINARY -serial /dev/stdout | ~/.cargo/bin/defmt-print -e $ELF_BINARY $* --log-format="$LOG_FORMAT"
echo -e "------------------------------------------------------------------------"
