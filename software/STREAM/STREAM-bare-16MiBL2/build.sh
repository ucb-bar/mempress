#!/bin/bash
riscv64-unknown-elf-gcc -fno-common -fno-builtin-printf -specs=htif_nano.specs -c stream.c
riscv64-unknown-elf-gcc -static -specs=htif_nano.specs stream.o -o stream.riscv

riscv64-unknown-elf-gcc -fno-common -fno-builtin-printf -specs=htif_nano.specs -c hello_world.c
riscv64-unknown-elf-gcc -static -specs=htif_nano.specs hello_world.o -o hello_world.riscv
