#!/bin/bash
riscv64-unknown-elf-gcc -fno-common -fno-builtin-printf -specs=htif_nano.specs -c stream.c
riscv64-unknown-elf-gcc -static -specs=htif_nano.specs stream.o -o stream.riscv
