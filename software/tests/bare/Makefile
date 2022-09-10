bmarkdir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

TARGET := riscv64-unknown-elf
CFLAGS := -mcmodel=medany -std=gnu99 -O2 -fno-common -fno-builtin-printf -Wall
LDFLAGS := -static -nostdlib -nostartfiles -lgcc

PROGRAMS := mempress-rocc

objs := crt.o syscalls.o
ldscript := link.ld

include $(bmarkdir)/../common.mk
