CC=riscv64-unknown-elf-gcc
OBJECT_OPT=-fno-common -fno-builtin-printf -specs=htif_nano.specs
BINARY_OPT=-static -specs=htif_nano.specs

all: mem-stride.riscv

mem-stride.riscv: mem-stride.o
	$(CC) $(BINARY_OPT) -o $@ $^

mem-stride.o: mem-stride.c
	$(CC) -D_1stream $(OBJECT_OPT) -o $@ $^

.PHONY: clean
clean:
	rm -f *.riscv
	rm -f *.o
