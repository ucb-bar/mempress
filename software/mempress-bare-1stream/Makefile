CC=riscv64-unknown-elf-gcc
OBJECT_OPT=-fno-common -fno-builtin-printf -specs=htif_nano.specs
BINARY_OPT=-static -specs=htif_nano.specs

all: mempress-rocc-1stream.riscv

mempress-rocc-1stream.riscv: mempress-rocc-1stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-1stream.o: mempress-rocc.c
	$(CC) -D_1stream $(OBJECT_OPT) -o $@ $^

.PHONY: clean
clean:
	rm -f *.riscv
	rm -f *.o
