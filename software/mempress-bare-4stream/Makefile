CC=riscv64-unknown-elf-gcc
OBJECT_OPT=-fno-common -fno-builtin-printf -specs=htif_nano.specs
BINARY_OPT=-static -specs=htif_nano.specs

all: mempress-rocc-4stream.riscv

mempress-rocc-4stream.riscv: mempress-rocc-4stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-4stream.o: mempress-rocc.c
	$(CC) -D_4stream $(OBJECT_OPT) -o $@ $^

.PHONY: clean
clean:
	rm -f *.riscv
	rm -f *.o
