CC=riscv64-unknown-elf-gcc
OBJECT_OPT=-fno-common -fno-builtin-printf -specs=htif_nano.specs
BINARY_OPT=-static -specs=htif_nano.specs

all: mempress-rocc-2MiBL2-4stream.riscv

mempress-rocc-2MiBL2-1stream.riscv: mempress-rocc-2MiBL2-1stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-2MiBL2-1stream.o: mempress-rocc.c
	$(CC) -D_2MiBL2 -D_1stream $(OBJECT_OPT) -o $@ $^

.PHONY: clean
clean:
	rm -f *.riscv
	rm -f *.o

OOOAINEJGIWEGBIWE
