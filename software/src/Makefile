
CC=riscv64-unknown-elf-gcc
OBJECT_OPT=-fno-common -fno-builtin-printf -specs=htif_nano.specs
BINARY_OPT=-static -specs=htif_nano.specs


all: mempress-rocc-2MiBL2-1stream.riscv mempress-rocc-2MiBL2-2stream.riscv mempress-rocc-2MiBL2-4stream.riscv mempress-rocc-2MiBL2-8stream.riscv mempress-rocc-2MiBL2-16stream.riscv mempress-rocc-4MiBL2-1stream.riscv mempress-rocc-4MiBL2-2stream.riscv mempress-rocc-4MiBL2-4stream.riscv mempress-rocc-4MiBL2-8stream.riscv mempress-rocc-4MiBL2-16stream.riscv mempress-rocc-8MiBL2-1stream.riscv mempress-rocc-8MiBL2-2stream.riscv mempress-rocc-8MiBL2-4stream.riscv mempress-rocc-8MiBL2-8stream.riscv mempress-rocc-8MiBL2-16stream.riscv mempress-rocc-16MiBL2-1stream.riscv mempress-rocc-16MiBL2-2stream.riscv mempress-rocc-16MiBL2-4stream.riscv mempress-rocc-16MiBL2-8stream.riscv mempress-rocc-16MiBL2-16stream.riscv


mempress-rocc-2MiBL2-1stream.riscv: mempress-rocc-2MiBL2-1stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-2MiBL2-2stream.riscv: mempress-rocc-2MiBL2-2stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-2MiBL2-4stream.riscv: mempress-rocc-2MiBL2-4stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-2MiBL2-8stream.riscv: mempress-rocc-2MiBL2-8stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-2MiBL2-16stream.riscv: mempress-rocc-2MiBL2-16stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-4MiBL2-1stream.riscv: mempress-rocc-4MiBL2-1stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-4MiBL2-2stream.riscv: mempress-rocc-4MiBL2-2stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-4MiBL2-4stream.riscv: mempress-rocc-4MiBL2-4stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-4MiBL2-8stream.riscv: mempress-rocc-4MiBL2-8stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-4MiBL2-16stream.riscv: mempress-rocc-4MiBL2-16stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-8MiBL2-1stream.riscv: mempress-rocc-8MiBL2-1stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-8MiBL2-2stream.riscv: mempress-rocc-8MiBL2-2stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-8MiBL2-4stream.riscv: mempress-rocc-8MiBL2-4stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-8MiBL2-8stream.riscv: mempress-rocc-8MiBL2-8stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-8MiBL2-16stream.riscv: mempress-rocc-8MiBL2-16stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-16MiBL2-1stream.riscv: mempress-rocc-16MiBL2-1stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-16MiBL2-2stream.riscv: mempress-rocc-16MiBL2-2stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-16MiBL2-4stream.riscv: mempress-rocc-16MiBL2-4stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-16MiBL2-8stream.riscv: mempress-rocc-16MiBL2-8stream.o
	$(CC) $(BINARY_OPT) -o $@ $^

mempress-rocc-16MiBL2-16stream.riscv: mempress-rocc-16MiBL2-16stream.o
	$(CC) $(BINARY_OPT) -o $@ $^


mempress-rocc-2MiBL2-1stream.o: mempress-rocc.c
	$(CC) -D_2MiBL2 -D_1stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-2MiBL2-2stream.o: mempress-rocc.c
	$(CC) -D_2MiBL2 -D_2stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-2MiBL2-4stream.o: mempress-rocc.c
	$(CC) -D_2MiBL2 -D_4stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-2MiBL2-8stream.o: mempress-rocc.c
	$(CC) -D_2MiBL2 -D_8stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-2MiBL2-16stream.o: mempress-rocc.c
	$(CC) -D_2MiBL2 -D_16stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-4MiBL2-1stream.o: mempress-rocc.c
	$(CC) -D_4MiBL2 -D_1stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-4MiBL2-2stream.o: mempress-rocc.c
	$(CC) -D_4MiBL2 -D_2stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-4MiBL2-4stream.o: mempress-rocc.c
	$(CC) -D_4MiBL2 -D_4stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-4MiBL2-8stream.o: mempress-rocc.c
	$(CC) -D_4MiBL2 -D_8stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-4MiBL2-16stream.o: mempress-rocc.c
	$(CC) -D_4MiBL2 -D_16stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-8MiBL2-1stream.o: mempress-rocc.c
	$(CC) -D_8MiBL2 -D_1stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-8MiBL2-2stream.o: mempress-rocc.c
	$(CC) -D_8MiBL2 -D_2stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-8MiBL2-4stream.o: mempress-rocc.c
	$(CC) -D_8MiBL2 -D_4stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-8MiBL2-8stream.o: mempress-rocc.c
	$(CC) -D_8MiBL2 -D_8stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-8MiBL2-16stream.o: mempress-rocc.c
	$(CC) -D_8MiBL2 -D_16stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-16MiBL2-1stream.o: mempress-rocc.c
	$(CC) -D_16MiBL2 -D_1stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-16MiBL2-2stream.o: mempress-rocc.c
	$(CC) -D_16MiBL2 -D_2stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-16MiBL2-4stream.o: mempress-rocc.c
	$(CC) -D_16MiBL2 -D_4stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-16MiBL2-8stream.o: mempress-rocc.c
	$(CC) -D_16MiBL2 -D_8stream $(OBJECT_OPT) -o $@ $^

mempress-rocc-16MiBL2-16stream.o: mempress-rocc.c
	$(CC) -D_16MiBL2 -D_16stream $(OBJECT_OPT) -o $@ $^


.PHONY: clean
clean:
	rm -f *.riscv
	rm -f *.o
