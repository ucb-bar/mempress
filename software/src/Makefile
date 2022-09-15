
CC=riscv64-unknown-elf-gcc
OBJECT_OPT=-fno-common -fno-builtin-printf -specs=htif_nano.specs
BINARY_OPT=-static -specs=htif_nano.specs

SRC=mempress-rocc.c
OBJ=$(SRC:%.c=%.o)
TARGET=$(SRC:%.c=%.riscv)

all: $(TARGET)

$(OBJ): $(SRC)
	$(CC) $(OBJECT_OPT) -o $@ $^

$(TARGET): $(OBJ)
	$(CC) $(BINARY_OPT) -o $@ $^

.PHONY: clean
clean:
	rm -f $(OBJ) $(TARGET)
