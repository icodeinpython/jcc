CC = gcc
CFLAGS = -Wall -Og -g -Iinclude -Wextra

C_SRC = $(shell find src -type f -name '*.c')

OBJ = $(C_SRC:.c=.o)

.PHONY: all clean test

all: jcc

jcc: $(OBJ)
	$(CC) $(CFLAGS) -o jcc $(OBJ)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	find -type f -name '*.o' -delete
	rm -f jcc
	rm -f test/*.s
	find test -type f -executable -delete
	rm -f lexdump

test: jcc
	bash ./test/test.sh

lexdump: src/lexer.c
	$(CC) $(CFLAGS) -o lexdump src/lexer.c tools/lexdump.c