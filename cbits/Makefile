all: *.c *.h
	gcc bin.c bitio.c model.c encode.c decode.c log.c -o arith

test_bitio: bitio.c test_bitio.c
	gcc test_bitio.c bitio.c -o test_bitio
