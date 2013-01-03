all: sha512_drv.so

sha512_drv.so: sha512_lid.c sha512.c
	gcc -o sha512_drv.so -L/usr/local/lib/erlang/lib/erl_interface-3.5.9/lib/ -fpic -shared -lei -lssl -lcrypto -I/usr/include -I/usr/local/lib/erlang/usr/include sha512_lid.c sha512.c

