build: main.s print.c
	nasm -f elf64 main.s
	gcc -c print.c
	gcc -o main print.o main.o

clean:
	rm main.o print.o main

run: build
	./main
