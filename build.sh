ca65 src/main.asm -o tmp/main.o && \
ld65 tmp/main.o -o program -C src/link.cfg && \
gcc src/main.c -l6502 -o sim -g
