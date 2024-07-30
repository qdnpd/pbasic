#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <lib6502.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

uint8_t mem[0xffff+1] = {};
FILE *sourcefile = NULL;
char codeline[64];


int strput( M6502 *mpu, uint16_t address, uint8_t data )
{
    uint8_t ptrLo = mem[data];
    uint8_t ptrHi = mem[data+1];
    uint16_t ptr = (ptrHi << 8) | ptrLo;
    uint8_t *buffer = &mem[ptr];

    fwrite(&buffer[1], buffer[0], 1, stdout );
    putchar('\n');

    return 0;
}

int strget( M6502 *mpu, uint16_t address, uint8_t data )
{
    uint8_t ptrLo = mem[data];
    uint8_t ptrHi = mem[data+1];
    uint16_t ptr = (ptrHi << 8) | ptrLo;
    uint8_t *buffer = &mem[ptr];
    char input[64];

    if(sourcefile) {
        fgets(input, 64, sourcefile);
        if( feof(sourcefile) )
        {
            fclose(sourcefile);
            sourcefile = NULL;
            return strget(mpu, address, data);
        }

        printf("%s", input);
    }
    else
        fgets(input, 64, stdin );

    memcpy( &buffer[1], input, 63 );

    uint8_t size = 0;
    for( ; buffer[size+1] != '\n'; size++ )
        ;
    buffer[0] = size;

    return 0;
}

int display_inputreq( M6502 *mpu, uint16_t address, uint8_t data )
{
    printf("> ");
    return 0;
}

int simend( M6502 *mpu, uint16_t address, uint8_t data )
{
    M6502_delete(mpu);
    exit(0);
}

int dump( M6502 *mpu, uint16_t address, uint8_t data )
{
    char buffer[64];
    FILE *file = fopen("memdump", "w");

    printf("------------DEBUG------------\n");
    printf("A:%02X X:%02X Y:%02X ARGS:%02X%02X %02X%02X\n", mem[0x70], mem[0x71], mem[0x72], mem[1], mem[0], mem[3], mem[2] );
    fwrite( mem, 0xffff, 1, file );
    fclose(file);
    getchar();

    return 0;
}

int main( int argc, char *argv[] )
{
    if(argc > 1) {
        sourcefile = fopen(argv[1], "r");
        if(!sourcefile)
            exit(1);
    }

    FILE *file = fopen("program", "r");
    if( file == NULL )
        return 0;
    fread( mem, 1, 0xffff, file );
    fclose(file);

    M6502 *mpu = M6502_new( 0, mem, 0 );
    M6502_setCallback( mpu, write, 0xfff0, strget );
    M6502_setCallback( mpu, write, 0xfff1, strput );
    M6502_setCallback( mpu, write, 0xfff2, simend );
    M6502_setCallback( mpu, read, 0xfff3, display_inputreq );
    M6502_setCallback( mpu, write, 0xfff3, dump );

    M6502_reset(mpu);

    while(1) {
        struct timespec begin, end;
        uint16_t oldpc;

        clock_gettime(CLOCK_REALTIME, &begin);
        M6502_tick(mpu);
        clock_gettime(CLOCK_REALTIME, &end);

        double spent = ((double)(end.tv_nsec - begin.tv_nsec) / 1000) +
                       ((double)(end.tv_sec - begin.tv_sec) * 1000000);

        usleep(1);
        //if(spent < 1) {
        //    usleep(1-spent);
        //}

        // if(mpu->registers->pc != oldpc) {
        //     oldpc = mpu->registers->pc;
        //     char buffer[128];
        //     M6502_disassemble(mpu, oldpc, buffer);
        //
        //     printf("%s\n", buffer);
        // }
    }
    // M6502_run(mpu);
}
