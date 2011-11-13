#include "stdio.h"
#include "string.h"

char gates_outputs[55] = {0};
char registers[0];
char circuit_outputs[9];
char circuit_inputs[16];

int main (int argc, char ** argv)
 {
    int read_bytes;
    int cycles;
    int i;
    int step_by_step;
   
    if (argc < 2 || argc > 3 || (argc == 3 && strcmp(argv[1],"-s")))
     {
        fprintf (stderr, "Usage: %s [-s] CYCLES\n", argv[0]);
        return -1;
     }
    for (i = 0 ; argv[argc-1][i] ; i++)
     {
        if ( argv[argc-1][i] < '0' || argv[argc-1][i] > '9' )
         {
            fprintf (stderr, "Usage: %s [-s] CYCLES\n", argv[0]);
            return -1;
         }
     }
    sscanf (argv[argc-1], "%d", &cycles);

    step_by_step = argc == 3;

    read_bytes = fread (circuit_inputs,1, 16, stdin);
    if (read_bytes != 16)
     {
        fprintf (stderr, "Incomplete input (%d input bits needed)\n",
                        16);
        return -1;
     }
    
    for (i = 0 ; i < 16 ; i++)
     {
       if (circuit_inputs[i] == '0')
         {
            circuit_inputs[i] = 0;
         }
        else if (circuit_inputs[i] == '1')
         {
            circuit_inputs[i] = 1;
         }
        else
         {
            fprintf (stderr, "Bad input format (input must consist of \
0s and 1s)\n");
            return -1;
         }
     }

    gates_outputs[0] = 0;
    gates_outputs[1] = 1;

    gates_outputs[2] = circuit_inputs[0];
    gates_outputs[3] = circuit_inputs[1];
    gates_outputs[4] = circuit_inputs[2];
    gates_outputs[5] = circuit_inputs[3];
    gates_outputs[6] = circuit_inputs[4];
    gates_outputs[7] = circuit_inputs[5];
    gates_outputs[8] = circuit_inputs[6];
    gates_outputs[9] = circuit_inputs[7];
    gates_outputs[10] = circuit_inputs[8];
    gates_outputs[11] = circuit_inputs[9];
    gates_outputs[12] = circuit_inputs[10];
    gates_outputs[13] = circuit_inputs[11];
    gates_outputs[14] = circuit_inputs[12];
    gates_outputs[15] = circuit_inputs[13];
    gates_outputs[16] = circuit_inputs[14];
    gates_outputs[17] = circuit_inputs[15];

    for (i = 0 ; i <= cycles ; i++ )
     {


        gates_outputs[18] = gates_outputs[2] != gates_outputs[10];
        gates_outputs[19] = gates_outputs[2] && gates_outputs[10];
        gates_outputs[20] = gates_outputs[3] != gates_outputs[11];
        gates_outputs[21] = gates_outputs[19] != gates_outputs[20];
        gates_outputs[22] = gates_outputs[19] && gates_outputs[20];
        gates_outputs[23] = gates_outputs[3] && gates_outputs[11];
        gates_outputs[24] = gates_outputs[23] || gates_outputs[22];
        gates_outputs[25] = gates_outputs[4] != gates_outputs[12];
        gates_outputs[26] = gates_outputs[24] != gates_outputs[25];
        gates_outputs[27] = gates_outputs[24] && gates_outputs[25];
        gates_outputs[28] = gates_outputs[4] && gates_outputs[12];
        gates_outputs[29] = gates_outputs[28] || gates_outputs[27];
        gates_outputs[30] = gates_outputs[5] != gates_outputs[13];
        gates_outputs[31] = gates_outputs[29] != gates_outputs[30];
        gates_outputs[32] = gates_outputs[29] && gates_outputs[30];
        gates_outputs[33] = gates_outputs[5] && gates_outputs[13];
        gates_outputs[34] = gates_outputs[33] || gates_outputs[32];
        gates_outputs[35] = gates_outputs[6] != gates_outputs[14];
        gates_outputs[36] = gates_outputs[34] != gates_outputs[35];
        gates_outputs[37] = gates_outputs[34] && gates_outputs[35];
        gates_outputs[38] = gates_outputs[6] && gates_outputs[14];
        gates_outputs[39] = gates_outputs[38] || gates_outputs[37];
        gates_outputs[40] = gates_outputs[7] != gates_outputs[15];
        gates_outputs[41] = gates_outputs[39] != gates_outputs[40];
        gates_outputs[42] = gates_outputs[39] && gates_outputs[40];
        gates_outputs[43] = gates_outputs[7] && gates_outputs[15];
        gates_outputs[44] = gates_outputs[43] || gates_outputs[42];
        gates_outputs[45] = gates_outputs[8] != gates_outputs[16];
        gates_outputs[46] = gates_outputs[44] != gates_outputs[45];
        gates_outputs[47] = gates_outputs[44] && gates_outputs[45];
        gates_outputs[48] = gates_outputs[8] && gates_outputs[16];
        gates_outputs[49] = gates_outputs[48] || gates_outputs[47];
        gates_outputs[50] = gates_outputs[9] != gates_outputs[17];
        gates_outputs[51] = gates_outputs[49] != gates_outputs[50];
        gates_outputs[52] = gates_outputs[49] && gates_outputs[50];
        gates_outputs[53] = gates_outputs[9] && gates_outputs[17];
        gates_outputs[54] = gates_outputs[53] || gates_outputs[52];

        if (i == cycles || step_by_step )
         {
            circuit_outputs[0] = gates_outputs[18] ? '1' : '0' ;
            circuit_outputs[1] = gates_outputs[21] ? '1' : '0' ;
            circuit_outputs[2] = gates_outputs[26] ? '1' : '0' ;
            circuit_outputs[3] = gates_outputs[31] ? '1' : '0' ;
            circuit_outputs[4] = gates_outputs[36] ? '1' : '0' ;
            circuit_outputs[5] = gates_outputs[41] ? '1' : '0' ;
            circuit_outputs[6] = gates_outputs[46] ? '1' : '0' ;
            circuit_outputs[7] = gates_outputs[51] ? '1' : '0' ;
            circuit_outputs[8] = gates_outputs[54] ? '1' : '0' ;

            fwrite (circuit_outputs, 1, 9, stdout);

            fprintf (stdout, "\n");
         }

     }

    return 0;
 }

