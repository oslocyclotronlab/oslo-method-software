
#include "ame2003_masses.h"

#include "ask_par.h"

#include <stdio.h>
#include <ctype.h>

static void prog_Qvalue()
{
    int Zi1=1, Zi2=22, Zo1=2, Ai1=1, Ai2=46, Ao1=4;
    printf(" Calculating Q-value for i1 + i2 -> o1 + o2 with AME 2003 tables:\n");
    ask_par_int(" Give A for i1 (%3d): ",      &Ai1);
    ask_par_Z  (" Give Z for i1 (%3d/%-2s): ", &Zi1);
    ask_par_int(" Give A for i2 (%3d): ",      &Ai2);
    ask_par_Z  (" Give Z for i2 (%3d/%-2s): ", &Zi2);
    ask_par_int(" Give A for o1 (%3d): ",      &Ao1);
    ask_par_Z  (" Give Z for o1 (%3d/%-2s): ", &Zo1);

    const double Q = ame2003_get_Q_keV(Ai1, Zi1, Ai2, Zi2, Ao1, Zo1);

    if( Q != -1e22 )
        printf(" Q-value is %8.5g MeV\n", Q/1e3 );
    else
        printf(" No values found in AME 2003\n" );
}

static void prog_AME2003mass()
{
    int Z=22, A=46;
    ask_par_int(" Give A for nucleus (%3d): ", &A);
    ask_par_Z  (" Give Z for nucleus (%3d/%-2s): ", &Z);

    const double mass_amu = ame2003_get_mass_amu(A, Z);

    if( mass_amu != 0 )
        printf(" AME2003 mass is %8.6g amu\n", mass_amu );
    else
        printf(" No value found in AME 2003\n" );
}

int main(int argc, char* argv[])
{
    ask_par_init( argc, argv );

    char cmd;
    do {  
        const char *input = ask_par_input
            ("\n\n\n"
             " Menu: Q-Value Mass(AME2003) Exit\n"
             " Press first letter : ");
        cmd = tolower(input[0]);
        switch (cmd) {
        case 'q': prog_Qvalue(); break;
        case 'm': prog_AME2003mass(); break;
        case 'e': break;
        default : printf(" Invalid choice. Try again\n");
        }
    } while (cmd != 'e');
    
    return 0;
}
