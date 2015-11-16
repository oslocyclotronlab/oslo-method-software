
#include "ask_par.h"

#include <stdarg.h>
#include <stdio.h>

/* these functions are used to get input: while there are command-line
   parameters, they are used, otherwise the user is asked to type in a
   value
*/

static int argc, arg=1;
static char** argv;
static char tmp[128];
static const char* input;

const char* ask_par_input(const char* ask, ...)
{
    if( arg < argc ) {
        input = argv[arg++];
    } else {
        va_list ap;
        va_start(ap, ask);
        vprintf(ask, ap);
        va_end(ap);

        // suppress stupid glibc 'security' warning
        if( !fgets(tmp, sizeof(tmp), stdin) ) { }

        input = tmp;
    }
    return input;
}

void ask_par_int(const char* ask, int* i)
{
    const char* r = ask_par_input(ask, *i);
    sscanf(r,"%d", i);
}

void ask_par_long_int(const char* ask, long int* i)
{
    const char* r = ask_par_input(ask, *i);
    sscanf(r,"%ld", i);
}

void ask_par_float(const char* ask, float* f)
{
    const char* r = ask_par_input(ask, *f);
    sscanf(r,"%f", f);
}

void ask_par_double(const char* ask, double* d)
{
    const char* r = ask_par_input(ask, *d);
    sscanf(r,"%lf", d);
}

void ask_par_init(int Argc, char** Argv)
{
    argc = Argc;
    argv = Argv;
}

/* for emacs */
/*** Local Variables: ***/
/*** indent-tabs-mode: nil ***/
/*** c-basic-offset: 4 ***/
/*** End: ***/
