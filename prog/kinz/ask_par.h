
#ifndef ASK_PAR_H
#define ASK_PAR_H 1

#ifdef __cplusplus
extern "C" {
#endif

/* these functions are used to get input: while there are command-line
   parameters, they are used, otherwise the user is asked to type in a
   value
*/

#ifdef __GNUC__
/* tell gcc that the arguments are like printf so it can check types and count */
const char* ask_par_input(const char *ask, ...) __attribute__ ((format (printf, 1, 2)));
#endif

void ask_par_init(int argc, char** argv);

const char* ask_par_input(const char* ask, ...);
void ask_par_int(const char* ask, int* i);
void ask_par_long_int(const char* ask, long int* i);
void ask_par_float(const char* ask, float* f);
void ask_par_double(const char* ask, double* d);
char ask_par_option(const char* ask);

#ifdef __cplusplus
}
#endif

#endif /* ASK_PAR_H */
