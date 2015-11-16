// -*-c++ -*-

#ifndef AME2003_MASSES_H
#define AME2003_MASSES_H 1

const char* get_element_name(int Z);
double ame2003_get_mass_amu(int A, int Z);
double ame2003_get_Q_keV(int Ai1, int Zi1, int Ai2, int Zi2, int Ao1, int Zo1);

/** Ask for a Z and accept either a number or a name */
void ask_par_Z(const char* ask, int* Z);

#endif
