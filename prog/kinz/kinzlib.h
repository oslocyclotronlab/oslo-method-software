// -*- c++ -*-

#ifndef KINZ_H
#define KINZ_H 1

float Rho(int Z);
float Relkin(float Mi, float Mf, float Mr, float Ti, float Q, float thetar);
float P2rel(float M, float T);

void scoef(int z1, double& mm1, double& m1, double& m2, double& rho, double& atrho,
           double& vfermi, double& lfctr, const double*& pcoef);
bool stop(int z1, double m1/*amu*/, int z2, double m2/*amu*/,
          double ee/*keV*/, double& se/*eV/ang*/, double& sn/*eV/ang*/);

double ziegler1985(int z1, double m1/*amu*/, int z2, double m2/*amu*/,
                   double e/*keV*/, float d/*um*/, int nstep);
double /*keV/ang*/ loss(int z1, double m1/*amu*/, int z2,
                        double m2/*amu*/, double e/*keV*/);
bool zstraggling(int A, int Z, int Ai, int Zi, int Af, int Zf,
                 double Ti, double d, double Ex, double theta,
                 double &Ekf_r, double& dE_r);

#endif /* KINZ_H */
