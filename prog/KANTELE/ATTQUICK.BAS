100 REM     ATTQUICK.BAS        SEPTEMBER 14, 1990        J. KANTELE

110 CLS

120 PRINT "*****************************************************************"

130 PRINT

140 PRINT " SIMPLE PROGRAM TO CALCULATE APPROXIMATE PHOTON HALF-THICKNESSES"

150 PRINT " BASED ON 3RD ORDER POLYNOMIAL FITS TO Z = 4 AND Z = 66 DATA."

160 PRINT " THE ACCURACY IS ABOUT 20 PER CENT FOR 100 keV < E < 5 MeV"

170 PRINT " AND 4 <= Z <= 90. FOR Z < 20, THE LOWER LIMIT IS APRROX. 50 keV."

180 PRINT

190 PRINT "*****************************************************************"

200 PRINT

210 INPUT " ENERGY (keV) = ", E

220 PRINT

230 INPUT " ABSORBER Z = ", Z

240 A = 1.6643 - .2892 * LOG(E) + .067 * (LOG(E)) ^ 2 - .001058 * (LOG(E)) ^ 3

250 B = -21.825 + 6.203 * LOG(E) - .384 * (LOG(E)) ^ 2 - .000689 * (LOG(E)) ^ 3

260 D = EXP(A + (B - A) * ((Z - 4) / 62) ^ 1.2)

270 PRINT

280 PRINT USING " HALF-THICKNESS = ##.##^^^^ g/cm^2"; D

290 PRINT

300 PRINT "*****************************************************************"

310 GOTO 200



