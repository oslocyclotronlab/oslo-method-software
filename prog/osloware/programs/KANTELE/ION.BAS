100 REM    ION.BAS        SEPTEMBER 7, 1989          J. KANTELE

110 COLOR 15, 1: CLS

120 PRINT "****************************************************************"

130 PRINT

140 PRINT " PROGRAM TO CALCULATE ION VELOCITIES AND MAGNETIC RIGIDITIES"

150 PRINT

160 PRINT "****************************************************************"

170 PRINT

180 INPUT " ION ENERGY (MeV) = ", E

190 PRINT

200 INPUT " ION MASS (a.m.u.) = ", M

210 PRINT

220 INPUT " ION CHARGE (in units of e) = ", Q

230 PRINT

240 V = SQR(E ^ 2 + 2 * E * M * 931.5) / (E + M * 931.5) * 29.98

250 B = V / 29.98

260 BRHO = SQR(E ^ 2 + 1863 * E * M) / (299.8 * Q)

270 PRINT USING " THE VELOCITY IS = ##.### cm/ns; v/c = #.###"; V, B

280 PRINT

290 PRINT USING " THE MAGNETIC RIGIDITY IS = ##.### Tm"; BRHO

300 PRINT

310 GOTO 160

320 END



