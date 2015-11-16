100 REM     QUICKRAN.BAS        SEPTEMBER 20, 1990       J. KANTELE

110 COLOR 10: CLS

120 PRINT "****************************************************************"

130 PRINT

140 PRINT " QUICK PROGRAM TO CALCULATE R(ex) AND R(99) FOR ANY Z "

150 PRINT

160 PRINT "****************************************************************"

170 PRINT

180 INPUT " ABSORBER Z = ", Z

190 PRINT

200 INPUT " ELECTRON ENERGY (keV) E = ", E

210 REM

220 RAL = EXP(-4.2034 + .6453 * LOG(E) + .3506 * LOG(E) ^ 2 - .045575 * LOG(E) ^ 3 + .001758 * LOG(E) ^ 4)

230 REX = RAL * (1.1 - .0055 * Z)

240 R99 = RAL * (1.24 + .00017 * Z - .000032 * Z ^ 2)

250 PRINT

260 COLOR 14: PRINT USING " R(ex) = ##.##^^^^ mg/cm^2; R(99) = ##.##^^^^ mg/cm^2 "; REX; R99

270 COLOR 10: GOTO 170

280 END

