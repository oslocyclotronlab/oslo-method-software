100 REM    GAMSPEED.BAS        FEBRUARY 25, 1991         J. KANTELE

110 COLOR 14, 4: CLS

120 PRINT "*****************************************************************"

130 PRINT

140 PRINT " PROGRAM TO CALCULATE WEISSKOPF ESTIMATES AND GLOBAL B(E2;up)"

150 PRINT " PREDICTIONS BY RAMAN [Ra87, Ra89] FOR GAMMA-RAY HALF-LIVES"

160 PRINT

170 PRINT "*****************************************************************"

180 PRINT

190 INPUT " GAMMA-RAY ENERGY (MeV) = ", E

200 PRINT

210 INPUT " MASS NUMBER OF NUCLEUS, A = ", A

220 PRINT

230 INPUT " ATOMIC NUMBER Z = ", Z

240 E1 = 6.73 * A ^ (-2 / 3) * E ^ (-3) * (1E-15)

250 E2 = 9.37 * A ^ (-4 / 3) * E ^ (-5) * (1E-09)

260 E3 = 1.98 * A ^ (-2) * E ^ (-7) * .01

270 E4 = 6.3 * A ^ (-8 / 3) * E ^ (-9) * 10000

280 E5 = 2.83 * A ^ (-10 / 3) * E ^ (-11) * (1E+11)

290 M1 = 2.24 * E ^ (-2) * (1E-14)

300 M2 = 3.12 * A ^ (-2 / 3) * E ^ (-5) * (1E-08)

310 M3 = 6.6 * A ^ (-4 / 3) * E ^ (-7) * .01

320 M4 = 2.1 * A ^ (-2) * E ^ (-9) * 100000!

330 M5 = 9.43 * A ^ (-8 / 3) * E ^ (-11) * (1E+11)

340 B = 5.94E-06 * A ^ (4 / 3)

350 T1 = 125 * E ^ (-4) * Z ^ (-2) * A ^ (.69) * (1E-12)

360 T = LOG(2) * T1

370 R = (4.082E+14) * E ^ (-5) * (1E-15) / T1 * 1E-12

380 PRINT

390 PRINT " THE WEISSKOPF ESTIMATES FOR THE HALF-LIVES ARE:"

400 PRINT

410 PRINT USING " E1 = ##.##^^^^ sec     M1 = ##.##^^^^ sec"; E1; M1

420 PRINT USING " E2 = ##.##^^^^ sec     M2 = ##.##^^^^ sec"; E2; M2

430 PRINT USING " E3 = ##.##^^^^ sec     M3 = ##.##^^^^ sec"; E3; M3

440 PRINT USING " E4 = ##.##^^^^ sec     M4 = ##.##^^^^ sec"; E4; M4

450 PRINT USING " E5 = ##.##^^^^ sec     M5 = ##.##^^^^ sec"; E5; M5

460 PRINT

470 PRINT USING " ONE WEISSKOPF-UNIT FOR THE B(E2) IS = ##.##^^^^ e^2b^2"; B

480 PRINT

490 PRINT USING " GLOBAL B(E2;up) PREDICTION FOR 2+(1) = ##.##^^^^ e^2b^2"; R

500 PRINT

510 PRINT USING " GLOBAL PRED. FOR GAMMA T(´) OF 2+(1) = ##.##^^^^ sec"; T

520 PRINT

530 PRINT "*****************************************************************"

540 PRINT

550 INPUT " DO YOU WISH TO CONTINUE? (1 = YES; 2 = NO) YOUR CHOICE = ", M

560 ON M GOTO 160, 570

570 COLOR 7, 0: CLS

580 END

