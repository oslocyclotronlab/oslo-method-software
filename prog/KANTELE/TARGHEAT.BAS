100 REM   TARGHEAT.BAS         JANUARY 7, 1992       J. KANTELE

110 COLOR 14, 6: CLS

120 PRINT "*****************************************************************"

130 PRINT

140 PRINT " PROGRAM TO CALCULATE THE UPPER LIMIT OF THE TEMPERATURE OF "

150 PRINT " A UNIFORM TARGET UNDER UNIFORM ION-BEAM BOMBARDMENT. THE BEAM "

160 PRINT " SPOT AND THE TARGET ARE CONCENTRIC CIRCLES WITH RADII R1 AND R2."

170 PRINT " THE HEAT P = I * S * D IS REMOVED BY MERE CONDUCTION P(Cond) OR"

180 PRINT " BY MERE RADIATION, P(Rad). THE TEMPERATURE OF THE ENVIRONMENT IS "

190 PRINT " TAKEN T(0) = 295 K. T(max) FOR EACH CASE IS COMPUTED. "

200 PRINT

210 PRINT " THE DEPENDENCE OF THERMAL CONDUCTIVITY K,  EMISSIVITY e, OR"

220 PRINT " STOPPING POWER S ON THE TEMPERATURE T IS IGNORED. "

230 PRINT

240 PRINT "*****************************************************************"

250 PRINT : DIM TI(20), N(20), RI(20), TU(20), RU(20)

260 PRINT " THE PROGRAM NEEDS DATA ON FOIL MATERIALS. DO YOU WANT TO SEE AN "

270 INPUT " AUXILIARY DATA TABLE? (1 = YES, 2 = NO). YOUR CHOICE = ", M

280 IF M = 1 THEN GOSUB 700

290 COLOR 14, 6: CLS : PRINT

300 INPUT " TARGET THICKNESS (in mg/cm^2), D =  ", D

310 PRINT

320 INPUT " DENSITY OF THE TARGET MATERIAL (in g/cm^3), RHO = ", RHO

330 PRINT

340 INPUT " STOPPING POWER dE/dx (in MeV/mg/cm^2), S =  ", S

350 PRINT

360 INPUT " RADII OF BEAM SPOT AND TARGET (in mm), R1, R2 = ", R1, R2

370 PRINT : IF R2 > R1 THEN 380 ELSE 360

380 INPUT " EMISSIVITY OF THE BEAM SPOT (AVER. OF TWO SIDES), E = ", E

390 PRINT

400 INPUT " BEAM INTENSITY (PARTICLE microAmps), I = ", I

410 PRINT

420 INPUT " THERMAL CONDUCTIVITY OF THE TARGET FOIL (in W/mK), K = ", K

430 PRINT

440 P = I * S * D: T0 = 295

450 C = P / (2 * 3.141593 * K * D / (RHO * 100000!))

460 G = T0 + C * LOG(R2 / R1) + C / 2

470 T1 = G - C / 2 - 273

480 TM = 295 + C * LOG(R2 / R1) + C / 2 - 273

490 PRINT : COLOR 11

500 PRINT USING " BEAM HEATS THE TARGET WITH TOTAL POWER  P = ##.##^^^^ W"; P

510 REM PRINT

520 REM PRINT USING " C = ##.###^^^^ , T1 = #####.# K "; C; T1

530 PRINT

540 PRINT USING " FOR MERE CONDUCTION LOSSES, T(max) = T(center) = ##.##^^^^C "; TM

550 B = 295 ^ 4 + P / (2 * 3.1516 * E * R1 ^ 2 * 5.67E-14)

560 PRINT

570 T = B ^ (1 / 4)

580 PRINT USING " FOR MERE RADIATION LOSSES, T = #### C "; T - 273

590 PRINT

600 IF T >= TM THEN 610 ELSE 620

610 PRINT USING " THE MAX. TEMPERATURE IS LOWER THAN #### C "; TM: GOTO 630

620 IF T < TM THEN PRINT USING " THE MAX. TEMPERATURE IS LOWER THAN #### C "; T - 273

630 PRINT

640 PRINT " NOTE: IN PRACTICE, BECAUSE OF NON-UNIFORMITIES IN BEAM AND/OR"

650 PRINT " TARGET, THE TRUE MAX. TEMPERATURE MAY BE MUCH HIGHER. "

660 PRINT : COLOR 13

670 PRINT " TO QUIT AT ANY TIME, JUST PRESS CTRL-C"

680 COLOR 14: GOTO 230

690 END

700 COLOR 15, 9: CLS : PRINT

710 PRINT " THE DATA IN THIS TABLE ARE FROM DIFFERENT SOURCES, AND ARE IN"

720 PRINT " SOME CASES AVERAGES OR ESTIMATES ONLY. IF A RANGE IS GIVEN FOR"

730 PRINT " THE EMISSIVITY e, IT CORRESPONDS APPROXIMATELY TO TEMPERATURES"

740 PRINT " FROM 100 C TO 500 C. IN MANY CASES, THE ACTUAL EMISSIVITY e IS "

750 PRINT " CLOSE TO THAT OF THE OXIDIZED CASE. K DECREASES SLIGHTLY WITH T."

760 PRINT

770 PRINT " MATERIAL  RHO (g/cm^3)  K (W/mK)  T(melt.) (C)  e(unoxid.)  e(oxid.) "

780 PRINT

790 PRINT "   Ag        10.5          418         961         .02-.35            "

800 PRINT "   Al        2.70          238         660         .02-.06     .15    "

810 PRINT "   Au        19.3          311        1063         .02-.03            "

820 PRINT "   Be        1.85          230        1278                            "

830 PRINT "   Cd        8.65           92         321                            "

840 PRINT "   Cu        8.96          400        1083           .02        .6    "

850 PRINT "   Fe        7.87           82        1535           .05        .74   "

860 PRINT "   Ni        8.90           90        1453         .045-.12     .4    "

870 PRINT "   Pb       11.35           35         328           .05        .63   "

880 PRINT

890 PRINT " HIT A KEY, WHEN READY TO CONTINUE WITH THE TABLE": PRINT

900 IF INKEY$ = "" THEN 900

910 PRINT " MATERIAL  RHO (g/cm^3)  K (W/mK)  T(melt.) (C)  e(unoxid.)  e(oxid.) "

920 PRINT

930 PRINT "   Sn        5.75           63         232           .04              "

940 PRINT "   Ta        16.6           54        2996           .2               "

950 PRINT "   U         18.9           25        1132           .54              "

960 PRINT "   W         19.3          170        3380         .024-.071          "

970 PRINT "   Zr        6.53           21        1852                            "

980 PRINT " Graphite    2.25          .49       >3550           .81              "

990 PRINT " Havar        8.3         14.1       >1000                            "

1000 PRINT " Kapton      1.42       .1-.35         300                            "

1010 PRINT " Mica Musc.   2.8        .5-.7     500-900                            "

1020 PRINT " Mylar        1.4       .15-.4     130-170                            "

1030 PRINT " Teflon       2.2           .2         260                            "

1040 PRINT

1050 PRINT " Approx. e's: Brick .75, Cast iron .2-.6, Paints .4-.9, Paper .8-.9   "

1060 PRINT : PRINT " HIT A KEY WHEN READY FOR THE CALCULATION"

1070 IF INKEY$ = "" THEN 1070

1080 RETURN



