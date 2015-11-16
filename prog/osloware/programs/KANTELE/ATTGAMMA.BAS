100 REM    ATTGAMMA.BAS        SEPTEMBER 10, 1990      J. KANTELE

110 COLOR 15, 1: CLS

120 PRINT "******************************************************************"

130 PRINT

140 PRINT " PROGRAM TO CALCULATE THE ATTENUATION OF PHOTONS FROM 1.56 keV TO "

150 PRINT " 30 MeV, IN ABSORBERS HAVING UP TO 3 COMPONENTS (Z = 1, 4,...,92)."

160 PRINT

170 PRINT " THE CALCULATION IS BASED ON INTERPOLATION BETWEEN DATA POINTS"

180 PRINT " OF E. STORM AND H.I. ISRAEL, Nucl. Data Tables A7 (1970) 565."

190 PRINT

200 PRINT " HALF-THICKNESSES ARE MOSTLY COMPUTED TO BETTER THAN 5 % FOR Z = "

210 PRINT " 4, 6, 13, 29, 50, 82, AND 92. FOR OTHER ELEMENTS AND FOR"

220 PRINT " COMPOSITE  MATERIALS, THE ACCURACY IS TYPICALLY 10 PER CENT."

230 PRINT " THE UNCERTAINTIES MAY BE LARGER FOR LIGHT ELEMENTS AND/OR LOW"

240 PRINT " ENERGIES, ESPECIALLY BELOW 10 keV. DO NOT APPLY TO Z = 2 OR 3."

250 PRINT

260 PRINT " THE PROGRAM GIVES A WARNING IN THE MOST DANGEROUS RANGES."

270 PRINT

280 PRINT "******************************************************************"

290 DIM A(3): DIM C(8): DIM D(8): DIM N(3): DIM Q(3): DIM T(9, 29): DIM Y(10): DIM Z(3)

300 FOR K = 1 TO 9

310     FOR L = 1 TO 29

320         READ T(K, L)

330     NEXT L

340 NEXT K

350 DATA 1, 1.56, 2, 5, 8.98, 8.99, 15, 20, 29.2, 29.21, 50, 88, 88.01, 115.6, 115.61, 200, 350, 500, 700, 1000, 1500, 2000, 3000, 4000, 6000, 10000, 15000, 20000, 30000

360 DATA 1, .356, .655, 1.66, 1.79, 1.79, 1.84, 1.88, 1.94, 1.94, 2.07, 2.29, 2.29, 2.45, 2.45, 2.86, 3.48, 4.01, 4.65, 5.5, 6.74, 7.95, 10, 11.9, 15.4, 21.3, 27.3, 32.2, 39.7

370 DATA 4, .0044, .00943, .172, .873, .873, 2.3, 3.25, 3.8, 3.8, 4.45, 5.05, 5.05, 5.45, 5.45, 6.36, 7.7, 8.94, 10.6, 12.3, 15.1, 17.6, 22.2, 26, 32.6, 42.5, 51.3, 56.4, 63.2

380 DATA 6, .0011, .0023,.0364, .225, .225, .89, 1.67, 2.62, 2.62, 3.72, 4.43, 4.43, 4.78, 4.78, 5.65, 6.7, 8, 9.1, 10.9, 13.4, 15.7, 19.5, 22.8, 28.1, 35.4, 41, 44.2, 47.2

390 DATA 13, .000158, .000305, .00365, .0196, .0196, .0891, .204, .575, .58, 1.9, 3.7, 3.7, 4.4, 4.4, 5.67, 7, 8.22, 9.3, 11.3, 13.9, 16.1, 19.5, 22.4, 26.1, 29.9, 31.9, 32.1, 31.7

400 DATA 29, .000172, .000328, .00368, .0181, .00243, .00938, .0205, .0589, .059, .268, 1.14, 1.14, 1.98, 1.98, 4.49, 6.7, 8.31, 9.7, 11.8, 14.5, 16.5, 19.3, 20.9, 22.4, 22.5, 21.7, 20.4, 18.8

410 DATA 50, .00023, .000424, .000824, .00383, .00383, .0151, .033, .0899, .0158,.0654, .297, .297, .598, .6, 2.14, 5.4, 7.46, 9.2, 12, 15, 16.9, 18.9, 19.5, 19.4, 18, 16.3, 15, 13.4

420 DATA 82, .00033, .00055, .00096, .00416, .00416, .0063, .0081, .0214, .0214, .088, .37, .0917, .18, .18, .697, 2.55, 4.36, 7.0, 9.85, 13.4, 15.2, 16.7, 16.7, 15.9, 14.3, 12.5, 11.4, 10

430 DATA 92, .00022, .00038, .00079, .00304, .00304, .0109, .01, .0158, .0158, .063, .267, .267, .521, .143, .538, 1.85, 3.57, 5.9, 8.9, 12.6, 14.6, 15.8, 15.9, 15.3, 13.7, 11.8, 10.9, 9.45

440 PRINT

450 PRINT " THE PROGRAM NEEDS ABSORBER DATA. DO YOU WANT TO SEE"

460 INPUT " AN AUXILIARY DATA TABLE ? (1 = YES, 2 = N0). YOUR CHOICE = ", A1

470 IF A1 = 1 THEN 1330

480 PRINT : Z(1) = 0: Z(2) = 0: Z(3) = 0

490 INPUT " ENTER NUMBER OF COMPONENT ELEMENTS (< 4), V = ", V

500 PRINT

510 PRINT " ENTER ATOMIC NUMBER(S) OF COMPONENT(S), Z1,.., ZV (ONE PER LINE) "

520 FOR N = 1 TO V

530 INPUT Z(N)

540 NEXT N

550 IF V = 1 THEN A(1) = 1: GOTO 620

560 PRINT

570 PRINT " ENTER FRACTIONS BY WEIGHT (SUM = 1!),  A1,.., AV (ONE PER LINE) "

580 FOR N = 1 TO V

590 INPUT A(N)

600 NEXT N

610 IF ABS(1 - A(1) - A(2) - A(3)) > .001 THEN 570

620 PRINT

630 PRINT " ENTER THE DENSITY OF ABSORBER (g/cm^3); IF NOT KNOWN, GIVE ANY"

640 INPUT " POSITIVE  NUMBER, RHO = ", RHO

650 PRINT

660 PRINT " WITH CORRECT DENSITY, THE ABSORBER THICKNESS MAY ALSO BE ENTERED "

670 INPUT " IN mm (M); OTHERWISE, ONLY IN g/cm^2 (G). 1 = M, 2 = G. CHOICE (1 OR 2) = ", B1

680 PRINT

690 IF B1 = 1 THEN 710

700 INPUT " ENTER ABSORBER THICKNESS (IN g/cm^2) = ", X: GOTO 730

710 INPUT " ENTER ABSORBER THICKNESS (IN mm) = ", XM

720 X = XM * RHO / 10

730 PRINT

740 INPUT " ENTER PHOTON ENERGY (keV) E = ", E

750 FOR J = 1 TO 28

760 IF T(1, J + 1) - E >= 0 THEN 780

770 NEXT J

780 FOR I = 2 TO 9

790 GOSUB 1290

800 NEXT I

810 FOR N = 1 TO V

820 FOR M = 1 TO 8

830 IF T(M + 1, 1) - Z(N) >= 0 THEN 850

840 NEXT M

850 A = T(M + 1, 1) - Z(N): B = T(M + 1, 1) - T(M, 1)

860 IF A = 0 THEN 980

870 FOR I = 1 TO 2

880 N(I) = T(M + I - 1, 1)

890 GOSUB 1310

900 NEXT I

910 N(3) = Z(N): GOSUB 1310

920 IF E < 35 THEN C = 1.5 ELSE C = 1

930 C1 = 1: C2 = 1: C(2) = .03: C(3) = .05: C(4) = .084: C(5) = .134: C(6) = .176: C(7) = .248: C(8) = .274

940 IF E > Q(2) THEN 970

950 IF E > Q(3) THEN C2 = C(M) ELSE C2 = 1

960 IF (E > Q(1)) * (E < Q(3)) THEN C1 = C(M - 1) ELSE C1 = 1

970 D(N) = EXP(LOG(C2 * Y(M + 1)) + (LOG(Y(M) / C1) - LOG(C2 * Y(M + 1))) * (A / B) ^ C): GOTO 990

980 D(N) = Y(M + 1)

990 IF Z(N) > 13 THEN 1010

1000 GOTO 1050

1010 F = T(M + 1, 1)

1020 IF E > EXP(-5.573 + 1.2774 * LOG(F) + .11828 * (LOG(F)) ^ 2 + .005 * (LOG(F)) ^ 3) THEN 1050

1030 COLOR 12

1040 PRINT : PRINT " ----- WARNING: DANGEROUSLY LOW ENERGY FOR GIVEN Z -----"

1050 NEXT N

1060 D1 = 0

1070 FOR I = 1 TO V

1080 D1 = D1 + A(I) / D(I)

1090 NEXT I

1100 PRINT

1110 D = D1 ^ (-1)

1120 DMM = D * 10 / RHO

1130 P = 2 ^ (-X / D)

1140 IF B1 = 2 THEN 1160

1150 PRINT USING " Z = ##;##;##; D(1/2) =##.##^^^^ g/cm^2 =##.##^^^^ mm"; Z(1); Z(2); Z(3); D; DMM: GOTO 1170

1160 PRINT USING " Z = ##;##;##; D(1/2) =##.##^^^^ g/cm^2"; Z(1); Z(2); Z(3); D

1170 PRINT

1180 PRINT USING " PENETRATING FRACTION OF INTENSITY IS = ##.##^^^^"; P: COLOR 15, 1

1190 PRINT

1200 PRINT "******************************************************************"

1210 PRINT

1220 PRINT " DO YOU WISH TO CONTINUE? ENTER YOUR CHOICE BY NUMBER: "

1230 PRINT

1240 INPUT " 1 = SAME ABSORBER; 2 = ANOTHER ABS.; 3 = FROM START; 4 = QUIT. YOUR CHOICE = ", M1

1250 ON M1 GOTO 730, 1260, 1270, 1280

1260 RESTORE: CLS : COLOR 15, 1: GOTO 440

1270 RESTORE: GOTO 100

1280 END

1290 Y(I) = EXP(LOG(T(I, J)) + (LOG(E) - LOG(T(1, J))) / (LOG(T(1, J + 1)) - LOG(T(1, J))) * (LOG(T(I, J + 1)) - LOG(T(I, J))))

1300 RETURN

1310 Q(I) = EXP(-7.708 + 4.488 * LOG(N(I)) - .681 * (LOG(N(I))) ^ 2 + .06585 * (LOG(N(I))) ^ 3)

1320 RETURN

1330 COLOR 14, 4: CLS

1340 PRINT "  MATERIAL (Z)   DENSITY (g/cm^3)   MATERIAL (Z)   DENSITY (g/cm^3)  "

1350 PRINT "    Be (4)            1.85             Cd (48)           8.65        "

1360 PRINT "    Graphite (6)      2.25             Sn (50)           7.28 (5.75) "

1370 PRINT "    NE Plastic (6)    1.03             Sb (51)           6.69        "

1380 PRINT "    Al (13)           2.70             Te (52)           6.24        "

1390 PRINT "    Si (14)           2.33             Ta (73)          16.6         "

1400 PRINT "    Fe (26)           7.87             W  (74)          19.3         "

1410 PRINT "    Ni (28)           8.9              Pt (78)          21.45        "

1420 PRINT "    Cu (29)           8.96             Au (79)          19.3         "

1430 PRINT "    Ge (32)           5.32             Pb (82)          11.34        "

1440 PRINT "    Pd (46)          12.0              Bi (83)           9.75        "

1450 PRINT "    Ag (47)          10.5              U  (92)          18.9         "

1460 PRINT " -----------------------------------------------------------------"

1470 PRINT "    SOME COMPOUNDS:      MASS FRACTIONS      DENSITY (g/cm^3) "

1480 PRINT "    Paraffin (CnH2n+2)      .8, .2                 .85 "

1490 PRINT "    Lucite (C5H8O2)n    .60, .08, .32          1.18 - 1.19"

1500 PRINT "    Glass (Si02 etc)       <.5, <.5                2.5 "

1510 PRINT "    NaI (11, 53)          .153, .847               3.67 "

1520 PRINT "    BaF2 (56,9)           .783, .217               4.89  "

1530 PRINT "    BGO (Bi4Ge3O12)     .671, .175, .154           7.13  "

1540 PRINT "    Hevimet (W,Ni,Cu)    .9, .075, .025            17  "

1550 PRINT " *******************************************************************"

1560 COLOR 15, 1, 1

1570 GOTO 480

