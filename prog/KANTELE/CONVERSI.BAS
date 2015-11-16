100 REM     CONVERSI.BAS            APRIL 25, 1993            J. KANTELE

110 COLOR 14, 5: CLS : PRINT

120 PRINT "********************************************************************"

130 PRINT

140 PRINT " THE PROGRAM CALCULATES THE VALUES OF THE FOLLOWING QUANTITIES"

150 PRINT " IN SEVERAL DIFFERENT UNITS: "

160 PRINT

170 PRINT "     1. LENGTH"

180 PRINT "     2. AREA"

190 PRINT "     3. VOLUME"

200 PRINT "     4. MASS/WEIGHT"

210 PRINT "     5. PRESSURE"

220 PRINT "     6. ENERGY, WORK"

230 PRINT "     7. POWER"

240 PRINT "     8. TIME"

250 PRINT "     9. ANGLE"

260 PRINT "    10. TEMPERATURE"

270 PRINT

280 PRINT "     NOTE: SOME UNITS, SUCH AS GALLON, POUND, CALORIE, BTU,..., HAVE "

290 PRINT "     MANY DEFINITIONS. HERE, THE MOST COMMON ONES (MOSTLY THE U.S., "

300 PRINT "     OR THE THERMOCHEMICAL UNITS)  ARE USED. "

310 PRINT

320 PRINT "********************************************************************"

330 PRINT : COLOR 15

340 INPUT " GIVE THE QUANTITY BY NUMBER (1...10; QUIT = CTRL-C): ", N

350 COLOR 14, 4: CLS : PRINT

360 ON N GOTO 370, 670, 1020, 1390, 1690, 2030, 2400, 2720, 3030, 3290

370 COLOR 14, 4: CLS : PRINT

380 PRINT " YOUR INPUT UNIT OF LENGTH? [1 = micrometer; 2 = mm; 3 = m; 4 = km;"

390 PRINT

400 PRINT " 5 = mil (= 0.001 inches); 6 = inch; 7 = foot; 8 = yard; 9 = mile)]"

410 PRINT : DIM R(9, 9), K(9, 9)

420 INPUT " ENTER THE NUMBER OF THE UNIT (1...9): ", N1

430 PRINT

440 INPUT " ENTER THE NUMERICAL VALUE, L = ", L

450 PRINT

460 R(1, 1) = 1: R(1, 2) = 1000: R(1, 3) = 1000000!: R(1, 4) = 1E+09: R(1, 5) = 25.4

470 R(1, 6) = 25400: R(1, 7) = 304800!: R(1, 8) = 914400!: R(1, 9) = 1609344000#

480 FOR I = 1 TO 9

490 FOR J = 1 TO 9

500 K(J, I) = R(1, I) / R(1, J)

510 P(J) = L * K(J, N1)

520 NEXT J, I

530 PRINT " THE LENGTH IN DIFFERENT UNITS: "

540 PRINT : COLOR 15

550 PRINT USING " ##.#####^^^^ micrometers "; P(1)

560 PRINT USING " ##.#####^^^^ mm "; P(2)

570 PRINT USING " ##.#####^^^^ m "; P(3)

580 PRINT USING " ##.#####^^^^ km "; P(4)

590 PRINT USING " ##.#####^^^^ mils "; P(5)

600 PRINT USING " ##.#####^^^^ inches "; P(6)

610 PRINT USING " ##.#####^^^^ feet "; P(7)

620 PRINT USING " ##.#####^^^^ yards "; P(8)

630 PRINT USING " ##.#####^^^^ miles "; P(9)

640 PRINT : COLOR 14

650 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N21

660 ON N21 GOTO 370, 110

670 COLOR 14, 4: CLS : PRINT

680 PRINT " YOUR INPUT UNIT OF AREA? [1 = mm^2; 2 = cm^2; 3 = m^2; "

690 PRINT

700 PRINT " 4 = Eur. acre; 5 = ha; 6 = km^2; 7 = squ-inch; 8 = squ-foot; "

710 PRINT

720 PRINT " 9 = square yard; 10 = acre; 11 = square mile ]"

730 PRINT : DIM RA(11, 11), KA(11, 11), PA(11)

740 INPUT " ENTER THE NUMBER OF THE UNIT (1...11): ", N2

750 PRINT

760 INPUT " ENTER THE NUMERICAL VALUE, A = ", L

770 PRINT

780 RA(1, 1) = 1: RA(1, 2) = 100: RA(1, 3) = 1000000!: RA(1, 4) = 1E+08

790 RA(1, 5) = 1E+10: RA(1, 6) = 1E+12: RA(1, 7) = 645.16: RA(1, 8) = 92903!

800 RA(1, 9) = 836127.36#: RA(1, 10) = 4.046873E+09: RA(1, 11) = 2.589988E+12

810 FOR I = 1 TO 11

820 FOR J = 1 TO 11

830 KA(J, I) = RA(1, I) / RA(1, J)

840 PA(J) = L * KA(J, N2)

850 NEXT J, I

860 PRINT " THE AREA IN DIFFERENT UNITS: "

870 PRINT : COLOR 15

880 PRINT USING " ##.#####^^^^ mm^2 "; PA(1)

890 PRINT USING " ##.#####^^^^ cm^2 "; PA(2)

900 PRINT USING " ##.#####^^^^ m^2 "; PA(3)

910 PRINT USING " ##.#####^^^^ European acres (= 100 m^2) "; PA(4)

920 PRINT USING " ##.#####^^^^ ha "; PA(5)

930 PRINT USING " ##.#####^^^^ km^2 "; PA(6)

940 PRINT USING " ##.#####^^^^ square inches "; PA(7)

950 PRINT USING " ##.#####^^^^ square feet "; PA(8)

960 PRINT USING " ##.#####^^^^ square yards "; PA(9)

970 PRINT USING " ##.#####^^^^ acres "; PA(10)

980 PRINT USING " ##.#####^^^^ square miles "; PA(11)

990 PRINT : COLOR 14

1000 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N22

1010 ON N22 GOTO 670, 110

1020 COLOR 14, 4: CLS : PRINT

1030 PRINT " YOUR INPUT UNIT OF VOLUME? [1 = mm^3; 2 = ml (cm^3); 3 = litre; "

1040 PRINT

1050 PRINT " 4 = m^3; 5 = cubic inch; 6 = fluid ounce; 7 = pint; 8 = quart; "

1060 PRINT

1070 PRINT " 9 = gallon (U.S.); 10 = cubic foot; 11 = bushel; 12 = oil barrel ]"

1080 PRINT : DIM RT(12, 12), KT(12, 12), PT(12)

1090 INPUT " ENTER THE NUMBER OF THE UNIT (1...12): ", N3

1100 PRINT

1110 INPUT " ENTER THE NUMERICAL VALUE, V = ", L

1120 PRINT

1130 RT(1, 1) = 1: RT(1, 2) = 1000: RT(1, 3) = 1000000!: RT(1, 4) = 1E+09

1140 RT(1, 5) = 16387.06: RT(1, 6) = 29573.53: RT(1, 7) = 473176.5

1150 RT(1, 8) = 946352.95#: RT(1, 9) = 3785411.8#: RT(1, 10) = 28316847#

1160 RT(1, 11) = 3.52391E+07: RT(1, 12) = 1.589873E+08

1170 FOR I = 1 TO 12

1180 FOR J = 1 TO 12

1190 KT(J, I) = RT(1, I) / RT(1, J)

1200 PT(J) = L * KT(J, N3)

1210 NEXT J, I

1220 PRINT " THE VOLUME IN DIFFERENT UNITS: "

1230 PRINT : COLOR 15

1240 PRINT USING " ##.#####^^^^ mm^3 "; PT(1)

1250 PRINT USING " ##.#####^^^^ cm^3 (ml) "; PT(2)

1260 PRINT USING " ##.#####^^^^ dm^3 (l) "; PT(3)

1270 PRINT USING " ##.#####^^^^ m^3 "; PT(4)

1280 PRINT USING " ##.#####^^^^ cubic inches"; PT(5)

1290 PRINT USING " ##.#####^^^^ fluid ounces "; PT(6)

1300 PRINT USING " ##.#####^^^^ pints "; PT(7)

1310 PRINT USING " ##.#####^^^^ quarts "; PT(8)

1320 PRINT USING " ##.#####^^^^ gallons (U.S.) "; PT(9)

1330 PRINT USING " ##.#####^^^^ cubic feet "; PT(10)

1340 PRINT USING " ##.#####^^^^ bushels "; PT(11)

1350 PRINT USING " ##.#####^^^^ barrels "; PT(12)

1360 PRINT : COLOR 14

1370 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N23

1380 ON N23 GOTO 1020, 110

1390 COLOR 14, 4: CLS : PRINT

1400 PRINT " YOUR INPUT UNIT OF MASS? [1 = mg; 2 = g; 3 = kg; 4 = tonne (1000 kg); "

1410 PRINT

1420 PRINT " 5 = grain; 6 = ounce; 7 = pound (lb); 8 = ton (2000 lb) ]"

1430 PRINT : DIM RM(8, 8), KM(8, 8), PM(8)

1440 INPUT " ENTER THE NUMBER OF THE UNIT (1...8): ", N4

1450 PRINT

1460 INPUT " ENTER THE NUMERICAL VALUE, M = ", L

1470 PRINT

1480 RM(1, 1) = 1: RM(1, 2) = 1000: RM(1, 3) = 1000000!: RM(1, 4) = 1E+09

1490 RM(1, 5) = 64.9891: RM(1, 6) = 28349.52: RM(1, 7) = 453592.4

1500 RM(1, 8) = 9.071847E+08

1510 FOR I = 1 TO 8

1520 FOR J = 1 TO 8

1530 KM(J, I) = RM(1, I) / RM(1, J)

1540 PM(J) = L * KM(J, N4)

1550 NEXT J, I

1560 PRINT " THE MASS IN DIFFERENT UNITS: "

1570 PRINT : COLOR 15

1580 PRINT USING " ##.#####^^^^ mg "; PM(1)

1590 PRINT USING " ##.#####^^^^ g "; PM(2)

1600 PRINT USING " ##.#####^^^^ kg "; PM(3)

1610 PRINT USING " ##.#####^^^^ tonnes (1000 kg) "; PM(4)

1620 PRINT USING " ##.#####^^^^ grains "; PM(5)

1630 PRINT USING " ##.#####^^^^ ounces "; PM(6)

1640 PRINT USING " ##.#####^^^^ pounds (lb) "; PM(7)

1650 PRINT USING " ##.#####^^^^ tons (2000 lbs) "; PM(8)

1660 PRINT : COLOR 14

1670 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N24

1680 ON N24 GOTO 1390, 110

1690 COLOR 14, 4: CLS : PRINT

1700 PRINT " NOTE: THE SI UNIT OF PRESSURE IS 1 Pa (Pascal) = 1 N/m^2 "

1710 PRINT

1720 PRINT " YOUR INPUT UNIT OF PRESSURE? [1 = Pa; 2 = kPa (1000 Pa);  "

1730 PRINT

1740 PRINT " 3 = mb (millibar); 4 = b (bar); 5 = torr (mmHg); 6 = atm (std);"

1750 PRINT

1760 PRINT " 7 = atm (technical atmosphere); 8 = psi (lbs/squ-inch) ]"

1770 PRINT : DIM RP(8, 8), KP(8, 8), PP(8)

1780 INPUT " ENTER THE NUMBER OF THE UNIT (1...8): ", N5

1790 PRINT

1800 INPUT " ENTER THE NUMERICAL VALUE, P = ", L

1810 PRINT

1820 RP(1, 1) = 1: RP(1, 2) = 1000: RP(1, 3) = 100: RP(1, 4) = 100000!

1830 RP(1, 5) = 133.322: RP(1, 6) = 101325!: RP(1, 7) = 98066.5

1840 RP(1, 8) = 6894.757

1850 FOR I = 1 TO 8

1860 FOR J = 1 TO 8

1870 KP(J, I) = RP(1, I) / RP(1, J)

1880 PP(J) = L * KP(J, N5)

1890 NEXT J, I

1900 PRINT " THE PRESSURE IN DIFFERET UNITS: "

1910 PRINT : COLOR 15

1920 PRINT USING " ##.#####^^^^ Pa "; PP(1)

1930 PRINT USING " ##.#####^^^^ kPa "; PP(2)

1940 PRINT USING " ##.#####^^^^ mb "; PP(3)

1950 PRINT USING " ##.#####^^^^ b (bar) "; PP(4)

1960 PRINT USING " ##.#####^^^^ torr (mmHg) "; PP(5)

1970 PRINT USING " ##.#####^^^^ atm (standard) "; PP(6)

1980 PRINT USING " ##.#####^^^^ atm (technical) "; PP(7)

1990 PRINT USING " ##.#####^^^^ psi (lbs/squ-inch) "; PP(8)

2000 PRINT : COLOR 14

2010 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N25

2020 ON N25 GOTO 1690, 110

2030 COLOR 14, 4: CLS : PRINT

2040 PRINT "  YOUR INPUT UNIT OF ENERGY?"

2050 PRINT

2060 PRINT " [1 = erg (= dyne x cm); 2 = J (joule = Nm = Ws = VC) "

2070 PRINT

2080 PRINT "  3 = kJ; 4 = cal; 5 = kcal; 6 = kWh; 7 = GWh (= 10^6 kWh);"

2090 PRINT

2100 PRINT "  8 = BTU; 9 = eV (electron volt); 10 = keV; 11 = MeV ]"

2110 PRINT : DIM RE(11, 11), KE(11, 11), PE(11)

2120 INPUT " ENTER THE NUMBER OF THE UNIT (1...11): ", N6

2130 PRINT

2140 INPUT " ENTER THE NUMERICAL VALUE, W = ", L

2150 PRINT

2160 RE(1, 1) = 1: RE(1, 2) = 1E+07: RE(1, 3) = 1E+10: RE(1, 4) = 4.184E+07

2170 RE(1, 5) = 4.184E+10: RE(1, 6) = 3.6E+13: RE(1, 7) = 3.6E+19: RE(1, 8) = 1.05435E+10

2180 RE(1, 9) = 1.60219E-12: RE(1, 10) = 1.60219E-09: RE(1, 11) = 1.60219E-06

2190 FOR I = 1 TO 11

2200 FOR J = 1 TO 11

2210 KE(J, I) = RE(1, I) / RE(1, J)

2220 PE(J) = L * KE(J, N6)

2230 NEXT J, I

2240 PRINT " THE ENERGY IN DIFFERENT UNITS: "

2250 PRINT : COLOR 15

2260 PRINT USING " ##.#####^^^^ erg "; PE(1)

2270 PRINT USING " ##.#####^^^^ J (joule) "; PE(2)

2280 PRINT USING " ##.#####^^^^ kJ (kilojoule) "; PE(3)

2290 PRINT USING " ##.#####^^^^ cal "; PE(4)

2300 PRINT USING " ##.#####^^^^ kcal "; PE(5)

2310 PRINT USING " ##.#####^^^^ kWh "; PE(6)

2320 PRINT USING " ##.#####^^^^ GWh "; PE(7)

2330 PRINT USING " ##.#####^^^^ BTU "; PE(8)

2340 PRINT USING " ##.#####^^^^ eV "; PE(9)

2350 PRINT USING " ##.#####^^^^ keV "; PE(10)

2360 PRINT USING " ##.#####^^^^ MeV "; PE(11)

2370 PRINT : COLOR 14

2380 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N26

2390 ON N26 GOTO 2030, 110

2400 COLOR 14, 4: CLS : PRINT

2410 PRINT " YOUR INOUT UNIT OF POWER? [1 = erg/s (= dyne x cm/s); 2 = W (= J/s);"

2420 PRINT

2430 PRINT " 3 = kW; 4 = h (horsepower); 5 = cal/s; 6 = lb x feet/s;"

2440 PRINT

2450 PRINT " 7 = BTU/s; 8 = BTU/h ]"

2460 PRINT : DIM RTE(8, 8), KTE(8, 8), PTE(8)

2470 INPUT " ENTER THE NUMBER OF THE UNIT (1...8): ", N7

2480 PRINT

2490 INPUT " ENTER THE NUMERICAL VALUE, P = ", L

2500 PRINT

2510 RTE(1, 1) = 1: RTE(1, 2) = 1E+07: RTE(1, 3) = 1E+10: RTE(1, 4) = 7.35499E+09

2520 RTE(1, 5) = 4.184E+07: RTE(1, 6) = 1.35582E+07: RTE(1, 7) = 1.05435E+10

2530 RTE(1, 8) = 2928751!

2540 FOR I = 1 TO 8

2550 FOR J = 1 TO 8

2560 KTE(J, I) = RTE(1, I) / RTE(1, J)

2570 PTE(J) = L * KTE(J, N7)

2580 NEXT J, I

2590 PRINT " THE POWER IN DIFFERENT UNITS: "

2600 PRINT : COLOR 15

2610 PRINT USING " ##.#####^^^^ ergs/s "; PTE(1)

2620 PRINT USING " ##.#####^^^^ W "; PTE(2)

2630 PRINT USING " ##.#####^^^^ kW "; PTE(3)

2640 PRINT USING " ##.#####^^^^ hp "; PTE(4)

2650 PRINT USING " ##.#####^^^^ cal/s "; PTE(5)

2660 PRINT USING " ##.#####^^^^ lb x feet/s "; PTE(6)

2670 PRINT USING " ##.#####^^^^ BTU/s "; PTE(7)

2680 PRINT USING " ##.#####^^^^ BTU/h "; PTE(8)

2690 PRINT : COLOR 14

2700 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N27

2710 ON N27 GOTO 2400, 110

2720 COLOR 14, 4: CLS : PRINT

2730 PRINT " YOUR INPUT UNIT OF TIME? [1 = ns = 1E - 9)s; 2 = microsec; 3 = ms; "

2740 PRINT

2750 PRINT " 4 = s; 5 = min; 6 = h; 7 = d; 8 = a (cal. year); 9 = a (solar year)]"

2760 PRINT : DIM RI(9, 9), KI(9, 9), PI(9)

2770 INPUT " ENTER THE NUMBER OF THE UNIT (1...9): ", N8

2780 PRINT

2790 INPUT " ENTER THE NUMERICAL VALUE, T = ", L

2800 PRINT

2810 RI(1, 1) = 1: RI(1, 2) = 1000: RI(1, 3) = 1000000!: RI(1, 4) = 1E+09: RI(1, 5) = 6E+10

2820 RI(1, 6) = 3.6E+12: RI(1, 7) = 8.640001E+13: RI(1, 8) = 3.1536E+16

2830 RI(1, 9) = 3.155693E+16

2840 FOR I = 1 TO 9

2850 FOR J = 1 TO 9

2860 KI(J, I) = RI(1, I) / RI(1, J)

2870 PI(J) = L * KI(J, N8)

2880 NEXT J, I

2890 PRINT " THE TIME IN DIFFERENT UNITS: "

2900 PRINT : COLOR 15

2910 PRINT USING " ##.#####^^^^ ns (nanosec) "; PI(1)

2920 PRINT USING " ##.#####^^^^ microsec "; PI(2)

2930 PRINT USING " ##.#####^^^^ ms  "; PI(3)

2940 PRINT USING " ##.#####^^^^ sec"; PI(4)

2950 PRINT USING " ##.#####^^^^ min "; PI(5)

2960 PRINT USING " ##.#####^^^^ h "; PI(6)

2970 PRINT USING " ##.#####^^^^ d "; PI(7)

2980 PRINT USING " ##.#####^^^^ a (calendar year) "; PI(8)

2990 PRINT USING " ##.#####^^^^ a (solar year) "; PI(9)

3000 PRINT : COLOR 14

3010 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N28

3020 ON N28 GOTO 2720, 110

3030 COLOR 14, 4: CLS : PRINT

3040 PRINT " YOUR INPUT UNIT OF ANGLE? [1 = sec;  2 = min; 3 = degree; "

3050 PRINT

3060 PRINT " 4 = radian; 5 = grade)]"

3070 PRINT : DIM RK(5, 5), KK(5, 5), PK(5)

3080 INPUT " ENTER THE NUMBER OF THE UNIT (1...5): ", N9

3090 PRINT

3100 INPUT " ENTER THE NUMERICAL VALUE, A = ", L

3110 PRINT

3120 RK(1, 1) = 1: RK(1, 2) = 60: RK(1, 3) = 3600

3130 RK(1, 4) = 206264.806#: RK(1, 5) = 3240

3140 FOR I = 1 TO 5

3150 FOR J = 1 TO 5

3160 KK(J, I) = RK(1, I) / RK(1, J)

3170 PK(J) = L * KK(J, N9)

3180 NEXT J, I

3190 PRINT " THE ANGLE IN DIFFERENT UNITS: "

3200 PRINT : COLOR 15

3210 PRINT USING " ##.#####^^^^ sec "; PK(1)

3220 PRINT USING " ##.#####^^^^ min "; PK(2)

3230 PRINT USING " ##.#####^^^^ deg  "; PK(3)

3240 PRINT USING " ##.#####^^^^ rad(ians) "; PK(4)

3250 PRINT USING " ##.#####^^^^ grades "; PK(5)

3260 PRINT : COLOR 14

3270 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N29

3280 ON N29 GOTO 3030, 110

3290 COLOR 14, 4: CLS : PRINT

3300 PRINT " YOUR INPUT TEMPERATURE SCALE? [1 = Kelvin (K); 2 = Celsius (C);"

3310 PRINT

3320 PRINT " 3 = Fahrenheit (F); 4 = Rankine (R)]"

3330 PRINT

3340 INPUT " ENTER THE NUMBER OF THE UNIT (1...4): ", N10

3350 PRINT

3360 INPUT " ENTER THE NUMERICAL VALUE, T = ", L

3370 PRINT

3380 IF N10 = 1 THEN T = L: GOTO 3420

3390 IF N10 = 2 THEN T = L + 273.15: GOTO 3420

3400 IF N10 = 3 THEN T = (L + 459.67) / 1.8: GOTO 3420

3410 IF N10 = 4 THEN T = L / 1.8: GOTO 3420

3420 PL(1) = T: PL(2) = T - 273.15: PL(3) = 1.8 * (T - 273.15) + 32

3430 PL(4) = 1.8 * T

3440 FOR I = 1 TO 4

3450 IF ABS(PL(I)) < .005 THEN PL(I) = 0

3460 NEXT I

3470 PRINT " THE TEMPERATURE IN DIFFERENT SCALES: "

3480 PRINT : COLOR 15

3490 PRINT USING " ##.####^^^^ K (Kelvin) "; PL(1)

3500 PRINT USING " ##.####^^^^ C (Celsius or Centigrade) "; PL(2)

3510 PRINT USING " ##.####^^^^ F (Fahrenheit) "; PL(3)

3520 PRINT USING " ##.####^^^^ R (Rankine) "; PL(4)

3530 PRINT : COLOR 14

3540 INPUT " WANT TO CONTINUE (1) OR RETURN TO MENU (2)? CHOICE (1 OR 2) = ", N210

3550 ON N210 GOTO 3290, 110

3560 END

