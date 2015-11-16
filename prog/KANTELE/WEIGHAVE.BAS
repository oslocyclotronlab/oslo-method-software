100 REM    WEIGHAVE.BAS         JULY 3, 1990          J. KANTELE

110 CLS

120 PRINT "*****************************************************************"

130 PRINT "PROGRAM TO CALCULATE WEIGHTED AVERAGES OF VALUES WITH ERRORS;"

140 PRINT "THE INPUTS ARE THE VALUES AND THEIR ABSOLUTE UNCERTAINTIES"

150 PRINT "*****************************************************************"

160 PRINT

170 INPUT "HOW MANY VALUES? ", N

180 PRINT

190 FOR I = 1 TO N

200 INPUT "(X, delta-X) (ONE PAIR PER LINE) = ", X(I), D(I)

210 NEXT I

220 S1 = 0: S2 = 0: DZ = 0

230 FOR K = 1 TO N

240 S1 = S1 + (1 / D(K)) ^ 2 * X(K)

250 S2 = S2 + (1 / D(K)) ^ 2

260 NEXT K

270 W = S1 / S2

280 DZ = S2 ^ (-1 / 2)

290 PRINT

300 PRINT USING "WEIGHTED AVERAGE X = ##.###^^^^ "; W

310 PRINT

320 Z100 = DZ / W * 100

330 PRINT USING "ERROR = ##.###^^^^ = ##.##^^^^ PER CENT "; DZ; Z100

340 PRINT

350 RESTORE

360 GOTO 150

