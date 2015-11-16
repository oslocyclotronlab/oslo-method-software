100 REM     Z-PER-A.BAS         SEPTEMBER 22, 1992             J. KANTELE

110 COLOR 14: CLS

120 PRINT "****************************************************************"

130 PRINT

140 PRINT " PROGRAM TO COMPUTE THE NEAREST INTEGER Z AND THE RATIO OF Z AND "

150 PRINT " THE MOST STABLE MASS A OF AN ELEMENT, FOR A GIVEN ISOBAR A. "

160 PRINT " A VERY APPROXIMATE FORMULA."

170 PRINT

180 PRINT "****************************************************************"

190 PRINT

200 INPUT " MASS-NUMBER A = ", A

210 PRINT

220 Z1 = .5 * A * (1 - .006 * A ^ (2 / 3))

230 Z2 = INT(Z1 + .5)

240 ZA = Z1 / A

250 PRINT USING " Z = ###; A = ###; Z/A = #.###"; Z2; A; ZA

260 PRINT

270 COLOR 12: PRINT " TO QUIT, HIT CTRL-C": COLOR 14

280 PRINT : GOTO 180

290 END



