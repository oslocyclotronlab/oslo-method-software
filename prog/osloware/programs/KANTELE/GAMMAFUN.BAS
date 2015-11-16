100 REM   GAMMAFUN.BAS         DECEMBER 29, 1992          J. KANTELE

110 COLOR 14: CLS

120 PRINT "****************************************************************"

130 PRINT

140 PRINT " PROGRAM TO CALCULATE APPROXIMATE VALUE FOR THE MODULUS OF "

150 PRINT " THE GAMMA FUNCTION, |Gamma(X + iY)|, FOR X > 0 AND Y > 0. "

160 PRINT " BASIS OF CALCULATION: D.H. WILKINSON, NIM 82 (1970) 122; "

170 PRINT " APPROXIMATION B, N = 5. "

180 PRINT

190 PRINT "****************************************************************"

200 PRINT

210 INPUT " X = ", X

220 PRINT

230 INPUT " Y = ", Y

240 PRINT

250 Z1 = LOG(X ^ 2 + Y ^ 2) + LOG((1 + X) ^ 2 + Y ^ 2) + LOG((2 + X) ^ 2 + Y ^ 2) + LOG((3 + X) ^ 2 + Y ^ 2) + LOG((4 + X) ^ 2 + Y ^ 2)

260 G5 = EXP((4.5 + X) * LOG((5 + X) ^ 2 + Y ^ 2) - 2 * Y * ATN(Y / (5 + X)) - 2 * (5 + X) + LOG(2 * 3.1416) + (5 + X) / (6 * ((5 + X) ^ 2 + Y ^ 2)) - Z1)

270 COLOR 15

280 PRINT USING " THE MODULUS OF GAMMA FUNCTION IS = ##.####^^^^"; SQR(G5)

290 PRINT

300 COLOR 12: PRINT " TO QUIT, JUST TYPE CTRL-C": COLOR 14

310 GOTO 180

320 END

