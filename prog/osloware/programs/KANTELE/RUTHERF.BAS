100 REM    RUTHERF.BAS         MARCH 17, 1990         J. KANTELE

101 REM    modified           October 7, 1994         W. Trzaska

105 COLOR 14, 4: CLS

110 PRINT "*****************************************************************"

120 PRINT "             PROGRAM TO CALCULATE NON-RELATIVISTIC"

121 PRINT "             RUTHERFORD-SCATTERING CROSS SECTIONS"

140 PRINT "*****************************************************************"

150 PRINT

160 INPUT " ATOMIC NUMBER OF PROJECTILE, Z1 = ", Z1

180 INPUT " MASS OF PROJECTILE, M1 = ", M1

190 PRINT

200 INPUT " LAB. ENERGY OF PROJECTILE (MeV), E = ", E

210 PRINT

220 INPUT " ATOMIC NUMBER OF TARGET NUCLEUS, Z2 = ", Z2

240 INPUT " MASS OF TARGET NUCLEUS, M2 = ", M2

250 PRINT

251 REM

252 REM      calculating basic kinematic properties:

253 REM

254 REM      P1 - momentum of the projectile

255 REM      VCM - velocity of the center of mass

256 REM      EINCM - energy available in the center of mass

257 REM      V3 - velocity of the scatterd particle in C.M.

258 REM

259          P1 = SQR(2! * E * M1)

260          VCM = P1 / (M1 + M2)

261          EINCM = E * M2 / (M1 + M2)

262          V3 = SQR(2! * M1 * EINCM * M2 / (M1 + M2)) / M1

263          PI = 3.141592654#

264 REM

265 INPUT " LAB. SCATTERING ANGLE (DEGR.), PSI = ", PSI

266        PSI = PSI / 180! * PI

267 REM

268 REM     calculating TCM - angle in the center of mass

269 REM

270         TMP = VCM / V3 * SIN(PSI)

271         TCM = ATN(TMP / SQR(1! - TMP * TMP)) + PSI

272 REM

273 PRINT USING " C.M. SCATTERING ANGLE (DEGR.), TCM = ###.##"; TCM * 180! / PI

274 REM

275 REM     calculating EOUT - energy of the scattered particle

276 REM

277 EOUT = M1 / 2! * ((VCM + V3 * COS(TCM)) ^ 2 + (V3 * SIN(TCM)) ^ 2)

278 PRINT USING " ENERGY OF THE SCATTERED PARTICLE = ###.### MeV"; EOUT

279 REM

280 D = 1.296 / (SIN(TCM / 2!)) ^ 4 * (Z1 * Z2 / E) ^ 2 * (1! + M1 / M2) ^ 2

281 REM

282 PRINT

300 PRINT USING " d(sigma)/d(omega) = ##.##^^^^ mb/sr"; D

310 PRINT

311 COLOR 11: PRINT " 1 - REPEAT ALL, 2 - NEW ANGLE, 3 - QUIT ": COLOR 14

312 PRINT

313 INPUT " YOUR CHOICE (1,2,3) ? = ", TMP

314 PRINT "*****************************************************************"

315 IF TMP = 1 GOTO 100

316 IF TMP = 2 GOTO 265

340 END



