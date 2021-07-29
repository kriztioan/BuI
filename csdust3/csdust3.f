C***********************************************************************ABBQ0011
C   PROGRAM CSDUST3.F                                                   ABBQ0012
C***********************************************************************ABBQ0013
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ0014
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF,NDF8M=8*NDF)   ABBQ0015
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2,NCEIC=8*NFC2+NDF+1)      ABBQ0016
      PARAMETER (NQWT0=7*(ND*NC+ND*(ND+1)/2)+2*ND+NC1+NPMAX)            ABBQ0017
      PARAMETER (ND4=4*ND,NDUM=NQWT0+9*NDF+NFC2+2*(ND+NF),NQG=4)        ABBQ0018
      PARAMETER (NCQW=2*(ND*NC+ND*(ND+1)/2)+4*NQG)                      ABBQ0019
      COMMON DUMMY(NDUM)                                                ABBQ0020
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                ABBQ0021
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              ABBQ0022
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      ABBQ0023
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   ABBQ0024
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ0025
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ0026
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             ABBQ0027
      COMMON/MODEL/RMAX,RMIN,RHOCS,RHOBAR,TAUOF,IOF                     ABBQ0028
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            ABBQ0029
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ0030
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         ABBQ0031
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ0032
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        ABBQ0033
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ0034
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG)                         ABBQ0035
C***********************************************************************ABBQ0036
C   OPEN TEMPORARY FILES                                                ABBQ0037
C***********************************************************************ABBQ0038
      CALL OPENFL                                                       ABBQ0039
      CLOSE(6)

C***********************************************************************ABBQ0040
C   READ NUMBER OF MODELS TO BE RUN                                     ABBQ0041
C***********************************************************************ABBQ0042
      READ (5,100) NMODEL                                               ABBQ0043
  100 FORMAT (7X,I3)                                                    ABBQ0044
      DO 20 IM=1,NMODEL                                                 ABBQ0045
      IMODEL=IM                                                         ABBQ0046
      ITER=0                                                            ABBQ0047
C***********************************************************************ABBQ0048
C   SET UP SCRATCH FILES, INITIALIZE PROGRAM                            ABBQ0049
C   VARIABLES, PRINT INITIAL CONDITIONS                                 ABBQ0050
C***********************************************************************ABBQ0051
      CALL START                                                        ABBQ0052
C***********************************************************************ABBQ0053
C   CALCULATE INTENSITY FROM INITIAL GUESS OF DUST TEMPERATURE          ABBQ0054
C   AT EACH GRID AND FREQUENCY POINT                                    ABBQ0055
C***********************************************************************ABBQ0056
      CALL GETAJ                                                        ABBQ0057
      DO 10 IT=1,ITMAX                                                  ABBQ0058
      ITER=IT                                                           ABBQ0059
C***********************************************************************ABBQ0060
C   COMPUTE CORRECTIONS AND UPDATE MEAN INTENSITY (AJ)                  ABBQ0061
C   AND DUST TEMPERATURE (TD)                                           ABBQ0062
C***********************************************************************ABBQ0063
      CALL GETTD                                                        ABBQ0064
C***********************************************************************ABBQ0065
C    UPDATE TEMPERATURE DISTRIBUTION AND CALCULATE EDDINGTON FLUX       ABBQ0066
C    AND EMISSIVITY AT EACH GRID POINT                                  ABBQ0067
C***********************************************************************ABBQ0068
      CALL UPDATE                                                       ABBQ0069
C***********************************************************************ABBQ0070
C   PRINT ITERATION RESULTS, CALCULATE OTHER                            ABBQ0071
C   MODEL CHARACTERISTICS, AND PRINT OUT FINAL                          ABBQ0072
C   MODEL RESULTS                                                       ABBQ0073
C***********************************************************************ABBQ0074
      CALL OUTPUT                                                       ABBQ0075
      IF (ITCON.EQ.1) GO TO 20                                          ABBQ0076
   10 CONTINUE                                                          ABBQ0077
   20 CONTINUE                                                          ABBQ0078
      STOP                                                              ABBQ0079
      END                                                               ABBQ0080
                                                                        ABBQ0081
C***********************************************************************ABBQ0082
C   SUBROUTINE EXPOFF CALLS THE RIDGE SUBROUTINE SIGNAL TO              ABBQ0083
C   TURN OFF THE OVERFLOW AND UNDERFLOW OF NUMBERS, REPLACING           ABBQ0084
C   THEM WITH THE LARGEST AND SMALLEST ALLOWABLE NUMBERS ON THE         ABBQ0085
C   MACHINE                                                             ABBQ0086
C***********************************************************************ABBQ0087
      SUBROUTINE EXPOFF                                                 ABBQ0088
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ0089
      CALL SIGNAL(8,QQ,1)                                               ABBQ0090
      RETURN                                                            ABBQ0091
      END                                                               ABBQ0092
                                                                        ABBQ0093
C***********************************************************************ABBQ0094
C   SUBROUTINE BCD CALCULATES THE CONSTANTS IN THE BOUNDARY             ABBQ0095
C   EQUATIONS OF THE COMBINED MOMENT EQUATION                           ABBQ0096
C   B1,B2 ---  ALPHA AND BETA AT CLOUD SURFACE                          ABBQ0097
C   C1,C2 ---  ALPHA AND BETA AT CLOUD CENTER                           ABBQ0098
C***********************************************************************ABBQ0099
      SUBROUTINE BCD (C1,C2,B1,B2)                                      ABBQ0100
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ0101
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ0102
      DIMENSION C1(1),C2(1),B1(1),B2(1)                                 ABBQ0103
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                ABBQ0104
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            ABBQ0105
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ0106
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ0107
      CON=-2.0*AJ0I                                                     ABBQ0108
C***********************************************************************ABBQ0109
C    COMPUTE BOUNDARY CONSTANTS AT OUTER BOUNDARY, R0                   ABBQ0110
C***********************************************************************ABBQ0111
      DO 10 J=1,NFD                                                     ABBQ0112
      B1(J)=-FBD(J)                                                     ABBQ0113
      B2(J)=-CON*FLUXSD(J)                                              ABBQ0114
   10 CONTINUE                                                          ABBQ0115
C***********************************************************************ABBQ0116
C   INNER BOUNDARY AT RC.NE.0? IC=1=> NO   IC=0 => YES                  ABBQ0117
C***********************************************************************ABBQ0118
      IF (IC.EQ.0) GO TO 22                                             ABBQ0119
C***********************************************************************ABBQ0120
C   USE TOTAL NET FLUX AT INNER BOUNDARY RC?  NH=0 => NO  NH=1 => YES   ABBQ0121
C***********************************************************************ABBQ0122
      IF (NH.EQ.1) GO TO 33                                             ABBQ0123
C***********************************************************************ABBQ0124
C   COMPUTE BOUNDARY CONSTANTS AT INNER BOUNDARY RC FOR IC=1, & NH=0    ABBQ0125
C***********************************************************************ABBQ0126
   15 DO 20 J=1,NFD                                                     ABBQ0127
      C1(J)=FCD(J)                                                      ABBQ0128
      C2(J)=CON*FLUXCD(J)                                               ABBQ0129
   20 CONTINUE                                                          ABBQ0130
      RETURN                                                            ABBQ0131
   22 CONTINUE                                                          ABBQ0132
C***********************************************************************ABBQ0133
C   COMPUTE BOUNDARY CONSTANTS AT RC FOR IC=0                           ABBQ0134
C***********************************************************************ABBQ0135
      DO 30 J=1,NFD                                                     ABBQ0136
      C1(J)=0.0                                                         ABBQ0137
      C2(J)=0.0                                                         ABBQ0138
   30 CONTINUE                                                          ABBQ0139
      RETURN                                                            ABBQ0140
   33 CONTINUE                                                          ABBQ0141
C***********************************************************************ABBQ0142
C   COMPUTE BOUNDARY CONSTANTS AT RC FOR IC=1,  NH=1                    ABBQ0143
C***********************************************************************ABBQ0144
      DO 40 J=1,NFD                                                     ABBQ0145
      C1(J)=0.0                                                         ABBQ0146
      C2(J)=-AJ0I*FLUXCD(J)                                             ABBQ0147
   40 CONTINUE                                                          ABBQ0148
      RETURN                                                            ABBQ0149
      END                                                               ABBQ0150
                                                                        ABBQ0151
C***********************************************************************ABBQ0152
C   SUBROUTINE BRANRW PROVIDES READ/WRITE ACCESS TO                     ABBQ0153
C   DATA FILES STORED ON EXTERNAL DISK                                  ABBQ0154
C   ARRAY --- CORE DATA STORAGE ADDRESS BEING READ OR WRITTEN           ABBQ0155
C   IFILE --- TEMPORARY DISK FILE BEING READ OR WRITTEN                 ABBQ0156
C   NWORD --- DIMENSION OF ARRAY                                        ABBQ0157
C***********************************************************************ABBQ0158
      SUBROUTINE BRANRW (IFILE,ARRAY,NWORD)                             ABBQ0159
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ0160
      DIMENSION ARRAY(NWORD)                                            ABBQ0161
      ENTRY BRANRD (IFILE,ARRAY,NWORD)                                  ABBQ0162
      READ (IFILE) ARRAY                                                ABBQ0163
      REWIND IFILE                                                      ABBQ0164
      RETURN                                                            ABBQ0165
      ENTRY BRANWT (IFILE,ARRAY,NWORD)                                  ABBQ0166
      WRITE (IFILE) ARRAY                                               ABBQ0167
      REWIND IFILE                                                      ABBQ0168
      RETURN                                                            ABBQ0169
      END                                                               ABBQ0170
                                                                        ABBQ0171
C***********************************************************************ABBQ0172
C   SUBROUTINE GETBJ CALCULATES THE PLANCK FUNCTION, ITS TEMPERATURE    ABBQ0173
C   DERIVATIVE, AND THEIR RATIO FOR A GIVEN TEMPERATURE AND FREQUENCY   ABBQ0174
C   F    --- FREQUENCY                                                  ABBQ0175
C   T    --- TEMPERATURE                                                ABBQ0176
C   MODE --- SUBROUTINE CONTROL PARAMETER                               ABBQ0177
C   BJ0  --- COMPUTED VALUE OF PLANCK RADIATION FUNCTION                ABBQ0178
C   BJ1  --- TEMPERATURE DERIVATIVE OF BJ0 IF MODE.NE.3,                ABBQ0179
C           TEMPERATURE DERIVATIVE OF BJ0 DIVIDED BY BJ0 IF MODE=3      ABBQ0180
C***********************************************************************ABBQ0181
      SUBROUTINE GETBJ (F,T,MODE,BJ0,BJ1)                               ABBQ0182
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ0183
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      ABBQ0184
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            ABBQ0185
      IF (T.GT.0.0) GOTO 5                                              ABBQ0186
      BJ0=0.0                                                           ABBQ0187
      BJ1=0.0                                                           ABBQ0188
      WRITE(6,111)                                                      ABBQ0189
  111 FORMAT(40X,'TEMPERATURE IN SUBROUTINE GETBJ.LE.ZERO')             ABBQ0190
      RETURN                                                            ABBQ0191
    5 CONTINUE                                                          ABBQ0192
      C1=1.0/CTH                                                        ABBQ0193
      TI=1.0/T                                                          ABBQ0194
      X1=HK*F*TI                                                        ABBQ0195
C********************************************************************** ABBQ0196
C   EXP(709) IS EQUAL TO THE LARGEST NUMBER AVAILABLE ON THE            ABBQ0197
C   RIDGE 32                                                            ABBQ0198
C********************************************************************** ABBQ0199
      IF (X1.LT.7.09E2) GO TO 20                                        ABBQ0200
      BJ0=C1*F*F*F*DEXP(-X1)                                            ABBQ0201
C***********************************************************************ABBQ0202
C   IF MODE < 1  ---  RETURNS B(T)                                      ABBQ0203
C   IF MODE = 3  ---  RETURNS B, (DB/DT)/B                              ABBQ0204
C   ELSE         ---  RETURNS B, DB/DT                                  ABBQ0205
C***********************************************************************ABBQ0206
      IF (MODE.GE.1) BJ1=BJ0*X1*TI                                      ABBQ0207
      IF (MODE.EQ.3) BJ1=X1*TI                                          ABBQ0208
      RETURN                                                            ABBQ0209
   20 CONTINUE                                                          ABBQ0210
      X2=DEXP(X1)                                                       ABBQ0211
      IF (X2.GT.1.0) GO TO 40                                           ABBQ0212
      BJ0=(2.0*F*F*BOLTZ*T)/(CV*CV)*AJ0I                                ABBQ0213
      IF (MODE.GE.1) BJ1=BJ0*TI                                         ABBQ0214
      IF (MODE.EQ.3) BJ1=TI                                             ABBQ0215
      RETURN                                                            ABBQ0216
   40 CONTINUE                                                          ABBQ0217
      BJ0=C1*F*F*F*(1.0/(X2-1.0))                                       ABBQ0218
      IF (MODE.GE.1) BJ1=BJ0*(X1*TI*X2/(X2-1.0))                        ABBQ0219
      IF (MODE.EQ.3) BJ1=(X1*TI*X2/(X2-1.0))                            ABBQ0220
      RETURN                                                            ABBQ0221
      END                                                               ABBQ0222
                                                                        ABBQ0223
C***********************************************************************ABBQ0224
C   SUBROUTINE GETAU CALCULATES THE OPTICAL DEPTH (USING TRAPEZOIDAL    ABBQ0225
C   RULE) AS A FUNCTION OF POSITION FOR A GIVEN OPACITY                 ABBQ0226
C   X --- R                                                             ABBQ0227
C   Y --- OPACITY                                                       ABBQ0228
C   Z --- OPTICAL DEPTH                                                 ABBQ0229
C   N --- NUMBER OF INTEGRATION WEIGHTS                                 ABBQ0230
C***********************************************************************ABBQ0231
      SUBROUTINE GETAU (X,Y,Z,N)                                        ABBQ0232
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ0233
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ0234
      PARAMETER (NDP1=ND+1)                                             ABBQ0235
      DIMENSION X(1),Y(1),Z(1)                                          ABBQ0236
      COMMON/SPARE/P(NDP1),V(NDP1),U(NDP1)                              ABBQ0237
C***********************************************************************ABBQ0238
C   CALCULATE OPTICAL DEPTH FROM CLOUD SURFACE                          ABBQ0239
C***********************************************************************ABBQ0240
      N1=N-1                                                            ABBQ0241
      DO 10 L=1,N1                                                      ABBQ0242
      P(L)=0.5*(X(L+1)-X(L))*(Y(L+1)+Y(L))                              ABBQ0243
   10 CONTINUE                                                          ABBQ0244
      Z(N)=0.0                                                          ABBQ0245
      DO 20 L=1,N1                                                      ABBQ0246
      J=N-L                                                             ABBQ0247
      Z(J)=Z(J+1)+P(J)                                                  ABBQ0248
   20 CONTINUE                                                          ABBQ0249
      RETURN                                                            ABBQ0250
      END                                                               ABBQ0251
                                                                        ABBQ0252
C***********************************************************************ABBQ0253
C   SUBROUTINE OPENFL OPENS THE SCRATCH STORAGE FILES ON THE            ABBQ0254
C   RIDGE 32 HARD DISK                                                  ABBQ0255
C***********************************************************************ABBQ0256
      SUBROUTINE OPENFL                                                 ABBQ0257
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ0258
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ0259
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ0260
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ0261
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ0262
C***********************************************************************ABBQ0263
C   TURN OFF OVERFLOW/UNDERFLOW FEATURES                                ABBQ0264
C***********************************************************************ABBQ0265
      CALL EXPOFF                                                       ABBQ0266
C***********************************************************************ABBQ0267
C   ASSIGN LOGICAL UNIT NUMBERS TO DISK FILES                           ABBQ0268
C***********************************************************************ABBQ0269
      NGPD=8                                                            ABBQ0270
      NJCBD=9                                                           ABBQ0271
      NBTAD=10                                                          ABBQ0272
      NCGD=11                                                           ABBQ0273
      NEFQW=12                                                          ABBQ0274
      NJFPD=13                                                          ABBQ0275
      NCED=14                                                           ABBQ0276
      NAHD=15                                                           ABBQ0277
      NEID=16                                                           ABBQ0278
      NEICD=17                                                          ABBQ0279
      NQASD=18                                                          ABBQ0280
      NGBD=19                                                           ABBQ0281
      NCYQW=20                                                          ABBQ0282
C***********************************************************************ABBQ0283
C   DETERMINE FORMAT OF OUTPUT DATA                                     ABBQ0284
C***********************************************************************ABBQ0285
      OPEN (UNIT=6,FORM='PRINT')                                        ABBQ0286
C***********************************************************************ABBQ0287
C   CREATE TEMPORARY DISK FILES                                         ABBQ0288
C***********************************************************************ABBQ0289
      OPEN(UNIT=NGPD,FORM='UNFORMATTED',STATUS='SCRATCH')               ABBQ0290
      OPEN(UNIT=NJCBD,FORM='UNFORMATTED',STATUS='SCRATCH')              ABBQ0291
      OPEN (UNIT=NBTAD,FORM='UNFORMATTED',STATUS='SCRATCH')             ABBQ0292
      OPEN (UNIT=NCGD,FORM='UNFORMATTED',STATUS='SCRATCH')              ABBQ0293
      OPEN (UNIT=NEFQW,FORM='UNFORMATTED',STATUS='SCRATCH')             ABBQ0294
      OPEN (UNIT=NJFPD,FORM='UNFORMATTED',STATUS='SCRATCH')             ABBQ0295
      OPEN (UNIT=NCED,FORM='UNFORMATTED',STATUS='SCRATCH')              ABBQ0296
      OPEN (UNIT=NAHD,FORM='UNFORMATTED',STATUS='SCRATCH')              ABBQ0297
      OPEN (UNIT=NEID,FORM='UNFORMATTED',STATUS='SCRATCH')              ABBQ0298
      OPEN (UNIT=NEICD,FORM='UNFORMATTED',STATUS='SCRATCH')             ABBQ0299
      OPEN (UNIT=NQASD,FORM='UNFORMATTED',STATUS='SCRATCH')             ABBQ0300
      OPEN (UNIT=NGBD,FORM='UNFORMATTED',STATUS='SCRATCH')              ABBQ0301
      IF (IGEOM.NE.1) RETURN                                            ABBQ0302
      OPEN (UNIT=NCYQW,FORM='UNFORMATTED',STATUS='SCRATCH')             ABBQ0303
      RETURN                                                            ABBQ0304
      END                                                               ABBQ0305
                                                                        ABBQ0306
C***********************************************************************ABBQ0307
C   SUBROUTINE START SETS UP FILES, READS INPUT DATA,                   ABBQ0308
C   AND INITIALIZES VARIABLES. COMPUTED PARAMETERS AND                  ABBQ0309
C   INITIAL CONDITIONS ARE PRINTED IN TABULAR FORMAT                    ABBQ0310
C***********************************************************************ABBQ0311
      SUBROUTINE START                                                  ABBQ0312
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ0313
      CHARACTER*8 XYES,XNO,YEDFTR,YOUT,YC,YSCA,YB,YH,                   ABBQ0314
     1            YGEOM,XPLAN,XCYL,XSPHER,YCORE                         ABBQ0315
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ0316
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         ABBQ0317
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ0318
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          ABBQ0319
      PARAMETER (ND5=5*ND,NF5=5*NF,NX1=6*NDF+ND5+1,NX2=NX1+ND5,         ABBQ0320
     1           NX3=NX2+NF5,NX4=NX3+NF5,NX5=NX4+NF5)                   ABBQ0321
      PARAMETER (NDP1=ND+1,ND3P1=3*(ND+1))                              ABBQ0322
      PARAMETER (NDF7=7*NDF,NDX=3*(ND+1))                               ABBQ0323
      COMMON DUMMY(NDF7),TAU0(NF),TOF(ND),CHIF(ND),                     ABBQ0324
     1       TAUX(ND)                                                   ABBQ0325
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),             ABBQ0326
     1          ETA(NF,ND),AH(NF,ND),CGBAR(NF,ND),QABS(NF,ND),          ABBQ0327
     2          QSCA(NF,ND),CHICG(NF,ND),BTDAV(NF,ND),BTDAVI(NF,ND)     ABBQ0328
      DIMENSION ABUNDI(5,ND),TDI(5,ND),QABSI(NF,5),QSCAI(NF,5),         ABBQ0329
     1          GBARI(NF,5),ALBEDI(NF,5),GRAINC(5),AC(5),GRAINM(5),     ABBQ0330
     2          AM(5)                                                   ABBQ0331
      EQUIVALENCE (DUMMY(1),AJ(1,1),AH(1,1)),                           ABBQ0332
     1            (DUMMY(NDF1P1),FK(1,1),QABS(1,1)),                    ABBQ0333
     2            (DUMMY(NDF2P1),ZETA(1,1),QSCA(1,1)),                  ABBQ0334
     3            (DUMMY(NDF3P1),CHI(1,1)),                             ABBQ0335
     4            (DUMMY(NDF4P1),ETA(1,1)),                             ABBQ0336
     5            (DUMMY(NDF5P1),BTDAV(1,1),CGBAR(1,1)),                ABBQ0337
     6            (DUMMY(NDF6P1),BTDAVI(1,1),CHICG(1,1)),               ABBQ0338
     7            (DUMMY(NDF6P1),ABUNDI(1,1)),(DUMMY(NX1),TDI(1,1)),    ABBQ0339
     8            (DUMMY(NX2),QABSI(1,1)),(DUMMY(NX3),QSCAI(1,1)),      ABBQ0340
     9            (DUMMY(NX4),GBARI(1,1)),(DUMMY(NX5),ALBEDI(1,1))      ABBQ0341
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                ABBQ0342
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              ABBQ0343
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      ABBQ0344
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   ABBQ0345
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ0346
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ0347
      COMMON/MODEL/RMAX,RMIN,RHOCS,RHOBAR,TAUOF,IOF                     ABBQ0348
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            ABBQ0349
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ0350
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         ABBQ0351
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ0352
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ0353
      IF (IMODEL.GT.1) GO TO 55                                         ABBQ0354
      XYES='YES     '                                                   ABBQ0355
      XNO='NO      '                                                    ABBQ0356
      XPLAN='PLANE   '                                                  ABBQ0357
      XCYL='CYLINDER'                                                   ABBQ0358
      XSPHER='SPHERE  '                                                 ABBQ0359
      PI=3.141592653589793E0                                            ABBQ0360
      CV=2.997924562E10                                                 ABBQ0361
      PLANK=6.62565E-27                                                 ABBQ0362
      BOLTZ=1.380542E-16                                                ABBQ0363
      PARSEC=3.0856E18                                                  ABBQ0364
      HK=PLANK/BOLTZ                                                    ABBQ0365
      NR=ND                                                             ABBQ0366
      N1=NR-1                                                           ABBQ0367
      NP=NPMAX                                                          ABBQ0368
      NB1D=NDF                                                          ABBQ0369
      NB2D=2*NDF                                                        ABBQ0370
      NB3D=3*NDF                                                        ABBQ0371
      NGP=5*(3*NF+2*ND)                                                 ABBQ0372
C********************************************************************** ABBQ0373
C   THE CONTROL PARAMETER IGEOM DETERMINES THE GEOMETRY OF THE DUST     ABBQ0374
C   CLOUD MODELED. 0 = PLANAR, 1 = CYLINDRICAL, 2 = SPHERICAL.          ABBQ0375
C   IEMRG DETERMINES WHETHER OUTPUT INFORMATION WILL BE PRINTED FOR     ABBQ0376
C   ONE OR ALL THETA DIRECTIONS IN THE CYLINDRICAL CASE.                ABBQ0377
C********************************************************************** ABBQ0378
      READ (5,100) IGEOM,IEMRG                                          ABBQ0379
      IF (IGEOM.EQ.0) NP=20                                             ABBQ0380
      READ (5,100) IEDFTR,IOUT,IC,NH,ISCA,IB,IDIST                      ABBQ0381
C***********************************************************************ABBQ0382
C   BEGIN READING INPUT DATA SET                                        ABBQ0383
C***********************************************************************ABBQ0384
  100 FORMAT (7(7X,I1))                                                 ABBQ0385
      READ (5,200) AJ0,EPS,ITMAX                                        ABBQ0386
  200 FORMAT (1P2E10.3,2X,I2)                                           ABBQ0387
      READ (5,300) (R(L),L=1,NR)                                        ABBQ0388
  300 FORMAT (1P7E10.3,10X)                                             ABBQ0389
      IF (IC.EQ.0) NP=NR                                                ABBQ0390
      CTH=0.5*CV*CV*AJ0/PLANK                                           ABBQ0391
      AJ0I=1.0/AJ0                                                      ABBQ0392
      READ (5,400) NFD,IOF,(WLAMDA(J),J=1,NFD)                          ABBQ0393
  400 FORMAT (2I5/(1P7E10.3,10X))                                       ABBQ0394
      IF (IB.NE.0) READ (5,300) (FLUXSD(J),J=1,NFD)                     ABBQ0395
      READ (5,500) IMIX                                                 ABBQ0396
  500 FORMAT (I5,5X,1P2E10.3)                                           ABBQ0397
      DO 20 IG=1,IMIX                                                   ABBQ0398
      READ (5,600) GRAINC(IG),AC(IG),GRAINM(IG),AM(IG),IREADA           ABBQ0399
  600 FORMAT (A8,2X,1PE10.3,A8,2X,1PE10.3,10X,I5)                       ABBQ0400
      READ (5,700) (QABSI(J,IG),QSCAI(J,IG),GBARI(J,IG),J=1,NFD)        ABBQ0401
  700 FORMAT (1P3E10.3,50X)                                             ABBQ0402
      IF (IREADA.EQ.0) READ (5,300) ABUNDI(IG,1)                        ABBQ0403
      IF (IREADA.NE.0) READ (5,300) (ABUNDI(IG,L),L=1,NR)               ABBQ0404
      IF (IREADA.NE.0) GO TO 20                                         ABBQ0405
      DO 10 L=1,NR                                                      ABBQ0406
      ABUNDI(IG,L)=ABUNDI(IG,1)                                         ABBQ0407
   10 CONTINUE                                                          ABBQ0408
   20 CONTINUE                                                          ABBQ0409
      READ (5,500) IREADT,TDS,TDC                                       ABBQ0410
C***********************************************************************ABBQ0411
C   COMPUTE FREQUENCY GRID FROM WAVELENGTH GRID                         ABBQ0412
C***********************************************************************ABBQ0413
      CON=1.0E4*CV                                                      ABBQ0414
      FREQD(1)=CON/WLAMDA(1)                                            ABBQ0415
      WFD(1)=0.0                                                        ABBQ0416
      DO 30 J=1,NFD                                                     ABBQ0417
      IF (J.EQ.NFD) GO TO 22                                            ABBQ0418
      FREQD(J+1)=CON/WLAMDA(J+1)                                        ABBQ0419
      DW=0.5*(FREQD(J+1)-FREQD(J))                                      ABBQ0420
      WFD(J)=WFD(J)+DW                                                  ABBQ0421
      WFD(J+1)=DW                                                       ABBQ0422
   22 CONTINUE                                                          ABBQ0423
C***********************************************************************ABBQ0424
C   INITIALIZE BOUNDARY FLUXES; CONVERT INPUT FLUXSD FROM INTENSITY     ABBQ0425
C   TO FLUX                                                             ABBQ0426
C***********************************************************************ABBQ0427
      FLUXSD(J)=0.25*FLUXSD(J)                                          ABBQ0428
      IF (IB.EQ.0) FLUXSD(J)=0.0                                        ABBQ0429
      IF (IC.EQ.0) FLUXCD(J)=0.0                                        ABBQ0430
   30 CONTINUE                                                          ABBQ0431
C***********************************************************************ABBQ0432
C   INITIALIZE TEMPERATURE PROFILE                                      ABBQ0433
C***********************************************************************ABBQ0434
      IF (IREADT.NE.0) READ (5,300) (TD(L),L=1,NR)                      ABBQ0435
      XX=(TDC-TDS)*DSQRT(R(1))                                          ABBQ0436
      DO 40 IG=1,IMIX                                                   ABBQ0437
      DO 40 L=1,NR                                                      ABBQ0438
      IF (IREADT.NE.0) GO TO 35                                         ABBQ0439
      IF (IG.EQ.1.AND.IC.NE.0) TD(L)=TDS+XX/DSQRT(R(L))                 ABBQ0440
      IF (IG.EQ.1.AND.IC.EQ.0) TD(L)=TDC+(TDS-TDC)*R(L)                 ABBQ0441
   35 CONTINUE                                                          ABBQ0442
      TDI(IG,L)=TD(L)                                                   ABBQ0443
   40 CONTINUE                                                          ABBQ0444
C***********************************************************************ABBQ0445
C   WRITE GRAIN PROPERTIES (ABUNDI,TDI, QABSI, QSCAI, GBARI)            ABBQ0446
C   ONTO DISK FILE                                                      ABBQ0447
C***********************************************************************ABBQ0448
      CALL BRANWT (NGPD,DUMMY(NDF6P1),NGP)                              ABBQ0449
      DO 50 J=1,NFD                                                     ABBQ0450
      DO 50 L=1,NR                                                      ABBQ0451
      CALL GETBJ (FREQD(J),TD(L),2,BJ0,BJ1)                             ABBQ0452
      BTDAV(J,L)=BJ0                                                    ABBQ0453
      BTDAVI(J,L)=BJ1                                                   ABBQ0454
   50 CONTINUE                                                          ABBQ0455
C***********************************************************************ABBQ0456
C   WRITE BTDAV, BTDAVI ONTO DISK FILE                                  ABBQ0457
C***********************************************************************ABBQ0458
      CALL BRANWT (NBTAD,DUMMY(NDF5P1),NB2D)                            ABBQ0459
C***********************************************************************ABBQ0460
C   INITIALIZE FK, ZETA, AH, AJ, AND BOUNDARY FACTORS FBD,FCD           ABBQ0461
C***********************************************************************ABBQ0462
      FKCON=1.0/3.0                                                     ABBQ0463
      FBCON=0.5                                                         ABBQ0464
      FCCON=0.0                                                         ABBQ0465
      IF (IC.NE.0) FCCON=0.5                                            ABBQ0466
      DO 51 J=1,NFD                                                     ABBQ0467
      FBD(J)=FBCON                                                      ABBQ0468
      FCD(J)=FCCON                                                      ABBQ0469
      DO 51 L=1,NR                                                      ABBQ0470
      FK(J,L)=FKCON                                                     ABBQ0471
      ZETA(J,L)=1.0                                                     ABBQ0472
      AJ(J,L)=0.0                                                       ABBQ0473
   51 CONTINUE                                                          ABBQ0474
C***********************************************************************ABBQ0475
C   WRITE FK, ZETA ESTIMATES TO DISK FILE                               ABBQ0476
C***********************************************************************ABBQ0477
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 ABBQ0478
C***********************************************************************ABBQ0479
C   INITIATE DISK FILE FOR AH                                           ABBQ0480
C***********************************************************************ABBQ0481
      CALL BRANWT (NAHD,DUMMY(1),NB1D)                                  ABBQ0482
C***********************************************************************ABBQ0483
C   INITIATE DISK FILE FOR AJ                                           ABBQ0484
C***********************************************************************ABBQ0485
      CALL BRANWT (NEICD,DUMMY(1),NFC2)                                 ABBQ0486
   55 CONTINUE                                                          ABBQ0487
C***********************************************************************ABBQ0488
C   READ ABUNDI, TDI, QABSI, QSCAI, GBARI FROM DISK FILE                ABBQ0489
C***********************************************************************ABBQ0490
      CALL BRANRD (NGPD,DUMMY(NDF6P1),NGP)                              ABBQ0491
C***********************************************************************ABBQ0492
C   INITIALIZE CONVERGENCE PARAMETERS AND ZERO OUT CORRECTION ARRAYS    ABBQ0493
C***********************************************************************ABBQ0494
      ITCON=0                                                           ABBQ0495
      ITCONT=0                                                          ABBQ0496
      ITCONJ=0                                                          ABBQ0497
      DO 70 L=1,NR                                                      ABBQ0498
      DTD(L)=0.0                                                        ABBQ0499
      DJD(L)=0.0                                                        ABBQ0500
      DHD(L)=0.0                                                        ABBQ0501
   70 CONTINUE                                                          ABBQ0502
C***********************************************************************ABBQ0503
C   READ DUST SHELL AND HEAT SOURCE PARAMETERS                          ABBQ0504
C***********************************************************************ABBQ0505
      READ (5,300) RMAX,TAUOF,RHOCS,TSTAR,TLUM                          ABBQ0506
      RMIN=R(1)*RMAX                                                    ABBQ0507
      R0=RMAX*PARSEC                                                    ABBQ0508
      ETA0=R0*AJ0I                                                      ABBQ0509
      R0I=1.0/R0                                                        ABBQ0510
      ETA0I=AJ0*R0I                                                     ABBQ0511
      CONQAS=QABSI(IOF,1)+QSCAI(IOF,1)                                  ABBQ0512
      CON=R0*CONQAS                                                     ABBQ0513
      IF (IDIST.EQ.0) GO TO 95                                          ABBQ0514
C***********************************************************************ABBQ0515
C   DETERMINE GAUSSIAN DENSITY DISTRIBUTION  (IDIST=1)                  ABBQ0516
C***********************************************************************ABBQ0517
      CS=DLOG(RHOCS)                                                    ABBQ0518
      CX=DSQRT(CS)                                                      ABBQ0519
      CXR=CX*R(1)                                                       ABBQ0520
      IF (RHOCS.EQ.1.0) RHOBAR=TAUOF/(CON*(1.0-R(1)))                   ABBQ0521
      IF (RHOCS.GT.1.0) RHOBAR=2.0*CX*TAUOF/(CON*DSQRT(PI)*             ABBQ0522
     1                            (DERF(CX)-DERF(CXR)))                 ABBQ0523
      DO 90 L=1,NR                                                      ABBQ0524
      RHOD(L)=RHOBAR*DEXP(-CS*R(L)*R(L))                                ABBQ0525
      SUMQAS=0.0                                                        ABBQ0526
      DO 80 IG=1,IMIX                                                   ABBQ0527
      SUMQAS=SUMQAS+ABUNDI(IG,L)*(QABSI(IOF,IG)+QSCAI(IOF,IG))          ABBQ0528
   80 CONTINUE                                                          ABBQ0529
      CHIF(L)=R0*RHOD(L)*SUMQAS                                         ABBQ0530
   90 CONTINUE                                                          ABBQ0531
      GO TO 135                                                         ABBQ0532
C***********************************************************************ABBQ0533
C   DETERMINE MODIFIED POWER LAW DENSITY DISTRIBUTION (IDIST=0)         ABBQ0534
C   CALCULATE THE MATCHING POINT USING THE SECANT METHOD                ABBQ0535
C***********************************************************************ABBQ0536
   95 CONTINUE                                                          ABBQ0537
      POW=RHOCS                                                         ABBQ0538
      POW1=-POW+1.0                                                     ABBQ0539
      IF (POW.NE.1.0) POWI=1.0/POW1                                     ABBQ0540
      RIN=R(1)                                                          ABBQ0541
      IF (IC.EQ.0) RIN=R(2)                                             ABBQ0542
      RPOW=(1.0-RIN**POW1)                                              ABBQ0543
      LNR=-DLOG(RIN)                                                    ABBQ0544
C***********************************************************************ABBQ0545
C   MATCHING POINT OF INNER GAUSSIAN AND OUTER POWER LAW DENSITY        ABBQ0546
C   PROFILES OCCURS AT 0.8*TAUOF FROM THE SURFACE                       ABBQ0547
C***********************************************************************ABBQ0548
      ALPHA=.8                                                          ABBQ0549
      TM=ALPHA*TAUOF                                                    ABBQ0550
      TMATCH=(1.0-ALPHA)*TAUOF                                          ABBQ0551
      IF (POW.EQ.1.0) THEN                                              ABBQ0552
        RHOSRF=TAUOF/(CON*LNR)                                          ABBQ0553
        RMATCH=DEXP(-TM/(CON*RHOSRF))                                   ABBQ0554
      ELSE                                                              ABBQ0555
        RHOSRF=TAUOF*POW1/(CON*RPOW)                                    ABBQ0556
        RMATCH=(1.0-TM*POW1/(CON*RHOSRF))**POWI                         ABBQ0557
      ENDIF                                                             ABBQ0558
C***********************************************************************ABBQ0559
C   CALCULATE THE DENSITY DISTRIBUTION USING THE CONSTRAINT ON TAU      ABBQ0560
C***********************************************************************ABBQ0561
      RMPOW=RMATCH**(-POW)                                              ABBQ0562
      C1=2*TMATCH/(CON*PI**.5)                                          ABBQ0563
      C2=POW*DLOG(RMATCH)-DLOG(RHOSRF)                                  ABBQ0564
      YM=RMATCH-R(1)                                                    ABBQ0565
      M=100                                                             ABBQ0566
      XSTP=1.0D-16                                                      ABBQ0567
C***********************************************************************ABBQ0568
C    SOLVE FOR GAUSSIAN WIDTH (SIGMA) USING THE SECANT METHOD           ABBQ0569
C    INITIAL GUESSES FOR XA AND XB MAY HAVE TO BE ALTERED               ABBQ0570
C    TO GUARANTEE CONVERGENCE OF THE SECANT METHOD                      ABBQ0571
C***********************************************************************ABBQ0572
      XA=9.0                                                            ABBQ0573
      XB=10.0                                                           ABBQ0574
      DO 110 I=1,M                                                      ABBQ0575
      CA=DEXP(((YM/XA)**2.0)-C2)                                        ABBQ0576
      CB=DEXP(((YM/XB)**2.0)-C2)                                        ABBQ0577
      FNA=XA-C1/(CA*ERF(YM*XA))                                         ABBQ0578
      FNB=XB-C1/(CB*ERF(YM*XB))                                         ABBQ0579
      VALUE=(XA*FNB-XB*FNA)/(FNB-FNA)                                   ABBQ0580
      XA=XB                                                             ABBQ0581
      XB=VALUE                                                          ABBQ0582
      IF (XB.LE.0.0) WRITE (6,101)                                      ABBQ0583
  101 FORMAT (1H1,15X,'GAUSSIAN WILL NOT MATCH TO POWER LAW ON THIS RADIABBQ0584
     1AL GRID')                                                         ABBQ0585
      IF (XB.LE.0.0) STOP                                               ABBQ0586
      IF (ABS(XB-XA).LE.XSTP) GO TO 115                                 ABBQ0587
  110 CONTINUE                                                          ABBQ0588
  115 CONTINUE                                                          ABBQ0589
      RHOBAR=DEXP(((YM/XB)**2.0)-C2)                                    ABBQ0590
      SGMA2=XB**2.0                                                     ABBQ0591
C***********************************************************************ABBQ0592
C   CALCULATE THE DUST DENSITY                                          ABBQ0593
C***********************************************************************ABBQ0594
      DO 130 L=1,NR                                                     ABBQ0595
      IF (R(L).LE.RMATCH) THEN                                          ABBQ0596
        RHOD(L)=RHOBAR*EXP(-(R(L)-R(1))**2/SGMA2)                       ABBQ0597
      ELSE                                                              ABBQ0598
        RHOD(L)=RHOSRF*(R(L)**(-POW))                                   ABBQ0599
      ENDIF                                                             ABBQ0600
      SUMQAS=0.0                                                        ABBQ0601
      DO 120 IG=1,IMIX                                                  ABBQ0602
      SUMQAS=SUMQAS+ABUNDI(IG,L)*(QABSI(IOF,IG)+QSCAI(IOF,IG))          ABBQ0603
  120 CONTINUE                                                          ABBQ0604
      CHIF(L)=R0*RHOD(L)*SUMQAS                                         ABBQ0605
  130 CONTINUE                                                          ABBQ0606
  135 CALL GETAU (R,CHIF,TOF,NR)                                        ABBQ0607
      RATIO=TAUOF/TOF(1)                                                ABBQ0608
      DO 140 L=1,NR                                                     ABBQ0609
      RHOD(L)=RATIO*RHOD(L)                                             ABBQ0610
      TOF(L)=RATIO*TOF(L)                                               ABBQ0611
  140 CONTINUE                                                          ABBQ0612
      RHOBAR=RATIO*RHOBAR                                               ABBQ0613
      WOF=WLAMDA(IOF)                                                   ABBQ0614
C***********************************************************************ABBQ0615
C   WRITE MODEL PARAMETERS TO OUTPUT                                    ABBQ0616
C***********************************************************************ABBQ0617
      IF (IDIST.EQ.0) THEN                                              ABBQ0618
        WRITE (6,111) RMAX,RMIN,POW,RHOBAR,WOF,TAUOF,NR,NP,             ABBQ0619
     1                NFD,EPS,AJ0,ITMAX                                 ABBQ0620
      ELSE                                                              ABBQ0621
        WRITE (6,112) RMAX,RMIN,RHOCS,RHOBAR,WOF,TAUOF,NR,NP,           ABBQ0622
     1                NFD,EPS,AJ0,ITMAX                                 ABBQ0623
      ENDIF                                                             ABBQ0624
  111 FORMAT(1H1,45X,'INPUT PARAMETERS FOR DUST CLOUD MODEL'/           ABBQ0625
     1//20X,'MODEL PARAMETERS'//60X,'OUTER CLOUD RADIUS, RMAX ='        ABBQ0626
     2,1PE10.3,' PARSECS'/60X,'INNER CLOUD RADIUS, RMIN = ',            ABBQ0627
     31PE10.3,' PARSECS'/37X,'EXPONENT OF POWER LAW DENSITY DISTRIBUTIONABBQ0628
     4, POW = ',1PE10.3/44X,'NUMBER DENSITY OF DUST AT CENTER, RHOBAR = ABBQ0629
     5', 51PE10.3//38X,'TOTAL OPTICAL DEPTH (',1PE10.3,' MICRON), TAUOF ABBQ0630
     6= ',1PE10.3//59X,'NUMBER OF GRID POINTS, NR = ',I3/53X,'NUMBER OF ABBQ0631
     7IMPACT PARAMETERS, NP = ',I3/53X,'NUMBER OF FREQUENCY POINTS, NFD ABBQ0632
     8=',I3///58X,'CONVERGENCE PARAMETER, EPS = ',1PE10.3/57X,          ABBQ0633
     9'NORMALIZATION CONSTANT, AJ0 = ',1PE10.3/52X,'MAXIMUM NO. OF ITERAABBQ0634
     1TIONS, ITMAX = ',I3/)                                             ABBQ0635
  112 FORMAT(1H1,45X,'INPUT PARAMETERS FOR DUST CLOUD MODEL'///20X,'MODEABBQ0636
     1L PARAMETERS'//60X,'OUTER CLOUD RADIUS, RMAX =',1PE10.3,' PARSECS'ABBQ0637
     2/60X,'INNER CLOUD RADIUS, RMIN = ',1PE10.3,' PARSECS'/40X,'RATIO OABBQ0638
     3F CENTRAL TO SURFACE DENSITIES, RHOCS = ',1PE10.3/44X,'NUMBER DENSABBQ0639
     4ITY OF DUST AT CENTER, RHOBAR = ',1PE10.3//38X,'TOTAL OPTICAL DEPTABBQ0640
     5H (',1PE10.3,' MICRON), TAUOF = ',1PE10.3//59X,'NUMBER OF GRID POIABBQ0641
     6NTS, NR = ',I3/53X,'NUMBER OF IMPACT PARAMETERS, NP = ',I3/53X,'NUABBQ0642
     7MBER OF FREQUENCY POINTS, NFD =',I3///58X,'CONVERGENCE PARAMETER, ABBQ0643
     8EPS = ',1PE10.3/57X,'NORMALIZATION CONSTANT, AJ0 = ',1PE10.3/52X,'ABBQ0644
     9MAXIMUM NO. OF ITERATIONS, ITMAX = ',I3/)                         ABBQ0645
      IF (IGEOM.EQ.0) YGEOM=XPLAN                                       ABBQ0646
      IF (IGEOM.EQ.1) YGEOM=XCYL                                        ABBQ0647
      IF (IGEOM.EQ.2) YGEOM=XSPHER                                      ABBQ0648
      IF (IEDFTR.EQ.0) YEDFTR=XYES                                      ABBQ0649
      IF (IEDFTR.EQ.1) YEDFTR=XNO                                       ABBQ0650
      IF (IOUT.EQ.0) YOUT=XNO                                           ABBQ0651
      IF (IOUT.EQ.1) YOUT=XYES                                          ABBQ0652
      IF (IC.EQ.0) YC=XNO                                               ABBQ0653
      IF (IC.EQ.1) YC=XYES                                              ABBQ0654
      IF (NH.EQ.0) YH=XNO                                               ABBQ0655
      IF (NH.EQ.1) YH=XYES                                              ABBQ0656
      IF (ISCA.EQ.0) YSCA=XNO                                           ABBQ0657
      IF (ISCA.EQ.1) YSCA=XYES                                          ABBQ0658
      IF (IB.EQ.0) YB=XNO                                               ABBQ0659
      IF (IB.EQ.1) YB=XYES                                              ABBQ0660
      WRITE (6,222) YEDFTR,IEDFTR,YOUT,IOUT,YC,IC,YH,NH,                ABBQ0661
     1              YSCA,ISCA,YB,IB,YGEOM,IGEOM                         ABBQ0662
  222 FORMAT (///52X,                                                   ABBQ0663
     1 'EDDINGTON APPROXIMATION --- ',A8,' (IEDFTR = ',I1,')'/47X,      ABBQ0664
     2 'DETAILED PRINTOUT OF RESULTS --- ',A8,' (IOUT   = ',I1,')'/     ABBQ0665
     3 63X,'CENTRAL CORE --- ',A8,' (IC     = ',I1,')'/                 ABBQ0666
     4 39X,'USE TOTAL NET FLUX AT INNER BOUNDARY --- ',A8,' (NH     = ',ABBQ0667
     5 I1,')'/41X,'FIRST-ORDER ANISOTROPIC SCATTERING --- ',A8,         ABBQ0668
     6 ' (ISCA   = ',I1,')'/44X,'INCIDENT FLUX AT OUTER BOUNDARY --- ', ABBQ0669
     7 A8,' (IB     = ',I1,')'/61X,'CLOUD GEOMETRY --- ',A8,            ABBQ0670
     8 ' (IGEOM  = ',I1,')'/)                                           ABBQ0671
      WRITE (6,333) (IG,GRAINC(IG),AC(IG),GRAINM(IG),AM(IG),IG=1,IMIX)  ABBQ0672
  333 FORMAT(//20X,'DUST COMPONENT',I2//49X,'RADIUS OF INNER CORE (',A8,ABBQ0673
     1 '), AC = ',1PE10.3,' MICRON'/47X,'RADIUS OF OUTER MANTLE (',A8 , ABBQ0674
     2'), AM = ',1PE10.3,' MICRON')                                     ABBQ0675
      IF (IC.EQ.0) GO TO 175                                            ABBQ0676
C***********************************************************************ABBQ0677
C   INITIALIZE PARAMETERS FOR CENTRAL HEAT SOURCE                       ABBQ0678
C   IF TSTAR.GT.0 HEAT SOURCE IS A BLACKBODY OF TEMPERATURE TSTAR       ABBQ0679
C   IF TSTAR.LE.0 HEAT SOURCE IS A POWER LAW OF INDEX TSTAR             ABBQ0680
C***********************************************************************ABBQ0681
      RMIN=R(1)*R0                                                      ABBQ0682
      IF (TSTAR.LT.0.0) GO TO 165                                       ABBQ0683
      CON=0.25*AJ0                                                      ABBQ0684
      SUMF=0.0                                                          ABBQ0685
      DO 150 J=1,NFD                                                    ABBQ0686
      CALL GETBJ (FREQD(J),TSTAR,0,BJ0,BJ1)                             ABBQ0687
      FLUXCD(J)=CON*BJ0                                                 ABBQ0688
      SUMF=SUMF+FLUXCD(J)*WFD(J)                                        ABBQ0689
  150 CONTINUE                                                          ABBQ0690
      XX=(TLUM*3.9E33)/(SUMF*16.0*PI*PI)                                ABBQ0691
      RSTAR=DSQRT(XX)                                                   ABBQ0692
      TSTAR1=(SUMF/4.5117E-06)**0.25                                    ABBQ0693
      PNORM=(RSTAR*RSTAR)/(RMIN*RMIN)                                   ABBQ0694
      RSTAR=RSTAR/6.96E10                                               ABBQ0695
      DO 160 J=1,NFD                                                    ABBQ0696
      FLUXCD(J)=PNORM*FLUXCD(J)                                         ABBQ0697
  160 CONTINUE                                                          ABBQ0698
      SUMF1=4.5117E-06*TSTAR**4                                         ABBQ0699
      XRATIO=SUMF/SUMF1                                                 ABBQ0700
      TLUM1=TLUM/XRATIO                                                 ABBQ0701
      XRATIO=100.0*XRATIO                                               ABBQ0702
C***********************************************************************ABBQ0703
C   WRITE STELLAR CHARACTERISTICS TO OUTPUT                             ABBQ0704
C***********************************************************************ABBQ0705
      WRITE (6,444) TSTAR,TSTAR1,RSTAR,TLUM,TLUM1,SUMF,SUMF1,XRATIO     ABBQ0706
  444 FORMAT (///20X,'CENTRAL HEAT SOURCE'//40X,'SPECIFIED TEMPERATURE OABBQ0707
     1F CENTRAL STAR, TSTAR = ',1PE10.3,' DEGREES'/39X,'EFFECTIVE TEMPERABBQ0708
     2ATURE OF CENTRAL STAR, TSTAR1 = ',1PE10.3,' DEGREES'/55X,'RADIUS OABBQ0709
     3F CENTRAL STAR, RSTAR = ',1PE10.3,' SOLAR RADII'/42X,'LUMINOSITY OABBQ0710
     4F CENTRAL STAR .GE. 912A, TLUM = ',1PE10.3,' SOLAR UNITS'/45X,    ABBQ0711
     5 'TOTAL LUMINOSITY OF CENTRAL STAR, TLUM1 = ',1PE10.3,' SOLAR UNITABBQ0712
     6S'/49X,'INTEGRATED NET FLUX .GE. 912A, SUMF = ',1PE10.3,' ERGS/(SEABBQ0713
     7C*HZ*CM**2)'/52X,'TOTAL INTEGRATED NET FLUX, SUMF1 = ',1PE10.3,   ABBQ0714
     8' ERGS/(SEC*HZ*CM**2)'/35X,'FRACTION OF ENERGY FOR HEATING THE GRAABBQ0715
     9INS, XRATIO = ',1PE10.3,' PERCENT')                               ABBQ0716
      GO TO 175                                                         ABBQ0717
  165 CONTINUE                                                          ABBQ0718
C***********************************************************************ABBQ0719
C   CENTRAL HEAT SOURCE WITH POWER LAW ENERGY DISTRIBUTION              ABBQ0720
C***********************************************************************ABBQ0721
      IF (TSTAR.EQ.(-1.0)) CON=1.0/DLOG(FREQD(NFD)/FREQD(1))            ABBQ0722
      XX=1.0+TSTAR                                                      ABBQ0723
      IF (TSTAR.NE.(-1.0)) CON=XX/(FREQD(NFD)**XX-FREQD(1)**XX)         ABBQ0724
      PNORM=(TLUM*3.9E33*CON)/(16.0*PI*RMIN*RMIN)                       ABBQ0725
      DO 170 J=1,NFD                                                    ABBQ0726
      FLUXCD(J)=PNORM*FREQD(J)**TSTAR                                   ABBQ0727
  170 CONTINUE                                                          ABBQ0728
C***********************************************************************ABBQ0729
C   WRITE CHARACTERISTICS OF POWER LAW SOURCE TO OUTPUT                 ABBQ0730
C***********************************************************************ABBQ0731
      WRITE (6,555) TSTAR,TLUM                                          ABBQ0732
  555 FORMAT (///20X,'CENTRAL HEAT SOURCE'//51X,'SPECTRAL INDEX (POWER LABBQ0733
     1AW), TSTAR = ',1PE10.3/62X,'TOTAL LUMINOSITY, TLUM = ',1PE10.3,   ABBQ0734
     2 ' SOLAR UNITS')                                                  ABBQ0735
  175 CONTINUE                                                          ABBQ0736
      DO 180 IG=1,IMIX                                                  ABBQ0737
      DO 180 J=1,NFD                                                    ABBQ0738
      ALBEDI(J,IG)=QSCAI(J,IG)/(QABSI(J,IG)+QSCAI(J,IG))                ABBQ0739
  180 CONTINUE                                                          ABBQ0740
      DO 190 K1=1,IMIX,2                                                ABBQ0741
      K2=MIN0(IMIX,K1+2)                                                ABBQ0742
      WRITE (6,991) (K,K,K,K,K=K1,K2)                                   ABBQ0743
  991 FORMAT (1H1,1X,'IF',5X,'MICRON',2X,2(3X,'QABSI(',I2,')',3X,       ABBQ0744
     1 'QSCAI(',I2,')',3X,'GBARI(',I2,')',2X,'ALBEDI(',I2,')')/)        ABBQ0745
      DO 190 J=1,NFD                                                    ABBQ0746
      WRITE (6,992) J,WLAMDA(J),(QABSI(J,K),QSCAI(J,K),GBARI(J,K),      ABBQ0747
     1              ALBEDI(J,K),K=K1,K2)                                ABBQ0748
  992 FORMAT (1X,I3,1X,1P9E12.3)                                        ABBQ0749
  190 CONTINUE                                                          ABBQ0750
      DO 210 K1=1,IMIX,5                                                ABBQ0751
      K2=MIN0(IMIX,K1+4)                                                ABBQ0752
      WRITE (6,993) (K,K,K=K1,K2)                                       ABBQ0753
  993 FORMAT (1H1,1X,'IR',7X,'R',3X,5(3X,'ABUNDI(',I2,')',2X,'TDI(',    ABBQ0754
     1 I2,')')/)                                                        ABBQ0755
      DO 210 L=1,NR                                                     ABBQ0756
      WRITE (6,994) L,R(L),(ABUNDI(K,L),TDI(K,L),K=K1,K2)               ABBQ0757
  994 FORMAT (1H9,I3,1X,1P11E11.3)                                      ABBQ0758
  210 CONTINUE                                                          ABBQ0759
C***********************************************************************ABBQ0760
C   INITIALIZE DUST ABSORPTION AND SCATTERING COEFFICIENTS              ABBQ0761
C***********************************************************************ABBQ0762
      DO 220 J=1,NFD                                                    ABBQ0763
      DO 220 L=1,NR                                                     ABBQ0764
      CGBAR(J,L)=0.0                                                    ABBQ0765
  220 CONTINUE                                                          ABBQ0766
      DO 240 J=1,NFD                                                    ABBQ0767
      DO 240 L=1,NR                                                     ABBQ0768
      SUMA=0.0                                                          ABBQ0769
      SUMS=0.0                                                          ABBQ0770
      SUMG=0.0                                                          ABBQ0771
      DO 230 IG=1,IMIX                                                  ABBQ0772
      SUMA=SUMA+ABUNDI(IG,L)*QABSI(J,IG)                                ABBQ0773
      XX=ABUNDI(IG,L)*QSCAI(J,IG)                                       ABBQ0774
      SUMS=SUMS+XX                                                      ABBQ0775
      SUMG=SUMG+XX*GBARI(J,IG)                                          ABBQ0776
  230 CONTINUE                                                          ABBQ0777
      XX=R0*RHOD(L)                                                     ABBQ0778
      QABS(J,L)=SUMA*XX                                                 ABBQ0779
      QSCA(J,L)=SUMS*XX                                                 ABBQ0780
      IF (ISCA.NE.0) CGBAR(J,L)=SUMG*XX                                 ABBQ0781
  240 CONTINUE                                                          ABBQ0782
C***********************************************************************ABBQ0783
C   WRITE QABS, QSCA ONTO DISK FILE                                     ABBQ0784
C***********************************************************************ABBQ0785
      CALL BRANWT (NQASD,DUMMY(NDF1P1),NB2D)                            ABBQ0786
C***********************************************************************ABBQ0787
C   WRITE CGBAR ONTO DISK FILE                                          ABBQ0788
C***********************************************************************ABBQ0789
      CALL BRANWT(NGBD,DUMMY(NDF5P1),NB1D)                              ABBQ0790
C***********************************************************************ABBQ0791
C   CALCULATE OPTICAL DEPTH PROFILE                                     ABBQ0792
C***********************************************************************ABBQ0793
      DO 260 J=1,NFD                                                    ABBQ0794
      DO 250 L=1,NR                                                     ABBQ0795
      XX=QABS(J,L)+QSCA(J,L)                                            ABBQ0796
      CHICG(J,L)=XX-CGBAR(J,L)                                          ABBQ0797
      CHIF(L)=XX                                                        ABBQ0798
  250 CONTINUE                                                          ABBQ0799
      CALL GETAU (R,CHIF,TAUX,NR)                                       ABBQ0800
      TAU0(J)=TAUX(1)                                                   ABBQ0801
  260 CONTINUE                                                          ABBQ0802
C***********************************************************************ABBQ0803
C   READ BTDAV, BTDAVI FROM DISK FILE                                   ABBQ0804
C***********************************************************************ABBQ0805
      CALL BRANRD (NBTAD,DUMMY(NDF5P1),NB1D)                            ABBQ0806
C***********************************************************************ABBQ0807
C   CALCULATE AND WRITE CHI AND ETA ONTO DISK FILE                      ABBQ0808
C***********************************************************************ABBQ0809
      DO 270 J=1,NFD                                                    ABBQ0810
      DO 270 L=1,NR                                                     ABBQ0811
      CHI(J,L)=QABS(J,L)                                                ABBQ0812
      ETA(J,L)=QABS(J,L)*BTDAV(J,L)                                     ABBQ0813
  270 CONTINUE                                                          ABBQ0814
      CALL BRANWT (NCED,DUMMY(NDF3P1),NB2D)                             ABBQ0815
C***********************************************************************ABBQ0816
C   WRITE CHICG ONTO DISK FILE                                          ABBQ0817
C***********************************************************************ABBQ0818
      CALL BRANWT (NCGD,DUMMY(NDF6P1),NB1D)                             ABBQ0819
C***********************************************************************ABBQ0820
C   PRINT OUT PHYSICAL PROPERTIES OF MODEL                              ABBQ0821
C***********************************************************************ABBQ0822
      WRITE (6,666) (J,WLAMDA(J),FREQD(J),WFD(J),FLUXCD(J),             ABBQ0823
     1               FLUXSD(J),TAU0(J),J=1,NFD)                         ABBQ0824
  666 FORMAT (1H1,1X,'IF',4X,'MICRON',7X,'FREQD',7X,'WFD',8X,'FLUXCD',  ABBQ0825
     1 6X,'FLUXSD',7X,'TAU0'//(1X,I3,1P6E12.4))                         ABBQ0826
      WRITE (6,777)                                                     ABBQ0827
  777 FORMAT (1H1,1X,'IR',10X,'R',13X,'RHOD',11X,'TD',12X,              ABBQ0828
     1 'TOF')                                                           ABBQ0829
      WRITE (6,888) (L,R(L),RHOD(L),TD(L),TOF(L),L=1,NR)                ABBQ0830
  888 FORMAT (1H9,I3,5X,1PE10.3,5X,1PE10.3,5X,1PE10.3,                  ABBQ0831
     1 5X,1PE10.3)                                                      ABBQ0832
C***********************************************************************ABBQ0833
C   COMPUTE QUADRATURE WEIGHTS FOR THE SOLUTION OF                      ABBQ0834
C   MOMENT AND RAY EQUATIONS                                            ABBQ0835
C***********************************************************************ABBQ0836
      CALL QUDWTS                                                       ABBQ0837
      RETURN                                                            ABBQ0838
      END                                                               ABBQ0839
                                                                        ABBQ0840
C***********************************************************************ABBQ0841
C   SUBROUTINE QUDWTS CALCULATES AND STORES THE QUADRATURE              ABBQ0842
C   WEIGHTS FOR THE MOMENT AND RAY EQUATIONS. WEIGHTS FOR EVALUATING THEABBQ0843
C   MOMENT INTEGRALS ARE COMPUTED BY CALLING WTPLSP FOR THE SPHERICAL   ABBQ0844
C   CASE AND WTCYL FOR THE CYLINDRICAL CASE                             ABBQ0845
C***********************************************************************ABBQ0846
      SUBROUTINE QUDWTS                                                 ABBQ0847
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ0848
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ0849
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         ABBQ0850
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   ABBQ0851
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     ABBQ0852
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     ABBQ0853
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    ABBQ0854
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     ABBQ0855
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1,                     ABBQ0856
     6           NQWT61=NQWT5+NQWT,NQWT99=12*NQWT,NQWT91=9*NQWT+1)      ABBQ0857
      PARAMETER (NQG=4,NQGT=NQWT2+NQG,NQGT1=NQGT+NQG,                   ABBQ0858
     1           NQGT2=NQGT1+NQG,NCQW=NQGT2+NQG-1,NTHETA=20)            ABBQ0859
      COMMON QW(NQWT0),X(NPMAX),WJ1(NPMAX),WH1(NPMAX),WK1(NPMAX)        ABBQ0860
      DIMENSION WZ1(NQWT),WZ2(NQWT),WZ3(NQWT),WJ(NQWT),WK(NQWT),        ABBQ0861
     1          WB(NPMAX),WC(NC1),XMUSQ(NQWT),P2R3(NQWT),               ABBQ0862
     2          WZETA1(ND),WZETA2(ND),WH(NQWT),WY(NQWT),XL(NPMAX)       ABBQ0863
      DIMENSION WJO(NQG),WHO(NQG),WKO(NQG),WYO(NQG)                     ABBQ0864
      EQUIVALENCE (QW(1),WZ1(1)),(QW(NQWT1),WZ2(1)),(QW(NQWT2),WZ3(1)), ABBQ0865
     1            (QW(NQWT3),WJ(1)),(QW(NQWT4),WK(1)),(QW(NQWT5),WB(1)),ABBQ0866
     2            (QW(NQWT6),WC(1)),(QW(NQWT7),XMUSQ(1)),               ABBQ0867
     3            (QW(NQWT8),P2R3(1)),(QW(NQWT9),WZETA1(1)),            ABBQ0868
     4            (QW(NQWT10),WZETA2(1))                                ABBQ0869
      EQUIVALENCE (CQW(1),WH(1)),(CQW(NQWT1),WY(1)),(CQW(NQWT2),WJO(1)),ABBQ0870
     1            (CQW(NQGT),WHO(1)),(CQW(NQGT1),WKO(1)),               ABBQ0871
     2            (CQW(NQGT2),WYO(1))                                   ABBQ0872
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ0873
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ0874
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             ABBQ0875
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ0876
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ0877
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ0878
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG),XMU(NTHETA)             ABBQ0879
      NQW=NQWT0                                                         ABBQ0880
C***********************************************************************ABBQ0881
C   COMPUTE QUADRATURE WEIGHTS FOR COMBINED MOMENT EQUATIONS            ABBQ0882
C***********************************************************************ABBQ0883
      WR1(1)=R(2)-R(1)                                                  ABBQ0884
      WR2(1)=0.5*WR1(1)*WR1(1)                                          ABBQ0885
      XX=(R(1)**IGEOM)                                                  ABBQ0886
      IF (R(1).EQ.0.0) XX=1.0                                           ABBQ0887
      WR3(1)=0.5*(R(2)**IGEOM)/XX                                       ABBQ0888
      IF (R(1).EQ.0.0) GO TO 5                                          ABBQ0889
      XX=DLOG(R(2)/R(1))/(R(2)-R(1))                                    ABBQ0890
      WZETA1(1)=R(2)*XX-1.0                                             ABBQ0891
      WZETA2(1)=1.0-R(1)*XX                                             ABBQ0892
      GO TO 6                                                           ABBQ0893
    5 CONTINUE                                                          ABBQ0894
      WZETA1(1)=0.0                                                     ABBQ0895
      WZETA2(1)=0.5                                                     ABBQ0896
    6 CONTINUE                                                          ABBQ0897
      DO 10 L=2,N1                                                      ABBQ0898
      B=R(L)-R(L-1)                                                     ABBQ0899
      F=R(L+1)-R(L)                                                     ABBQ0900
      R2=R(L)**IGEOM                                                    ABBQ0901
      WR1(L)=F*R2                                                       ABBQ0902
      WR2(L)=F*(R(L-1)**IGEOM)                                          ABBQ0903
      WR3(L)=B*R2                                                       ABBQ0904
      WR4(L)=B*(R(L+1)**IGEOM)                                          ABBQ0905
      WR5(L)=B*F*(B+F)*R2                                               ABBQ0906
      IF (IGEOM.EQ.0) GOTO 10                                           ABBQ0907
      XX=DLOG(R(L+1)/R(L))/F                                            ABBQ0908
      WZETA1(L)=R(L+1)*XX-1.0                                           ABBQ0909
      WZETA2(L)=1.0-R(L)*XX                                             ABBQ0910
   10 CONTINUE                                                          ABBQ0911
      WR1(NR)=R(NR)-R(N1)                                               ABBQ0912
      WR2(NR)=0.5*WR1(NR)*WR1(NR)                                       ABBQ0913
      WR3(NR)=0.5*(R(N1)**IGEOM)/(R(NR)**IGEOM)                         ABBQ0914
   12 CONTINUE                                                          ABBQ0915
      IF (IGEOM.NE.0) GOTO 45                                           ABBQ0916
C***********************************************************************ABBQ0917
C   COMPUTE QUADRATURE WEIGHTS FOR THE PLANAR CASE                      ABBQ0918
C***********************************************************************ABBQ0919
      CALL WTPLSP (XMU,WJ1,WH1,WK1,NTHETA,0)                            ABBQ0920
      DO 20 K=1,NTHETA                                                  ABBQ0921
      WK(K)=WK1(K)                                                      ABBQ0922
      WB(K)=WH1(K)                                                      ABBQ0923
      WC(K)=WB(K)                                                       ABBQ0924
      WJ(K)=WJ1(K)                                                      ABBQ0925
      XMUSQ(K)=XMU(K)*XMU(K)                                            ABBQ0926
   20 CONTINUE                                                          ABBQ0927
      JZ=0                                                              ABBQ0928
      DO 40 IP=1,NTHETA                                                 ABBQ0929
C***********************************************************************ABBQ0930
C   CALCULATE IMPACT PARAMETERS FOR PLANAR CASE                         ABBQ0931
C***********************************************************************ABBQ0932
      P(IP)=DSQRT(1.0-XMUSQ(IP))                                        ABBQ0933
      XMUI=1.0/XMU(IP)                                                  ABBQ0934
      JZ=JZ+1                                                           ABBQ0935
      WZ1(JZ)=0.5*XMUI*(R(2)-R(1))                                      ABBQ0936
      WZ2(JZ)=2.0*WZ1(JZ)*WZ1(JZ)                                       ABBQ0937
      DO 30 L=2,N1                                                      ABBQ0938
      JZ=JZ+1                                                           ABBQ0939
      B=R(L)-R(L-1)                                                     ABBQ0940
      F=R(L+1)-R(L)                                                     ABBQ0941
      WZ1(JZ)=F                                                         ABBQ0942
      WZ2(JZ)=B*F*(B+F)*XMUI*XMUI                                       ABBQ0943
      WZ3(JZ)=B                                                         ABBQ0944
   30 CONTINUE                                                          ABBQ0945
      JZ=JZ+1                                                           ABBQ0946
      WZ1(JZ)=0.5*XMUI*(R(NR)-R(N1))                                    ABBQ0947
      WZ2(JZ)=2.0*WZ1(JZ)*WZ1(JZ)                                       ABBQ0948
   40 CONTINUE                                                          ABBQ0949
C***********************************************************************ABBQ0950
C    WRITE QUADRATURE WEIGHTS FOR PLANAR CASE ONTO DISK                 ABBQ0951
C***********************************************************************ABBQ0952
      CALL BRANWT (NEFQW,QW(1),NQW)                                     ABBQ0953
      RETURN                                                            ABBQ0954
   45 CONTINUE                                                          ABBQ0955
C***********************************************************************ABBQ0956
C   CALCULATE IMPACT PARAMETERS FOR CYLINDRICAL AND SPHERICAL CASES     ABBQ0957
C***********************************************************************ABBQ0958
      IP0=0                                                             ABBQ0959
      IF (IC.NE.0) IP0=NC                                               ABBQ0960
      NP=IP0+NR                                                         ABBQ0961
      IP1=IP0+1                                                         ABBQ0962
      IP2=IP1+1                                                         ABBQ0963
      NPS1=NP-1                                                         ABBQ0964
      DO 50 IP=IP1,NP                                                   ABBQ0965
      JP=IP-IP0                                                         ABBQ0966
      P(IP)=R(JP)                                                       ABBQ0967
   50 CONTINUE                                                          ABBQ0968
      IF (IC.EQ.0) GO TO 65                                             ABBQ0969
C***********************************************************************ABBQ0970
C   CALCULATE IMPACT PARAMETERS THROUGH CORE                            ABBQ0971
C***********************************************************************ABBQ0972
      P(1)=0.0                                                          ABBQ0973
      DP=R(2)-R(1)                                                      ABBQ0974
      DO 60 JP=2,IP0                                                    ABBQ0975
      IP=IP1-JP+1                                                       ABBQ0976
      P(IP)=P(IP+1)-DP                                                  ABBQ0977
      DP=2.1*DP                                                         ABBQ0978
      IF (DP.GE.P(IP)) DP=0.5*P(IP)                                     ABBQ0979
   60 CONTINUE                                                          ABBQ0980
   65 CONTINUE                                                          ABBQ0981
C***********************************************************************ABBQ0982
C   COMPUTE AND ASSIGN WEIGHTS FOR MOMENT INTEGRALS                     ABBQ0983
C***********************************************************************ABBQ0984
      X(1)=0.0                                                          ABBQ0985
      J=0                                                               ABBQ0986
      IPMAX=IP0                                                         ABBQ0987
      DO 120 L=1,NR                                                     ABBQ0988
      IPMAX=IPMAX+1                                                     ABBQ0989
      IF (IPMAX.EQ.1) GO TO 75                                          ABBQ0990
      R2=R(L)*R(L)                                                      ABBQ0991
      IPM1=IPMAX-1                                                      ABBQ0992
      DO 70 IP=1,IPM1                                                   ABBQ0993
      P2=P(IPMAX-IP)*P(IPMAX-IP)                                        ABBQ0994
      IF (IGEOM.EQ.1) P2=P(IP+1)*P(IP+1)                                ABBQ0995
      IF (IGEOM.EQ.1) X(IP+1)=DSQRT((P2/R2))                            ABBQ0996
      IF (IGEOM.EQ.2) X(IP+1)=DSQRT(1.0-P2/R2)                          ABBQ0997
   70 CONTINUE                                                          ABBQ0998
   75 CONTINUE                                                          ABBQ0999
C***********************************************************************ABBQ1000
C   CALL SUBROUTINE TO COMPUTE MOMENT INTEGRAL WEIGHTS                  ABBQ1001
C***********************************************************************ABBQ1002
      IF (IGEOM.EQ.1) CALL WTCYL(X,WJ1,WH1,WK1,WY1,                     ABBQ1003
     1    WJO,WHO,WKO,WYO,IPMAX)                                        ABBQ1004
      IF (IGEOM.EQ.2) CALL WTPLSP (X,WJ1,WH1,WK1,IPMAX,2)               ABBQ1005
      J=J+1                                                             ABBQ1006
      K=J                                                               ABBQ1007
      DO 80 I=1,IP1                                                     ABBQ1008
      LL=IPMAX-I+1                                                      ABBQ1009
      WJ(K)=WJ1(LL)                                                     ABBQ1010
      WK(K)=WK1(LL)                                                     ABBQ1011
      IF (IGEOM.NE.1) GO TO 77                                          ABBQ1012
      WH(K)=WK1(LL)                                                     ABBQ1013
      WY(K)=WY1(LL)                                                     ABBQ1014
   77 CONTINUE                                                          ABBQ1015
      IF (L.EQ.1) WC(I)=WH1(LL)                                         ABBQ1016
      K=K+NR                                                            ABBQ1017
   80 CONTINUE                                                          ABBQ1018
      IF (IPMAX.LE.IP1) GO TO 120                                       ABBQ1019
      K=K-1                                                             ABBQ1020
      DO 90 I=IP2,IPMAX                                                 ABBQ1021
      LL=IPMAX-I+1                                                      ABBQ1022
      WJ(K)=WJ1(LL)                                                     ABBQ1023
      WK(K)=WK1(LL)                                                     ABBQ1024
      IF (IGEOM.NE.1) GO TO 88                                          ABBQ1025
      WH(K)=WH1(LL)                                                     ABBQ1026
      WY(K)=WY1(LL)                                                     ABBQ1027
   88 CONTINUE                                                          ABBQ1028
      K=K+NR-(I-IP0)                                                    ABBQ1029
   90 CONTINUE                                                          ABBQ1030
      IF (L.NE.NR) GO TO 120                                            ABBQ1031
      DO 110 I=1,IPMAX                                                  ABBQ1032
      LL=IPMAX-I+1                                                      ABBQ1033
      WB(I)=WH1(LL)                                                     ABBQ1034
  110 CONTINUE                                                          ABBQ1035
  120 CONTINUE                                                          ABBQ1036
C***********************************************************************ABBQ1037
C   COMPUTE WEIGHTS FOR COMBINED RAY EQUATIONS FOR                      ABBQ1038
C   SPHERICAL AND CYLINDRICAL CASES                                     ABBQ1039
C***********************************************************************ABBQ1040
      IBC=1                                                             ABBQ1041
  122 CONTINUE                                                          ABBQ1042
      JZ=0                                                              ABBQ1043
      IR0=1                                                             ABBQ1044
      STH=1.0                                                           ABBQ1045
      IF (IGEOM.EQ.1) STH=BETA(IBC)                                     ABBQ1046
      DO 150 IP=1,NPS1                                                  ABBQ1047
      IF (IP.GT.IP1) IR0=IR0+1                                          ABBQ1048
      IR1=IR0+1                                                         ABBQ1049
      P2=P(IP)*P(IP)                                                    ABBQ1050
      DO 130 L=IR0,NR                                                   ABBQ1051
      R2=R(L)*R(L)                                                      ABBQ1052
      X(L)=DSQRT(R2-P2)                                                 ABBQ1053
  130 CONTINUE                                                          ABBQ1054
      JZ=JZ+1                                                           ABBQ1055
      WZ1(JZ)=0.5*(X(IR1)-X(IR0))/STH                                   ABBQ1056
      WZ2(JZ)=2.0*WZ1(JZ)*WZ1(JZ)                                       ABBQ1057
      IF (R(IR0).EQ.0.0) GO TO 133                                      ABBQ1058
      XMUSQ(JZ)=(X(IR0)/R(IR0))**2                                      ABBQ1059
      P2R3(JZ)=P2/R(IR0)**3                                             ABBQ1060
      GO TO 135                                                         ABBQ1061
  133 CONTINUE                                                          ABBQ1062
      XMUSQ(JZ)=1.0                                                     ABBQ1063
      P2R3(JZ)=0.0                                                      ABBQ1064
  135 CONTINUE                                                          ABBQ1065
      IF (IP.EQ.NPS1) GO TO 144                                         ABBQ1066
      DO 140 L=IR1,N1                                                   ABBQ1067
      JZ=JZ+1                                                           ABBQ1068
      B=(X(L)-X(L-1))/STH                                               ABBQ1069
      F=(X(L+1)-X(L))/STH                                               ABBQ1070
      WZ1(JZ)=F                                                         ABBQ1071
      WZ2(JZ)=B*F*(B+F)                                                 ABBQ1072
      WZ3(JZ)=B                                                         ABBQ1073
      XMUSQ(JZ)=(X(L)/R(L))**2                                          ABBQ1074
      P2R3(JZ)=P2/R(L)**3                                               ABBQ1075
  140 CONTINUE                                                          ABBQ1076
  144 CONTINUE                                                          ABBQ1077
      JZ=JZ+1                                                           ABBQ1078
      WZ1(JZ)=0.5*(X(NR)-X(N1))/STH                                     ABBQ1079
      WZ2(JZ)=2.0*WZ1(JZ)*WZ1(JZ)                                       ABBQ1080
      XMUSQ(JZ)=(X(NR)/R(NR))**2                                        ABBQ1081
      P2R3(JZ)=P2/R(NR)**3                                              ABBQ1082
  150 CONTINUE                                                          ABBQ1083
C***********************************************************************ABBQ1084
C   WRITE WEIGHTS TO DISK FILE                                          ABBQ1085
C***********************************************************************ABBQ1086
      WRITE (NEFQW) QW                                                  ABBQ1087
      IBC=IBC+1                                                         ABBQ1088
      IF (IGEOM.EQ.1.AND.IBC.LE.NQG) GOTO 122                           ABBQ1089
      REWIND NEFQW                                                      ABBQ1090
      IF (IGEOM.EQ.1) CALL BRANWT (NCYQW,CQW(1),NCQW)                   ABBQ1091
      RETURN                                                            ABBQ1092
      END                                                               ABBQ1093
                                                                        ABBQ1094
C***********************************************************************ABBQ1095
C   SUBROUTINE WTCYL COMPUTES THE QUADRATURE WEIGHTS FOR                ABBQ1096
C   THE MOMENT INTEGRALS IN THE CYLINDRICAL CASE                        ABBQ1097
C   X     --- ARRAY OF ABSCISSAS                                        ABBQ1098
C   N     --- PARAMETER SPECIFYING THE NUMBER OF INTEGRATION INTERVALS  ABBQ1099
C   PH    --- RENORMALIZATION FACTOR FOR WH ARRAY                       ABBQ1100
C   PJ    --- RENORMALIZATION FACTOR FOR WJ ARRAY                       ABBQ1101
C   PK    --- RENORMALIZATION FACTOR FOR WK ARRAY                       ABBQ1102
C   PY    --- RENORMALIZATION FACTOR FOR WY ARRAY                       ABBQ1103
C   SUMH  --- SUM OF WH ARRAY ELEMENTS                                  ABBQ1104
C   SUMJ  --- SUM OF WJ ARRAY ELEMENTS                                  ABBQ1105
C   SUMK  --- SUM OF WK ARRAY ELEMENTS                                  ABBQ1106
C   SUMY  --- SUM OF WY ARRAY ELEMENTS                                  ABBQ1107
C   PHO   --- RENORMALIZATION FACTOR FOR WHO ARRAY                      ABBQ1108
C   PJO   --- RENORMALIZATION FACTOR FOR WJO ARRAY                      ABBQ1109
C   PKO   --- RENORMALIZATION FACTOR FOR WKO ARRAY                      ABBQ1110
C   PYO   --- RENORMALIZATION FACTOR FOR WYO ARRAY                      ABBQ1111
C   SUMHO --- SUM OF WHO ARRAY ELEMENTS                                 ABBQ1112
C   SUMJO --- SUM OF WJO ARRAY ELEMENTS                                 ABBQ1113
C   SUMKO --- SUM OF WKO ARRAY ELEMENTS                                 ABBQ1114
C   SUMYO --- SUM OF WYO ARRAY ELEMENTS                                 ABBQ1115
C***********************************************************************ABBQ1116
      SUBROUTINE WTCYL (X,WJ,WH,WK,WY,WJO,WHO,WKO,WYO,N)                ABBQ1117
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ1118
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ1119
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   ABBQ1120
     1           NQWT2=NQWT1+NQWT)                                      ABBQ1121
      PARAMETER (NQG=4,NQGT=NQWT2+NQG,NQGT1=NQGT+NQG,                   ABBQ1122
     1           NQGT2=NQGT1+NQG,NCQW=NQGT2+NQG-1,NTHETA=20)            ABBQ1123
      DIMENSION X(1),WJ(1),WH(1),WK(1),WY(NPMAX)                        ABBQ1124
      DIMENSION PHI(NPMAX),WJI(NPMAX),WHI(NPMAX),WKI(NPMAX),WYI(NPMAX)  ABBQ1125
      DIMENSION OMEGA(NQG),OMEG2(NQG)                                   ABBQ1126
      DIMENSION WJO(NQG),WHO(NQG),WKO(NQG),WYO(NQG)                     ABBQ1127
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      ABBQ1128
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ1129
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ1130
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG)                         ABBQ1131
      NP1=N-1                                                           ABBQ1132
      C3=1.0/3.0                                                        ABBQ1133
      IF (N.GT.1) GO TO 5                                               ABBQ1134
C***********************************************************************ABBQ1135
C   SET PHI QUADRATURE WEIGHTS FOR N=1 TO RENORMALIZATION CONSTANTS     ABBQ1136
C***********************************************************************ABBQ1137
      WJ(1)=PI/2.0                                                      ABBQ1138
      WH(1)=1.0                                                         ABBQ1139
      WK(1)=PI/4.0                                                      ABBQ1140
      WY(1)=1.0                                                         ABBQ1141
      RETURN                                                            ABBQ1142
    5 CONTINUE                                                          ABBQ1143
C***********************************************************************ABBQ1144
C   COMPUTE PHI QUADRATURE WEIGHTS FOR N>1                              ABBQ1145
C***********************************************************************ABBQ1146
      WJI(1)=0.0                                                        ABBQ1147
      WHI(1)=0.0                                                        ABBQ1148
      WKI(1)=0.0                                                        ABBQ1149
      WYI(1)=0.0                                                        ABBQ1150
      DO 10 L=1,N                                                       ABBQ1151
      PHI(L)=DASIN(X(L))                                                ABBQ1152
   10 CONTINUE                                                          ABBQ1153
      DO 20 L=1,NP1                                                     ABBQ1154
      B=X(L)                                                            ABBQ1155
      F=X(L+1)                                                          ABBQ1156
C***********************************************************************ABBQ1157
C   COMPUTE PHI WEIGHTS FOR J INTEGRAL                                  ABBQ1158
C***********************************************************************ABBQ1159
      WJI(L)=WJI(L)+0.5*(PHI(L+1)-PHI(L))                               ABBQ1160
      WJI(L+1)=0.5*(PHI(L+1)-PHI(L))                                    ABBQ1161
C***********************************************************************ABBQ1162
C   USE TRAPEZOIDAL RULE FOR WH AND WK                                  ABBQ1163
C***********************************************************************ABBQ1164
      WHI(L)=WHI(L)+0.5*(F-B)                                           ABBQ1165
      WHI(L+1)=0.5*(F-B)                                                ABBQ1166
      WKI(L)=WKI(L)+0.5*(DSQRT(1.0-B*B))*(F-B)                          ABBQ1167
      WKI(L+1)=0.5*(DSQRT(1.0-F*F))*(F-B)                               ABBQ1168
      IF (ISCA.EQ.0) GO TO 20                                           ABBQ1169
C***********************************************************************ABBQ1170
C   COMPUTE PHI WEIGHTS FOR Y INTEGRAL                                  ABBQ1171
C***********************************************************************ABBQ1172
      WYI(L)=WYI(L)+0.5*X(L)*(PHI(L+1)-PHI(L))                          ABBQ1173
      WYI(L+1)=0.5*X(L+1)*(PHI(L+1)-PHI(L))                             ABBQ1174
   20 CONTINUE                                                          ABBQ1175
      DO 30 L=1,N                                                       ABBQ1176
      WJ(L)=WJI(N+1-L)                                                  ABBQ1177
      WH(L)=WHI(N+1-L)                                                  ABBQ1178
      WK(L)=WKI(N+1-L)                                                  ABBQ1179
      IF (ISCA.EQ.0) GO TO 30                                           ABBQ1180
      WY(L)=WYI(N+1-L)                                                  ABBQ1181
   30 CONTINUE                                                          ABBQ1182
      SUMJ=0.0                                                          ABBQ1183
      SUMH=0.0                                                          ABBQ1184
      SUMK=0.0                                                          ABBQ1185
      SUMY=0.0                                                          ABBQ1186
      DO 40 L=1,N                                                       ABBQ1187
      SUMJ=SUMJ+WJ(L)                                                   ABBQ1188
      SUMH=SUMH+WH(L)                                                   ABBQ1189
      SUMK=SUMK+WK(L)                                                   ABBQ1190
      IF (ISCA.EQ.0) GO TO 40                                           ABBQ1191
      SUMY=SUMY+WY(L)                                                   ABBQ1192
   40 CONTINUE                                                          ABBQ1193
      PJ=PI/(2.0*SUMJ)                                                  ABBQ1194
      PH=1.0/SUMH                                                       ABBQ1195
      PK=PI/(4.0*SUMK)                                                  ABBQ1196
      IF (ISCA.EQ.0) GO TO 44                                           ABBQ1197
      PY=1.0/SUMY                                                       ABBQ1198
   44 CONTINUE                                                          ABBQ1199
C***********************************************************************ABBQ1200
C   RENORMALIZE PHI WEIGHTS                                             ABBQ1201
C***********************************************************************ABBQ1202
      DO 50 L=1,N                                                       ABBQ1203
      WJ(L)=PJ*WJ(L)                                                    ABBQ1204
      WH(L)=PH*WH(L)                                                    ABBQ1205
      WK(L)=PK*WK(L)                                                    ABBQ1206
      IF (ISCA.EQ.0) GO TO 50                                           ABBQ1207
      WY(L)=PY*WY(L)                                                    ABBQ1208
   50 CONTINUE                                                          ABBQ1209
C***********************************************************************ABBQ1210
C   OMEGA=COS(THETA) ARE THE ABSCISSAS FOR THE GAUSSIAN                 ABBQ1211
C   QUADRATURES                                                         ABBQ1212
C   BETA'S = SIN(THETA); USED IN RAY EQN SOLUTION                       ABBQ1213
C***********************************************************************ABBQ1214
      OMEGA(1)=0.0694318442                                             ABBQ1215
      OMEGA(2)=0.3300094782                                             ABBQ1216
      OMEGA(3)=0.6699905218                                             ABBQ1217
      OMEGA(4)=0.9305681558                                             ABBQ1218
      DO 80 L=1,NQG                                                     ABBQ1219
      OMEG2(L)=OMEGA(L)*OMEGA(L)                                        ABBQ1220
      BETA(L)=DSQRT(1.0-OMEG2(L))                                       ABBQ1221
   80 CONTINUE                                                          ABBQ1222
C***********************************************************************ABBQ1223
C   WJO, WKO,WHO, AND WYO ARE GAUSSIAN QUADRATURES FOR                  ABBQ1224
C   THE THETA INTEGRATION                                               ABBQ1225
C***********************************************************************ABBQ1226
      WJO(1)=0.1739274226                                               ABBQ1227
      WJO(2)=0.3260725774                                               ABBQ1228
      WJO(3)=0.3260725774                                               ABBQ1229
      WJO(4)=0.1739274226                                               ABBQ1230
      DO 100 L=1,NQG                                                    ABBQ1231
      WHO(L)=BETA(L)*WJO(L)                                             ABBQ1232
      WKO(L)=OMEG2(L)*WJO(L)                                            ABBQ1233
      WYO(L)=OMEGA(L)*WJO(L)                                            ABBQ1234
  100 CONTINUE                                                          ABBQ1235
      SUMJO=0.0                                                         ABBQ1236
      SUMHO=0.0                                                         ABBQ1237
      SUMKO=0.0                                                         ABBQ1238
      SUMYO=0.0                                                         ABBQ1239
      DO 60 L=1,NQG                                                     ABBQ1240
      SUMJO=SUMJO+WJO(L)                                                ABBQ1241
      SUMHO=SUMHO+WHO(L)                                                ABBQ1242
      SUMKO=SUMKO+WKO(L)                                                ABBQ1243
      SUMYO=SUMYO+WYO(L)                                                ABBQ1244
   60 CONTINUE                                                          ABBQ1245
      PJO=1.0/SUMJO                                                     ABBQ1246
      PHO=PI/(4*SUMHO)                                                  ABBQ1247
      PKO=C3/SUMKO                                                      ABBQ1248
      PYO=0.5/SUMYO                                                     ABBQ1249
      DO 70 L=1,NQG                                                     ABBQ1250
C***********************************************************************ABBQ1251
C   RENORMALIZE THETA WEIGHTS                                           ABBQ1252
C***********************************************************************ABBQ1253
      WJO(L)=PJO*WJO(L)                                                 ABBQ1254
      WHO(L)=PHO*WHO(L)                                                 ABBQ1255
      WKO(L)=PKO*WKO(L)                                                 ABBQ1256
      IF (ISCA.EQ.0) GO TO 70                                           ABBQ1257
      WYO(L)=PYO*WYO(L)                                                 ABBQ1258
   70 CONTINUE                                                          ABBQ1259
      RETURN                                                            ABBQ1260
      END                                                               ABBQ1261
                                                                        ABBQ1262
C***********************************************************************ABBQ1263
C   SUBROUTINE WTPLSP CALCULATES THE QUADRATURE WEIGHTS                 ABBQ1264
C   FOR THE MOMENT INTEGRALS IN THE PLANAR AND SPHERICAL                ABBQ1265
C   CASES                                                               ABBQ1266
C   X     --- ARRAY OF ABSCISSAS                                        ABBQ1267
C   N     --- PARAMETER SPECIFYING THE NUMBER OF INTEGRATION INTERVALS  ABBQ1268
C   PH    --- RENORMALIZATION FACTOR FOR WH ARRAY                       ABBQ1269
C   PJ    --- RENORMALIZATION FACTOR FOR WJ ARRAY                       ABBQ1270
C   PK    --- RENORMALIZATION FACTOR FOR WK ARRAY                       ABBQ1271
C   PY    --- RENORMALIZATION FACTOR FOR WY ARRAY                       ABBQ1272
C   SUMH  --- SUM OF WH ARRAY ELEMENTS                                  ABBQ1273
C   SUMJ  --- SUM OF WJ ARRAY ELEMENTS                                  ABBQ1274
C   SUMK  --- SUM OF WK ARRAY ELEMENTS                                  ABBQ1275
C   SUMY  --- SUM OF WY ARRAY ELEMENTS                                  ABBQ1276
C***********************************************************************ABBQ1277
      SUBROUTINE WTPLSP (X,WJ,WH,WK,N,IG)                               ABBQ1278
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ1279
      DIMENSION X(1),WJ(1),WH(1),WK(1)                                  ABBQ1280
      C3=1.0/3.0                                                        ABBQ1281
      C6=1.0/6.0                                                        ABBQ1282
      C12=1.0/12.0                                                      ABBQ1283
      IF (IG.NE.0) GOTO 11                                              ABBQ1284
C***********************************************************************ABBQ1285
C   COMPUTE GAUSSIAN QUADRATURE WEIGHTS FOR MOMENT INTEGRALS IN         ABBQ1286
C   PLANAR CASE                                                         ABBQ1287
C***********************************************************************ABBQ1288
      IF (N.EQ.20) GOTO 5                                               ABBQ1289
      WRITE (6,111)N                                                    ABBQ1290
  111 FORMAT(1H1,40X,'INPUT NTHETA ( = ',I2,' ) IS NOT EQUAL TO 20'/)   ABBQ1291
      STOP                                                              ABBQ1292
    5 CONTINUE                                                          ABBQ1293
      X(1)=9.96869316259256D-01                                         ABBQ1294
      X(2)=9.83577911866012D-01                                         ABBQ1295
      X(3)=9.59963099538093D-01                                         ABBQ1296
      X(4)=9.26522808214715D-01                                         ABBQ1297
      X(5)=8.83969092344513D-01                                         ABBQ1298
      X(6)=8.33208746518991D-01                                         ABBQ1299
      X(7)=7.75323580615695D-01                                         ABBQ1300
      X(8)=7.11547287818728D-01                                         ABBQ1301
      X(9)=6.43239129897547D-01                                         ABBQ1302
      X(10)=5.71854959066743D-01                                        ABBQ1303
      X(11)=4.98916184517151D-01                                        ABBQ1304
      X(12)=4.25977341817173D-01                                        ABBQ1305
      X(13)=3.54592954222632D-01                                        ABBQ1306
      X(14)=2.86284388984712D-01                                        ABBQ1307
      X(15)=2.22507407964513D-01                                        ABBQ1308
      X(16)=1.64621085368438D-01                                        ABBQ1309
      X(17)=1.13858697085453D-01                                        ABBQ1310
      X(18)=7.1300985081249D-02                                         ABBQ1311
      X(19)=3.7851287849502D-02                                         ABBQ1312
      X(20)=1.4204211159358D-02                                         ABBQ1313
      WK(1)=7.975792736277D-03                                          ABBQ1314
      WK(2)=1.7913752325739D-02                                         ABBQ1315
      WK(3)=2.6382564429126D-02                                         ABBQ1316
      WK(4)=3.2734644317573D-02                                         ABBQ1317
      WK(5)=3.6587916553227D-02                                         ABBQ1318
      WK(6)=3.7847390376922D-02                                         ABBQ1319
      WK(7)=3.6697420472018D-02                                         ABBQ1320
      WK(8)=3.3556431453475D-02                                         ABBQ1321
      WK(9)=2.9002404745589D-02                                         ABBQ1322
      WK(10)=2.3682290414246D-02                                        ABBQ1323
      WK(11)=1.8220503418269D-02                                        ABBQ1324
      WK(12)=1.3140919256662D-02                                        ABBQ1325
      WK(13)=8.813556078243D-03                                         ABBQ1326
      WK(14)=5.432090806123D-03                                         ABBQ1327
      WK(15)=3.022489170907D-03                                         ABBQ1328
      WK(16)=1.477444441971D-03                                         ABBQ1329
      WK(17)=6.07045704267D-04                                          ABBQ1330
      WK(18)=1.93888309618D-04                                          ABBQ1331
      WK(19)=4.1039102087D-05                                           ABBQ1332
      WK(20)=3.749220993D-06                                            ABBQ1333
      DO 10 L=1,N                                                       ABBQ1334
      WH(L)=WK(L)/X(L)                                                  ABBQ1335
      WJ(L)=WH(L)/X(L)                                                  ABBQ1336
   10 CONTINUE                                                          ABBQ1337
      GOTO 25                                                           ABBQ1338
   11 CONTINUE                                                          ABBQ1339
C***********************************************************************ABBQ1340
C   COMPUTE QUADRATURE WEIGHTS FOR MOMENT INTEGRALS IN                  ABBQ1341
C   SHERICAL CASE                                                       ABBQ1342
C***********************************************************************ABBQ1343
      N1=N-1                                                            ABBQ1344
      IF (N.GT.1) GO TO 15                                              ABBQ1345
C***********************************************************************ABBQ1346
C   SET WEIGHTS FOR N=1 TO RENORMALIZATION CONSTANTS                    ABBQ1347
C***********************************************************************ABBQ1348
      WJ(1)=1.0                                                         ABBQ1349
      WH(1)=0.5                                                         ABBQ1350
      WK(1)=C3                                                          ABBQ1351
      RETURN                                                            ABBQ1352
   15 CONTINUE                                                          ABBQ1353
C***********************************************************************ABBQ1354
C   COMPUTE WEIGHTS FOR N>1 USING TRAPEZOIDAL RULE                      ABBQ1355
C***********************************************************************ABBQ1356
      WJ(1)=0.0                                                         ABBQ1357
      WH(1)=0.0                                                         ABBQ1358
      WK(1)=0.0                                                         ABBQ1359
      DO 20 L=1,N1                                                      ABBQ1360
      B=X(L)                                                            ABBQ1361
      F=X(L+1)                                                          ABBQ1362
      BF=B*F                                                            ABBQ1363
      B2=B*B                                                            ABBQ1364
      B3=B2*B                                                           ABBQ1365
      F2=F*F                                                            ABBQ1366
      F3=F2*F                                                           ABBQ1367
      FB2=F*B2                                                          ABBQ1368
      F2B=F2*B                                                          ABBQ1369
      WJ(L)=WJ(L)+0.5*(F-B)                                             ABBQ1370
      WJ(L+1)=0.5*(F-B)                                                 ABBQ1371
      WH(L)=WH(L)+C6*(F2+BF-2.0*B2)                                     ABBQ1372
      WH(L+1)=C6*(2.0*F2-BF-B2)                                         ABBQ1373
      WK(L)=WK(L)+C12*(F3+F2B+FB2-3.0*B3)                               ABBQ1374
      WK(L+1)=C12*(3.0*F3-F2B-FB2-B3)                                   ABBQ1375
   20 CONTINUE                                                          ABBQ1376
   25 CONTINUE                                                          ABBQ1377
      SUMJ=0.0                                                          ABBQ1378
      SUMH=0.0                                                          ABBQ1379
      SUMK=0.0                                                          ABBQ1380
      DO 30 L=1,N                                                       ABBQ1381
      SUMJ=SUMJ+WJ(L)                                                   ABBQ1382
      SUMH=SUMH+WH(L)                                                   ABBQ1383
      SUMK=SUMK+WK(L)                                                   ABBQ1384
   30 CONTINUE                                                          ABBQ1385
      PJ=1.0/SUMJ                                                       ABBQ1386
      PH=0.5/SUMH                                                       ABBQ1387
      PK=1.0/(3.0*SUMK)                                                 ABBQ1388
C***********************************************************************ABBQ1389
C   RENORMALIZE WEIGHTS                                                 ABBQ1390
C***********************************************************************ABBQ1391
      DO 40 L=1,N                                                       ABBQ1392
      WJ(L)=PJ*WJ(L)                                                    ABBQ1393
      WH(L)=PH*WH(L)                                                    ABBQ1394
      WK(L)=PK*WK(L)                                                    ABBQ1395
   40 CONTINUE                                                          ABBQ1396
      RETURN                                                            ABBQ1397
      END                                                               ABBQ1398
                                                                        ABBQ1399
C***********************************************************************ABBQ1400
C   SUBROUTINE GETTD SOLVES FOR THE CORRECTIONS TO THE                  ABBQ1401
C   MEAN INTENSITY (AJ) AND THE DUST TEMPERATURE (TD).                  ABBQ1402
C   AJ AND TD ARE UPDATED                                               ABBQ1403
C   A  --- ELEMENTS OF W, DF(J)/DJ(I-1,J)                               ABBQ1404
C   B  --- ELEMENTS OF W, DF(J)/DJ(I,J)                                 ABBQ1405
C   C  --- ELEMENTS OF W, DF(J)/DJ(I+1,J)                               ABBQ1406
C   D1 --- F                                                            ABBQ1407
C   D2 --- U                                                            ABBQ1408
C   V  --- V                                                            ABBQ1409
C   P1 --- E                                                            ABBQ1410
C   P2 --- G                                                            ABBQ1411
C   W  --- P                                                            ABBQ1412
C   Z  --- V*W(INVERSE)*F                                               ABBQ1413
C   Q  --- Q                                                            ABBQ1414
C   Y  --- V*W(INVERSE)*U                                               ABBQ1415
C***********************************************************************ABBQ1416
      SUBROUTINE GETTD                                                  ABBQ1417
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ1418
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ1419
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ1420
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          ABBQ1421
      PARAMETER (ND5=5*ND,ND1P1=ND+1,ND2P1=2*ND+1,                      ABBQ1422
     1           ND3P1=3*ND+1,ND4P1=4*ND+1)                             ABBQ1423
      PARAMETER (NX=7*NDF+9*ND)                                         ABBQ1424
      COMMON DUMMY(NX),DD1(ND),DD2(ND),DD3(ND),DD4(ND),DD5(ND),         ABBQ1425
     1       DD6(ND),DD7(ND),BLOCK(ND5),W(ND,ND)                        ABBQ1426
      DIMENSION A(ND),B(ND),C(ND),D(ND),F(ND),G(ND),H(ND),Q(ND),V(ND),  ABBQ1427
     1          Y(ND),Z(ND),D1(ND),G1(ND),P1(ND),D2(ND),G2(ND),P2(ND)   ABBQ1428
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),DX(NDF)                 ABBQ1429
      EQUIVALENCE (DD1(1),D(1),P1(1)),(DD2(1),G(1),G2(1),P2(1)),        ABBQ1430
     1            (DD3(1),B(1)),(DD4(1),C(1)),                          ABBQ1431
     2            (DD5(1),Y(1),G1(1)),(DD6(1),Q(1),Z(1)),               ABBQ1432
     3            (DD7(1),V(1))                                         ABBQ1433
      EQUIVALENCE (DUMMY(1),AJ(1,1)),(DUMMY(NDF1P1),FK(1,1)),           ABBQ1434
     1            (DUMMY(NDF2P1),ZETA(1,1)),(DUMMY(NDF3P1),DX(1))       ABBQ1435
      EQUIVALENCE (BLOCK(1),A(1)),(BLOCK(ND1P1),D1(1)),                 ABBQ1436
     1            (BLOCK(ND2P1),D2(1)),(BLOCK(ND3P1),F(1)),             ABBQ1437
     2            (BLOCK(ND4P1),H(1))                                   ABBQ1438
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              ABBQ1439
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   ABBQ1440
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ1441
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ1442
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ1443
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ1444
C***********************************************************************ABBQ1445
C   FORM E, G, AND V ELEMENTS OF GRAND MATRIX                           ABBQ1446
C***********************************************************************ABBQ1447
      CALL JACOB1                                                       ABBQ1448
      DO 10 L=1,NR                                                      ABBQ1449
      Z(L)=0.0                                                          ABBQ1450
      DO 10 I=1,NR                                                      ABBQ1451
      W(L,I)=0.0                                                        ABBQ1452
   10 CONTINUE                                                          ABBQ1453
      DO 60 J=1,NFD                                                     ABBQ1454
C***********************************************************************ABBQ1455
C   FORM W, U, AND F ELEMENTS OF GRAND MATRIX                           ABBQ1456
C***********************************************************************ABBQ1457
      CALL JACOB2 (J,A,B,C,D1,D2,V)                                     ABBQ1458
      A(1)=1.0/B(1)                                                     ABBQ1459
      H(1)=-C(1)*A(1)                                                   ABBQ1460
      G1(1)=D1(1)*A(1)                                                  ABBQ1461
      DO 20 I=2,NR                                                      ABBQ1462
      F(I)=1.0/(B(I)+A(I)*H(I-1))                                       ABBQ1463
      H(I)=-C(I)*F(I)                                                   ABBQ1464
      G1(I)=(D1(I)-A(I)*G1(I-1))*F(I)                                   ABBQ1465
   20 CONTINUE                                                          ABBQ1466
C***********************************************************************ABBQ1467
C   WRITE A, D1, D2, F & H ONTO DISK FILE                               ABBQ1468
C***********************************************************************ABBQ1469
      WRITE (NEID) BLOCK                                                ABBQ1470
      IF (J.EQ.NFD) REWIND NEID                                         ABBQ1471
C***********************************************************************ABBQ1472
C   CONPUTE V*W(INVERSE)*U, V*W(INVERSE)*F                              ABBQ1473
C***********************************************************************ABBQ1474
      Y(NR)=G1(NR)                                                      ABBQ1475
      Z(NR)=Z(NR)+V(NR)*Y(NR)                                           ABBQ1476
      DO 30 K=1,N1                                                      ABBQ1477
      Y(NR-K)=G1(NR-K)+H(NR-K)*Y(NR-K+1)                                ABBQ1478
      Z(NR-K)=Z(NR-K)+V(NR-K)*Y(NR-K)                                   ABBQ1479
   30 CONTINUE                                                          ABBQ1480
      DO 50 L=1,NR                                                      ABBQ1481
      G2(1)=0.0                                                         ABBQ1482
      IF (L.EQ.1) G2(1)=D2(1)*A(1)                                      ABBQ1483
      DO 40 I=2,NR                                                      ABBQ1484
      IF (I.EQ.L) G2(I)=(D2(I)-A(I)*G2(I-1))*F(I)                       ABBQ1485
      IF (I.NE.L) G2(I)=-A(I)*G2(I-1)*F(I)                              ABBQ1486
   40 CONTINUE                                                          ABBQ1487
      Y(NR)=G2(NR)                                                      ABBQ1488
      W(NR,L)=W(NR,L)-V(NR)*Y(NR)                                       ABBQ1489
      DO 50 K=1,N1                                                      ABBQ1490
      Y(NR-K)=G2(NR-K)+H(NR-K)*Y(NR-K+1)                                ABBQ1491
      W(NR-K,L)=W(NR-K,L)-V(NR-K)*Y(NR-K)                               ABBQ1492
   50 CONTINUE                                                          ABBQ1493
   60 CONTINUE                                                          ABBQ1494
C***********************************************************************ABBQ1495
C   READ P1, P2 FROM DISK FILE                                          ABBQ1496
C***********************************************************************ABBQ1497
      CALL JACOB3 (P1,P2)                                               ABBQ1498
      DO 70 I=1,NR                                                      ABBQ1499
      Q(I)=P1(I)-Z(I)                                                   ABBQ1500
      W(I,I)=P2(I)+W(I,I)                                               ABBQ1501
   70 CONTINUE                                                          ABBQ1502
C***********************************************************************ABBQ1503
C   SOLVE SYSTEM OF EQUATIONS FOR J CORRECTIONS                         ABBQ1504
C***********************************************************************ABBQ1505
      CALL LINEQ1 (W,Q,G2,NR,ND)                                        ABBQ1506
      I1=NR                                                             ABBQ1507
      DO 110 J=1,NFD                                                    ABBQ1508
C***********************************************************************ABBQ1509
C   READ A,D1,D2,F & H FROM DISK FILE                                   ABBQ1510
C***********************************************************************ABBQ1511
      READ (NEID) BLOCK                                                 ABBQ1512
      IF (J.EQ.NFD) REWIND NEID                                         ABBQ1513
      D(1)=D1(1)-D2(1)*Q(1)                                             ABBQ1514
      G(1)=D(1)*A(1)                                                    ABBQ1515
      DO 80 I=2,NR                                                      ABBQ1516
      D(I)=D1(I)-D2(I)*Q(I)                                             ABBQ1517
      G(I)=(D(I)-A(I)*G(I-1))*F(I)                                      ABBQ1518
   80 CONTINUE                                                          ABBQ1519
      Y(NR)=G(NR)                                                       ABBQ1520
      DX(I1)=Y(NR)                                                      ABBQ1521
      DO 90 K=1,N1                                                      ABBQ1522
      Y(NR-K)=G(NR-K)+H(NR-K)*Y(NR-K+1)                                 ABBQ1523
      DX(I1-K)=Y(NR-K)                                                  ABBQ1524
   90 CONTINUE                                                          ABBQ1525
      I1=I1+NR                                                          ABBQ1526
  110 CONTINUE                                                          ABBQ1527
      ITCONT=1                                                          ABBQ1528
      ITCONJ=1                                                          ABBQ1529
C***********************************************************************ABBQ1530
C   READ AJ,FK,ZETA, FROM DISK FILE                                     ABBQ1531
C***********************************************************************ABBQ1532
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 ABBQ1533
      DO 120 L=1,NR                                                     ABBQ1534
C***********************************************************************ABBQ1535
C   COMPUTE DUST TEMPERATURE CORRECTIONS AND TEST FOR CONVERGENCE       ABBQ1536
C***********************************************************************ABBQ1537
      DTD(L)=Q(L)/(TD(L)+0.5*Q(L))                                      ABBQ1538
      IF (Q(L).LE.(-TD(L))) Q(L)=-0.9*TD(L)                             ABBQ1539
      IF (Q(L).GE.TD(L)) Q(L)=0.9*TD(L)                                 ABBQ1540
      TD(L)=TD(L)+Q(L)                                                  ABBQ1541
      IF (DABS(DTD(L)).GT.EPS) ITCONT=0                                 ABBQ1542
      I=L-NR                                                            ABBQ1543
      DO 120 J=1,NFD                                                    ABBQ1544
C***********************************************************************ABBQ1545
C   COMPUTE MEAN INTENSITY CORRECTIONS AND CHECK FOR CONVERGENCE        ABBQ1546
C***********************************************************************ABBQ1547
      I=I+NR                                                            ABBQ1548
      XX=DX(I)/(AJ(J,L)+0.5*DX(I))                                      ABBQ1549
      IF (DABS(XX).GT.DABS(DJD(L))) DJD(L)=XX                           ABBQ1550
      IF (DABS(XX).GT.EPS) ITCONJ=0                                     ABBQ1551
      IF (DX(I).LE.(-AJ(J,L))) DX(I)=-0.9*AJ(J,L)                       ABBQ1552
      IF (DX(I).GE.AJ(J,L)) DX(I)=0.9*AJ(J,L)                           ABBQ1553
      AJ(J,L)=AJ(J,L)+DX(I)                                             ABBQ1554
  120 CONTINUE                                                          ABBQ1555
C***********************************************************************ABBQ1556
C   WRITE AJ,FK, AND ZETA TO DISK FILE                                  ABBQ1557
C***********************************************************************ABBQ1558
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 ABBQ1559
      RETURN                                                            ABBQ1560
      END                                                               ABBQ1561
                                                                        ABBQ1562
C***********************************************************************ABBQ1563
C   SUBROUTINE JACOB1 FORMS THE V, E, AND G ELEMENTS OF                 ABBQ1564
C   THE GRAND ARRAY USED TO SOLVE FOR DTD AND DJD                       ABBQ1565
C   WORD1 --- DUMMY ARRAY USED TO WRITE ONTO NJCBD                      ABBQ1566
C   P1SUM --- ELEMENTS OF E                                             ABBQ1567
C   P2SUM --- ELEMENTS OF G                                             ABBQ1568
C   WORD2 --- DUMMY ARRAY USED TO WRITE ONTO NJCBD                      ABBQ1569
C***********************************************************************ABBQ1570
      SUBROUTINE JACOB1                                                 ABBQ1571
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ1572
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ1573
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ1574
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          ABBQ1575
      PARAMETER (ND5=5*ND,NF5=5*NF,NX1=6*NDF+ND5+1,NX2=NX1+ND5,         ABBQ1576
     1           NX3=NX2+NF5,NX4=NX3+NF5,NX5=NX4+NF5)                   ABBQ1577
      PARAMETER (ND2=2*ND,ND7=7*ND,NDF7=7*NDF)                          ABBQ1578
      COMMON DUMMY(NDF7),AJ1(ND),CHI1(ND),ETA1(ND),FKZETA(ND),W(ND),    ABBQ1579
     1       DEDT(ND),V(ND),P1(ND),P2(ND)                               ABBQ1580
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHICG(NF,ND),           ABBQ1581
     1          QABS(NF,ND),BTDAV(NF,ND),BTDAVI(NF,ND),WORD1(ND7),      ABBQ1582
     2          WORD2(ND2)                                              ABBQ1583
      EQUIVALENCE (DUMMY(1),AJ(1,1)),(DUMMY(NDF1P1),FK(1,1)),           ABBQ1584
     1            (DUMMY(NDF2P1),ZETA(1,1)),(DUMMY(NDF3P1),CHICG(1,1)), ABBQ1585
     2            (DUMMY(NDF4P1),QABS(1,1)),(DUMMY(NDF5P1),BTDAV(1,1)), ABBQ1586
     3            (DUMMY(NDF6P1),BTDAVI(1,1)),(AJ1(1),WORD1(1)),        ABBQ1587
     4            (P1(1),WORD2(1))                                      ABBQ1588
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ1589
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ1590
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ1591
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         ABBQ1592
C***********************************************************************ABBQ1593
C   READ AJ, FK, ZETA FROM DISK FILE                                    ABBQ1594
C***********************************************************************ABBQ1595
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 ABBQ1596
C***********************************************************************ABBQ1597
C   READ CHICG FROM DISK FILE                                           ABBQ1598
C***********************************************************************ABBQ1599
      CALL BRANRD (NCGD,DUMMY(NDF3P1),NB1D)                             ABBQ1600
C***********************************************************************ABBQ1601
C   READ QABS FROM DISK FILE                                            ABBQ1602
C***********************************************************************ABBQ1603
      CALL BRANRD (NQASD,DUMMY(NDF4P1),NB1D)                            ABBQ1604
C***********************************************************************ABBQ1605
C   READ BTDAV FROM DISK FILE                                           ABBQ1606
C***********************************************************************ABBQ1607
      CALL BRANRD (NBTAD,DUMMY(NDF5P1),NB2D)                            ABBQ1608
C***********************************************************************ABBQ1609
C   CALCULATE DE/DT, DE/DJ, E                                           ABBQ1610
C***********************************************************************ABBQ1611
      DO 20 J=1,NFD                                                     ABBQ1612
      DO 10 L=1,NR                                                      ABBQ1613
      AJ1(L)=AJ(J,L)                                                    ABBQ1614
      W(L)=ZETA(J,L)*CHICG(J,L)                                         ABBQ1615
      FKZETA(L)=FK(J,L)*ZETA(J,L)                                       ABBQ1616
      CHI1(L)=QABS(J,L)                                                 ABBQ1617
      ETA1(L)=CHI1(L)*BTDAV(J,L)                                        ABBQ1618
      DEDT(L)=CHI1(L)*BTDAVI(J,L)                                       ABBQ1619
      V(L)=QABS(J,L)*WFD(J)                                             ABBQ1620
   10 CONTINUE                                                          ABBQ1621
C***********************************************************************ABBQ1622
C   WRITE AJ1 ONTO DISK FILE                                            ABBQ1623
C***********************************************************************ABBQ1624
      WRITE (NJCBD) WORD1                                               ABBQ1625
   20 CONTINUE                                                          ABBQ1626
C***********************************************************************ABBQ1627
C   CALCULATE E, DE/DT                                                  ABBQ1628
C***********************************************************************ABBQ1629
      DO 40 L=1,NR                                                      ABBQ1630
      P1SUM=0.0                                                         ABBQ1631
      P2SUM=0.0                                                         ABBQ1632
      DO 30 J=1,NFD                                                     ABBQ1633
      QW=QABS(J,L)*WFD(J)                                               ABBQ1634
      P1SUM=P1SUM+QW*(AJ(J,L)-BTDAV(J,L))                               ABBQ1635
      P2SUM=P2SUM+QW*BTDAVI(J,L)                                        ABBQ1636
   30 CONTINUE                                                          ABBQ1637
      P1(L)=-P1SUM                                                      ABBQ1638
      P2(L)=-P2SUM                                                      ABBQ1639
   40 CONTINUE                                                          ABBQ1640
C***********************************************************************ABBQ1641
C   WRITE P1, P2 TO DISK FILE                                           ABBQ1642
C***********************************************************************ABBQ1643
      WRITE (NJCBD) WORD2                                               ABBQ1644
      REWIND NJCBD                                                      ABBQ1645
      RETURN                                                            ABBQ1646
      END                                                               ABBQ1647
                                                                        ABBQ1648
C***********************************************************************ABBQ1649
C   SUBROUTINE JACOB2 FORMS THE F, U, AND W ELEMENTS                    ABBQ1650
C   OF THE GRAND ARRAY USED TO SOLVE FOR DTD AND DJD                    ABBQ1651
C   A    --- ELEMENTS OF A                                              ABBQ1652
C   B    --- ELEMENTS OF B                                              ABBQ1653
C   C    --- ELEMENTS OF C                                              ABBQ1654
C   D1   --- ELEMENTS OF F                                              ABBQ1655
C   D2   --- ELEMENTS OF U                                              ABBQ1656
C   V    --- DE/DJ                                                      ABBQ1657
C   VV   --- DE/DJ                                                      ABBQ1658
C   WORD --- DUMMY ARRAY USED TO READ FILE NJCBD                        ABBQ1659
C***********************************************************************ABBQ1660
      SUBROUTINE JACOB2 (J,A,B,C,D1,D2,V)                               ABBQ1661
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ1662
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ1663
      PARAMETER (ND7=7*ND)                                              ABBQ1664
      DIMENSION A(1),B(1),C(1),D1(1),D2(1),V(1)                         ABBQ1665
      COMMON AJ1(ND),CHI1(ND),ETA1(ND),FKZETA(ND),W(ND),DEDT(ND),VV(ND) ABBQ1666
      DIMENSION WORD(ND7)                                               ABBQ1667
      EQUIVALENCE (AJ1(1),WORD(1))                                      ABBQ1668
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ1669
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ1670
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             ABBQ1671
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ1672
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        ABBQ1673
      IF (J.EQ.1) CALL BCD (B1,B2,B1N,B2N)                              ABBQ1674
C***********************************************************************ABBQ1675
C   READ AJ1 FROM DISK FILE                                             ABBQ1676
C***********************************************************************ABBQ1677
      READ (NJCBD) WORD                                                 ABBQ1678
      DO 10 L=1,NR                                                      ABBQ1679
      V(L)=VV(L)                                                        ABBQ1680
   10 CONTINUE                                                          ABBQ1681
C***********************************************************************ABBQ1682
C   CALCULATE A, B, C, F, U ARRAYS FOR INTERIOR POINTS                  ABBQ1683
C***********************************************************************ABBQ1684
      DO 20 L=2,N1                                                      ABBQ1685
      X13=W(L-1)*W(L+1)                                                 ABBQ1686
      T12=WR1(L)*X13+WR2(L)*W(L)*W(L+1)                                 ABBQ1687
      T34=WR3(L)*X13+WR4(L)*W(L)*W(L-1)                                 ABBQ1688
      T5=WR5(L)*X13*W(L)                                                ABBQ1689
      A(L)=T12*FKZETA(L-1)                                              ABBQ1690
      B(L)=-(T12+T34)*FKZETA(L)-T5*CHI1(L)                              ABBQ1691
      C(L)=T34*FKZETA(L+1)                                              ABBQ1692
      D1(L)=-(A(L)*AJ1(L-1)+B(L)*AJ1(L)+C(L)*AJ1(L+1)+T5*ETA1(L))       ABBQ1693
      D2(L)=T5*DEDT(L)                                                  ABBQ1694
   20 CONTINUE                                                          ABBQ1695
C***********************************************************************ABBQ1696
C   CALCULATE A, B, C, F, U ARRAYS AT BOUNDARIES                        ABBQ1697
C***********************************************************************ABBQ1698
      B11=B1(J)                                                         ABBQ1699
      B12=B2(J)                                                         ABBQ1700
      C11=W(1)*WR1(1)*(1.5*W(2)-WR3(1)*W(1))                            ABBQ1701
      C12=WR2(1)*W(1)*W(2)                                              ABBQ1702
      B(1)=W(2)*FKZETA(1)+B11*C11+C12*CHI1(1)                           ABBQ1703
      C(1)=-W(2)*FKZETA(2)                                              ABBQ1704
      D1(1)=-(B(1)*AJ1(1)+C(1)*AJ1(2)+C11*B12-C12*ETA1(1))              ABBQ1705
      D2(1)=-C12*DEDT(1)                                                ABBQ1706
      BN1=B1N(J)                                                        ABBQ1707
      BN2=B2N(J)                                                        ABBQ1708
      CN1=W(NR)*WR1(NR)*(1.5*W(N1)-WR3(NR)*W(NR))                       ABBQ1709
      CN2=WR2(NR)*W(N1)*W(NR)                                           ABBQ1710
      A(NR)=W(N1)*FKZETA(N1)                                            ABBQ1711
      B(NR)=-W(N1)*FKZETA(NR)+CN1*BN1-CN2*CHI1(NR)                      ABBQ1712
      D1(NR)=-(A(NR)*AJ1(N1)+B(NR)*AJ1(NR)+CN1*BN2+CN2*ETA1(NR))        ABBQ1713
      D2(NR)=CN2*DEDT(NR)                                               ABBQ1714
      RETURN                                                            ABBQ1715
      END                                                               ABBQ1716
                                                                        ABBQ1717
C***********************************************************************ABBQ1718
C   SUBROUTINE JACOB3 READS P1 AND P2 FROM DISK FILE                    ABBQ1719
C   WORD --- DUMMY ARRAY FOR READING FILE NJCBD                         ABBQ1720
C   P1   --- ARRAY OF E(J)                                              ABBQ1721
C   P2   --- ARRAY OF DE(J)/DT                                          ABBQ1722
C***********************************************************************ABBQ1723
      SUBROUTINE JACOB3 (P1,P2)                                         ABBQ1724
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ1725
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ1726
      PARAMETER (ND2=2*ND)                                              ABBQ1727
      DIMENSION P1(1),P2(1)                                             ABBQ1728
      COMMON WORD(ND2)                                                  ABBQ1729
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ1730
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ1731
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ1732
      READ (NJCBD) WORD                                                 ABBQ1733
      DO 10 L=1,NR                                                      ABBQ1734
      P1(L)=WORD(L)                                                     ABBQ1735
      P2(L)=WORD(NR+L)                                                  ABBQ1736
   10 CONTINUE                                                          ABBQ1737
      REWIND NJCBD                                                      ABBQ1738
      RETURN                                                            ABBQ1739
      END                                                               ABBQ1740
                                                                        ABBQ1741
C***********************************************************************ABBQ1742
C   SUBROUTINE LINEQ1 SOLVES A SYSTEM OF LINEARIZED EQUATIONS           ABBQ1743
C   BY OF GAUSSIAN ELIMINATION WITH PIVOTAL CONDENSATION                ABBQ1744
C   A     --- N BY N ARRAY OF COEFFICIENTS                              ABBQ1745
C   B     --- N DIMENSIONAL SOLUTION VECTOR                             ABBQ1746
C   Z     --- DIVISION FACTORS, Z(I) = 1/A(I,I)                         ABBQ1747
C   N     --- SIZE OF A ARRAY                                           ABBQ1748
C   ND    --- MAXIMUM ALLOWED N                                         ABBQ1749
C   ISING --- CONTROL PARAMETER, 2 - A IS SINGULAR, 1 - A NON-SINGULAR  ABBQ1750
C***********************************************************************ABBQ1751
      SUBROUTINE LINEQ1 (A,B,Z,N,ND)                                    ABBQ1752
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ1753
      DIMENSION A(ND,ND),B(ND),Z(ND)                                    ABBQ1754
      ISING=1                                                           ABBQ1755
      NM1=N-1                                                           ABBQ1756
      IF (NM1.EQ.0) GO TO 99                                            ABBQ1757
C***********************************************************************ABBQ1758
C   PERFORM PIVOTAL CONDENSATION                                        ABBQ1759
C***********************************************************************ABBQ1760
      DO 50 J=1,NM1                                                     ABBQ1761
      J1=J+1                                                            ABBQ1762
      LMAX=J                                                            ABBQ1763
      RMAX=DABS(A(J,J))                                                 ABBQ1764
      DO 10 K=J1,N                                                      ABBQ1765
      RNEXT=DABS(A(K,J))                                                ABBQ1766
      RMAX=RNEXT                                                        ABBQ1767
      IF (RMAX.GE.RNEXT) GO TO 10                                       ABBQ1768
      LMAX=K                                                            ABBQ1769
   10 CONTINUE                                                          ABBQ1770
      IF (LMAX.NE.J) GO TO 11                                           ABBQ1771
      IF (A(J,J)) 22,77,22                                              ABBQ1772
   11 CONTINUE                                                          ABBQ1773
C***********************************************************************ABBQ1774
C   PERFORM GAUSSIAN ELIMINATION ON DIAGONALLY DOMINANT MATRIX          ABBQ1775
C***********************************************************************ABBQ1776
      DO 20 L=J,N                                                       ABBQ1777
      W=A(J,L)                                                          ABBQ1778
      A(J,L)=A(LMAX,L)                                                  ABBQ1779
      A(LMAX,L)=W                                                       ABBQ1780
   20 CONTINUE                                                          ABBQ1781
      W=B(J)                                                            ABBQ1782
      B(J)=B(LMAX)                                                      ABBQ1783
      B(LMAX)=W                                                         ABBQ1784
C***********************************************************************ABBQ1785
C   TEST FOR SINGULARITY OF COEFFICIENT MATRIX                          ABBQ1786
C***********************************************************************ABBQ1787
      IF (A(J,J)) 22,77,22                                              ABBQ1788
   22 CONTINUE                                                          ABBQ1789
      Z(J)=1.0/A(J,J)                                                   ABBQ1790
      DO 40 K=J1,N                                                      ABBQ1791
      IF (A(K,J)) 25,40,25                                              ABBQ1792
   25 CONTINUE                                                          ABBQ1793
      W=-Z(J)*A(K,J)                                                    ABBQ1794
      DO 30 L=J1,N                                                      ABBQ1795
      A(K,L)=W*A(J,L)+A(K,L)                                            ABBQ1796
   30 CONTINUE                                                          ABBQ1797
      B(K)=W*B(J)+B(K)                                                  ABBQ1798
   40 CONTINUE                                                          ABBQ1799
   50 CONTINUE                                                          ABBQ1800
      IF (A(N,N)) 55,77,55                                              ABBQ1801
   55 CONTINUE                                                          ABBQ1802
      Z(N)=1.0/A(N,N)                                                   ABBQ1803
      B(N)=Z(N)*B(N)                                                    ABBQ1804
      DO 70 K=1,NM1                                                     ABBQ1805
      J=N-K                                                             ABBQ1806
      J1=J+1                                                            ABBQ1807
      W=0.0                                                             ABBQ1808
      DO 60 I=J1,N                                                      ABBQ1809
      W=A(J,I)*B(I)+W                                                   ABBQ1810
   60 CONTINUE                                                          ABBQ1811
      B(J)=(B(J)-W)*Z(J)                                                ABBQ1812
   70 CONTINUE                                                          ABBQ1813
      RETURN                                                            ABBQ1814
   77 CONTINUE                                                          ABBQ1815
      ISING=2                                                           ABBQ1816
      WRITE (6,111) ISING                                               ABBQ1817
  111 FORMAT (1H1,20X,'COEFFICIENT MATRIX IS SINGULAR --- ISING = ',I1) ABBQ1818
      STOP                                                              ABBQ1819
   99 CONTINUE                                                          ABBQ1820
      IF (A(1,1).EQ.0.0) GO TO 77                                       ABBQ1821
      B(1)=B(1)/A(1,1)                                                  ABBQ1822
      RETURN                                                            ABBQ1823
      END                                                               ABBQ1824
                                                                        ABBQ1825
C***********************************************************************ABBQ1826
C   SUBROUTINE UPDATE CALCULATES THE TEMPERATURE DISTRIBUTION FOR       ABBQ1827
C   EACH GRAIN TYPE, UPDATES THE EDDINGTON FLUX, AND THE EMISSIVITY     ABBQ1828
C   AT EACH RADIAL AND FREQUENCY GRID POINT                             ABBQ1829
C***********************************************************************ABBQ1830
      SUBROUTINE UPDATE                                                 ABBQ1831
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ1832
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ1833
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ1834
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          ABBQ1835
      PARAMETER (ND5=5*ND,NF5=5*NF,NY1=NDF4P1+ND5,NY2=NY1+ND5,          ABBQ1836
     1           NY3=NY2+NF5,NY4=NY3+NF5)                               ABBQ1837
      PARAMETER (NDF7=7*NDF)                                            ABBQ1838
      COMMON DUMMY(NDF7)                                                ABBQ1839
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),             ABBQ1840
     1          ETA(NF,ND),CHICG(NF,ND),AH(NF,ND),QABS(NF,ND),          ABBQ1841
     2          QSCA(NF,ND),BTDAV(NF,ND),BTDAVI(NF,ND),H(NF,ND),        ABBQ1842
     3          ABUNDI(5,ND),TDI(5,ND),QABSI(NF,5),QSCAI(NF,5),         ABBQ1843
     4          GBARI(NF,5)                                             ABBQ1844
      EQUIVALENCE (DUMMY(1),AJ(1,1)),(DUMMY(NDF1P1),FK(1,1),QABS(1,1)), ABBQ1845
     1            (DUMMY(NDF2P1),ZETA(1,1),QSCA(1,1)),                  ABBQ1846
     2            (DUMMY(NDF3P1),CHI(1,1),CHICG(1,1)),                  ABBQ1847
     3            (DUMMY(NDF4P1),ETA(1,1),H(1,1)),                      ABBQ1848
     4            (DUMMY(NDF5P1),BTDAV(1,1)),                           ABBQ1849
     5            (DUMMY(NDF6P1),AH(1,1),BTDAVI(1,1)),                  ABBQ1850
     6            (DUMMY(NDF4P1),ABUNDI(1,1)),(DUMMY(NY1),TDI(1,1)),    ABBQ1851
     7            (DUMMY(NY2),QABSI(1,1)),(DUMMY(NY3),QSCAI(1,1)),      ABBQ1852
     8            (DUMMY(NY4),GBARI(1,1))                               ABBQ1853
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              ABBQ1854
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   ABBQ1855
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ1856
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ1857
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             ABBQ1858
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            ABBQ1859
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ1860
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         ABBQ1861
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ1862
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        ABBQ1863
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ1864
      ITMAXT=100                                                        ABBQ1865
      IXX=-ITER-1                                                       ABBQ1866
      IF (ITER.GE.9) IXX=-10                                            ABBQ1867
      EPST=10.0**IXX                                                    ABBQ1868
      IF (IMIX.GT.1) GO TO 11                                           ABBQ1869
      DO 10 J=1,NFD                                                     ABBQ1870
      DO 10 L=1,NR                                                      ABBQ1871
      CALL GETBJ (FREQD(J),TD(L),2,BJ0,BJ1)                             ABBQ1872
      BTDAV(J,L)=BJ0                                                    ABBQ1873
      BTDAVI(J,L)=BJ1                                                   ABBQ1874
   10 CONTINUE                                                          ABBQ1875
      GO TO 66                                                          ABBQ1876
   11 CONTINUE                                                          ABBQ1877
C***********************************************************************ABBQ1878
C   READ AJ FROM DISK FILE                                              ABBQ1879
C***********************************************************************ABBQ1880
      CALL BRANRD (NJFPD,DUMMY(1),NB1D)                                 ABBQ1881
C***********************************************************************ABBQ1882
C   READ ABUNDI, TDI, QABSI, QSCAI, GBARI FROM DISK FILE                ABBQ1883
C***********************************************************************ABBQ1884
      CALL BRANRD (NGPD,DUMMY(NDF4P1),NGP)                              ABBQ1885
C***********************************************************************ABBQ1886
C   CALCULATE TEMPERATURE FOR EACH GRAIN TYPE                           ABBQ1887
C***********************************************************************ABBQ1888
      DO 40 IG=1,IMIX                                                   ABBQ1889
      DO 40 L=1,NR                                                      ABBQ1890
      TT=TDI(IG,L)                                                      ABBQ1891
      DO 30 IT=1,ITMAXT                                                 ABBQ1892
      SNUM=0.0                                                          ABBQ1893
      SDEN=0.0                                                          ABBQ1894
      DO 20 J=1,NFD                                                     ABBQ1895
      XX=QABSI(J,IG)*WFD(J)                                             ABBQ1896
      CALL GETBJ (FREQD(J),TT,2,BJ0,BJ1)                                ABBQ1897
C***********************************************************************ABBQ1898
C   SET ENERGY BALANCE CONDITION                                        ABBQ1899
C***********************************************************************ABBQ1900
      SNUM=SNUM+XX*(BJ0-AJ(J,L))                                        ABBQ1901
      SDEN=SDEN-XX*BJ1                                                  ABBQ1902
   20 CONTINUE                                                          ABBQ1903
C***********************************************************************ABBQ1904
C   TEST FOR TEMPERATURE CONVERGENCE IN MULTIPLE GRAIN CASE             ABBQ1905
C***********************************************************************ABBQ1906
      DTDI=SNUM/SDEN                                                    ABBQ1907
      ERROR=DABS(DTDI/(TT+0.5*DTDI))                                    ABBQ1908
      IF (DTDI.GE.TT) DTDI=0.9*TT                                       ABBQ1909
      IF (DTDI.LT.(-TT)) DTDI=-0.9*TT                                   ABBQ1910
      TT=TT+DTDI                                                        ABBQ1911
      IF (ERROR.LT.EPST) GO TO 33                                       ABBQ1912
   30 CONTINUE                                                          ABBQ1913
      WRITE (6,111) IG,L,TT,DTDI                                        ABBQ1914
  111 FORMAT (/5X,'NO CONVERGENCE IN GETTDI --- IG = ',I2,2X,'IR = ',   ABBQ1915
     1 I3,'TDI = ',1PE10.3,2X,'DTDI = ',1PE10.3)                        ABBQ1916
      STOP                                                              ABBQ1917
   33 CONTINUE                                                          ABBQ1918
C***********************************************************************ABBQ1919
C   UPDATE TEMPERATURE PROFILE                                          ABBQ1920
C***********************************************************************ABBQ1921
      TDI(IG,L)=TT                                                      ABBQ1922
   40 CONTINUE                                                          ABBQ1923
C***********************************************************************ABBQ1924
C   WRITE ABUNDI, TDI, QABSI, QSCAI, GBARI ONTO DISK FILE               ABBQ1925
C***********************************************************************ABBQ1926
      CALL BRANWT (NGPD,DUMMY(NDF4P1),NGP)                              ABBQ1927
C***********************************************************************ABBQ1928
C   READ QABS FROM DISK FILE                                            ABBQ1929
C***********************************************************************ABBQ1930
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB1D)                            ABBQ1931
      DO 60 J=1,NFD                                                     ABBQ1932
      DO 60 L=1,NR                                                      ABBQ1933
      SUM=0.0                                                           ABBQ1934
C***********************************************************************ABBQ1935
C   CALCULATE THERMAL EMISSION TERM                                     ABBQ1936
C***********************************************************************ABBQ1937
      DO 50 IG=1,IMIX                                                   ABBQ1938
      CALL GETBJ (FREQD(J),TDI(IG,L),0,BJ0,BJ1)                         ABBQ1939
      SUM=SUM+ABUNDI(IG,L)*QABSI(J,IG)*BJ0                              ABBQ1940
   50 CONTINUE                                                          ABBQ1941
      BTDAV(J,L)=R0*RHOD(L)*SUM/QABS(J,L)                               ABBQ1942
C***********************************************************************ABBQ1943
C   CALCULATE B, (DB/DT)/B                                              ABBQ1944
C***********************************************************************ABBQ1945
      CALL GETBJ (FREQD(J),TD(L),3,BJ0,BJ10)                            ABBQ1946
      BTDAVI(J,L)=BTDAV(J,L)*BJ10                                       ABBQ1947
   60 CONTINUE                                                          ABBQ1948
   66 CONTINUE                                                          ABBQ1949
C***********************************************************************ABBQ1950
C   WRITE BTDAV, BTDAVI TO DISK FILE                                    ABBQ1951
C***********************************************************************ABBQ1952
      CALL BRANWT (NBTAD,DUMMY(NDF5P1),NB2D)                            ABBQ1953
      IF (ISCA.EQ.0) GO TO 115                                          ABBQ1954
C***********************************************************************ABBQ1955
C   READ AJ, FK, ZETA FROM DISK FILE                                    ABBQ1956
C***********************************************************************ABBQ1957
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 ABBQ1958
C***********************************************************************ABBQ1959
C   READ CHICG FROM DISK FILE                                           ABBQ1960
C***********************************************************************ABBQ1961
      CALL BRANRD (NCGD,DUMMY(NDF3P1),NB1D)                             ABBQ1962
C***********************************************************************ABBQ1963
C   READ AH FROM DISK FILE                                              ABBQ1964
C***********************************************************************ABBQ1965
      CALL BRANRD (NAHD,DUMMY(NDF6P1),NB1D)                             ABBQ1966
C***********************************************************************ABBQ1967
C   UPDATE BOUNDARY CONSTANTS AND FLUX                                  ABBQ1968
C***********************************************************************ABBQ1969
      CALL BCD (B1,B2,B1N,B2N)                                          ABBQ1970
      DO 70 J=1,NFD                                                     ABBQ1971
      H(J,1)=-(B1(J)*AJ(J,1)+B2(J))                                     ABBQ1972
      H(J,NR)=-(B1N(J)*AJ(J,NR)+B2N(J))                                 ABBQ1973
   70 CONTINUE                                                          ABBQ1974
C***********************************************************************ABBQ1975
C   UPDATE FLUX FOR SPHERICAL AND CYLINDRICAL CASES                     ABBQ1976
C***********************************************************************ABBQ1977
      DO 80 L=2,N1                                                      ABBQ1978
      WR12=WR1(L)*WR1(L)                                                ABBQ1979
      WR32=WR3(L)*WR3(L)                                                ABBQ1980
      WR5R2I=1.0/(WR5(L)*(R(L)**IGEOM))                                 ABBQ1981
      WT1=WR12*WR5R2I                                                   ABBQ1982
      WT2=(WR32-WR12)*WR5R2I                                            ABBQ1983
      WT3=-WR32*WR5R2I                                                  ABBQ1984
      DO 80 J=1,NFD                                                     ABBQ1985
      FPJ1=FK(J,L-1)*ZETA(J,L-1)*AJ(J,L-1)                              ABBQ1986
      FPJ2=FK(J,L)*ZETA(J,L)*AJ(J,L)                                    ABBQ1987
      FPJ3=FK(J,L+1)*ZETA(J,L+1)*AJ(J,L+1)                              ABBQ1988
      H(J,L)=(WT1*FPJ1+WT2*FPJ2+WT3*FPJ3)/(ZETA(J,L)*CHICG(J,L))        ABBQ1989
   80 CONTINUE                                                          ABBQ1990
      IF (ITER.EQ.1) GO TO 101                                          ABBQ1991
      L1=1                                                              ABBQ1992
C***********************************************************************ABBQ1993
C  FOR IC=0 MONOCHROMATIC FLUX AT CENTER MUST VANISH BY SYMMETRY        ABBQ1994
C***********************************************************************ABBQ1995
      IF (IC.EQ.0) DHD(1)=0.0                                           ABBQ1996
      IF (IC.EQ.0) L1=2                                                 ABBQ1997
      DO 90 L=L1,NR                                                     ABBQ1998
      DO 90 J=1,NFD                                                     ABBQ1999
      XX=(AH(J,L)-H(J,L))/(0.5*(AH(J,L)+H(J,L)))                        ABBQ2000
      DHD(L)=DMAX1(DABS(XX),DABS(DHD(L)))                               ABBQ2001
   90 CONTINUE                                                          ABBQ2002
  101 CONTINUE                                                          ABBQ2003
C***********************************************************************ABBQ2004
C   UPDATE AND WRITE AH ONTO DISK FILE                                  ABBQ2005
C***********************************************************************ABBQ2006
      DO 110 L=1,NR                                                     ABBQ2007
      DO 110 J=1,NFD                                                    ABBQ2008
      AH(J,L)=H(J,L)                                                    ABBQ2009
  110 CONTINUE                                                          ABBQ2010
      CALL BRANWT (NAHD,DUMMY(NDF6P1),NB1D)                             ABBQ2011
  115 CONTINUE                                                          ABBQ2012
C***********************************************************************ABBQ2013
C   READ AJ FROM DISK FILE IF ISCA=0                                    ABBQ2014
C***********************************************************************ABBQ2015
      IF (ISCA.EQ.0) CALL BRANRD (NJFPD,DUMMY(1),NB1D)                  ABBQ2016
C***********************************************************************ABBQ2017
C   READ QABS FROM DISK FILE                                            ABBQ2018
C***********************************************************************ABBQ2019
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB2D)                            ABBQ2020
C***********************************************************************ABBQ2021
C   READ CHI FROM DISK FILE                                             ABBQ2022
C***********************************************************************ABBQ2023
      CALL BRANRD (NCED,DUMMY(NDF3P1),NB1D)                             ABBQ2024
C***********************************************************************ABBQ2025
C   UPDATE EMISSIVITY                                                   ABBQ2026
C***********************************************************************ABBQ2027
      DO 120 J=1,NFD                                                    ABBQ2028
      DO 120 L=1,NR                                                     ABBQ2029
      ETA(J,L)=QSCA(J,L)*AJ(J,L)+QABS(J,L)*BTDAV(J,L)                   ABBQ2030
  120 CONTINUE                                                          ABBQ2031
C***********************************************************************ABBQ2032
C   WRITE CHI, ETA ONTO DISK FILE                                       ABBQ2033
C***********************************************************************ABBQ2034
      CALL BRANWT (NCED,DUMMY(NDF3P1),NB2D)                             ABBQ2035
C***********************************************************************ABBQ2036
C   SOLVE RAY EQUATIONS TO DETERMINE "INTENSITY" U, CONFIGURATION       ABBQ2037
C   FUNCTION, AND ANISOTROPY FACTOR                                     ABBQ2038
C***********************************************************************ABBQ2039
      IF (IGEOM.EQ.0) CALL EDFTPL                                       ABBQ2040
      IF (IGEOM.EQ.1) CALL EDFTCY                                       ABBQ2041
      IF (IGEOM.EQ.2) CALL EDFTSP                                       ABBQ2042
      RETURN                                                            ABBQ2043
      END                                                               ABBQ2044
                                                                        ABBQ2045
C***********************************************************************ABBQ2046
C   SUBROUTINE EDFTPL SOLVES THE RAY EQUATIONS TO DETERMINE THE         ABBQ2047
C   INTENSITY-LIKE FUNCTION U, THE EMERGENT INTENSITY, THE ANISOTROPY   ABBQ2048
C   FACTOR, THE BOUNDARY FACTORS, AND THE CONFIGURATION FUNCTION FOR    ABBQ2049
C   THE PLANAR CASE                                                     ABBQ2050
C***********************************************************************ABBQ2051
      SUBROUTINE EDFTPL                                                 ABBQ2052
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ2053
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ2054
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         ABBQ2055
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ2056
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          ABBQ2057
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1,          ABBQ2058
     3           NDF8M=NDF8P1-1)                                        ABBQ2059
      PARAMETER (NFC1P1=NFC2+1,NFC2P1=2*NFC2+1,NFC3P1=3*NFC2+1,         ABBQ2060
     1           NFC4P1=4*NFC2+1,NFC5P1=5*NFC2+1,                       ABBQ2061
     2           NFC6P1=6*NFC2+1,NFC7P1=7*NFC2+1,                       ABBQ2062
     3           NFC8P1=8*NFC2+1,NCEIC=NFC8P1+NDF)                      ABBQ2063
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   ABBQ2064
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     ABBQ2065
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     ABBQ2066
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    ABBQ2067
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     ABBQ2068
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1)                     ABBQ2069
      PARAMETER (NXY1=9*NDF+NFC2,NTHETA=20)                             ABBQ2070
      COMMON QW(NQWT0),DUMMY(NXY1),AVB(NF),AVC(NF),DP(ND),Y(ND)         ABBQ2071
      DIMENSION WZ1(NQWT),WZ2(NQWT),WZ3(NQWT),WJ(NQWT),                 ABBQ2072
     1          WK(NQWT),WB(NPMAX),WC(NC1),XMUSQ(NQWT),P2R3(NQWT),      ABBQ2073
     2          WZETA1(ND),WZETA2(ND)                                   ABBQ2074
      DIMENSION FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),ETA(NF,ND),AH(NF,ND),  ABBQ2075
     1          CGBAR(NF,ND),AH0(NF,ND),AH1(NF,ND),AVJ(NF,ND),          ABBQ2076
     2          AVK(NF,ND),G(NF,ND),H(NF,ND),U(NF,ND),EI(NF,ND),        ABBQ2077
     3          EIC(NF,NC2)                                             ABBQ2078
      EQUIVALENCE (QW(1),WZ1(1)),(QW(NQWT1),WZ2(1)),(QW(NQWT2),WZ3(1)), ABBQ2079
     1            (QW(NQWT3),WJ(1)),(QW(NQWT4),WK(1)),(QW(NQWT5),WB(1)),ABBQ2080
     2            (QW(NQWT6),WC(1)),(QW(NQWT7),XMUSQ(1)),               ABBQ2081
     3            (QW(NQWT8),P2R3(1)),(QW(NQWT9),WZETA1(1)),            ABBQ2082
     4            (QW(NQWT10),WZETA2(1))                                ABBQ2083
      EQUIVALENCE (DUMMY(1),CHI(1,1)),(DUMMY(NDF1P1),FK(1,1),ETA(1,1)), ABBQ2084
     1            (DUMMY(NDF2P1),ZETA(1,1),AH(1,1),AH0(1,1)),           ABBQ2085
     2            (DUMMY(NDF3P1),CGBAR(1,1),AH1(1,1)),                  ABBQ2086
     3            (DUMMY(NDF4P1),AVJ(1,1)),(DUMMY(NDF5P1),AVK(1,1)),    ABBQ2087
     4            (DUMMY(NDF6P1),G(1,1),U(1,1)),(DUMMY(NDF7P1),H(1,1)), ABBQ2088
     5            (DUMMY(NDF8P1),EI(1,1)),(DUMMY(NDF9P1),EIC(1,1))      ABBQ2089
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                ABBQ2090
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   ABBQ2091
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ2092
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ2093
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             ABBQ2094
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ2095
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ2096
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        ABBQ2097
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ2098
      IF ((ITCONT*ITCONJ).NE.0) ITCON=1                                 ABBQ2099
      IF (IEDFTR.EQ.0.AND.ITCON.EQ.0) GO TO 258                         ABBQ2100
C***********************************************************************ABBQ2101
C   READ CHI, ETA FROM DISK FILE                                        ABBQ2102
C***********************************************************************ABBQ2103
      CALL BRANRD (NCED,DUMMY(1),NB2D)                                  ABBQ2104
C***********************************************************************ABBQ2105
C   READ AH FROM DISK FILE                                              ABBQ2106
C***********************************************************************ABBQ2107
      CALL BRANRD (NAHD,DUMMY(NDF2P1),NB1D)                             ABBQ2108
C***********************************************************************ABBQ2109
C   READ CGBAR FROM DISK FILE                                           ABBQ2110
C***********************************************************************ABBQ2111
      CALL BRANRD (NGBD,DUMMY(NDF3P1),NB1D)                             ABBQ2112
C***********************************************************************ABBQ2113
C   READ QUADRATURE WEIGHTS FROM DISK FILE                              ABBQ2114
C***********************************************************************ABBQ2115
      CALL BRANRD (NEFQW,QW(1),NQW)                                     ABBQ2116
C***********************************************************************ABBQ2117
C   CALCULATE BOUNDARY CONSTANTS FOR RAY EQUATIONS                      ABBQ2118
C***********************************************************************ABBQ2119
      CALL EFBC (B1,B2,B1N,B2N)                                         ABBQ2120
C***********************************************************************ABBQ2121
C   INITIALIZE INTEGRALS AND ANISOTROPIC SCATTERING FUNCTION            ABBQ2122
C***********************************************************************ABBQ2123
      DO 10 L=1,NR                                                      ABBQ2124
      DO 10,J=1,NFD                                                     ABBQ2125
      AVJ(J,L)=0.0                                                      ABBQ2126
      AVK(J,L)=0.0                                                      ABBQ2127
      AH0(J,L)=3.0*CGBAR(J,L)*AH(J,L)/CHI(J,L)                          ABBQ2128
   10 CONTINUE                                                          ABBQ2129
      IF (ISCA.NE.0) GO TO 22                                           ABBQ2130
C***********************************************************************ABBQ2131
C   INITIALIZE ANISOTROPIC SCATTERING FUNCTION FOR ISCA=0               ABBQ2132
C***********************************************************************ABBQ2133
      DO 20 L=1,NR                                                      ABBQ2134
      DO 20 J=1,NFD                                                     ABBQ2135
      AH0(J,L)=0.0                                                      ABBQ2136
      AH1(J,L)=0.0                                                      ABBQ2137
   20 CONTINUE                                                          ABBQ2138
      GO TO 42                                                          ABBQ2139
   22 CONTINUE                                                          ABBQ2140
C***********************************************************************ABBQ2141
C   CALCULATE FLUX AT INNER AND OUTER BOUNDARIES                        ABBQ2142
C***********************************************************************ABBQ2143
      F=R(3)-R(2)                                                       ABBQ2144
      B=R(2)-R(1)                                                       ABBQ2145
      BF=B+F                                                            ABBQ2146
      BFI=1.0/(B*F*BF)                                                  ABBQ2147
      W1=-F*(BF+B)*BFI                                                  ABBQ2148
      W2=BF*BF*BFI                                                      ABBQ2149
      W3=-B*B*BFI                                                       ABBQ2150
      F=R(NR)-R(N1)                                                     ABBQ2151
      B=R(N1)-R(NR-2)                                                   ABBQ2152
      BF=B+F                                                            ABBQ2153
      BFI=1.0/(B*F*BF)                                                  ABBQ2154
      W1N=F*F*BFI                                                       ABBQ2155
      W2N=-BF*BF*BFI                                                    ABBQ2156
      W3N=B*(BF+F)*BFI                                                  ABBQ2157
      DO 30 J=1,NFD                                                     ABBQ2158
      AH1(J,1)=W1*AH0(J,1)+W2*AH0(J,2)+W3*AH0(J,3)                      ABBQ2159
      AH1(J,NR)=W1N*AH0(J,NR-2)+W2N*AH0(J,N1)+W3N*AH0(J,NR)             ABBQ2160
   30 CONTINUE                                                          ABBQ2161
C***********************************************************************ABBQ2162
C   CALCULATE FLUX AT INTERIOR POINTS                                   ABBQ2163
C***********************************************************************ABBQ2164
      DO 40 L=2,N1                                                      ABBQ2165
      WR12=WR1(L)*WR1(L)                                                ABBQ2166
      WR32=WR3(L)*WR3(L)                                                ABBQ2167
      WR5I=1.0/WR5(L)                                                   ABBQ2168
      WT1=-WR12*WR5I                                                    ABBQ2169
      WT2=(WR12-WR32)*WR5I                                              ABBQ2170
      WT3=WR32*WR5I                                                     ABBQ2171
      DO 40 J=1,NFD                                                     ABBQ2172
      AH1(J,L)=WT1*AH0(J,L-1)+WT2*AH0(J,L)+WT3*AH0(J,L+1)               ABBQ2173
   40 CONTINUE                                                          ABBQ2174
   42 CONTINUE                                                          ABBQ2175
C***********************************************************************ABBQ2176
C   INITIALIZE H INTEGRALS                                              ABBQ2177
C***********************************************************************ABBQ2178
      DO 50 J=1,NFD                                                     ABBQ2179
      AVB(J)=0.0                                                        ABBQ2180
      AVC(J)=0.0                                                        ABBQ2181
   50 CONTINUE                                                          ABBQ2182
      IZ=0                                                              ABBQ2183
C***********************************************************************ABBQ2184
C   SOLVE RAY EQUATIONS TO DETERMINE THE "INTENSITY" U                  ABBQ2185
C***********************************************************************ABBQ2186
      DO 170 IP=1,NTHETA                                                ABBQ2187
      IZ=IZ+1                                                           ABBQ2188
      XMU=DSQRT(XMUSQ(IP))                                              ABBQ2189
      DO 70 J=1,NFD                                                     ABBQ2190
C***********************************************************************ABBQ2191
C   CON1 AND CON2 COMPUTE SCATTERING COEFFICIENTS                       ABBQ2192
C***********************************************************************ABBQ2193
      CON1=AH0(J,1)*XMU                                                 ABBQ2194
      CON2=XMUSQ(IP)*AH1(J,1)                                           ABBQ2195
      C11=CHI(J,1)*(3.0*CHI(J,2)-CHI(J,1))*WZ1(IZ)                      ABBQ2196
      C12=CHI(J,1)*CHI(J,2)*WZ2(IZ)                                     ABBQ2197
      B1I=1.0/(CHI(J,2)+CHI(J,1)*C12+B1(J)*C11)                         ABBQ2198
      G(J,1)=((ETA(J,1)-CON2)*C12-(B2(J)+CON1)*C11)*B1I                 ABBQ2199
      H(J,1)=CHI(J,2)*B1I                                               ABBQ2200
   70 CONTINUE                                                          ABBQ2201
      DO 80 L=2,N1                                                      ABBQ2202
      IZ=IZ+1                                                           ABBQ2203
      DO 80 J=1,NFD                                                     ABBQ2204
      CON2=XMUSQ(IP)*AH1(J,L)                                           ABBQ2205
      X13=CHI(J,L-1)*CHI(J,L+1)                                         ABBQ2206
      T=WZ2(IZ)*X13*CHI(J,L)                                            ABBQ2207
      AL=WZ1(IZ)*(X13+CHI(J,L)*CHI(J,L+1))                              ABBQ2208
      CL=WZ3(IZ)*(X13+CHI(J,L)*CHI(J,L-1))                              ABBQ2209
      BL=-(AL+CL+T*CHI(J,L))                                            ABBQ2210
      DL=-T*(ETA(J,L)-CON2)                                             ABBQ2211
      EL=1.0/(BL+AL*H(J,L-1))                                           ABBQ2212
      G(J,L)=(DL-AL*G(J,L-1))*EL                                        ABBQ2213
      H(J,L)=-CL*EL                                                     ABBQ2214
   80 CONTINUE                                                          ABBQ2215
      IZ=IZ+1                                                           ABBQ2216
      DO 90 J=1,NFD                                                     ABBQ2217
      CON1=AH0(J,NR)*XMU                                                ABBQ2218
      CON2=XMUSQ(IP)*AH1(J,NR)                                          ABBQ2219
      CN1=CHI(J,NR)*(3.0*CHI(J,N1)-CHI(J,NR))*WZ1(IZ)                   ABBQ2220
      CN2=CHI(J,N1)*CHI(J,NR)*WZ2(IZ)                                   ABBQ2221
      AN=CHI(J,N1)                                                      ABBQ2222
      BN=-CHI(J,N1)+B1N(J)*CN1-CHI(J,NR)*CN2                            ABBQ2223
      DN=-((B2N(J)+CON1)*CN1+(ETA(J,NR)-CON2)*CN2)                      ABBQ2224
      U(J,NR)=(DN-AN*G(J,N1))/(BN+AN*H(J,N1))                           ABBQ2225
   90 CONTINUE                                                          ABBQ2226
      LL=NR+1                                                           ABBQ2227
      DO 110 L=2,NR                                                     ABBQ2228
      DO 110 J=1,NFD                                                    ABBQ2229
      U(J,LL-L)=G(J,LL-L)+H(J,LL-L)*U(J,LL-L+1)                         ABBQ2230
  110 CONTINUE                                                          ABBQ2231
C***********************************************************************ABBQ2232
C   COMPUTE H INTEGRAL AT OUTER BOUNDARY                                ABBQ2233
C***********************************************************************ABBQ2234
      DO 130 J=1,NFD                                                    ABBQ2235
      AVB(J)=AVB(J)+WB(IP)*U(J,NR)                                      ABBQ2236
  130 CONTINUE                                                          ABBQ2237
      IF (IC.EQ.0) GO TO 145                                            ABBQ2238
C***********************************************************************ABBQ2239
C   COMPUTE H INTEGRAL AT INNER BOUNDARY                                ABBQ2240
C***********************************************************************ABBQ2241
      DO 140 J=1,NFD                                                    ABBQ2242
      AVC(J)=AVC(J)+WC(IP)*U(J,1)                                       ABBQ2243
  140 CONTINUE                                                          ABBQ2244
  145 CONTINUE                                                          ABBQ2245
C***********************************************************************ABBQ2246
C   COMPUTE J AND K INTEGRALS                                           ABBQ2247
C***********************************************************************ABBQ2248
      DO 150 L=1,NR                                                     ABBQ2249
      DO 150 J=1,NFD                                                    ABBQ2250
      AVJ(J,L)=AVJ(J,L)+WJ(IP)*U(J,L)                                   ABBQ2251
      AVK(J,L)=AVK(J,L)+WK(IP)*U(J,L)                                   ABBQ2252
  150 CONTINUE                                                          ABBQ2253
      IF (ITCON.NE.1) GO TO 170                                         ABBQ2254
C***********************************************************************ABBQ2255
C   COMPUTE EMERGENT INTENSITIES IF CONVERGENCE HAS OCCURRED            ABBQ2256
C***********************************************************************ABBQ2257
      DO 160 J=1,NFD                                                    ABBQ2258
      EI(J,IP)=U(J,NR)-(B1N(J)*U(J,NR)+B2N(J))                          ABBQ2259
  160 CONTINUE                                                          ABBQ2260
  170 CONTINUE                                                          ABBQ2261
      IF (IEDFTR.EQ.0) GO TO 215                                        ABBQ2262
C***********************************************************************ABBQ2263
C   COMPUTE BOUNDARY FACTORS                                            ABBQ2264
C***********************************************************************ABBQ2265
      DO 190 J=1,NFD                                                    ABBQ2266
      FCD(J)=AVC(J)/AVJ(J,1)                                            ABBQ2267
      FBD(J)=AVB(J)/AVJ(J,NR)                                           ABBQ2268
  190 CONTINUE                                                          ABBQ2269
C***********************************************************************ABBQ2270
C   COMPUTE ANISOTROPY FACTORS AND CONFIGURATION FUNCTION               ABBQ2271
C***********************************************************************ABBQ2272
      DO 210 L=1,NR                                                     ABBQ2273
      DO 210 J=1,NFD                                                    ABBQ2274
      FK(J,L)=AVK(J,L)/AVJ(J,L)                                         ABBQ2275
      ZETA(J,L)=1.0                                                     ABBQ2276
  210 CONTINUE                                                          ABBQ2277
  215 CONTINUE                                                          ABBQ2278
      IF (ITCON.EQ.0) GO TO 216                                         ABBQ2279
C***********************************************************************ABBQ2280
C   WRITE EI ONTO DISK FILE                                             ABBQ2281
C***********************************************************************ABBQ2282
      CALL BRANWT (NEID,DUMMY(NDF8P1),NB1D)                             ABBQ2283
C***********************************************************************ABBQ2284
C   WRITE EIC ONTO DISK FILE                                            ABBQ2285
C***********************************************************************ABBQ2286
  216 CONTINUE                                                          ABBQ2287
      IF (IEDFTR.EQ.0) RETURN                                           ABBQ2288
C***********************************************************************ABBQ2289
C   WRITE AJ, FK, ZETA ONTO DISK FILE                                   ABBQ2290
C***********************************************************************ABBQ2291
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 ABBQ2292
  258 CONTINUE                                                          ABBQ2293
C***********************************************************************ABBQ2294
C   UPDATE AJ, AH, ETA                                                  ABBQ2295
C***********************************************************************ABBQ2296
      CALL GETAJ                                                        ABBQ2297
      RETURN                                                            ABBQ2298
      END                                                               ABBQ2299
                                                                        ABBQ2300
C***********************************************************************ABBQ2301
C   SUBROUTINE EDFTSP SOLVES THE RAY EQUATIONS TO DETERMINE THE         ABBQ2302
C   INTENSITY-LIKE FUNCTION U, THE EMERGENT INTENSITY, THE ANISOTROPY   ABBQ2303
C   FACTOR, THE BOUNDARY FACTORS, AND THE CONFIGURATION FUNCTION FOR    ABBQ2304
C   THE SPHERICAL CASE                                                  ABBQ2305
C***********************************************************************ABBQ2306
      SUBROUTINE EDFTSP                                                 ABBQ2307
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ2308
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ2309
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         ABBQ2310
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ2311
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          ABBQ2312
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1)          ABBQ2313
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   ABBQ2314
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     ABBQ2315
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     ABBQ2316
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    ABBQ2317
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     ABBQ2318
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1)                     ABBQ2319
      PARAMETER (NXY1=9*NDF+NFC2)                                       ABBQ2320
      COMMON QW(NQWT0),DUMMY(NXY1),AVB(NF),AVC(NF),DP(ND),Y(ND)         ABBQ2321
      DIMENSION WZ1(NQWT),WZ2(NQWT),WZ3(NQWT),WJ(NQWT),                 ABBQ2322
     1          WK(NQWT),WB(NPMAX),WC(NC1),XMUSQ(NQWT),P2R3(NQWT),      ABBQ2323
     2          WZETA1(ND),WZETA2(ND)                                   ABBQ2324
      DIMENSION FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),                       ABBQ2325
     1          ETA(NF,ND),AH(NF,ND),CGBAR(NF,ND),EIC(NF,NC2),          ABBQ2326
     2          AH0(NF,ND),AH1(NF,ND)                                   ABBQ2327
      DIMENSION AVJ(NF,ND),AVK(NF,ND),G(NF,ND),H(NF,ND),U(NF,ND),       ABBQ2328
     1          EI(NF,ND)                                               ABBQ2329
      EQUIVALENCE (QW(1),WZ1(1)),(QW(NQWT1),WZ2(1)),(QW(NQWT2),WZ3(1)), ABBQ2330
     1            (QW(NQWT3),WJ(1)),(QW(NQWT4),WK(1)),(QW(NQWT5),WB(1)),ABBQ2331
     2            (QW(NQWT6),WC(1)),(QW(NQWT7),XMUSQ(1)),               ABBQ2332
     3            (QW(NQWT8),P2R3(1)),(QW(NQWT9),WZETA1(1)),            ABBQ2333
     4            (QW(NQWT10),WZETA2(1))                                ABBQ2334
      EQUIVALENCE (DUMMY(1),CHI(1,1)),(DUMMY(NDF1P1),FK(1,1),ETA(1,1)), ABBQ2335
     1            (DUMMY(NDF2P1),ZETA(1,1),AH(1,1),AH0(1,1)),           ABBQ2336
     2            (DUMMY(NDF3P1),CGBAR(1,1),AH1(1,1)),                  ABBQ2337
     3            (DUMMY(NDF4P1),AVJ(1,1)),(DUMMY(NDF5P1),AVK(1,1)),    ABBQ2338
     4            (DUMMY(NDF6P1),G(1,1),U(1,1)),(DUMMY(NDF7P1),H(1,1)), ABBQ2339
     5            (DUMMY(NDF8P1),EI(1,1)),(DUMMY(NDF9P1),EIC(1,1))      ABBQ2340
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                ABBQ2341
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   ABBQ2342
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ2343
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ2344
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             ABBQ2345
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ2346
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ2347
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        ABBQ2348
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ2349
      IF ((ITCONT*ITCONJ).NE.0) ITCON=1                                 ABBQ2350
      IF (IEDFTR.EQ.0.AND.ITCON.EQ.0) GO TO 258                         ABBQ2351
C***********************************************************************ABBQ2352
C   READ CHI, ETA FROM DISK FILE                                        ABBQ2353
C***********************************************************************ABBQ2354
      CALL BRANRD (NCED,DUMMY(1),NB2D)                                  ABBQ2355
C***********************************************************************ABBQ2356
C   READ AH FROM DISK FILE                                              ABBQ2357
C***********************************************************************ABBQ2358
      CALL BRANRD (NAHD,DUMMY(NDF2P1),NB1D)                             ABBQ2359
C***********************************************************************ABBQ2360
C   READ CGBAR FROM DISK FILE                                           ABBQ2361
C***********************************************************************ABBQ2362
      CALL BRANRD (NGBD,DUMMY(NDF3P1),NB1D)                             ABBQ2363
C***********************************************************************ABBQ2364
C   READ QUADRATURE WEIGHTS FROM DISK FILE                              ABBQ2365
C***********************************************************************ABBQ2366
      CALL BRANRD (NEFQW,QW(1),NQW)                                     ABBQ2367
C***********************************************************************ABBQ2368
C   CALCULATE BOUNDARY CONSTANTS FOR RAY EQUATIONS                      ABBQ2369
C***********************************************************************ABBQ2370
      CALL EFBC (B1,B2,B1N,B2N)                                         ABBQ2371
C***********************************************************************ABBQ2372
C   INITIALIZE INTEGRALS AND ANISOTROPIC SCATTERING FUNCTION            ABBQ2373
C***********************************************************************ABBQ2374
      DO 10 L=1,NR                                                      ABBQ2375
      DO 10,J=1,NFD                                                     ABBQ2376
      AVJ(J,L)=0.0                                                      ABBQ2377
      AVK(J,L)=0.0                                                      ABBQ2378
      AH0(J,L)=3.0*CGBAR(J,L)*AH(J,L)/CHI(J,L)                          ABBQ2379
   10 CONTINUE                                                          ABBQ2380
      IF (ISCA.NE.0) GO TO 22                                           ABBQ2381
C***********************************************************************ABBQ2382
C   INITIALIZE ANISOTROPIC SCATTERING FUNCTION FOR ISCA=0               ABBQ2383
C***********************************************************************ABBQ2384
      DO 20 L=1,NR                                                      ABBQ2385
      DO 20 J=1,NFD                                                     ABBQ2386
      AH0(J,L)=0.0                                                      ABBQ2387
      AH1(J,L)=0.0                                                      ABBQ2388
   20 CONTINUE                                                          ABBQ2389
      GO TO 42                                                          ABBQ2390
   22 CONTINUE                                                          ABBQ2391
C***********************************************************************ABBQ2392
C   CALCULATE FLUX PROFILE                                              ABBQ2393
C***********************************************************************ABBQ2394
      F=R(3)-R(2)                                                       ABBQ2395
      B=R(2)-R(1)                                                       ABBQ2396
      BF=B+F                                                            ABBQ2397
      BFI=1.0/(B*F*BF)                                                  ABBQ2398
      W1=-F*(BF+B)*BFI                                                  ABBQ2399
      W2=BF*BF*BFI                                                      ABBQ2400
      W3=-B*B*BFI                                                       ABBQ2401
      F=R(NR)-R(N1)                                                     ABBQ2402
      B=R(N1)-R(NR-2)                                                   ABBQ2403
      BF=B+F                                                            ABBQ2404
      BFI=1.0/(B*F*BF)                                                  ABBQ2405
      W1N=F*F*BFI                                                       ABBQ2406
      W2N=-BF*BF*BFI                                                    ABBQ2407
      W3N=B*(BF+F)*BFI                                                  ABBQ2408
      DO 30 J=1,NFD                                                     ABBQ2409
      AH1(J,1)=W1*AH0(J,1)+W2*AH0(J,2)+W3*AH0(J,3)                      ABBQ2410
      AH1(J,NR)=W1N*AH0(J,NR-2)+W2N*AH0(J,N1)+W3N*AH0(J,NR)             ABBQ2411
   30 CONTINUE                                                          ABBQ2412
C***********************************************************************ABBQ2413
C   CALCULATE FLUX AT INTERIOR POINTS FOR SPHERICAL GEOMETRY            ABBQ2414
C***********************************************************************ABBQ2415
      DO 40 L=2,N1                                                      ABBQ2416
      WR12=WR1(L)*WR1(L)                                                ABBQ2417
      WR32=WR3(L)*WR3(L)                                                ABBQ2418
      WR5R2I=1.0/(WR5(L)*R(L)*R(L))                                     ABBQ2419
      WT1=-WR12*WR5R2I                                                  ABBQ2420
      WT2=(WR12-WR32)*WR5R2I                                            ABBQ2421
      WT3=WR32*WR5R2I                                                   ABBQ2422
      DO 40 J=1,NFD                                                     ABBQ2423
      AH1(J,L)=WT1*AH0(J,L-1)+WT2*AH0(J,L)+WT3*AH0(J,L+1)               ABBQ2424
   40 CONTINUE                                                          ABBQ2425
   42 CONTINUE                                                          ABBQ2426
C***********************************************************************ABBQ2427
C   INITIALIZE H INTEGRALS                                              ABBQ2428
C***********************************************************************ABBQ2429
      DO 50 J=1,NFD                                                     ABBQ2430
      AVB(J)=0.0                                                        ABBQ2431
      AVC(J)=0.0                                                        ABBQ2432
   50 CONTINUE                                                          ABBQ2433
      MB=-NR                                                            ABBQ2434
      IZ=0                                                              ABBQ2435
C***********************************************************************ABBQ2436
C   SOLVE RAY EQUATIONS TO DETERMINE THE "INTENSITY" U                  ABBQ2437
C***********************************************************************ABBQ2438
      DO 170 IP=1,NP                                                    ABBQ2439
      IF (IP-IP1) 54,54,55                                              ABBQ2440
   54 CONTINUE                                                          ABBQ2441
      IR0=1                                                             ABBQ2442
      JR=0                                                              ABBQ2443
      MW=NR                                                             ABBQ2444
      GO TO 66                                                          ABBQ2445
   55 CONTINUE                                                          ABBQ2446
      IR0=IR0+1                                                         ABBQ2447
      JR=IP-IP1                                                         ABBQ2448
      MW=MW-1                                                           ABBQ2449
      DO 60 J=1,NFD                                                     ABBQ2450
      B1(J)=0.0                                                         ABBQ2451
      B2(J)=0.0                                                         ABBQ2452
   60 CONTINUE                                                          ABBQ2453
   66 CONTINUE                                                          ABBQ2454
      IR1=IR0+1                                                         ABBQ2455
      MB=MB+MW                                                          ABBQ2456
      IF (IP.EQ.NP) GO TO 115                                           ABBQ2457
      IZ=IZ+1                                                           ABBQ2458
      XMU=DSQRT(XMUSQ(IZ))                                              ABBQ2459
      DO 70 J=1,NFD                                                     ABBQ2460
C***********************************************************************ABBQ2461
C   CON1 AND CON2 COMPUTE SCATTERING COEFFICIENTS                       ABBQ2462
C***********************************************************************ABBQ2463
      CON1=AH0(J,IR0)*XMU                                               ABBQ2464
      CON2=XMUSQ(IZ)*AH1(J,IR0)+AH0(J,IR0)*P2R3(IZ)                     ABBQ2465
      C11=CHI(J,IR0)*(3.0*CHI(J,IR1)-CHI(J,IR0))*WZ1(IZ)                ABBQ2466
      C12=CHI(J,IR0)*CHI(J,IR1)*WZ2(IZ)                                 ABBQ2467
      B1I=1.0/(CHI(J,IR1)+CHI(J,IR0)*C12+B1(J)*C11)                     ABBQ2468
      G(J,IR0)=((ETA(J,IR0)-CON2)*C12-(B2(J)+CON1)*C11)*B1I             ABBQ2469
      H(J,IR0)=CHI(J,IR1)*B1I                                           ABBQ2470
   70 CONTINUE                                                          ABBQ2471
      IF (IR0.EQ.N1) GO TO 88                                           ABBQ2472
      DO 80 L=IR1,N1                                                    ABBQ2473
      IZ=IZ+1                                                           ABBQ2474
      DO 80 J=1,NFD                                                     ABBQ2475
      CON2=XMUSQ(IZ)*AH1(J,L)+AH0(J,L)*P2R3(IZ)                         ABBQ2476
      X13=CHI(J,L-1)*CHI(J,L+1)                                         ABBQ2477
      T=WZ2(IZ)*X13*CHI(J,L)                                            ABBQ2478
      AL=WZ1(IZ)*(X13+CHI(J,L)*CHI(J,L+1))                              ABBQ2479
      CL=WZ3(IZ)*(X13+CHI(J,L)*CHI(J,L-1))                              ABBQ2480
      BL=-(AL+CL+T*CHI(J,L))                                            ABBQ2481
      DL=-T*(ETA(J,L)-CON2)                                             ABBQ2482
      EL=1.0/(BL+AL*H(J,L-1))                                           ABBQ2483
      G(J,L)=(DL-AL*G(J,L-1))*EL                                        ABBQ2484
      H(J,L)=-CL*EL                                                     ABBQ2485
   80 CONTINUE                                                          ABBQ2486
   88 CONTINUE                                                          ABBQ2487
      IZ=IZ+1                                                           ABBQ2488
      XMU=DSQRT(XMUSQ(IZ))                                              ABBQ2489
      DO 90 J=1,NFD                                                     ABBQ2490
      CON1=AH0(J,NR)*XMU                                                ABBQ2491
      CON2=XMUSQ(IZ)*AH1(J,NR)+AH0(J,NR)*P2R3(IZ)                       ABBQ2492
      CN1=CHI(J,NR)*(3.0*CHI(J,N1)-CHI(J,NR))*WZ1(IZ)                   ABBQ2493
      CN2=CHI(J,N1)*CHI(J,NR)*WZ2(IZ)                                   ABBQ2494
      AN=CHI(J,N1)                                                      ABBQ2495
      BN=-CHI(J,N1)+B1N(J)*CN1-CHI(J,NR)*CN2                            ABBQ2496
      DN=-((B2N(J)+CON1)*CN1+(ETA(J,NR)-CON2)*CN2)                      ABBQ2497
      U(J,NR)=(DN-AN*G(J,N1))/(BN+AN*H(J,N1))                           ABBQ2498
   90 CONTINUE                                                          ABBQ2499
      LL=NR+IR0                                                         ABBQ2500
      DO 110 L=IR1,NR                                                   ABBQ2501
      DO 110 J=1,NFD                                                    ABBQ2502
      U(J,LL-L)=G(J,LL-L)+H(J,LL-L)*U(J,LL-L+1)                         ABBQ2503
  110 CONTINUE                                                          ABBQ2504
      GO TO 125                                                         ABBQ2505
  115 CONTINUE                                                          ABBQ2506
      DO 120 J=1,NFD                                                    ABBQ2507
      U(J,NR)=B2N(J)                                                    ABBQ2508
  120 CONTINUE                                                          ABBQ2509
  125 CONTINUE                                                          ABBQ2510
C***********************************************************************ABBQ2511
C   COMPUTE H INTEGRAL AT OUTER BOUNDARY                                ABBQ2512
C***********************************************************************ABBQ2513
      DO 130 J=1,NFD                                                    ABBQ2514
      AVB(J)=AVB(J)+WB(IP)*U(J,NR)                                      ABBQ2515
  130 CONTINUE                                                          ABBQ2516
      IF (IC.EQ.0.OR.IP.GT.IP1) GO TO 145                               ABBQ2517
C***********************************************************************ABBQ2518
C   COMPUTE H INTEGRAL AT INNER BOUNDARY                                ABBQ2519
C***********************************************************************ABBQ2520
      DO 140 J=1,NFD                                                    ABBQ2521
      AVC(J)=AVC(J)+WC(IP)*U(J,IR0)                                     ABBQ2522
  140 CONTINUE                                                          ABBQ2523
  145 CONTINUE                                                          ABBQ2524
      JR=JR+1                                                           ABBQ2525
      WJT=WJ(JR+MB)                                                     ABBQ2526
      WKT=WK(JR+MB)                                                     ABBQ2527
C***********************************************************************ABBQ2528
C   COMPUTE J AND K INTEGRALS                                           ABBQ2529
C***********************************************************************ABBQ2530
      DO 150 J=1,NFD                                                    ABBQ2531
      AVJ(J,JR)=AVJ(J,JR)+WJT*U(J,JR)                                   ABBQ2532
      AVK(J,JR)=AVK(J,JR)+WKT*U(J,JR)                                   ABBQ2533
  150 CONTINUE                                                          ABBQ2534
      IF (JR.LT.NR) GO TO 145                                           ABBQ2535
      IF (ITCON.NE.1) GO TO 170                                         ABBQ2536
C***********************************************************************ABBQ2537
C   COMPUTE EMERGENT INTENSITIES IF CONVERGENCE HAS OCCURRED            ABBQ2538
C***********************************************************************ABBQ2539
      DO 160 J=1,NFD                                                    ABBQ2540
      UVNR=U(J,NR)-(B1N(J)*U(J,NR)+B2N(J))                              ABBQ2541
      IF (IC.EQ.0) EI(J,IP)=UVNR                                        ABBQ2542
      IF (IC.EQ.0) GO TO 160                                            ABBQ2543
      IF (IP.EQ.1) EI(J,1)=UVNR                                         ABBQ2544
      IF (IP.LE.(IP1+1)) EIC(J,IP)=UVNR                                 ABBQ2545
      IF (IP.GE.IP1.AND.IP.LT.NP) EI(J,IP-IP0+1)=UVNR                   ABBQ2546
  160 CONTINUE                                                          ABBQ2547
  170 CONTINUE                                                          ABBQ2548
      IF (ITCON.NE.1.OR.IC.NE.0) GO TO 185                              ABBQ2549
      DO 180 J=1,NFD                                                    ABBQ2550
      EI(J,NR)=B2N(J)                                                   ABBQ2551
  180 CONTINUE                                                          ABBQ2552
  185 CONTINUE                                                          ABBQ2553
      IF (IEDFTR.EQ.0) GO TO 215                                        ABBQ2554
C***********************************************************************ABBQ2555
C   COMPUTE BOUNDARY FACTORS                                            ABBQ2556
C***********************************************************************ABBQ2557
      DO 190 J=1,NFD                                                    ABBQ2558
      FCD(J)=AVC(J)/AVJ(J,1)                                            ABBQ2559
      FBD(J)=AVB(J)/AVJ(J,NR)                                           ABBQ2560
  190 CONTINUE                                                          ABBQ2561
C***********************************************************************ABBQ2562
C   COMPUTE ANISOTROPY FACTORS                                          ABBQ2563
C***********************************************************************ABBQ2564
      DO 210 L=1,NR                                                     ABBQ2565
      DO 210 J=1,NFD                                                    ABBQ2566
      FK(J,L)=AVK(J,L)/AVJ(J,L)                                         ABBQ2567
  210 CONTINUE                                                          ABBQ2568
  215 CONTINUE                                                          ABBQ2569
      IF (ITCON.EQ.0) GO TO 216                                         ABBQ2570
C***********************************************************************ABBQ2571
C   WRITE EI ONTO DISK FILE                                             ABBQ2572
C***********************************************************************ABBQ2573
      CALL BRANWT (NEID,DUMMY(NDF8P1),NB1D)                             ABBQ2574
C***********************************************************************ABBQ2575
C   WRITE EIC ONTO DISK FILE                                            ABBQ2576
C***********************************************************************ABBQ2577
      IF (IC.NE.0) CALL BRANWT (NEICD,DUMMY(NDF9P1),NFC2)               ABBQ2578
  216 CONTINUE                                                          ABBQ2579
      IF (IEDFTR.EQ.0) RETURN                                           ABBQ2580
C***********************************************************************ABBQ2581
C   COMPUTE CONFIGURATION FUNCTION                                      ABBQ2582
C***********************************************************************ABBQ2583
      DO 250 J=1,NFD                                                    ABBQ2584
      DO 220 L=1,NR                                                     ABBQ2585
      Y(L)=3.0-1.0/FK(J,L)                                              ABBQ2586
  220 CONTINUE                                                          ABBQ2587
      DO 230 L=1,N1                                                     ABBQ2588
      DP(L)=WZETA1(L)*Y(L)+WZETA2(L)*Y(L+1)                             ABBQ2589
  230 CONTINUE                                                          ABBQ2590
      Y(1)=0.0                                                          ABBQ2591
      DO 240 L=2,NR                                                     ABBQ2592
      Y(L)=Y(L-1)+DP(L-1)                                               ABBQ2593
  240 CONTINUE                                                          ABBQ2594
      DO 250 L=1,NR                                                     ABBQ2595
      ZETA(J,L)=DEXP(Y(L))                                              ABBQ2596
  250 CONTINUE                                                          ABBQ2597
C***********************************************************************ABBQ2598
C   WRITE AJ, FK, ZETA ONTO DISK FILE                                   ABBQ2599
C***********************************************************************ABBQ2600
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 ABBQ2601
  258 CONTINUE                                                          ABBQ2602
C***********************************************************************ABBQ2603
C   UPDATE AJ, AH, ETA                                                  ABBQ2604
C***********************************************************************ABBQ2605
      CALL GETAJ                                                        ABBQ2606
      RETURN                                                            ABBQ2607
      END                                                               ABBQ2608
                                                                        ABBQ2609
C***********************************************************************ABBQ2610
C   SUBROUTINE EDFTCY SOLVES THE RAY EQUATIONS TO DETERMINE THE         ABBQ2611
C   INTENSITY-LIKE FUNCTION U, THE EMERGENT INTENSITY, THE ANISOTROPY   ABBQ2612
C   FACTOR, THE BOUNDARY FACTORS, AND THE CONFIGURATION FUNCTION FOR    ABBQ2613
C   THE CYLINDRICAL CASE                                                ABBQ2614
C***********************************************************************ABBQ2615
      SUBROUTINE EDFTCY                                                 ABBQ2616
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ2617
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ2618
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         ABBQ2619
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ2620
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          ABBQ2621
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1,          ABBQ2622
     3           NDF8M=NDF8P1-1)                                        ABBQ2623
      PARAMETER (NFC1P1=NFC2+1,NFC2P1=2*NFC2+1,NFC3P1=3*NFC2+1,         ABBQ2624
     1           NFC4P1=4*NFC2+1,NFC5P1=5*NFC2+1,                       ABBQ2625
     2           NFC6P1=6*NFC2+1,NFC7P1=7*NFC2+1,                       ABBQ2626
     3           NFC8P1=8*NFC2+1,NCEIC=NFC8P1+NDF)                      ABBQ2627
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   ABBQ2628
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     ABBQ2629
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     ABBQ2630
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    ABBQ2631
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     ABBQ2632
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1,                     ABBQ2633
     6           NQWT61=NQWT5+NQWT,NQWT99=12*NQWT,NQWT91=9*NQWT+1)      ABBQ2634
      PARAMETER (NXY1=9*NDF+NFC2,NTHETA=20)                             ABBQ2635
      PARAMETER (NQG=4,NQGT=NQWT2+NQG,NQGT1=NQGT+NQG,                   ABBQ2636
     1           NQGT2=NQGT1+NQG,NCQW=NQGT2+NQG-1)                      ABBQ2637
      COMMON QW(NQWT0),DUMMY(NXY1),AVB(NF),AVC(NF),DP(ND),Y(ND)         ABBQ2638
      DIMENSION WZ1(NQWT),WZ2(NQWT),WZ3(NQWT),WJ(NQWT),                 ABBQ2639
     1          WK(NQWT),WB(NPMAX),WC(NC1),XMUSQ(NQWT),P2R3(NQWT),      ABBQ2640
     2          WZETA1(ND),WZETA2(ND),WY(NQWT),WH(NQWT)                 ABBQ2641
      DIMENSION FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),ETA(NF,ND),AH(NF,ND),  ABBQ2642
     1          CGBAR(NF,ND),AH0(NF,ND),AH1(NF,ND),AVP(NF,ND),          ABBQ2643
     2          AVY(NF,ND),AY0(NF,ND),GY(NF,ND),HY(NF,ND),UY(NF,ND)     ABBQ2644
      DIMENSION AVJ(NF,ND),AVK(NF,ND),AVQ(NF,ND),G(NF,ND),H(NF,ND),     ABBQ2645
     1          U(NF,ND),QRAT(NF,ND),EI(NF,ND),EIC(NF,NC2),AP0(NF,ND)   ABBQ2646
      DIMENSION WJO(NQG),WHO(NQG),WKO(NQG),WYO(NQG)                     ABBQ2647
      EQUIVALENCE (QW(1),WZ1(1)),(QW(NQWT1),WZ2(1)),(QW(NQWT2),WZ3(1)), ABBQ2648
     1            (QW(NQWT3),WJ(1)),(QW(NQWT4),WK(1)),(QW(NQWT5),WB(1)),ABBQ2649
     2            (QW(NQWT6),WC(1)),(QW(NQWT7),XMUSQ(1)),               ABBQ2650
     3            (QW(NQWT8),P2R3(1)),(QW(NQWT9),WZETA1(1)),            ABBQ2651
     4            (QW(NQWT10),WZETA2(1))                                ABBQ2652
      EQUIVALENCE (CQW(1),WH(1)),(CQW(NQWT1),WY(1)),(CQW(NQWT2),WJO(1)),ABBQ2653
     1            (CQW(NQGT),WHO(1)),(CQW(NQGT1),WKO(1)),               ABBQ2654
     2            (CQW(NQGT2),WYO(1))                                   ABBQ2655
      EQUIVALENCE (DUMMY(1),CHI(1,1)),(DUMMY(NDF1P1),FK(1,1),ETA(1,1)), ABBQ2656
     1            (DUMMY(NDF2P1),ZETA(1,1),AH(1,1),AH0(1,1)),           ABBQ2657
     2            (DUMMY(NDF3P1),CGBAR(1,1),AH1(1,1)),                  ABBQ2658
     3            (DUMMY(NDF4P1),AVJ(1,1)),(DUMMY(NDF5P1),AVK(1,1)),    ABBQ2659
     4            (DUMMY(NDF6P1),G(1,1),U(1,1)),(DUMMY(NDF7P1),H(1,1)), ABBQ2660
     5            (DUMMY(NDF8P1),EI(1,1)),(DUMMY(NDF9P1),EIC(1,1))      ABBQ2661
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                ABBQ2662
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   ABBQ2663
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ2664
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ2665
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             ABBQ2666
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ2667
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ2668
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        ABBQ2669
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ2670
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG)                         ABBQ2671
      SAVE AVP,AVY                                                      ABBQ2672
      IF ((ITCONT*ITCONJ).NE.0) ITCON=1                                 ABBQ2673
      IF (IEDFTR.EQ.0.AND.ITCON.EQ.0) GO TO 266                         ABBQ2674
C***********************************************************************ABBQ2675
C   READ CHI, ETA FROM DISK FILE                                        ABBQ2676
C***********************************************************************ABBQ2677
      CALL BRANRD (NCED,DUMMY(1),NB2D)                                  ABBQ2678
C***********************************************************************ABBQ2679
C   READ AH FROM DISK FILE                                              ABBQ2680
C***********************************************************************ABBQ2681
      CALL BRANRD (NAHD,DUMMY(NDF2P1),NB1D)                             ABBQ2682
C***********************************************************************ABBQ2683
C   READ CGBAR FROM DISK FILE                                           ABBQ2684
C***********************************************************************ABBQ2685
      CALL BRANRD (NGBD,DUMMY(NDF3P1),NB1D)                             ABBQ2686
C***********************************************************************ABBQ2687
C   READ QUADRATURE WEIGHTS FROM DISK FILE                              ABBQ2688
C***********************************************************************ABBQ2689
      CALL BRANRD (NCYQW,CQW(1),NCQW)                                   ABBQ2690
C***********************************************************************ABBQ2691
C   CALCULATE BOUNDARY CONSTANTS FOR RAY EQUATIONS                      ABBQ2692
C***********************************************************************ABBQ2693
      CALL EFBC (B1,B2,B1N,B2N)                                         ABBQ2694
      PI=3.141592653589793E0                                            ABBQ2695
      TPII=2.0/PI                                                       ABBQ2696
      IF (ITER.NE.1) GOTO 15                                            ABBQ2697
      DO 10 L=1,NR                                                      ABBQ2698
      DO 10 J=1,NFD                                                     ABBQ2699
      AVY(J,L)=0.0                                                      ABBQ2700
      AVP(J,L)=0.0                                                      ABBQ2701
   10 CONTINUE                                                          ABBQ2702
   15 CONTINUE                                                          ABBQ2703
C***********************************************************************ABBQ2704
C   INITIALIZE INTEGRALS AND ANISOTROPIC SCATTERING FUNCTION            ABBQ2705
C***********************************************************************ABBQ2706
    1 DO 20 L=1,NR                                                      ABBQ2707
      DO 20,J=1,NFD                                                     ABBQ2708
      AVJ(J,L)=0.0                                                      ABBQ2709
      AVK(J,L)=0.0                                                      ABBQ2710
      AH0(J,L)=3.0*CGBAR(J,L)*AH(J,L)/CHI(J,L)                          ABBQ2711
      AY0(J,L)=3.0*CGBAR(J,L)*AVY(J,L)                                  ABBQ2712
      AP0(J,L)=3.0*CGBAR(J,L)*AVP(J,L)                                  ABBQ2713
      AVQ(J,L)=0.0                                                      ABBQ2714
      AVY(J,L)=0.0                                                      ABBQ2715
      AVP(J,L)=0.0                                                      ABBQ2716
      IF (ITER.EQ.1.OR.IEDFTR.EQ.0) QRAT(J,L)=2.0/3.0                   ABBQ2717
   20 CONTINUE                                                          ABBQ2718
      IF (ISCA.NE.0) GO TO 33                                           ABBQ2719
C***********************************************************************ABBQ2720
C   INITIALIZE ANISOTROPIC SCATTERING FUNCTION FOR ISCA=0               ABBQ2721
C***********************************************************************ABBQ2722
      DO 30 L=1,NR                                                      ABBQ2723
      DO 30 J=1,NFD                                                     ABBQ2724
      AH0(J,L)=0.0                                                      ABBQ2725
      AH1(J,L)=0.0                                                      ABBQ2726
      AY0(J,L)=0.0                                                      ABBQ2727
      AP0(J,L)=0.0                                                      ABBQ2728
   30 CONTINUE                                                          ABBQ2729
      GO TO 42                                                          ABBQ2730
   33 CONTINUE                                                          ABBQ2731
C***********************************************************************ABBQ2732
C   CALCULATE FLUX PROFILE                                              ABBQ2733
C***********************************************************************ABBQ2734
      F=R(3)-R(2)                                                       ABBQ2735
      B=R(2)-R(1)                                                       ABBQ2736
      BF=B+F                                                            ABBQ2737
      BFI=1.0/(B*F*BF)                                                  ABBQ2738
      W1=-F*(BF+B)*BFI                                                  ABBQ2739
      W2=BF*BF*BFI                                                      ABBQ2740
      W3=-B*B*BFI                                                       ABBQ2741
      F=R(NR)-R(N1)                                                     ABBQ2742
      B=R(N1)-R(NR-2)                                                   ABBQ2743
      BF=B+F                                                            ABBQ2744
      BFI=1.0/(B*F*BF)                                                  ABBQ2745
      W1N=F*F*BFI                                                       ABBQ2746
      W2N=-BF*BF*BFI                                                    ABBQ2747
      W3N=B*(BF+F)*BFI                                                  ABBQ2748
      DO 35 J=1,NFD                                                     ABBQ2749
      AH1(J,1)=W1*AH0(J,1)+W2*AH0(J,2)+W3*AH0(J,3)                      ABBQ2750
      AH1(J,NR)=W1N*AH0(J,NR-2)+W2N*AH0(J,N1)+W3N*AH0(J,NR)             ABBQ2751
   35 CONTINUE                                                          ABBQ2752
C***********************************************************************ABBQ2753
C   CALCULATE FLUX AT INTERIOR POINTS FOR SPHERICAL GEOMETRY            ABBQ2754
C***********************************************************************ABBQ2755
      DO 40 L=2,N1                                                      ABBQ2756
      WR12=WR1(L)*WR1(L)                                                ABBQ2757
      WR32=WR3(L)*WR3(L)                                                ABBQ2758
      WR5R2I=1.0/(WR5(L)*(R(L)**IGEOM))                                 ABBQ2759
      WT1=-WR12*WR5R2I                                                  ABBQ2760
      WT2=(WR12-WR32)*WR5R2I                                            ABBQ2761
      WT3=WR32*WR5R2I                                                   ABBQ2762
      DO 40 J=1,NFD                                                     ABBQ2763
      AH1(J,L)=WT1*AH0(J,L-1)+WT2*AH0(J,L)+WT3*AH0(J,L+1)               ABBQ2764
   40 CONTINUE                                                          ABBQ2765
   42 CONTINUE                                                          ABBQ2766
C***********************************************************************ABBQ2767
C   INITIALIZE H INTEGRALS                                              ABBQ2768
C***********************************************************************ABBQ2769
      DO 50 J=1,NFD                                                     ABBQ2770
      AVB(J)=0.0                                                        ABBQ2771
      AVC(J)=0.0                                                        ABBQ2772
   50 CONTINUE                                                          ABBQ2773
      IBC=1                                                             ABBQ2774
   52 CONTINUE                                                          ABBQ2775
      SBET=DSQRT(1.0-BETA(IBC)*BETA(IBC))                               ABBQ2776
      BTSQ=BETA(IBC)*BETA(IBC)                                          ABBQ2777
      READ (NEFQW) QW                                                   ABBQ2778
      MB=-NR                                                            ABBQ2779
      IZ=0                                                              ABBQ2780
C***********************************************************************ABBQ2781
C   SOLVE RAY EQUATIONS TO DETERMINE THE "INTENSITY" U                  ABBQ2782
C***********************************************************************ABBQ2783
      DO 170 IP=1,NP                                                    ABBQ2784
      IF (IP-IP1) 54,54,55                                              ABBQ2785
   54 CONTINUE                                                          ABBQ2786
      IR0=1                                                             ABBQ2787
      JR=0                                                              ABBQ2788
      MW=NR                                                             ABBQ2789
      GO TO 66                                                          ABBQ2790
   55 CONTINUE                                                          ABBQ2791
      IR0=IR0+1                                                         ABBQ2792
      JR=IP-IP1                                                         ABBQ2793
      MW=MW-1                                                           ABBQ2794
      DO 60 J=1,NFD                                                     ABBQ2795
      B1(J)=0.0                                                         ABBQ2796
      B2(J)=0.0                                                         ABBQ2797
   60 CONTINUE                                                          ABBQ2798
   66 CONTINUE                                                          ABBQ2799
      IR1=IR0+1                                                         ABBQ2800
      MB=MB+MW                                                          ABBQ2801
      IF (IP.EQ.NP) GO TO 115                                           ABBQ2802
      IZ=IZ+1                                                           ABBQ2803
      XMU=DSQRT(XMUSQ(IZ))                                              ABBQ2804
      SMU=DSQRT(1.0-XMU*XMU)                                            ABBQ2805
      DO 70 J=1,NFD                                                     ABBQ2806
C***********************************************************************ABBQ2807
C   CON0, CON1, AND CON2 COMPUTE SCATTERING COEFFICIENTS                ABBQ2808
C***********************************************************************ABBQ2809
      CON0=SBET*AP0(J,IR0)+BETA(IBC)*SMU*AY0(J,IR0)                     ABBQ2810
      CON1=AH0(J,IR0)*XMU*BETA(IBC)                                     ABBQ2811
      CON2=BTSQ*(XMUSQ(IZ)*AH1(J,IR0)+AH0(J,IR0)*P2R3(IZ))              ABBQ2812
      C11=CHI(J,IR0)*(3.0*CHI(J,IR1)-CHI(J,IR0))*WZ1(IZ)                ABBQ2813
      C12=CHI(J,IR0)*CHI(J,IR1)*WZ2(IZ)                                 ABBQ2814
      B1I=1.0/(CHI(J,IR1)+CHI(J,IR0)*C12+B1(J)*C11)                     ABBQ2815
      G(J,IR0)=((ETA(J,IR0)+CON0-CON2)*C12-(B2(J)+CON1)*C11)*B1I        ABBQ2816
      H(J,IR0)=CHI(J,IR1)*B1I                                           ABBQ2817
   70 CONTINUE                                                          ABBQ2818
      IF (IR0.EQ.N1) GO TO 88                                           ABBQ2819
      DO 80 L=IR1,N1                                                    ABBQ2820
      IZ=IZ+1                                                           ABBQ2821
      DO 80 J=1,NFD                                                     ABBQ2822
      CON2=BTSQ*(XMUSQ(IZ)*AH1(J,L)+AH0(J,L)*P2R3(IZ))                  ABBQ2823
      CON0=SBET*AP0(J,L)+BETA(IBC)*SMU*AY0(J,L)                         ABBQ2824
      X13=CHI(J,L-1)*CHI(J,L+1)                                         ABBQ2825
      T=WZ2(IZ)*X13*CHI(J,L)                                            ABBQ2826
      AL=WZ1(IZ)*(X13+CHI(J,L)*CHI(J,L+1))                              ABBQ2827
      CL=WZ3(IZ)*(X13+CHI(J,L)*CHI(J,L-1))                              ABBQ2828
      BL=-(AL+CL+T*CHI(J,L))                                            ABBQ2829
      DL=-T*(ETA(J,L)+CON0-CON2)                                        ABBQ2830
      EL=1.0/(BL+AL*H(J,L-1))                                           ABBQ2831
      G(J,L)=(DL-AL*G(J,L-1))*EL                                        ABBQ2832
      H(J,L)=-CL*EL                                                     ABBQ2833
   80 CONTINUE                                                          ABBQ2834
   88 CONTINUE                                                          ABBQ2835
      IZ=IZ+1                                                           ABBQ2836
      XMU=DSQRT(XMUSQ(IZ))                                              ABBQ2837
      DO 90 J=1,NFD                                                     ABBQ2838
      CON0=SBET*AP0(J,NR)+BETA(IBC)*SMU*AY0(J,NR)                       ABBQ2839
      CON1=AH0(J,NR)*XMU*BETA(IBC)                                      ABBQ2840
      CON2=BTSQ*(XMUSQ(IZ)*AH1(J,NR)+AH0(J,NR)*P2R3(IZ))                ABBQ2841
      CN1=CHI(J,NR)*(3.0*CHI(J,N1)-CHI(J,NR))*WZ1(IZ)                   ABBQ2842
      CN2=CHI(J,N1)*CHI(J,NR)*WZ2(IZ)                                   ABBQ2843
      AN=CHI(J,N1)                                                      ABBQ2844
      BN=-CHI(J,N1)+B1N(J)*CN1-CHI(J,NR)*CN2                            ABBQ2845
      DN=-((B2N(J)+CON1)*CN1+(ETA(J,NR)+CON0-CON2)*CN2)                 ABBQ2846
      U(J,NR)=(DN-AN*G(J,N1))/(BN+AN*H(J,N1))                           ABBQ2847
   90 CONTINUE                                                          ABBQ2848
      LL=NR+IR0                                                         ABBQ2849
      DO 110 L=IR1,NR                                                   ABBQ2850
      DO 110 J=1,NFD                                                    ABBQ2851
      U(J,LL-L)=G(J,LL-L)+H(J,LL-L)*U(J,LL-L+1)                         ABBQ2852
  110 CONTINUE                                                          ABBQ2853
      GO TO 125                                                         ABBQ2854
  115 CONTINUE                                                          ABBQ2855
      DO 120 J=1,NFD                                                    ABBQ2856
      U(J,NR)=B2N(J)                                                    ABBQ2857
  120 CONTINUE                                                          ABBQ2858
  125 CONTINUE                                                          ABBQ2859
C***********************************************************************ABBQ2860
C   COMPUTE H INTEGRAL AT OUTER BOUNDARY                                ABBQ2861
C***********************************************************************ABBQ2862
      DO 130 J=1,NFD                                                    ABBQ2863
      AVB(J)=AVB(J)+WB(IP)*U(J,NR)*WHO(IBC)*TPII                        ABBQ2864
  130 CONTINUE                                                          ABBQ2865
      IF (IC.EQ.0) GO TO 145                                            ABBQ2866
      IF (IP.GT.IP1) GO TO 145                                          ABBQ2867
C***********************************************************************ABBQ2868
C   COMPUTE H INTEGRAL AT INNER BOUNDARY                                ABBQ2869
C***********************************************************************ABBQ2870
      DO 140 J=1,NFD                                                    ABBQ2871
      AVC(J)=AVC(J)+WC(IP)*U(J,IR0)*WHO(IBC)*TPII                       ABBQ2872
  140 CONTINUE                                                          ABBQ2873
  145 CONTINUE                                                          ABBQ2874
      JR=JR+1                                                           ABBQ2875
      WJT=WJ(JR+MB)                                                     ABBQ2876
      WKT=WK(JR+MB)                                                     ABBQ2877
      WYT=WY(JR+MB)                                                     ABBQ2878
C***********************************************************************ABBQ2879
C   COMPUTE J AND K INTEGRALS                                           ABBQ2880
C***********************************************************************ABBQ2881
      DO 150 J=1,NFD                                                    ABBQ2882
      AVJ(J,JR)=AVJ(J,JR)+WJT*U(J,JR)*WJO(IBC)*TPII                     ABBQ2883
      AVK(J,JR)=AVK(J,JR)+WKT*U(J,JR)*WJO(IBC)*TPII-                    ABBQ2884
     1          TPII*WKT*U(J,JR)*WKO(IBC)                               ABBQ2885
      AVQ(J,JR)=AVQ(J,JR)+TPII*WJT*U(J,JR)*(WJO(IBC)-WKO(IBC))          ABBQ2886
      IF (ISCA.EQ.0) GOTO 150                                           ABBQ2887
      AVY(J,JR)=AVY(J,JR)+WYT*U(J,JR)*WHO(IBC)*TPII                     ABBQ2888
      AVP(J,JR)=AVP(J,JR)+WJT*U(J,JR)*WYO(IBC)*TPII                     ABBQ2889
  150 CONTINUE                                                          ABBQ2890
      IF (JR.LT.NR) GO TO 145                                           ABBQ2891
      IF (ITCON.NE.1) GO TO 170                                         ABBQ2892
C***********************************************************************ABBQ2893
C   COMPUTE EMERGENT INTENSITIES IF CONVERGENCE HAS OCCURRED            ABBQ2894
C***********************************************************************ABBQ2895
      DO 160 J=1,NFD                                                    ABBQ2896
      UVNR=U(J,NR)-(B1N(J)*U(J,NR)+B2N(J))                              ABBQ2897
      IF (IC.EQ.0) EI(J,IP)=UVNR                                        ABBQ2898
      IF (IC.EQ.0) GO TO 160                                            ABBQ2899
      IF (IP.EQ.1) EI(J,1)=UVNR                                         ABBQ2900
      IF (IP.LE.(IP1+1)) EIC(J,IP)=UVNR                                 ABBQ2901
      IF (IP.GE.IP1.AND.IP.LT.NP) EI(J,IP-IP0+1)=UVNR                   ABBQ2902
  160 CONTINUE                                                          ABBQ2903
  170 CONTINUE                                                          ABBQ2904
      DO 180 J=1,NFD                                                    ABBQ2905
      DO 180 L=1,NR                                                     ABBQ2906
      IF (IBC.EQ.NQG) QRAT(J,L)=AVQ(J,L)/AVJ(J,L)                       ABBQ2907
      IF (IBC.EQ.NQG.AND.ITER.GE.ITMAX.AND.ITCON.NE.1)                  ABBQ2908
     1    WRITE (NEICD) QRAT(J,L)                                       ABBQ2909
      IF (L.GT.1.OR.IC.NE.0.OR.ITCON.NE.1) GOTO 175                     ABBQ2910
      EI(J,NR)=B2N(J)                                                   ABBQ2911
C***********************************************************************ABBQ2912
C   WRITE EI ONTO DISK FILE                                             ABBQ2913
C***********************************************************************ABBQ2914
  175 CONTINUE                                                          ABBQ2915
      IF (ITCON.NE.1) GOTO 180                                          ABBQ2916
      WRITE (NEID) EI(J,L)                                              ABBQ2917
C***********************************************************************ABBQ2918
C   WRITE EIC ONTO DISK FILE                                            ABBQ2919
C***********************************************************************ABBQ2920
      IF (IC.NE.1.OR.L.GT.NC2) GOTO 180                                 ABBQ2921
      WRITE (NEICD) EIC(J,L)                                            ABBQ2922
  180 CONTINUE                                                          ABBQ2923
      IF (ITCON.NE.1) GOTO 195                                          ABBQ2924
      DO 190 J=1,NFD                                                    ABBQ2925
      DO 190 L=1,NR                                                     ABBQ2926
      IF (IBC.EQ.NQG) WRITE (NEICD) QRAT(J,L)                           ABBQ2927
  190 CONTINUE                                                          ABBQ2928
  195 CONTINUE                                                          ABBQ2929
      IBC=IBC+1                                                         ABBQ2930
      IF (IBC.LE.NQG) GOTO 52                                           ABBQ2931
      REWIND NEFQW                                                      ABBQ2932
      REWIND NEID                                                       ABBQ2933
      REWIND NEICD                                                      ABBQ2934
      IF (IEDFTR.EQ.0) GO TO 225                                        ABBQ2935
C***********************************************************************ABBQ2936
C   COMPUTE BOUNDARY FACTORS                                            ABBQ2937
C***********************************************************************ABBQ2938
      DO 210 J=1,NFD                                                    ABBQ2939
      FCD(J)=AVC(J)/AVJ(J,1)                                            ABBQ2940
      FBD(J)=AVB(J)/AVJ(J,NR)                                           ABBQ2941
  210 CONTINUE                                                          ABBQ2942
C***********************************************************************ABBQ2943
C   COMPUTE ANISOTROPY FACTORS                                          ABBQ2944
C***********************************************************************ABBQ2945
      DO 220 L=1,NR                                                     ABBQ2946
      DO 220 J=1,NFD                                                    ABBQ2947
      FK(J,L)=AVK(J,L)/AVJ(J,L)                                         ABBQ2948
  220 CONTINUE                                                          ABBQ2949
  225 CONTINUE                                                          ABBQ2950
      IF (IEDFTR.EQ.0) RETURN                                           ABBQ2951
C***********************************************************************ABBQ2952
C   COMPUTE CONFIGURATION FUNCTION                                      ABBQ2953
C***********************************************************************ABBQ2954
      DO 260 J=1,NFD                                                    ABBQ2955
      DO 230 L=1,NR                                                     ABBQ2956
      Y(L)=(2.0*FK(J,L)-QRAT(J,L))/FK(J,L)                              ABBQ2957
  230 CONTINUE                                                          ABBQ2958
      DO 240 L=1,N1                                                     ABBQ2959
      DP(L)=WZETA1(L)*Y(L)+WZETA2(L)*Y(L+1)                             ABBQ2960
  240 CONTINUE                                                          ABBQ2961
      Y(1)=0.0                                                          ABBQ2962
      DO 250 L=2,NR                                                     ABBQ2963
      Y(L)=Y(L-1)+DP(L-1)                                               ABBQ2964
  250 CONTINUE                                                          ABBQ2965
      DO 260 L=1,NR                                                     ABBQ2966
      ZETA(J,L)=DEXP(Y(L))                                              ABBQ2967
  260 CONTINUE                                                          ABBQ2968
C***********************************************************************ABBQ2969
C   WRITE AJ, FK, ZETA ONTO DISK FILE                                   ABBQ2970
C***********************************************************************ABBQ2971
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 ABBQ2972
  266 CONTINUE                                                          ABBQ2973
C***********************************************************************ABBQ2974
C   UPDATE AJ, AH, ETA                                                  ABBQ2975
C***********************************************************************ABBQ2976
      CALL GETAJ                                                        ABBQ2977
      RETURN                                                            ABBQ2978
      END                                                               ABBQ2979
                                                                        ABBQ2980
C***********************************************************************ABBQ2981
C   SUBROUTINE EFBC CALCULATES THE CONSTANTS IN THE BOUNDARY            ABBQ2982
C   CONDITIONS FOR THE RAY EQUATIONS                                    ABBQ2983
C   B1,B2 ---  ALPHA AND BETA AT CLOUD SURFACE                          ABBQ2984
C   C1,C2 ---  ALPHA AND BETA AT CLOUD CENTER                           ABBQ2985
C***********************************************************************ABBQ2986
      SUBROUTINE EFBC (C1,C2,B1,B2)                                     ABBQ2987
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ2988
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ2989
      DIMENSION C1(1),C2(1),B1(1),B2(1)                                 ABBQ2990
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                ABBQ2991
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            ABBQ2992
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ2993
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ2994
      CON=-4.0*AJ0I                                                     ABBQ2995
      DO 10 J=1,NFD                                                     ABBQ2996
      B1(J)=-1.0                                                        ABBQ2997
      B2(J)=-CON*FLUXSD(J)                                              ABBQ2998
   10 CONTINUE                                                          ABBQ2999
      IF (IC.EQ.0) GO TO 22                                             ABBQ3000
      IF (NH.EQ.1) GO TO 33                                             ABBQ3001
   15 DO 20 J=1,NFD                                                     ABBQ3002
      C1(J)=1.0                                                         ABBQ3003
      C2(J)=CON*FLUXCD(J)                                               ABBQ3004
   20 CONTINUE                                                          ABBQ3005
      RETURN                                                            ABBQ3006
   22 CONTINUE                                                          ABBQ3007
      DO 30 J=1,NFD                                                     ABBQ3008
      C1(J)=0.0                                                         ABBQ3009
      C2(J)=0.0                                                         ABBQ3010
   30 CONTINUE                                                          ABBQ3011
      RETURN                                                            ABBQ3012
   33 CONTINUE                                                          ABBQ3013
      DO 40 J=1,NFD                                                     ABBQ3014
      C1(J)=0.0                                                         ABBQ3015
      C2(J)=-2.0*AJ0I*FLUXCD(J)                                         ABBQ3016
   40 CONTINUE                                                          ABBQ3017
      RETURN                                                            ABBQ3018
      END                                                               ABBQ3019
                                                                        ABBQ3020
C***********************************************************************ABBQ3021
C   SUBROUTINE GETAJ UPDATES THE MEAN INTENSITY AJ. IF CONVERGENCE      ABBQ3022
C   HAS NOT BEEN REACHED, THE FLUX AND EMISSIVITY ARE UPDATED           ABBQ3023
C***********************************************************************ABBQ3024
      SUBROUTINE GETAJ                                                  ABBQ3025
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ3026
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ3027
      PARAMETER (NDF7=7*NDF)                                            ABBQ3028
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ3029
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          ABBQ3030
      COMMON DUMMY(NDF7)                                                ABBQ3031
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),             ABBQ3032
     1          ETA(NF,ND),AH(NF,ND),CHICG(NF,ND),BTDAV(NF,ND),         ABBQ3033
     2          QABS(NF,ND),QSCA(NF,ND),Q(NF,ND),H(NF,ND)               ABBQ3034
      EQUIVALENCE (DUMMY(1),AJ(1,1),Q(1,1)),                            ABBQ3035
     1            (DUMMY(NDF1P1),FK(1,1),QABS(1,1)),                    ABBQ3036
     2            (DUMMY(NDF2P1),ZETA(1,1),QSCA(1,1)),                  ABBQ3037
     3            (DUMMY(NDF3P1),CHI(1,1)),                             ABBQ3038
     2            (DUMMY(NDF4P1),ETA(1,1)),                             ABBQ3039
     3            (DUMMY(NDF5P1),CHICG(1,1),BTDAV(1,1),AH(1,1)),        ABBQ3040
     4            (DUMMY(NDF6P1),H(1,1))                                ABBQ3041
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   ABBQ3042
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ3043
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ3044
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             ABBQ3045
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ3046
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ3047
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        ABBQ3048
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ3049
C***********************************************************************ABBQ3050
C   READ AJ,FK,ZETA FROM DISK FILE                                      ABBQ3051
C***********************************************************************ABBQ3052
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 ABBQ3053
C***********************************************************************ABBQ3054
C   READ CHI, ETA FROM DISK FILE                                        ABBQ3055
C***********************************************************************ABBQ3056
      CALL BRANRD (NCED,DUMMY(NDF3P1),NB2D)                             ABBQ3057
C***********************************************************************ABBQ3058
C   READ CHICG FROM DISK FILE                                           ABBQ3059
C***********************************************************************ABBQ3060
      CALL BRANRD (NCGD,DUMMY(NDF5P1),NB1D)                             ABBQ3061
C***********************************************************************ABBQ3062
C   CALCULATE BOUNDARY CONSTANTS FOR COMBINED MOMENT EQUATIONS          ABBQ3063
C***********************************************************************ABBQ3064
      CALL BCD (B1,B2,B1N,B2N)                                          ABBQ3065
C***********************************************************************ABBQ3066
C   CALCULATE AJ BY GAUSSIAN ELIMINATION                                ABBQ3067
C***********************************************************************ABBQ3068
      DO 10 J=1,NFD                                                     ABBQ3069
      W1=ZETA(J,1)*CHICG(J,1)                                           ABBQ3070
      W2=ZETA(J,2)*CHICG(J,2)                                           ABBQ3071
      C11=W1*WR1(1)*(1.5*W2-WR3(1)*W1)                                  ABBQ3072
      C12=WR2(1)*W1*W2                                                  ABBQ3073
      B1I=1.0/(W2*FK(J,1)*ZETA(J,1)+B1(J)*C11+C12*CHI(J,1))             ABBQ3074
      Q(J,1)=-(C11*B2(J)-C12*ETA(J,1))*B1I                              ABBQ3075
      H(J,1)=W2*FK(J,2)*ZETA(J,2)*B1I                                   ABBQ3076
   10 CONTINUE                                                          ABBQ3077
      DO 20 L=2,N1                                                      ABBQ3078
      DO 20 J=1,NFD                                                     ABBQ3079
      W1=ZETA(J,L-1)*CHICG(J,L-1)                                       ABBQ3080
      W2=ZETA(J,L)*CHICG(J,L)                                           ABBQ3081
      W3=ZETA(J,L+1)*CHICG(J,L+1)                                       ABBQ3082
      X13=W1*W3                                                         ABBQ3083
      T12=WR1(L)*X13+WR2(L)*W2*W3                                       ABBQ3084
      T34=WR3(L)*X13+WR4(L)*W2*W1                                       ABBQ3085
      T5=WR5(L)*X13*W2                                                  ABBQ3086
      AL=T12*FK(J,L-1)*ZETA(J,L-1)                                      ABBQ3087
      BL=-(T12+T34)*FK(J,L)*ZETA(J,L)-T5*CHI(J,L)                       ABBQ3088
      CL=T34*FK(J,L+1)*ZETA(J,L+1)                                      ABBQ3089
      DL=-T5*ETA(J,L)                                                   ABBQ3090
      EL=1.0/(BL+AL*H(J,L-1))                                           ABBQ3091
      Q(J,L)=(DL-AL*Q(J,L-1))*EL                                        ABBQ3092
      H(J,L)=-CL*EL                                                     ABBQ3093
   20 CONTINUE                                                          ABBQ3094
      DO 30 J=1,NFD                                                     ABBQ3095
      WN=ZETA(J,NR)*CHICG(J,NR)                                         ABBQ3096
      WN1=ZETA(J,N1)*CHICG(J,N1)                                        ABBQ3097
      CN1=WN*WR1(NR)*(1.5*WN1-WR3(NR)*WN)                               ABBQ3098
      CN2=WR2(NR)*WN1*WN                                                ABBQ3099
      AN=WN1*FK(J,N1)*ZETA(J,N1)                                        ABBQ3100
      BN=-WN1*FK(J,NR)*ZETA(J,NR)+CN1*B1N(J)-CN2*CHI(J,NR)              ABBQ3101
      DN=-(CN1*B2N(J)+CN2*ETA(J,NR))                                    ABBQ3102
      Q(J,NR)=(DN-AN*Q(J,N1))/(BN+AN*H(J,N1))                           ABBQ3103
   30 CONTINUE                                                          ABBQ3104
      DO 40 L=1,N1                                                      ABBQ3105
      DO 40 J=1,NFD                                                     ABBQ3106
      Q(J,NR-L)=Q(J,NR-L)+H(J,NR-L)*Q(J,NR-L+1)                         ABBQ3107
   40 CONTINUE                                                          ABBQ3108
C***********************************************************************ABBQ3109
C   WRITE AJ, FK, AND ZETA ONTO DISK FILE                               ABBQ3110
C***********************************************************************ABBQ3111
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 ABBQ3112
      IF (ITER.NE.0) GO TO 55                                           ABBQ3113
C***********************************************************************ABBQ3114
C   READ QABS AND QSCA FROM DISK FILE                                   ABBQ3115
C***********************************************************************ABBQ3116
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB2D)                            ABBQ3117
C***********************************************************************ABBQ3118
C   UPDATE CHI                                                          ABBQ3119
C***********************************************************************ABBQ3120
      DO 50 J=1,NFD                                                     ABBQ3121
      DO 50 L=1,NR                                                      ABBQ3122
      CHI(J,L)=QABS(J,L)+QSCA(J,L)                                      ABBQ3123
   50 CONTINUE                                                          ABBQ3124
C***********************************************************************ABBQ3125
C   WRITE CHI TO DISK FILE                                              ABBQ3126
C***********************************************************************ABBQ3127
      CALL BRANWT (NCED,DUMMY(NDF3P1),NB2D)                             ABBQ3128
   55 CONTINUE                                                          ABBQ3129
      IF (ITCON.EQ.0) RETURN                                            ABBQ3130
C***********************************************************************ABBQ3131
C   UPDATE AH                                                           ABBQ3132
C***********************************************************************ABBQ3133
      DO 60 J=1,NFD                                                     ABBQ3134
      AH(J,1)=-(B1(J)*Q(J,1)+B2(J))                                     ABBQ3135
      AH(J,NR)=-(B1N(J)*Q(J,NR)+B2N(J))                                 ABBQ3136
   60 CONTINUE                                                          ABBQ3137
      DO 70 L=2,N1                                                      ABBQ3138
      WR12=WR1(L)*WR1(L)                                                ABBQ3139
      WR32=WR3(L)*WR3(L)                                                ABBQ3140
      WR5R2I=1.0/(WR5(L)*(R(L)**IGEOM))                                 ABBQ3141
      WT1=WR12*WR5R2I                                                   ABBQ3142
      WT2=(WR32-WR12)*WR5R2I                                            ABBQ3143
      WT3=-WR32*WR5R2I                                                  ABBQ3144
      DO 70 J=1,NFD                                                     ABBQ3145
      FPQ1=FK(J,L-1)*ZETA(J,L-1)*Q(J,L-1)                               ABBQ3146
      FPQ2=FK(J,L)*ZETA(J,L)*Q(J,L)                                     ABBQ3147
      FPQ3=FK(J,L+1)*ZETA(J,L+1)*Q(J,L+1)                               ABBQ3148
      AH(J,L)=(WT1*FPQ1+WT2*FPQ2+WT3*FPQ3)/(ZETA(J,L)*CHICG(J,L))       ABBQ3149
   70 CONTINUE                                                          ABBQ3150
C***********************************************************************ABBQ3151
C   WRITE AH TO DISK FILE                                               ABBQ3152
C***********************************************************************ABBQ3153
      CALL BRANWT (NAHD,DUMMY(NDF5P1),NB1D)                             ABBQ3154
C***********************************************************************ABBQ3155
C   READ QABS AND QSCA FROM DISK FILE                                   ABBQ3156
C***********************************************************************ABBQ3157
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB2D)                            ABBQ3158
C***********************************************************************ABBQ3159
C   READ BTDAV FROM DISK FILE                                           ABBQ3160
C***********************************************************************ABBQ3161
      CALL BRANRD (NBTAD,DUMMY(NDF5P1),NB1D)                            ABBQ3162
C***********************************************************************ABBQ3163
C   UPDATE ETA                                                          ABBQ3164
C***********************************************************************ABBQ3165
      DO 80 J=1,NFD                                                     ABBQ3166
      DO 80 L=1,NR                                                      ABBQ3167
      ETA(J,L)=QSCA(J,L)*AJ(J,L)+QABS(J,L)*BTDAV(J,L)                   ABBQ3168
   80 CONTINUE                                                          ABBQ3169
C***********************************************************************ABBQ3170
C   WRITE ETA ONTO DISK FILE                                            ABBQ3171
C***********************************************************************ABBQ3172
      CALL BRANWT (NCED,DUMMY(NDF3P1),NB2D)                             ABBQ3173
      RETURN                                                            ABBQ3174
      END                                                               ABBQ3175
                                                                        ABBQ3176
                                                                        ABBQ3177
C***********************************************************************ABBQ3178
C    SUBROUTINE OUTPUT PRINTS THE RESULTS OF EACH ITERATION AND         ABBQ3179
C    COMPUTES AND PRINTS OUT THE FINAL MODEL RESULTS                    ABBQ3180
C***********************************************************************ABBQ3181
      SUBROUTINE OUTPUT                                                 ABBQ3182
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ3183
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ3184
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         ABBQ3185
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ3186
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          ABBQ3187
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1,          ABBQ3188
     3           NDF8M=NDF8P1-1)                                        ABBQ3189
      PARAMETER (NFC1P1=NFC2+1,NFC2P1=2*NFC2+1,NFC3P1=3*NFC2+1,         ABBQ3190
     1           NFC4P1=4*NFC2+1,NFC5P1=5*NFC2+1,                       ABBQ3191
     2           NFC6P1=6*NFC2+1,NFC7P1=7*NFC2+1,                       ABBQ3192
     3           NFC8P1=8*NFC2+1,NCEIC=NFC8P1+NDF)                      ABBQ3193
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   ABBQ3194
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     ABBQ3195
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     ABBQ3196
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    ABBQ3197
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     ABBQ3198
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1)                     ABBQ3199
      PARAMETER (ND5=5*ND,NF5=5*NF,NY1=NDF4P1+ND5,NY2=NY1+ND5,          ABBQ3200
     1           NY3=NY2+NF5,NY4=NY3+NF5)                               ABBQ3201
      PARAMETER (NXY=6*NDF+NFC2,NTHETA=20)                              ABBQ3202
      PARAMETER (NQG=4,NQGT=NQWT2+NQG,NQGT1=NQGT+NQG,                   ABBQ3203
     1           NQGT2=NQGT1+NQG,NCQW=NQGT2+NQG-1)                      ABBQ3204
      COMMON DUMMY(NXY),TAU1(ND),SOURCE(ND),PLANKJ(ND),AJ1(ND),         ABBQ3205
     1       FK1(ND),ZETA1(ND),CHI1(ND),ETA1(ND),AH1(ND),TOF(ND),       ABBQ3206
     2       ALUM(NF),XU(ND),PP(ND),AHOUTC(NF),AHOUTS(NF),              ABBQ3207
     3       BDAHC(NF),BDAHS(NF),TB(ND),BK(NF)                          ABBQ3208
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),             ABBQ3209
     1          ETA(NF,ND),AH(NF,ND),QABS(NF,ND),BTDAV(NF,ND),          ABBQ3210
     2          EI(NF,ND),EITB(NF,ND),EIC(NF,NC2),XMU(NTHETA)           ABBQ3211
      DIMENSION HEATD(ND),COOLD(ND),AVFLUX(ND)                          ABBQ3212
      DIMENSION ABUNDI(5,ND),TDI(5,ND),QABSI(NF,5),QSCAI(NF,5),         ABBQ3213
     1          GBARI(NF,5),COOLDI(5,ND),HEATDI(5,ND)                   ABBQ3214
      DIMENSION QRAT(NF,ND),AVJ(NF,ND)                                  ABBQ3215
      DIMENSION QRA1(ND)                                                ABBQ3216
      EQUIVALENCE (DUMMY(1),AJ(1,1)),                                   ABBQ3217
     1            (DUMMY(NDF1P1),FK(1,1),EI(1,1),QABS(1,1)),            ABBQ3218
     2            (DUMMY(NDF2P1),ZETA(1,1),EITB(1,1),BTDAV(1,1)),       ABBQ3219
     3            (DUMMY(NDF3P1),CHI(1,1)),(DUMMY(NDF4P1),ETA(1,1)),    ABBQ3220
     4            (DUMMY(NDF5P1),AH(1,1)),(DUMMY(NDF6P1),EIC(1,1))      ABBQ3221
      EQUIVALENCE (ZETA1(1),COOLD(1)),(ETA1(1),HEATD(1)),               ABBQ3222
     2            (CHI1(1),AVFLUX(1))                                   ABBQ3223
      EQUIVALENCE (DUMMY(NDF4P1),ABUNDI(1,1)),(DUMMY(NY1),TDI(1,1)),    ABBQ3224
     1            (DUMMY(NY2),QABSI(1,1)),                              ABBQ3225
     2            (DUMMY(NY3),QSCAI(1,1),COOLDI(1,1)),                  ABBQ3226
     3            (DUMMY(NY4),GBARI(1,1))                               ABBQ3227
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                ABBQ3228
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              ABBQ3229
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      ABBQ3230
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   ABBQ3231
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    ABBQ3232
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          ABBQ3233
      COMMON/MODEL/RMAX,RMIN,RHOCS,RHOBAR,TAUOF,IOF                     ABBQ3234
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            ABBQ3235
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ3236
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         ABBQ3237
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ3238
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ3239
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG)                         ABBQ3240
      IF (IGEOM.EQ.0) NP=NTHETA                                         ABBQ3241
C***********************************************************************ABBQ3242
C   WRITE THE ITERATION RESULTS TO THE OUTPUT FILE                      ABBQ3243
C***********************************************************************ABBQ3244
      WRITE (6,441) ITER,ITCONT,ITCONJ                                  ABBQ3245
  441 FORMAT (1H1,5X,'ITERATION ',I2,10X,'ITCONT = ',I1,10X,'ITCONJ = ',ABBQ3246
     1 I1/2X,'IR',10X,'R',13X,'TD',13X,'DTD',12X,'DJD',12X,'DHD')       ABBQ3247
      WRITE (6,442) (L,R(L),TD(L),DTD(L),DJD(L),DHD(L),L=1,NR)          ABBQ3248
  442 FORMAT (1H9,I3,1P5E15.4)                                          ABBQ3249
      DO 10 L=1,NR                                                      ABBQ3250
      DJD(L)=0.0                                                        ABBQ3251
      DHD(L)=0.0                                                        ABBQ3252
   10 CONTINUE                                                          ABBQ3253
      IF (ITCON) 11,11,12                                               ABBQ3254
   11 CONTINUE                                                          ABBQ3255
      IF (ITER.GE.ITMAX) WRITE (6,443) ITER                             ABBQ3256
  443 FORMAT (1H1,20(/),45X,'NO CONVERGENCE AFTER ',I2,' ITERATIONS')   ABBQ3257
      IF (ITER.GE.ITMAX) GO TO 15                                       ABBQ3258
      RETURN                                                            ABBQ3259
   12 CONTINUE                                                          ABBQ3260
      WRITE (6,444) ITER                                                ABBQ3261
  444 FORMAT (1H1,20(/),44X,'SUCCESSFUL CONVERGENCE AFTER ',I2,         ABBQ3262
     1 ' ITERATIONS'///40X,'FINAL RESULTS FOR THERMAL PROPERTIES OF DIDCABBQ3263
     2 MODEL')                                                          ABBQ3264
   15 CONTINUE                                                          ABBQ3265
      IF (ITCON.NE.1.OR.IGEOM.NE.1.OR.IC.EQ.0) GOTO 35                  ABBQ3266
      DO 30 IBC=1,NQG                                                   ABBQ3267
      DO 20 J=1,NFD                                                     ABBQ3268
      DO 20 L=1,NC2                                                     ABBQ3269
      READ (NEICD) EIC(J,L)                                             ABBQ3270
   20 CONTINUE                                                          ABBQ3271
   30 CONTINUE                                                          ABBQ3272
   35 CONTINUE                                                          ABBQ3273
      READ (5,445)ICONV                                                 ABBQ3274
  445 FORMAT(7X,I1)                                                     ABBQ3275
      HC=AJ0/CTH                                                        ABBQ3276
      FOURPI=4.0*PI                                                     ABBQ3277
      IF (ITCON.EQ.0) GO TO 44                                          ABBQ3278
      PCON=180.0/PI                                                     ABBQ3279
      DO 40 IP=1,N1                                                     ABBQ3280
      IF (IC.EQ.0) PP(IP)=DASIN(R(IP))*PCON                             ABBQ3281
      IF (IC.NE.0) PP(IP+1)=DASIN(R(IP))*PCON                           ABBQ3282
   40 CONTINUE                                                          ABBQ3283
      IF (IC.EQ.0) PP(NR)=90.0                                          ABBQ3284
      IF (IC.NE.0) PP(1)=0.0                                            ABBQ3285
   44 CONTINUE                                                          ABBQ3286
C***********************************************************************ABBQ3287
C   READ AJ, FK, ZETA FROM DISK FILE                                    ABBQ3288
C***********************************************************************ABBQ3289
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 ABBQ3290
C***********************************************************************ABBQ3291
C   READ CHI, ETA FROM DISK FILE                                        ABBQ3292
C***********************************************************************ABBQ3293
      CALL BRANRD (NCED,DUMMY(NDF3P1),NB2D)                             ABBQ3294
C***********************************************************************ABBQ3295
C   READ AH, FROM DISK FILE                                             ABBQ3296
C***********************************************************************ABBQ3297
      CALL BRANRD (NAHD,DUMMY(NDF5P1),NB1D)                             ABBQ3298
      IF (IGEOM.EQ.2) CALL BRANRD (NEICD,DUMMY(NDF6P1),NFC2)            ABBQ3299
      DO 70 J=1,NFD                                                     ABBQ3300
      TF=HK*FREQD(J)                                                    ABBQ3301
      HFC=HC*FREQD(J)**3                                                ABBQ3302
C***********************************************************************ABBQ3303
C   CALCULATE BRIGHTNESS TEMPERATURE                                    ABBQ3304
C***********************************************************************ABBQ3305
      DO 50 L=1,NR                                                      ABBQ3306
      IF (IGEOM.EQ.1) READ (NEICD) QRAT(J,L)                            ABBQ3307
      AJ1(L)=AJ(J,L)*AJ0                                                ABBQ3308
      IF (AJ1(L).LE.0.0) TB(L)=0.0                                      ABBQ3309
      IF (AJ1(L).LE.0.0) GO TO 46                                       ABBQ3310
      TB(L)=TF/(DLOG(HFC/AJ1(L)+1.0))                                   ABBQ3311
   46 CONTINUE                                                          ABBQ3312
      FK1(L)=FK(J,L)                                                    ABBQ3313
      ZETA1(L)=ZETA(J,L)                                                ABBQ3314
      IF (IGEOM.EQ.1) QRA1(L)=QRAT(J,L)                                 ABBQ3315
      CHI1(L)=CHI(J,L)*R0I                                              ABBQ3316
      ETA1(L)=ETA(J,L)*ETA0I                                            ABBQ3317
      AH1(L)=AH(J,L)*AJ0                                                ABBQ3318
C***********************************************************************ABBQ3319
C   CALCULATE  FLUX AT BOUNDARIES AND THE SOURCE FUNCTION               ABBQ3320
C***********************************************************************ABBQ3321
      IF (L.EQ.1) BDAHC(J)=AH1(L)                                       ABBQ3322
      IF (L.EQ.NR) BDAHS(J)=AH1(L)                                      ABBQ3323
      SOURCE(L)=ETA1(L)/CHI1(L)                                         ABBQ3324
C***********************************************************************ABBQ3325
C   CALCULATE PLANCK FUNCTION FOR THE GRAINS                            ABBQ3326
C***********************************************************************ABBQ3327
      CALL GETBJ (FREQD(J),TD(L),0,BJ0,BJ1)                             ABBQ3328
      PLANKJ(L)=AJ0*BJ0                                                 ABBQ3329
   50 CONTINUE                                                          ABBQ3330
C***********************************************************************ABBQ3331
C   CALCULATE  OPTICAL DEPTH PROFILE                                    ABBQ3332
C***********************************************************************ABBQ3333
      CALL GETAU (R,CHI1,TAU1,NR)                                       ABBQ3334
      DO 60 L=1,NR                                                      ABBQ3335
      TAU1(L)=TAU1(L)*R0                                                ABBQ3336
      IF (J.EQ.IOF) TOF(L)=TAU1(L)                                      ABBQ3337
   60 CONTINUE                                                          ABBQ3338
      IF (IOUT.EQ.0) GO TO 65                                           ABBQ3339
      IF (IGEOM.EQ.1) GO TO 63                                          ABBQ3340
C***********************************************************************ABBQ3341
C   WRITE BRIGHTNESS TEMPERATURE, ANISOTROPY FACTORS, BOUNDARY FACTORS, ABBQ3342
C   OPTICAL DEPTH, DUST TEMPERATURE AND MEAN INTENSITY FOR EACH         ABBQ3343
C   FREQUENCY AND RADIAL GRID POINT                                     ABBQ3344
C***********************************************************************ABBQ3345
      WRITE (6,551) J,FREQD(J),WLAMDA(J),TF,FBD(J),FCD(J)               ABBQ3346
  551 FORMAT (1H1,11H**FREQUENCY,I3,2H**,2X,'FREQ =',1PE10.3,' HERTZ',  ABBQ3347
     1 2X,'LAMBDA=',1PE10.3,' MICRON',2X,'TF=',1PE10.3,' DEGREE',2X,    ABBQ3348
     2 'FBD=',1PE10.3,2X,'FCD=',1PE10.3/2X,'IR',6X,'TB',8X,             ABBQ3349
     3  'TAU',9X,'TD',8X,'CHI',8X,'ETA',9X,'FK',8X,'ZETA',9X,'AJ',9X,   ABBQ3350
     4  'BJ',7X,'SOURCE',7X,'AH')                                       ABBQ3351
      WRITE (6,552) (L,TB(L),TAU1(L),TD(L),CHI1(L),ETA1(L),             ABBQ3352
     1 FK1(L),ZETA1(L),AJ1(L),PLANKJ(L),SOURCE(L),AH1(L),L=1,NR)        ABBQ3353
  552 FORMAT (1H9,I3,1P11E11.3)                                         ABBQ3354
      GO TO 65                                                          ABBQ3355
   63 CONTINUE                                                          ABBQ3356
C***********************************************************************ABBQ3357
C   WRITE THE ABOVE FOR THE CYLINDRICAL CASE                            ABBQ3358
C***********************************************************************ABBQ3359
      WRITE (6,553) J,FREQD(J),WLAMDA(J),TF,FBD(J),FCD(J)               ABBQ3360
  553 FORMAT (1H1,11H**FREQUENCY,I3,2H**,2X,'FREQ =',1PE10.3,' HERTZ',  ABBQ3361
     1 2X,'LAMBDA=',1PE10.3,' MICRON',2X,'TF=',1PE10.3,' DEGREE',2X,    ABBQ3362
     2 'FBD=',1PE10.3,2X,'FCD=',1PE10.3/2X,'IR',6X,'TB',8X,             ABBQ3363
     3  'TAU',8X,'CHI',8X,'ETA',9X,'FK',7X,'QRAT',8X,'ZETA',9X,'AJ',9X, ABBQ3364
     4  'BJ',7X,'SOURCE',7X,'AH')                                       ABBQ3365
      WRITE (6,552) (L,TB(L),TAU1(L),CHI1(L),ETA1(L),FK1(L),            ABBQ3366
     1 QRA1(L),ZETA1(L),AJ1(L),PLANKJ(L),SOURCE(L),AH1(L),L=1,NR)       ABBQ3367
   65 CONTINUE                                                          ABBQ3368
C***********************************************************************ABBQ3369
C   CALCULATE FLUX AT BOUNDARIES AND THE OBSERVED FLUX                  ABBQ3370
C***********************************************************************ABBQ3371
      AHOUTC(J)=FLUXCD(J)-BDAHC(J)                                      ABBQ3372
      AHOUTS(J)=FLUXSD(J)+BDAHS(J)                                      ABBQ3373
      ALUM(J)=FOURPI*AHOUTS(J)                                          ABBQ3374
   70 CONTINUE                                                          ABBQ3375
      REWIND NEICD                                                      ABBQ3376
      CCOOLD=FOURPI*AJ0*R0I                                             ABBQ3377
      NBC=1                                                             ABBQ3378
      IF (IGEOM.EQ.1.AND.IEMRG.NE.0) NBC=NQG                            ABBQ3379
C***********************************************************************ABBQ3380
C   WRITE CONTINUUM RADIATION PROPERTIES TO OUTPUT                      ABBQ3381
C***********************************************************************ABBQ3382
      WRITE (6,554) (J,FREQD(J),WLAMDA(J),BDAHC(J),FLUXCD(J),AHOUTC(J)  ABBQ3383
     1 ,FCD(J),BDAHS(J),FLUXSD(J),AHOUTS(J),FBD(J),ALUM(J),             ABBQ3384
     2 J=1,NFD)                                                         ABBQ3385
  554 FORMAT (1H1,30X,'PROPERTIES OF CONTINUUM RADIATION AT INNER AND OUABBQ3386
     1TER BOUNDARIES'/2X,'IF',5X,'FREQD',5X,'MICRON',5X,'BDAHC',6X,     ABBQ3387
     2 'FLUXCD',    5X,'AHOUTC',6X,'FCD',7X,'BDAHS',6X,'FLUXSD',5X,     ABBQ3388
     3 'AHOUTS',6X,'FBD',6X,'OBS FLUX'/(1X,I3,1X,1P11E11.3))            ABBQ3389
      IF (IGEOM.EQ.1.AND.IEMRG.EQ.0) THET=DASIN(BETA(1))*PCON           ABBQ3390
C***********************************************************************ABBQ3391
C   READ EMERGENT INTENSITIES THROUGH CORE FROM THE DISK FILE           ABBQ3392
C***********************************************************************ABBQ3393
      IF (ITCON.EQ.0) GO TO 177                                         ABBQ3394
      DO 170 IBC=1,NBC                                                  ABBQ3395
      THET=DASIN(BETA(IBC))*PCON                                        ABBQ3396
      DO 90 J=1,NFD                                                     ABBQ3397
      BDAHS(J)=FOURPI*FLUXSD(J)                                         ABBQ3398
      BDAHC(J)=FOURPI*FLUXCD(J)                                         ABBQ3399
      DO 90 IP=1,NR                                                     ABBQ3400
      IF (IGEOM.EQ.1) READ (NEID) EI(J,IP)                              ABBQ3401
      IF (IP.GT.NC2.OR.IC.EQ.0) GOTO 90                                 ABBQ3402
      IF (IGEOM.EQ.1) READ (NEICD) EIC(J,IP)                            ABBQ3403
      EIC(J,IP)=AJ0*EIC(J,IP)                                           ABBQ3404
   90 CONTINUE                                                          ABBQ3405
      IF (IC.EQ.0.OR.IGEOM.EQ.0) GO TO 126                              ABBQ3406
      IF (IGEOM.EQ.1) WRITE (6,666) THET                                ABBQ3407
  666 FORMAT (2X,'THETA = ',1PE10.3)                                    ABBQ3408
C***********************************************************************ABBQ3409
C   WRITE EMERGENT INTENSITIES THROUGH CORE AND EMERGENT FLUX           ABBQ3410
C   TO OUTPUT                                                           ABBQ3411
C***********************************************************************ABBQ3412
      WRITE (6,771) (J,WLAMDA(J),(EIC(J,IP),IP=1,NC,2),EIC(J,NC1),      ABBQ3413
     1              EIC(J,NC2),BDAHC(J),BDAHS(J),ALUM(J),J=1,NFD)       ABBQ3414
  771 FORMAT (1H1,45X,'EMERGENT INTENSITIES AND FLUXES'/1X,'IF',        ABBQ3415
     1  5X,'MICRON',5X,'EIC(1)',5X,'EIC(3)',5X,'EIC(5)',5X,'EIC(7)',    ABBQ3416
     2 5X,'EIC(9)',4X,'EIC(10)',4X,'EIC(11)',5X,'FLUXCD',5X,'FLUXSD',4X,ABBQ3417
     3 'OBS FLUX'/(I3,1X,1P11E11.3))                                    ABBQ3418
  126 CONTINUE                                                          ABBQ3419
C***********************************************************************ABBQ3420
C   READ EMERGENT INTENSITIES OUTSIDE CORE FROM THE DISK FILE           ABBQ3421
C***********************************************************************ABBQ3422
      IF (IGEOM.NE.1) CALL BRANRD (NEID,DUMMY(NDF1P1),NB1D)             ABBQ3423
C***********************************************************************ABBQ3424
C   CALCULATE ANGULAR DISTRIBUTION OF EMERGENT INTENSITIES              ABBQ3425
C***********************************************************************ABBQ3426
      WRITE (6,773)                                                     ABBQ3427
  773 FORMAT (1H1,30X,'ANGULAR DISTRIBUTION OF EMERGENT INTENSITY FOR DUABBQ3428
     1ST COMPONENT'/)                                                   ABBQ3429
      XUCON=PI/180.0                                                    ABBQ3430
      NZ=NR                                                             ABBQ3431
      IF (IGEOM.EQ.0) NZ=NTHETA                                         ABBQ3432
      DO 150 IP=1,NZ                                                    ABBQ3433
      IF (IGEOM.NE.0) XU(IP)=DCOS(PP(IP)*XUCON)                         ABBQ3434
      IF (IGEOM.EQ.0) XU(IP)=XMU(IP)                                    ABBQ3435
      IF (IGEOM.EQ.0) PP(IP)=DACOS(XU(IP))                              ABBQ3436
      DO 140 J=1,NFD                                                    ABBQ3437
      EI(J,IP)=AJ0*EI(J,IP)                                             ABBQ3438
      IF (EI(J,IP).EQ.0.0) EITB(J,IP)=0.0                               ABBQ3439
      IF (EI(J,IP).EQ.0.0) GO TO 140                                    ABBQ3440
      TF=HK*FREQD(J)                                                    ABBQ3441
      HFC=HC*FREQD(J)**3                                                ABBQ3442
      XX=HFC/EI(J,IP)+1.0                                               ABBQ3443
      IF (XX.LE.0.0) EITB(J,IP)=0.0                                     ABBQ3444
      IF (XX.LE.0.0) GO TO 140                                          ABBQ3445
      EITB(J,IP)=TF/DLOG(XX)                                            ABBQ3446
  140 CONTINUE                                                          ABBQ3447
      IF (IC.NE.0) GO TO 143                                            ABBQ3448
      TOFP=TOF(IP)                                                      ABBQ3449
      RP=R(IP)                                                          ABBQ3450
      GO TO 147                                                         ABBQ3451
  143 CONTINUE                                                          ABBQ3452
      IF (IP.GT.1) GO TO 145                                            ABBQ3453
      TOFP=TOF(IP)                                                      ABBQ3454
      RP=0.0                                                            ABBQ3455
      GO TO 147                                                         ABBQ3456
  145 CONTINUE                                                          ABBQ3457
      TOFP=TOF(IP-1)                                                    ABBQ3458
      RP=R(IP-1)                                                        ABBQ3459
  147 CONTINUE                                                          ABBQ3460
C***********************************************************************ABBQ3461
C   WRITE ANGULAR DISTRIBUTION OF EMERGENT INTENSITIES                  ABBQ3462
C   TO OUTPUT                                                           ABBQ3463
C***********************************************************************ABBQ3464
      IF (IGEOM.EQ.1) GO TO 149                                         ABBQ3465
      WRITE (6,775) IP,TOFP,XU(IP),PP(IP),(EI(J,IP),                    ABBQ3466
     1              EITB(J,IP),J=1,NFD)                                 ABBQ3467
  775 FORMAT (5X,'IP = ',I3,4X,'TAU = ',1PE10.3,5X,                     ABBQ3468
     1 'XMU = ',1PE10.3,5X,'THETA = ',1PE10.3/(1X,'EMERG INT',1X,       ABBQ3469
     2 5(1PE10.3,1X,'(',1PE10.3,')',1X)))                               ABBQ3470
      GO TO 150                                                         ABBQ3471
  149 CONTINUE                                                          ABBQ3472
      WRITE (6,776) IP,TOFP,RP,XU(IP),PP(IP),(EI(J,IP),                 ABBQ3473
     1              EITB(J,IP),J=1,NFD)                                 ABBQ3474
  776 FORMAT (5X,'IP = ',I3,4X,'TAU = ',1PE10.3,5X,'R = ',1PE10.3,5X,   ABBQ3475
     1 'XMU = ',1PE10.3,5X,' PHI  = ',1PE10.3/(1X,'EMERG INT',1X,       ABBQ3476
     2 5(1PE10.3,1X,'(',1PE10.3,')',1X)))                               ABBQ3477
  150 CONTINUE                                                          ABBQ3478
      DO 160 L1=1,NFD,10                                                ABBQ3479
      L2=MIN0(NFD,L1+9)                                                 ABBQ3480
      WRITE (6,887) (WLAMDA(J),J=L1,L2)                                 ABBQ3481
  887 FORMAT (1H1,30X,'ANGULAR DISTRIBUTION OF EMERGENT INTENSITY AT SELABBQ3482
     1ECTED WAVELENGTHS'/3X,'I',7X,'R',5X,10('(',1PE9.2,')')/)          ABBQ3483
      DO 160 L=1,NZ                                                     ABBQ3484
      RP=R(L)                                                           ABBQ3485
      IF (IGEOM.EQ.0) RP=P(L)                                           ABBQ3486
      WRITE (6,888) L,RP,(EI(J,L),J=L1,L2)                              ABBQ3487
  888 FORMAT (1H9,I3,1X,1P11E11.3)                                      ABBQ3488
  160 CONTINUE                                                          ABBQ3489
C***********************************************************************ABBQ3490
C   CALL CONVOL TO CALCULATE CONVOLUTION INTEGRAL                       ABBQ3491
C***********************************************************************ABBQ3492
      IF (ICONV.EQ.1) CALL CONVOL(EI,EIC)                               ABBQ3493
  170 CONTINUE                                                          ABBQ3494
  177 CONTINUE                                                          ABBQ3495
      IF (IGEOM.EQ.1) REWIND NEICD                                      ABBQ3496
      IF (IGEOM.EQ.1) REWIND NEID                                       ABBQ3497
C***********************************************************************ABBQ3498
C   READ QABS AND QSCA FROM DISK FILE                                   ABBQ3499
C***********************************************************************ABBQ3500
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB1D)                            ABBQ3501
C***********************************************************************ABBQ3502
C   READ BTDAV FROM DISK FILE                                           ABBQ3503
C***********************************************************************ABBQ3504
      CALL BRANRD (NBTAD,DUMMY(NDF2P1),NB1D)                            ABBQ3505
C***********************************************************************ABBQ3506
C   READ GRAIN CHARACTERISTICS FROM DISK FILE                           ABBQ3507
C***********************************************************************ABBQ3508
      CALL BRANRD (NGPD,DUMMY(NDF4P1),NGP)                              ABBQ3509
      DO 220 L=1,NR                                                     ABBQ3510
      SUMAH=0.0                                                         ABBQ3511
      SUMHD=0.0                                                         ABBQ3512
      SUMCD=0.0                                                         ABBQ3513
      DO 180 J=1,NFD                                                    ABBQ3514
      XDUM=WFD(J)*AH(J,L)                                               ABBQ3515
      SUMAH=SUMAH+XDUM                                                  ABBQ3516
      XX=QABS(J,L)*WFD(J)                                               ABBQ3517
      SUMCD=SUMCD+XX*BTDAV(J,L)                                         ABBQ3518
      SUMHD=SUMHD+XX*AJ(J,L)                                            ABBQ3519
  180 CONTINUE                                                          ABBQ3520
C***********************************************************************ABBQ3521
C   CALCULATE NET FLUX                                                  ABBQ3522
C***********************************************************************ABBQ3523
      AVFLUX(L)=SUMAH*AJ0                                               ABBQ3524
      IF (IC.NE.0.AND.IGEOM.EQ.2) AVFLUX(L)=AVFLUX(L)*(R0*R(L))**2      ABBQ3525
      IF (IC.NE.0.AND.IGEOM.EQ.1) AVFLUX(L)=AVFLUX(L)*(R0*R(L))         ABBQ3526
C***********************************************************************ABBQ3527
C   CALCULATE HEATING AND COOLING RATES                                 ABBQ3528
C***********************************************************************ABBQ3529
      COOLD(L)=CCOOLD*SUMCD                                             ABBQ3530
      HEATD(L)=CCOOLD*SUMHD                                             ABBQ3531
      DO 210 IG=1,IMIX                                                  ABBQ3532
      SUMHDI=0.0                                                        ABBQ3533
      SUMCDI=0.0                                                        ABBQ3534
C***********************************************************************ABBQ3535
C   CALCULATE HEATING AND COOLING RATES FOR EACH GRAIN TYPE             ABBQ3536
C***********************************************************************ABBQ3537
      IF (IMIX.EQ.1) TDI(IG,L)=TD(L)                                    ABBQ3538
      DO 190 J=1,NFD                                                    ABBQ3539
      XX=QABSI(J,IG)*WFD(J)                                             ABBQ3540
      CALL GETBJ (FREQD(J),TDI(IG,L),0,BJ0,BJ1)                         ABBQ3541
      SUMCDI=SUMCDI+XX*BJ0                                              ABBQ3542
      SUMHDI=SUMHDI+XX*AJ(J,L)                                          ABBQ3543
  190 CONTINUE                                                          ABBQ3544
      XX=CCOOLD*RHOD(L)*R0*ABUNDI(IG,L)                                 ABBQ3545
      COOLDI(IG,L)=XX*SUMCDI                                            ABBQ3546
      HEATDI(IG,L)=XX*SUMHDI                                            ABBQ3547
  210 CONTINUE                                                          ABBQ3548
  220 CONTINUE                                                          ABBQ3549
C***********************************************************************ABBQ3550
C   WRITE CLOUD TEMPERATURE CHARACTERISTICS AND NET FLUX TO             ABBQ3551
C   OUTPUT                                                              ABBQ3552
C***********************************************************************ABBQ3553
      WRITE (6,996)                                                     ABBQ3554
  996 FORMAT (1H1,1X,'IR',7X,'R',8X,'RHOD',6X,'TAUOF',7X,'TD',          ABBQ3555
     1 7X,'AVFLUX',6X,'COOLD',6X,'HEATD')                               ABBQ3556
      WRITE (6,997) (L,R(L),RHOD(L),TOF(L),TD(L),AVFLUX(L),             ABBQ3557
     1COOLD(L),HEATD(L),L=1,NR)                                         ABBQ3558
  997 FORMAT (1H9,I3,1X,1P7E11.3)                                       ABBQ3559
C***********************************************************************ABBQ3560
C   WRITE CLOUD TEMPERATURE CHARACTERISTICS AND NET FLUX                ABBQ3561
C   FOR EACH GRAIN TYPE TO OUTPUT                                       ABBQ3562
C***********************************************************************ABBQ3563
      DO 230 K1=1,IMIX,2                                                ABBQ3564
      K2=MIN0(IMIX,K1+1)                                                ABBQ3565
      WRITE (6,998) (K,K,K,K,K=K1,K2)                                   ABBQ3566
  998 FORMAT (1H1,1X,'IR',6X,'R',5X,2(1X,'ABUNDI(',I2,')',2X,'TDI(',    ABBQ3567
     1 I2,')',3X,'COOLDI(',I2,')',1X,'HEATDI(',I2,')'))                 ABBQ3568
      DO 230 L=1,NR                                                     ABBQ3569
      WRITE (6,999) L,R(L),(ABUNDI(K,L),TDI(K,L),COOLDI(K,L),           ABBQ3570
     1              HEATDI(K,L),K=K1,K2)                                ABBQ3571
  999 FORMAT (1H9,I3,1X,1P9E11.3)                                       ABBQ3572
  230 CONTINUE                                                          ABBQ3573
      RETURN                                                            ABBQ3574
      END                                                               ABBQ3575
                                                                        ABBQ3576
C***********************************************************************ABBQ3577
C   THE SUBROUTINE CONVOL CALCULATES THE CONVOLUTION INTEGRAL           ABBQ3578
C   AT POINTS ON THE SURFACE OF THE CLOUD                               ABBQ3579
C   AR     --- BEAM RADIAL GRID                                         ABBQ3580
C   ANG    --- BEAM ANGULAR GRID                                        ABBQ3581
C   CONV   --- CONVOLUTION INTEGRAL                                     ABBQ3582
C   SUMAR  --- SUM OF RADIAL WEIGHTS                                    ABBQ3583
C   SUMANG --- SUM OF ANGULAR WEIGHTS                                   ABBQ3584
C   PANG   --- NORMALIZATION FACTOR FOR ANGULAR WEIGHTS                 ABBQ3585
C   WAR    --- TRAPEZOIDAL QUADRATURE WEIGHTS FOR RADIAL INTEGRATION    ABBQ3586
C   WTANG  --- TRAPEZOIDAL QUADRATURE WEIGHTS FOR ANGULAR INTEGRATION   ABBQ3587
C   SIGMA  --- HALF WIDTH OF BEAM PATTERN                               ABBQ3588
C***********************************************************************ABBQ3589
      SUBROUTINE CONVOL(E,EC)                                           ABBQ3590
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           ABBQ3591
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               ABBQ3592
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         ABBQ3593
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            ABBQ3594
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          ABBQ3595
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1,          ABBQ3596
     3           NDF8M=NDF8P1-1)                                        ABBQ3597
      PARAMETER (NFC1P1=NFC2+1,NFC2P1=2*NFC2+1,NFC3P1=3*NFC2+1,         ABBQ3598
     1           NFC4P1=4*NFC2+1,NFC5P1=5*NFC2+1,                       ABBQ3599
     2           NFC6P1=6*NFC2+1,NFC7P1=7*NFC2+1,                       ABBQ3600
     3           NFC8P1=8*NFC2+1,NCEIC=NFC8P1+NDF)                      ABBQ3601
      PARAMETER (NXY=6*NDF+NFC2,NANG=20,NP2MAX=2*NPMAX,                 ABBQ3602
     1           NTHETA=20)                                             ABBQ3603
      DIMENSION SIGMA(NF),WTANG(NANG),XMU(NTHETA),WAR(NP2MAX),ANG(NANG),ABBQ3604
     1          E(NF,ND),EC(NF,NC2),CONV(NF,10),AR(30),EINT1(NF,NPMAX)  ABBQ3605
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      ABBQ3606
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        ABBQ3607
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         ABBQ3608
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        ABBQ3609
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        ABBQ3610
      NP1=NP-1                                                          ABBQ3611
C***********************************************************************ABBQ3612
C   READ BEAM WIDTH FOR EACH FREQUENCY FROM THE INPUT DATA SET          ABBQ3613
C***********************************************************************ABBQ3614
      READ (5,300)(SIGMA(J),J=1,NFD)                                    ABBQ3615
  300 FORMAT (1P7E10.3,10X)                                             ABBQ3616
      IF (IC.EQ.0.OR.IGEOM.EQ.0) GO TO 35                               ABBQ3617
      DO 30 J=1,NFD                                                     ABBQ3618
      DO 10 L=1,NC                                                      ABBQ3619
      EINT1(J,L)=EC(J,L)                                                ABBQ3620
   10 CONTINUE                                                          ABBQ3621
      DO 20 LL=1,NR                                                     ABBQ3622
      EINT1(J,LL+NC)=E(J,LL)                                            ABBQ3623
   20 CONTINUE                                                          ABBQ3624
   30 CONTINUE                                                          ABBQ3625
      GO TO 45                                                          ABBQ3626
   35 CONTINUE                                                          ABBQ3627
      DO 40 J=1,NFD                                                     ABBQ3628
      DO 40 L=1,NR                                                      ABBQ3629
      EINT1(J,L)=E(J,L)                                                 ABBQ3630
   40 CONTINUE                                                          ABBQ3631
   45 CONTINUE                                                          ABBQ3632
      NANG1=NANG-1                                                      ABBQ3633
      CON1=2.0*PI/(NANG-1)                                              ABBQ3634
      DO 50 K=1,NANG                                                    ABBQ3635
      ANG(K)=(K-1)*CON1                                                 ABBQ3636
   50 CONTINUE                                                          ABBQ3637
C***********************************************************************ABBQ3638
C   CALCULATE WEIGHTS FOR THE ANGULAR INTEGRATION                       ABBQ3639
C***********************************************************************ABBQ3640
      WTANG(1)=0.0                                                      ABBQ3641
      DO 60 IANG2=1,NANG                                                ABBQ3642
      IF (IANG2.EQ.NANG) GO TO 65                                       ABBQ3643
      B=ANG(IANG2)                                                      ABBQ3644
      F=ANG(IANG2+1)                                                    ABBQ3645
      WTANG(IANG2)=WTANG(IANG2)+0.5*(F-B)                               ABBQ3646
      WTANG(IANG2+1)=0.5*(F-B)                                          ABBQ3647
   60 CONTINUE                                                          ABBQ3648
   65 CONTINUE                                                          ABBQ3649
      SUMANG=0.0                                                        ABBQ3650
      DO 70 IANG2=1,NANG                                                ABBQ3651
      SUMANG=SUMANG+WTANG(IANG2)                                        ABBQ3652
   70 CONTINUE                                                          ABBQ3653
      PANG=2.0*PI/SUMANG                                                ABBQ3654
      DO 80 IANG2=1,NANG                                                ABBQ3655
      WTANG(IANG2)=PANG*WTANG(IANG2)                                    ABBQ3656
   80 CONTINUE                                                          ABBQ3657
      CON3=P(NP)/9.0                                                    ABBQ3658
      DO 200 MN=1,10                                                    ABBQ3659
      DO 90 J=1,NFD                                                     ABBQ3660
      DO 90 L=1,10                                                      ABBQ3661
      CONV(J,L)=0.0                                                     ABBQ3662
   90 CONTINUE                                                          ABBQ3663
      Y=DFLOAT(MN-1)*CON3                                               ABBQ3664
      DO 180 J=1,NFD                                                    ABBQ3665
      CON4=1.0/(2.0*SIGMA(J)*SIGMA(J))                                  ABBQ3666
      CON5=CON4/PI                                                      ABBQ3667
C***********************************************************************ABBQ3668
C   CALCULATE RADIAL GRID ACROSS THE BEAM                               ABBQ3669
C***********************************************************************ABBQ3670
      CON2=4.0*SIGMA(J)/29.0                                            ABBQ3671
      DO 100 L=1,30                                                     ABBQ3672
      AR(L)=DFLOAT(L-1)*CON2                                            ABBQ3673
  100 CONTINUE                                                          ABBQ3674
C***********************************************************************ABBQ3675
C   CALCULATE RADIAL INTEGRATION WEIGHTS                                ABBQ3676
C***********************************************************************ABBQ3677
      WAR(1)=0.0                                                        ABBQ3678
      DO 110 L=1,29                                                     ABBQ3679
      B1=AR(L)                                                          ABBQ3680
      F1=AR(L+1)                                                        ABBQ3681
      B1F=B1*F1                                                         ABBQ3682
      B12=B1*B1                                                         ABBQ3683
      F12=F1*F1                                                         ABBQ3684
      WAR(L)=WAR(L)+(F12+B1F-2.0*B12)/6.0                               ABBQ3685
      WAR(L+1)=(2.0*F12-B1F-B12)/6.0                                    ABBQ3686
  110 CONTINUE                                                          ABBQ3687
      SUMR=0.0                                                          ABBQ3688
      DO 120 L=1,30                                                     ABBQ3689
      SUMR=SUMR+WAR(L)                                                  ABBQ3690
  120 CONTINUE                                                          ABBQ3691
      PAR=0.5*(4.0*SIGMA(J))**2.0                                       ABBQ3692
      PR=PAR/SUMR                                                       ABBQ3693
      DO 130 L=1,30                                                     ABBQ3694
      WAR(L)=PR*WAR(L)                                                  ABBQ3695
  130 CONTINUE                                                          ABBQ3696
      DO 170 M=1,10                                                     ABBQ3697
      X=DFLOAT(M-1)*CON3                                                ABBQ3698
      D=DSQRT(X*X+Y*Y)                                                  ABBQ3699
      PHI=PI/2.0                                                        ABBQ3700
      IF (X.EQ.0.0.AND.Y.EQ.0.0) PHI=0.0                                ABBQ3701
      IF (X.EQ.0.0.OR.Y.EQ.0.0) GOTO 135                                ABBQ3702
      PHI=DATAN(Y/X)                                                    ABBQ3703
  135 CONTINUE                                                          ABBQ3704
      DO 160 IANG=1,NANG                                                ABBQ3705
      CONVR=0.0                                                         ABBQ3706
      DO 150 L=1,30                                                     ABBQ3707
      IF (IGEOM.EQ.1) GO TO 136                                         ABBQ3708
C***********************************************************************ABBQ3709
C   CALCULATE IMPACT PARAMETER ACROSS THE CLOUD FOR PLANAR AND          ABBQ3710
C   SPHERICAL GEOMETRIES                                                ABBQ3711
C***********************************************************************ABBQ3712
      ARG=PI-ANG(IANG)+PHI                                              ABBQ3713
      IF (ANG(IANG).GT.PI) ARG=ANG(IANG)-PI-PHI                         ABBQ3714
      PV=DSQRT(D*D+AR(L)*AR(L)-2.0*D*AR(L)*DCOS(ARG))                   ABBQ3715
      GO TO 137                                                         ABBQ3716
  136 CONTINUE                                                          ABBQ3717
C***********************************************************************ABBQ3718
C   CALCULATE IMPACT PARAMETER ACROSS THE CLOUD FOR CYLINDRICAL         ABBQ3719
C   GEOMETRIES                                                          ABBQ3720
C***********************************************************************ABBQ3721
      PV=DABS(AR(L)*DCOS(PI-ANG(IANG))-D)                               ABBQ3722
      IF (ANG(IANG).LT.PI/2) PV=D+AR(L)*DCOS(ANG(IANG))                 ABBQ3723
  137 CONTINUE                                                          ABBQ3724
      IF (PV.GT.P(NP)) GO TO 150                                        ABBQ3725
      DO 140 LM=1,NP1                                                   ABBQ3726
      IF (PV.GE.P(LM).AND.PV.LT.P(LM+1)) LQ=LM                          ABBQ3727
  140 CONTINUE                                                          ABBQ3728
      IF (PV.LT.P(1)) EINT=EINT1(J,1)                                   ABBQ3729
      IF (PV.LT.P(1)) GOTO 145                                          ABBQ3730
C***********************************************************************ABBQ3731
C   INTERPOLATE EMERGENT INTENSITIES                                    ABBQ3732
C***********************************************************************ABBQ3733
      SLOPE=(EINT1(J,LQ+1)-EINT1(J,LQ))/(P(LQ+1)-P(LQ))                 ABBQ3734
      EINT=EINT1(J,LQ)-SLOPE*P(LQ)+SLOPE*PV                             ABBQ3735
C***********************************************************************ABBQ3736
C   CALCULATE CONVOLUTION INTEGRAL                                      ABBQ3737
C***********************************************************************ABBQ3738
  145 ZX=-((AR(L)*AR(L))*CON4)                                          ABBQ3739
      Z=DEXP(ZX)*CON5                                                   ABBQ3740
      ZY=EINT*WAR(L)*Z                                                  ABBQ3741
      CONVR=CONVR+ZY                                                    ABBQ3742
  150 CONTINUE                                                          ABBQ3743
      CONV(J,M)=CONV(J,M)+WTANG(IANG)*CONVR                             ABBQ3744
  160 CONTINUE                                                          ABBQ3745
  170 CONTINUE                                                          ABBQ3746
  180 CONTINUE                                                          ABBQ3747
C***********************************************************************ABBQ3748
C   WRITE CONVOLUTION INTEGRALS TO OUTPUT FILE                          ABBQ3749
C***********************************************************************ABBQ3750
      WRITE (6,111) Y                                                   ABBQ3751
  111 FORMAT (1H1,25X,'CONVOLVED INTENSITIES, AT GIVEN X AND Y  FROM CLOABBQ3752
     1UD CENTER Y = ',F5.3)                                             ABBQ3753
      WRITE (6,222)                                                     ABBQ3754
  222 FORMAT (1H9,1X,'IF',4X,'MICRON',3X,'SIGMA',3X,'X = 0.0',3X,       ABBQ3755
     1'X=0.111',3X,'X=0.222',3X,'X=0.333',3X,'X=0.444',3X,'X=0.556',    ABBQ3756
     23X,'X=0.667',3X,'X=0.778',3X,'X=0.889',3X,'X=1.000'/)             ABBQ3757
      DO 190 J=1,NFD                                                    ABBQ3758
      WRITE (6,333) J,WLAMDA(J),SIGMA(J),(CONV(J,M),M=1,10)             ABBQ3759
  333 FORMAT (1H9,I3,1P12E10.3)                                         ABBQ3760
  190 CONTINUE                                                          ABBQ3761
      IF (IGEOM.NE.2) RETURN                                            ABBQ3762
  200 CONTINUE                                                          ABBQ3763
      RETURN                                                            ABBQ3764
      END                                                               ABBQ3765
