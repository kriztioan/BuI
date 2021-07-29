C***********************************************************************
C   PROGRAM CSDUST3.F                                                   
C***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF,NDF8M=8*NDF)   
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2,NCEIC=8*NFC2+NDF+1)      
      PARAMETER (NQWT0=7*(ND*NC+ND*(ND+1)/2)+2*ND+NC1+NPMAX)            
      PARAMETER (ND4=4*ND,NDUM=NQWT0+9*NDF+NFC2+2*(ND+NF),NQG=4)        
      PARAMETER (NCQW=2*(ND*NC+ND*(ND+1)/2)+4*NQG)                      
      COMMON DUMMY(NDUM)                                                
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             
      COMMON/MODEL/RMAX,RMIN,RHOCS,RHOBAR,TAUOF,IOF                     
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG)                         
C***********************************************************************
C   OPEN TEMPORARY FILES                                                
C***********************************************************************
      CALL OPENFL                                                       
C***********************************************************************
C   READ NUMBER OF MODELS TO BE RUN                                     
C***********************************************************************
      READ (5,100) NMODEL                                               
  100 FORMAT (7X,I3)                                                    
      DO 20 IM=1,NMODEL                                                 
      IMODEL=IM                                                         
      ITER=0                                                            
C***********************************************************************
C   SET UP SCRATCH FILES, INITIALIZE PROGRAM                            
C   VARIABLES, PRINT INITIAL CONDITIONS                                 
C***********************************************************************
      CALL START                                                        
C***********************************************************************
C   CALCULATE INTENSITY FROM INITIAL GUESS OF DUST TEMPERATURE          
C   AT EACH GRID AND FREQUENCY POINT                                    
C***********************************************************************
      CALL GETAJ                                                        
      DO 10 IT=1,ITMAX                                                  
      ITER=IT                                                           
C***********************************************************************
C   COMPUTE CORRECTIONS AND UPDATE MEAN INTENSITY (AJ)                  
C   AND DUST TEMPERATURE (TD)                                           
C***********************************************************************
      CALL GETTD                                                        
C***********************************************************************
C    UPDATE TEMPERATURE DISTRIBUTION AND CALCULATE EDDINGTON FLUX       
C    AND EMISSIVITY AT EACH GRID POINT                                  
C***********************************************************************
      CALL UPDATE                                                       
C***********************************************************************
C   PRINT ITERATION RESULTS, CALCULATE OTHER                            
C   MODEL CHARACTERISTICS, AND PRINT OUT FINAL                          
C   MODEL RESULTS                                                       
C***********************************************************************
      CALL OUTPUT                                                       
      IF (ITCON.EQ.1) GO TO 20                                          
   10 CONTINUE                                                          
   20 CONTINUE                                                          
      STOP                                                              
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE EXPOFF CALLS THE RIDGE SUBROUTINE SIGNAL TO              
C   TURN OFF THE OVERFLOW AND UNDERFLOW OF NUMBERS, REPLACING           
C   THEM WITH THE LARGEST AND SMALLEST ALLOWABLE NUMBERS ON THE         
C   MACHINE                                                             
C***********************************************************************
      SUBROUTINE EXPOFF                                                 
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
C      CALL SIGNAL(8,QQ,1)                                               
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE BCD CALCULATES THE CONSTANTS IN THE BOUNDARY             
C   EQUATIONS OF THE COMBINED MOMENT EQUATION                           
C   B1,B2 ---  ALPHA AND BETA AT CLOUD SURFACE                          
C   C1,C2 ---  ALPHA AND BETA AT CLOUD CENTER                           
C***********************************************************************
      SUBROUTINE BCD (C1,C2,B1,B2)                                      
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      DIMENSION C1(1),C2(1),B1(1),B2(1)                                 
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      CON=-2.0*AJ0I                                                     
C***********************************************************************
C    COMPUTE BOUNDARY CONSTANTS AT OUTER BOUNDARY, R0                   
C***********************************************************************
      DO 10 J=1,NFD                                                     
      B1(J)=-FBD(J)                                                     
      B2(J)=-CON*FLUXSD(J)                                              
   10 CONTINUE                                                          
C***********************************************************************
C   INNER BOUNDARY AT RC.NE.0? IC=1=> NO   IC=0 => YES                  
C***********************************************************************
      IF (IC.EQ.0) GO TO 22                                             
C***********************************************************************
C   USE TOTAL NET FLUX AT INNER BOUNDARY RC?  NH=0 => NO  NH=1 => YES   
C***********************************************************************
      IF (NH.EQ.1) GO TO 33                                             
C***********************************************************************
C   COMPUTE BOUNDARY CONSTANTS AT INNER BOUNDARY RC FOR IC=1, & NH=0    
C***********************************************************************
   15 DO 20 J=1,NFD                                                     
      C1(J)=FCD(J)                                                      
      C2(J)=CON*FLUXCD(J)                                               
   20 CONTINUE                                                          
      RETURN                                                            
   22 CONTINUE                                                          
C***********************************************************************
C   COMPUTE BOUNDARY CONSTANTS AT RC FOR IC=0                           
C***********************************************************************
      DO 30 J=1,NFD                                                     
      C1(J)=0.0                                                         
      C2(J)=0.0                                                         
   30 CONTINUE                                                          
      RETURN                                                            
   33 CONTINUE                                                          
C***********************************************************************
C   COMPUTE BOUNDARY CONSTANTS AT RC FOR IC=1,  NH=1                    
C***********************************************************************
      DO 40 J=1,NFD                                                     
      C1(J)=0.0                                                         
      C2(J)=-AJ0I*FLUXCD(J)                                             
   40 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE BRANRW PROVIDES READ/WRITE ACCESS TO                     
C   DATA FILES STORED ON EXTERNAL DISK                                  
C   ARRAY --- CORE DATA STORAGE ADDRESS BEING READ OR WRITTEN           
C   IFILE --- TEMPORARY DISK FILE BEING READ OR WRITTEN                 
C   NWORD --- DIMENSION OF ARRAY                                        
C***********************************************************************
      SUBROUTINE BRANRW (IFILE,ARRAY,NWORD)                             
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      DIMENSION ARRAY(NWORD)                                            
      ENTRY BRANRD (IFILE,ARRAY,NWORD)                                  
      READ (IFILE) ARRAY                                                
      REWIND IFILE                                                      
      RETURN                                                            
      ENTRY BRANWT (IFILE,ARRAY,NWORD)                                  
      WRITE (IFILE) ARRAY                                               
      REWIND IFILE                                                      
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE GETBJ CALCULATES THE PLANCK FUNCTION, ITS TEMPERATURE    
C   DERIVATIVE, AND THEIR RATIO FOR A GIVEN TEMPERATURE AND FREQUENCY   
C   F    --- FREQUENCY                                                  
C   T    --- TEMPERATURE                                                
C   MODE --- SUBROUTINE CONTROL PARAMETER                               
C   BJ0  --- COMPUTED VALUE OF PLANCK RADIATION FUNCTION                
C   BJ1  --- TEMPERATURE DERIVATIVE OF BJ0 IF MODE.NE.3,                
C           TEMPERATURE DERIVATIVE OF BJ0 DIVIDED BY BJ0 IF MODE=3      
C***********************************************************************
      SUBROUTINE GETBJ (F,T,MODE,BJ0,BJ1)                               
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            
      IF (T.GT.0.0) GOTO 5                                              
      BJ0=0.0                                                           
      BJ1=0.0                                                           
      WRITE(6,111)                                                      
  111 FORMAT(40X,'TEMPERATURE IN SUBROUTINE GETBJ.LE.ZERO')             
      RETURN                                                            
    5 CONTINUE                                                          
      C1=1.0/CTH                                                        
      TI=1.0/T                                                          
      X1=HK*F*TI                                                        
C********************************************************************** 
C   EXP(709) IS EQUAL TO THE LARGEST NUMBER AVAILABLE ON THE            
C   RIDGE 32                                                            
C********************************************************************** 
      IF (X1.LT.7.09E2) GO TO 20                                        
      BJ0=C1*F*F*F*DEXP(-X1)                                            
C***********************************************************************
C   IF MODE < 1  ---  RETURNS B(T)                                      
C   IF MODE = 3  ---  RETURNS B, (DB/DT)/B                              
C   ELSE         ---  RETURNS B, DB/DT                                  
C***********************************************************************
      IF (MODE.GE.1) BJ1=BJ0*X1*TI                                      
      IF (MODE.EQ.3) BJ1=X1*TI                                          
      RETURN                                                            
   20 CONTINUE                                                          
      X2=DEXP(X1)                                                       
      IF (X2.GT.1.0) GO TO 40                                           
      BJ0=(2.0*F*F*BOLTZ*T)/(CV*CV)*AJ0I                                
      IF (MODE.GE.1) BJ1=BJ0*TI                                         
      IF (MODE.EQ.3) BJ1=TI                                             
      RETURN                                                            
   40 CONTINUE                                                          
      BJ0=C1*F*F*F*(1.0/(X2-1.0))                                       
      IF (MODE.GE.1) BJ1=BJ0*(X1*TI*X2/(X2-1.0))                        
      IF (MODE.EQ.3) BJ1=(X1*TI*X2/(X2-1.0))                            
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE GETAU CALCULATES THE OPTICAL DEPTH (USING TRAPEZOIDAL    
C   RULE) AS A FUNCTION OF POSITION FOR A GIVEN OPACITY                 
C   X --- R                                                             
C   Y --- OPACITY                                                       
C   Z --- OPTICAL DEPTH                                                 
C   N --- NUMBER OF INTEGRATION WEIGHTS                                 
C***********************************************************************
      SUBROUTINE GETAU (X,Y,Z,N)                                        
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NDP1=ND+1)                                             
      DIMENSION X(1),Y(1),Z(1)                                          
      COMMON/SPARE/P(NDP1),V(NDP1),U(NDP1)                              
C***********************************************************************
C   CALCULATE OPTICAL DEPTH FROM CLOUD SURFACE                          
C***********************************************************************
      N1=N-1                                                            
      DO 10 L=1,N1                                                      
      P(L)=0.5*(X(L+1)-X(L))*(Y(L+1)+Y(L))                              
   10 CONTINUE                                                          
      Z(N)=0.0                                                          
      DO 20 L=1,N1                                                      
      J=N-L                                                             
      Z(J)=Z(J+1)+P(J)                                                  
   20 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE OPENFL OPENS THE SCRATCH STORAGE FILES ON THE            
C   RIDGE 32 HARD DISK                                                  
C***********************************************************************
      SUBROUTINE OPENFL                                                 
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
C***********************************************************************
C   TURN OFF OVERFLOW/UNDERFLOW FEATURES                                
C***********************************************************************
      CALL EXPOFF                                                       
C***********************************************************************
C   ASSIGN LOGICAL UNIT NUMBERS TO DISK FILES                           
C***********************************************************************
      NGPD=8                                                            
      NJCBD=9                                                           
      NBTAD=10                                                          
      NCGD=11                                                           
      NEFQW=12                                                          
      NJFPD=13                                                          
      NCED=14                                                           
      NAHD=15                                                           
      NEID=16                                                           
      NEICD=17                                                          
      NQASD=18                                                          
      NGBD=19                                                           
      NCYQW=20                                                          
C***********************************************************************
C   DETERMINE FORMAT OF OUTPUT DATA                                     
C***********************************************************************
      OPEN (UNIT=6,FORM='FORMATTED')                                        
C***********************************************************************
C   CREATE TEMPORARY DISK FILES                                         
C***********************************************************************
      OPEN(UNIT=NGPD,FORM='UNFORMATTED',STATUS='SCRATCH')               
      OPEN(UNIT=NJCBD,FORM='UNFORMATTED',STATUS='SCRATCH')              
      OPEN (UNIT=NBTAD,FORM='UNFORMATTED',STATUS='SCRATCH')             
      OPEN (UNIT=NCGD,FORM='UNFORMATTED',STATUS='SCRATCH')              
      OPEN (UNIT=NEFQW,FORM='UNFORMATTED',STATUS='SCRATCH')             
      OPEN (UNIT=NJFPD,FORM='UNFORMATTED',STATUS='SCRATCH')             
      OPEN (UNIT=NCED,FORM='UNFORMATTED',STATUS='SCRATCH')              
      OPEN (UNIT=NAHD,FORM='UNFORMATTED',STATUS='SCRATCH')              
      OPEN (UNIT=NEID,FORM='UNFORMATTED',STATUS='SCRATCH')              
      OPEN (UNIT=NEICD,FORM='UNFORMATTED',STATUS='SCRATCH')             
      OPEN (UNIT=NQASD,FORM='UNFORMATTED',STATUS='SCRATCH')             
      OPEN (UNIT=NGBD,FORM='UNFORMATTED',STATUS='SCRATCH')              
      IF (IGEOM.NE.1) RETURN                                            
      OPEN (UNIT=NCYQW,FORM='UNFORMATTED',STATUS='SCRATCH')             
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE START SETS UP FILES, READS INPUT DATA,                   
C   AND INITIALIZES VARIABLES. COMPUTED PARAMETERS AND                  
C   INITIAL CONDITIONS ARE PRINTED IN TABULAR FORMAT                    
C***********************************************************************
      SUBROUTINE START                                                  
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      CHARACTER*8 XYES,XNO,YEDFTR,YOUT,YC,YSCA,YB,YH,                   
     1            YGEOM,XPLAN,XCYL,XSPHER,YCORE                         
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          
      PARAMETER (ND5=5*ND,NF5=5*NF,NX1=6*NDF+ND5+1,NX2=NX1+ND5,         
     1           NX3=NX2+NF5,NX4=NX3+NF5,NX5=NX4+NF5)                   
      PARAMETER (NDP1=ND+1,ND3P1=3*(ND+1))                              
      PARAMETER (NDF7=7*NDF,NDX=3*(ND+1))                               
      COMMON DUMMY(NDF7),TAU0(NF),TOF(ND),CHIF(ND),                     
     1       TAUX(ND)                                                   
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),             
     1          ETA(NF,ND),AH(NF,ND),CGBAR(NF,ND),QABS(NF,ND),          
     2          QSCA(NF,ND),CHICG(NF,ND),BTDAV(NF,ND),BTDAVI(NF,ND)     
      DIMENSION ABUNDI(5,ND),TDI(5,ND),QABSI(NF,5),QSCAI(NF,5),         
     1          GBARI(NF,5),ALBEDI(NF,5),GRAINC(5),AC(5),GRAINM(5),     
     2          AM(5)                                                   
      EQUIVALENCE (DUMMY(1),AJ(1,1),AH(1,1)),                           
     1            (DUMMY(NDF1P1),FK(1,1),QABS(1,1)),                    
     2            (DUMMY(NDF2P1),ZETA(1,1),QSCA(1,1)),                  
     3            (DUMMY(NDF3P1),CHI(1,1)),                             
     4            (DUMMY(NDF4P1),ETA(1,1)),                             
     5            (DUMMY(NDF5P1),BTDAV(1,1),CGBAR(1,1)),                
     6            (DUMMY(NDF6P1),BTDAVI(1,1),CHICG(1,1)),               
     7            (DUMMY(NDF6P1),ABUNDI(1,1)),(DUMMY(NX1),TDI(1,1)),    
     8            (DUMMY(NX2),QABSI(1,1)),(DUMMY(NX3),QSCAI(1,1)),      
     9            (DUMMY(NX4),GBARI(1,1)),(DUMMY(NX5),ALBEDI(1,1))      
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/MODEL/RMAX,RMIN,RHOCS,RHOBAR,TAUOF,IOF                     
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      IF (IMODEL.GT.1) GO TO 55                                         
      XYES='YES     '                                                   
      XNO='NO      '                                                    
      XPLAN='PLANE   '                                                  
      XCYL='CYLINDER'                                                   
      XSPHER='SPHERE  '                                                 
      PI=3.141592653589793E0                                            
      CV=2.997924562E10                                                 
      PLANK=6.62565E-27                                                 
      BOLTZ=1.380542E-16                                                
      PARSEC=3.0856E18                                                  
      HK=PLANK/BOLTZ                                                    
      NR=ND                                                             
      N1=NR-1                                                           
      NP=NPMAX                                                          
      NB1D=NDF                                                          
      NB2D=2*NDF                                                        
      NB3D=3*NDF                                                        
      NGP=5*(3*NF+2*ND)                                                 
C********************************************************************** 
C   THE CONTROL PARAMETER IGEOM DETERMINES THE GEOMETRY OF THE DUST     
C   CLOUD MODELED. 0 = PLANAR, 1 = CYLINDRICAL, 2 = SPHERICAL.          
C   IEMRG DETERMINES WHETHER OUTPUT INFORMATION WILL BE PRINTED FOR     
C   ONE OR ALL THETA DIRECTIONS IN THE CYLINDRICAL CASE.                
C********************************************************************** 
      READ (5,100) IGEOM,IEMRG                                          
      IF (IGEOM.EQ.0) NP=20                                             
      READ (5,100) IEDFTR,IOUT,IC,NH,ISCA,IB,IDIST                      
C***********************************************************************
C   BEGIN READING INPUT DATA SET                                        
C***********************************************************************
  100 FORMAT (7(7X,I1))                                                 
      READ (5,200) AJ0,EPS,ITMAX                                        
  200 FORMAT (1P2E10.3,2X,I2)                                           
      READ (5,300) (R(L),L=1,NR)                                        
  300 FORMAT (1P7E10.3,10X)                                             
      IF (IC.EQ.0) NP=NR                                                
      CTH=0.5*CV*CV*AJ0/PLANK                                           
      AJ0I=1.0/AJ0                                                      
      READ (5,400) NFD,IOF,(WLAMDA(J),J=1,NFD)                          
  400 FORMAT (2I5/(1P7E10.3,10X))                                       
      IF (IB.NE.0) READ (5,300) (FLUXSD(J),J=1,NFD)                     
      READ (5,500) IMIX                                                 
  500 FORMAT (I5,5X,1P2E10.3)                                           
      DO 20 IG=1,IMIX                                                   
      READ (5,600) GRAINC(IG),AC(IG),GRAINM(IG),AM(IG),IREADA           
  600 FORMAT (A8,2X,1PE10.3,A8,2X,1PE10.3,10X,I5)                       
      READ (5,700) (QABSI(J,IG),QSCAI(J,IG),GBARI(J,IG),J=1,NFD)        
  700 FORMAT (1P3E10.3,50X)                                             
      IF (IREADA.EQ.0) READ (5,300) ABUNDI(IG,1)                        
      IF (IREADA.NE.0) READ (5,300) (ABUNDI(IG,L),L=1,NR)               
      IF (IREADA.NE.0) GO TO 20                                         
      DO 10 L=1,NR                                                      
      ABUNDI(IG,L)=ABUNDI(IG,1)                                         
   10 CONTINUE                                                          
   20 CONTINUE                                                          
      READ (5,500) IREADT,TDS,TDC                                       
C***********************************************************************
C   COMPUTE FREQUENCY GRID FROM WAVELENGTH GRID                         
C***********************************************************************
      CON=1.0E4*CV                                                      
      FREQD(1)=CON/WLAMDA(1)                                            
      WFD(1)=0.0                                                        
      DO 30 J=1,NFD                                                     
      IF (J.EQ.NFD) GO TO 22                                            
      FREQD(J+1)=CON/WLAMDA(J+1)                                        
      DW=0.5*(FREQD(J+1)-FREQD(J))                                      
      WFD(J)=WFD(J)+DW                                                  
      WFD(J+1)=DW                                                       
   22 CONTINUE                                                          
C***********************************************************************
C   INITIALIZE BOUNDARY FLUXES; CONVERT INPUT FLUXSD FROM INTENSITY     
C   TO FLUX                                                             
C***********************************************************************
      FLUXSD(J)=0.25*FLUXSD(J)                                          
      IF (IB.EQ.0) FLUXSD(J)=0.0                                        
      IF (IC.EQ.0) FLUXCD(J)=0.0                                        
   30 CONTINUE                                                          
C***********************************************************************
C   INITIALIZE TEMPERATURE PROFILE                                      
C***********************************************************************
      IF (IREADT.NE.0) READ (5,300) (TD(L),L=1,NR)                      
      XX=(TDC-TDS)*DSQRT(R(1))                                          
      DO 40 IG=1,IMIX                                                   
      DO 40 L=1,NR                                                      
      IF (IREADT.NE.0) GO TO 35                                         
      IF (IG.EQ.1.AND.IC.NE.0) TD(L)=TDS+XX/DSQRT(R(L))                 
      IF (IG.EQ.1.AND.IC.EQ.0) TD(L)=TDC+(TDS-TDC)*R(L)                 
   35 CONTINUE                                                          
      TDI(IG,L)=TD(L)                                                   
   40 CONTINUE                                                          
C***********************************************************************
C   WRITE GRAIN PROPERTIES (ABUNDI,TDI, QABSI, QSCAI, GBARI)            
C   ONTO DISK FILE                                                      
C***********************************************************************
      CALL BRANWT (NGPD,DUMMY(NDF6P1),NGP)                              
      DO 50 J=1,NFD                                                     
      DO 50 L=1,NR                                                      
      CALL GETBJ (FREQD(J),TD(L),2,BJ0,BJ1)                             
      BTDAV(J,L)=BJ0                                                    
      BTDAVI(J,L)=BJ1                                                   
   50 CONTINUE                                                          
C***********************************************************************
C   WRITE BTDAV, BTDAVI ONTO DISK FILE                                  
C***********************************************************************
      CALL BRANWT (NBTAD,DUMMY(NDF5P1),NB2D)                            
C***********************************************************************
C   INITIALIZE FK, ZETA, AH, AJ, AND BOUNDARY FACTORS FBD,FCD           
C***********************************************************************
      FKCON=1.0/3.0                                                     
      FBCON=0.5                                                         
      FCCON=0.0                                                         
      IF (IC.NE.0) FCCON=0.5                                            
      DO 51 J=1,NFD                                                     
      FBD(J)=FBCON                                                      
      FCD(J)=FCCON                                                      
      DO 51 L=1,NR                                                      
      FK(J,L)=FKCON                                                     
      ZETA(J,L)=1.0                                                     
      AJ(J,L)=0.0                                                       
   51 CONTINUE                                                          
C***********************************************************************
C   WRITE FK, ZETA ESTIMATES TO DISK FILE                               
C***********************************************************************
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 
C***********************************************************************
C   INITIATE DISK FILE FOR AH                                           
C***********************************************************************
      CALL BRANWT (NAHD,DUMMY(1),NB1D)                                  
C***********************************************************************
C   INITIATE DISK FILE FOR AJ                                           
C***********************************************************************
      CALL BRANWT (NEICD,DUMMY(1),NFC2)                                 
   55 CONTINUE                                                          
C***********************************************************************
C   READ ABUNDI, TDI, QABSI, QSCAI, GBARI FROM DISK FILE                
C***********************************************************************
      CALL BRANRD (NGPD,DUMMY(NDF6P1),NGP)                              
C***********************************************************************
C   INITIALIZE CONVERGENCE PARAMETERS AND ZERO OUT CORRECTION ARRAYS    
C***********************************************************************
      ITCON=0                                                           
      ITCONT=0                                                          
      ITCONJ=0                                                          
      DO 70 L=1,NR                                                      
      DTD(L)=0.0                                                        
      DJD(L)=0.0                                                        
      DHD(L)=0.0                                                        
   70 CONTINUE                                                          
C***********************************************************************
C   READ DUST SHELL AND HEAT SOURCE PARAMETERS                          
C***********************************************************************
      READ (5,300) RMAX,TAUOF,RHOCS,TSTAR,TLUM                          
      RMIN=R(1)*RMAX                                                    
      R0=RMAX*PARSEC                                                    
      ETA0=R0*AJ0I                                                      
      R0I=1.0/R0                                                        
      ETA0I=AJ0*R0I                                                     
      CONQAS=QABSI(IOF,1)+QSCAI(IOF,1)                                  
      CON=R0*CONQAS                                                     
      IF (IDIST.EQ.0) GO TO 95                                          
C***********************************************************************
C   DETERMINE GAUSSIAN DENSITY DISTRIBUTION  (IDIST=1)                  
C***********************************************************************
      CS=DLOG(RHOCS)                                                    
      CX=DSQRT(CS)                                                      
      CXR=CX*R(1)                                                       
      IF (RHOCS.EQ.1.0) RHOBAR=TAUOF/(CON*(1.0-R(1)))                   
      IF (RHOCS.GT.1.0) RHOBAR=2.0*CX*TAUOF/(CON*DSQRT(PI)*             
     1                            (DERF(CX)-DERF(CXR)))                 
      DO 90 L=1,NR                                                      
      RHOD(L)=RHOBAR*DEXP(-CS*R(L)*R(L))                                
      SUMQAS=0.0                                                        
      DO 80 IG=1,IMIX                                                   
      SUMQAS=SUMQAS+ABUNDI(IG,L)*(QABSI(IOF,IG)+QSCAI(IOF,IG))          
   80 CONTINUE                                                          
      CHIF(L)=R0*RHOD(L)*SUMQAS                                         
   90 CONTINUE                                                          
      GO TO 135                                                         
C***********************************************************************
C   DETERMINE MODIFIED POWER LAW DENSITY DISTRIBUTION (IDIST=0)         
C   CALCULATE THE MATCHING POINT USING THE SECANT METHOD                
C***********************************************************************
   95 CONTINUE                                                          
      POW=RHOCS                                                         
      POW1=-POW+1.0                                                     
      IF (POW.NE.1.0) POWI=1.0/POW1                                     
      RIN=R(1)                                                          
      IF (IC.EQ.0) RIN=R(2)                                             
      RPOW=(1.0-RIN**POW1)                                              
      LNR=-DLOG(RIN)                                                    
C***********************************************************************
C   MATCHING POINT OF INNER GAUSSIAN AND OUTER POWER LAW DENSITY        
C   PROFILES OCCURS AT 0.8*TAUOF FROM THE SURFACE                       
C***********************************************************************
      ALPHA=.99                                                          
      TM=ALPHA*TAUOF                                                    
      TMATCH=(1.0-ALPHA)*TAUOF                                          
      IF (POW.EQ.1.0) THEN                                              
        RHOSRF=TAUOF/(CON*LNR)                                          
        RMATCH=DEXP(-TM/(CON*RHOSRF))                                   
      ELSE                                                              
        RHOSRF=TAUOF*POW1/(CON*RPOW)                                    
        RMATCH=(1.0-TM*POW1/(CON*RHOSRF))**POWI                         
      ENDIF                                                             
C***********************************************************************
C   CALCULATE THE DENSITY DISTRIBUTION USING THE CONSTRAINT ON TAU      
C***********************************************************************
      RMPOW=RMATCH**(-POW)                                              
      C1=2*TMATCH/(CON*PI**.5)                                          
      C2=POW*DLOG(RMATCH)-DLOG(RHOSRF)                                  
      YM=RMATCH-R(1)                                                    
      M=100                                                             
      XSTP=1.0D-16                                                      
C***********************************************************************
C    SOLVE FOR GAUSSIAN WIDTH (SIGMA) USING THE SECANT METHOD           
C    INITIAL GUESSES FOR XA AND XB MAY HAVE TO BE ALTERED               
C    TO GUARANTEE CONVERGENCE OF THE SECANT METHOD                      
C***********************************************************************
      XA=9.0                                                            
      XB=10.0                                                           
      DO 110 I=1,M                                                      
      CA=DEXP(((YM/XA)**2.0)-C2)                                        
      CB=DEXP(((YM/XB)**2.0)-C2)                                        
      FNA=XA-C1/(CA*ERF(YM*XA))                                         
      FNB=XB-C1/(CB*ERF(YM*XB))                                         
      VALUE=(XA*FNB-XB*FNA)/(FNB-FNA)                                   
      XA=XB                                                             
      XB=VALUE                                                          
      IF (XB.LE.0.0) WRITE (6,101)                                      
  101 FORMAT (1H1,15X,'GAUSSIAN WILL NOT MATCH TO POWER LAW ON THIS RADI
     1AL GRID')                                                         
      IF (XB.LE.0.0) STOP                                               
      IF (ABS(XB-XA).LE.XSTP) GO TO 115                                 
  110 CONTINUE                                                          
  115 CONTINUE                                                          
      RHOBAR=DEXP(((YM/XB)**2.0)-C2)                                    
      SGMA2=XB**2.0                                                     
C***********************************************************************
C   CALCULATE THE DUST DENSITY                                          
C***********************************************************************
      DO 130 L=1,NR                                                     
      IF (R(L).LE.RMATCH) THEN                                          
        RHOD(L)=RHOBAR*EXP(-(R(L)-R(1))**2/SGMA2)                       
      ELSE                                                              
        RHOD(L)=RHOSRF*(R(L)**(-POW))                                   
      ENDIF                                                             
      SUMQAS=0.0                                                        
      DO 120 IG=1,IMIX                                                  
      SUMQAS=SUMQAS+ABUNDI(IG,L)*(QABSI(IOF,IG)+QSCAI(IOF,IG))          
  120 CONTINUE                                                          
      CHIF(L)=R0*RHOD(L)*SUMQAS                                         
  130 CONTINUE                                                          
  135 CALL GETAU (R,CHIF,TOF,NR)                                        
      RATIO=TAUOF/TOF(1)                                                
      DO 140 L=1,NR                                                     
      RHOD(L)=RATIO*RHOD(L)                                             
      TOF(L)=RATIO*TOF(L)                                               
  140 CONTINUE                                                          
      RHOBAR=RATIO*RHOBAR                                               
      WOF=WLAMDA(IOF)                                                   
C***********************************************************************
C   WRITE MODEL PARAMETERS TO OUTPUT                                    
C***********************************************************************
      IF (IDIST.EQ.0) THEN                                              
        WRITE (6,111) RMAX,RMIN,POW,RHOBAR,WOF,TAUOF,NR,NP,             
     1                NFD,EPS,AJ0,ITMAX                                 
      ELSE                                                              
        WRITE (6,112) RMAX,RMIN,RHOCS,RHOBAR,WOF,TAUOF,NR,NP,           
     1                NFD,EPS,AJ0,ITMAX                                 
      ENDIF                                                             
  111 FORMAT(1H1,45X,'INPUT PARAMETERS FOR DUST CLOUD MODEL'/           
     1//20X,'MODEL PARAMETERS'//60X,'OUTER CLOUD RADIUS, RMAX ='        
     2,1PE10.3,' PARSECS'/60X,'INNER CLOUD RADIUS, RMIN = ',            
     31PE10.3,' PARSECS'/37X,'EXPONENT OF POWER LAW DENSITY DISTRIBUTION
     4, POW = ',1PE10.3/44X,'NUMBER DENSITY OF DUST AT CENTER, RHOBAR = 
     5', 1PE10.3//38X,'TOTAL OPTICAL DEPTH (',1PE10.3,' MICRON), TAUOF 
     6= ',1PE10.3//59X,'NUMBER OF GRID POINTS, NR = ',I3/53X,'NUMBER OF 
     7IMPACT PARAMETERS, NP = ',I3/53X,'NUMBER OF FREQUENCY POINTS, NFD 
     8=',I3///58X,'CONVERGENCE PARAMETER, EPS = ',1PE10.3/57X,          
     9'NORMALIZATION CONSTANT, AJ0 = ',1PE10.3/52X,'MAXIMUM NO. OF ITERA
     1TIONS, ITMAX = ',I3/)                                             
  112 FORMAT(1H1,45X,'INPUT PARAMETERS FOR DUST CLOUD MODEL'///20X,'MODE
     1L PARAMETERS'//60X,'OUTER CLOUD RADIUS, RMAX =',1PE10.3,' PARSECS'
     2/60X,'INNER CLOUD RADIUS, RMIN = ',1PE10.3,' PARSECS'/40X,'RATIO O
     3F CENTRAL TO SURFACE DENSITIES, RHOCS = ',1PE10.3/44X,'NUMBER DENS
     4ITY OF DUST AT CENTER, RHOBAR = ',1PE10.3//38X,'TOTAL OPTICAL DEPT
     5H (',1PE10.3,' MICRON), TAUOF = ',1PE10.3//59X,'NUMBER OF GRID POI
     6NTS, NR = ',I3/53X,'NUMBER OF IMPACT PARAMETERS, NP = ',I3/53X,'NU
     7MBER OF FREQUENCY POINTS, NFD =',I3///58X,'CONVERGENCE PARAMETER, 
     8EPS = ',1PE10.3/57X,'NORMALIZATION CONSTANT, AJ0 = ',1PE10.3/52X,'
     9MAXIMUM NO. OF ITERATIONS, ITMAX = ',I3/)                         
      IF (IGEOM.EQ.0) YGEOM=XPLAN                                       
      IF (IGEOM.EQ.1) YGEOM=XCYL                                        
      IF (IGEOM.EQ.2) YGEOM=XSPHER                                      
      IF (IEDFTR.EQ.0) YEDFTR=XYES                                      
      IF (IEDFTR.EQ.1) YEDFTR=XNO                                       
      IF (IOUT.EQ.0) YOUT=XNO                                           
      IF (IOUT.EQ.1) YOUT=XYES                                          
      IF (IC.EQ.0) YC=XNO                                               
      IF (IC.EQ.1) YC=XYES                                              
      IF (NH.EQ.0) YH=XNO                                               
      IF (NH.EQ.1) YH=XYES                                              
      IF (ISCA.EQ.0) YSCA=XNO                                           
      IF (ISCA.EQ.1) YSCA=XYES                                          
      IF (IB.EQ.0) YB=XNO                                               
      IF (IB.EQ.1) YB=XYES                                              
      WRITE (6,222) YEDFTR,IEDFTR,YOUT,IOUT,YC,IC,YH,NH,                
     1              YSCA,ISCA,YB,IB,YGEOM,IGEOM                         
  222 FORMAT (///52X,                                                   
     1 'EDDINGTON APPROXIMATION --- ',A8,' (IEDFTR = ',I1,')'/47X,      
     2 'DETAILED PRINTOUT OF RESULTS --- ',A8,' (IOUT   = ',I1,')'/     
     3 63X,'CENTRAL CORE --- ',A8,' (IC     = ',I1,')'/                 
     4 39X,'USE TOTAL NET FLUX AT INNER BOUNDARY --- ',A8,' (NH     = ',
     5 I1,')'/41X,'FIRST-ORDER ANISOTROPIC SCATTERING --- ',A8,         
     6 ' (ISCA   = ',I1,')'/44X,'INCIDENT FLUX AT OUTER BOUNDARY --- ', 
     7 A8,' (IB     = ',I1,')'/61X,'CLOUD GEOMETRY --- ',A8,            
     8 ' (IGEOM  = ',I1,')'/)                                           
      WRITE (6,333) (IG,GRAINC(IG),AC(IG),GRAINM(IG),AM(IG),IG=1,IMIX)  
  333 FORMAT(//20X,'DUST COMPONENT',I2//49X,'RADIUS OF INNER CORE (',A8,
     1 '), AC = ',1PE10.3,' MICRON'/47X,'RADIUS OF OUTER MANTLE (',A8 , 
     2'), AM = ',1PE10.3,' MICRON')                                     
      IF (IC.EQ.0) GO TO 175                                            
C***********************************************************************
C   INITIALIZE PARAMETERS FOR CENTRAL HEAT SOURCE                       
C   IF TSTAR.GT.0 HEAT SOURCE IS A BLACKBODY OF TEMPERATURE TSTAR       
C   IF TSTAR.LE.0 HEAT SOURCE IS A POWER LAW OF INDEX TSTAR             
C***********************************************************************
      RMIN=R(1)*R0                                                      
      IF (TSTAR.LT.0.0) GO TO 165                                       
      CON=0.25*AJ0                                                      
      SUMF=0.0                                                          
      DO 150 J=1,NFD                                                    
      CALL GETBJ (FREQD(J),TSTAR,0,BJ0,BJ1)                             
      FLUXCD(J)=CON*BJ0                                                 
      SUMF=SUMF+FLUXCD(J)*WFD(J)                                        
  150 CONTINUE                                                          
      XX=(TLUM*3.9E33)/(SUMF*16.0*PI*PI)                                
      RSTAR=DSQRT(XX)                                                   
      TSTAR1=(SUMF/4.5117E-06)**0.25                                    
      PNORM=(RSTAR*RSTAR)/(RMIN*RMIN)                                   
      RSTAR=RSTAR/6.96E10                                               
      DO 160 J=1,NFD                                                    
      FLUXCD(J)=PNORM*FLUXCD(J)                                         
  160 CONTINUE                                                          
      SUMF1=4.5117E-06*TSTAR**4                                         
      XRATIO=SUMF/SUMF1                                                 
      TLUM1=TLUM/XRATIO                                                 
      XRATIO=100.0*XRATIO                                               
C***********************************************************************
C   WRITE STELLAR CHARACTERISTICS TO OUTPUT                             
C***********************************************************************
      WRITE (6,444) TSTAR,TSTAR1,RSTAR,TLUM,TLUM1,SUMF,SUMF1,XRATIO     
  444 FORMAT (///20X,'CENTRAL HEAT SOURCE'//40X,'SPECIFIED TEMPERATURE O
     1F CENTRAL STAR, TSTAR = ',1PE10.3,' DEGREES'/39X,'EFFECTIVE TEMPER
     2ATURE OF CENTRAL STAR, TSTAR1 = ',1PE10.3,' DEGREES'/55X,'RADIUS O
     3F CENTRAL STAR, RSTAR = ',1PE10.3,' SOLAR RADII'/42X,'LUMINOSITY O
     4F CENTRAL STAR .GE. 912A, TLUM = ',1PE10.3,' SOLAR UNITS'/45X,    
     5 'TOTAL LUMINOSITY OF CENTRAL STAR, TLUM1 = ',1PE10.3,' SOLAR UNIT
     6S'/49X,'INTEGRATED NET FLUX .GE. 912A, SUMF = ',1PE10.3,' ERGS/(SE
     7C*HZ*CM**2)'/52X,'TOTAL INTEGRATED NET FLUX, SUMF1 = ',1PE10.3,   
     8' ERGS/(SEC*HZ*CM**2)'/35X,'FRACTION OF ENERGY FOR HEATING THE GRA
     9INS, XRATIO = ',1PE10.3,' PERCENT')                               
      GO TO 175                                                         
  165 CONTINUE                                                          
C***********************************************************************
C   CENTRAL HEAT SOURCE WITH POWER LAW ENERGY DISTRIBUTION              
C***********************************************************************
      IF (TSTAR.EQ.(-1.0)) CON=1.0/DLOG(FREQD(NFD)/FREQD(1))            
      XX=1.0+TSTAR                                                      
      IF (TSTAR.NE.(-1.0)) CON=XX/(FREQD(NFD)**XX-FREQD(1)**XX)         
      PNORM=(TLUM*3.9E33*CON)/(16.0*PI*RMIN*RMIN)                       
      DO 170 J=1,NFD                                                    
      FLUXCD(J)=PNORM*FREQD(J)**TSTAR                                   
  170 CONTINUE                                                          
C***********************************************************************
C   WRITE CHARACTERISTICS OF POWER LAW SOURCE TO OUTPUT                 
C***********************************************************************
      WRITE (6,555) TSTAR,TLUM                                          
  555 FORMAT (///20X,'CENTRAL HEAT SOURCE'//51X,'SPECTRAL INDEX (POWER L
     1AW), TSTAR = ',1PE10.3/62X,'TOTAL LUMINOSITY, TLUM = ',1PE10.3,   
     2 ' SOLAR UNITS')                                                  
  175 CONTINUE                                                          
      DO 180 IG=1,IMIX                                                  
      DO 180 J=1,NFD                                                    
      ALBEDI(J,IG)=QSCAI(J,IG)/(QABSI(J,IG)+QSCAI(J,IG))                
  180 CONTINUE                                                          
      DO 190 K1=1,IMIX,2                                                
      K2=MIN0(IMIX,K1+2)                                                
      WRITE (6,991) (K,K,K,K,K=K1,K2)                                   
  991 FORMAT (1H1,1X,'IF',5X,'MICRON',2X,2(3X,'QABSI(',I2,')',3X,       
     1 'QSCAI(',I2,')',3X,'GBARI(',I2,')',2X,'ALBEDI(',I2,')')/)        
      DO 190 J=1,NFD                                                    
      WRITE (6,992) J,WLAMDA(J),(QABSI(J,K),QSCAI(J,K),GBARI(J,K),      
     1              ALBEDI(J,K),K=K1,K2)                                
  992 FORMAT (1X,I3,1X,1P9E12.3)                                        
  190 CONTINUE                                                          
      DO 210 K1=1,IMIX,5                                                
      K2=MIN0(IMIX,K1+4)                                                
      WRITE (6,993) (K,K,K=K1,K2)                                       
  993 FORMAT (1H1,1X,'IR',7X,'R',3X,5(3X,'ABUNDI(',I2,')',2X,'TDI(',    
     1 I2,')')/)                                                        
      DO 210 L=1,NR                                                     
      WRITE (6,994) L,R(L),(ABUNDI(K,L),TDI(K,L),K=K1,K2)               
  994 FORMAT (1H9,I3,1X,1P11E11.3)                                      
  210 CONTINUE                                                          
C***********************************************************************
C   INITIALIZE DUST ABSORPTION AND SCATTERING COEFFICIENTS              
C***********************************************************************
      DO 220 J=1,NFD                                                    
      DO 220 L=1,NR                                                     
      CGBAR(J,L)=0.0                                                    
  220 CONTINUE                                                          
      DO 240 J=1,NFD                                                    
      DO 240 L=1,NR                                                     
      SUMA=0.0                                                          
      SUMS=0.0                                                          
      SUMG=0.0                                                          
      DO 230 IG=1,IMIX                                                  
      SUMA=SUMA+ABUNDI(IG,L)*QABSI(J,IG)                                
      XX=ABUNDI(IG,L)*QSCAI(J,IG)                                       
      SUMS=SUMS+XX                                                      
      SUMG=SUMG+XX*GBARI(J,IG)                                          
  230 CONTINUE                                                          
      XX=R0*RHOD(L)                                                     
      QABS(J,L)=SUMA*XX                                                 
      QSCA(J,L)=SUMS*XX                                                 
      IF (ISCA.NE.0) CGBAR(J,L)=SUMG*XX                                 
  240 CONTINUE                                                          
C***********************************************************************
C   WRITE QABS, QSCA ONTO DISK FILE                                     
C***********************************************************************
      CALL BRANWT (NQASD,DUMMY(NDF1P1),NB2D)                            
C***********************************************************************
C   WRITE CGBAR ONTO DISK FILE                                          
C***********************************************************************
      CALL BRANWT(NGBD,DUMMY(NDF5P1),NB1D)                              
C***********************************************************************
C   CALCULATE OPTICAL DEPTH PROFILE                                     
C***********************************************************************
      DO 260 J=1,NFD                                                    
      DO 250 L=1,NR                                                     
      XX=QABS(J,L)+QSCA(J,L)                                            
      CHICG(J,L)=XX-CGBAR(J,L)                                          
      CHIF(L)=XX                                                        
  250 CONTINUE                                                          
      CALL GETAU (R,CHIF,TAUX,NR)                                       
      TAU0(J)=TAUX(1)                                                   
  260 CONTINUE                                                          
C***********************************************************************
C   READ BTDAV, BTDAVI FROM DISK FILE                                   
C***********************************************************************
      CALL BRANRD (NBTAD,DUMMY(NDF5P1),NB1D)                            
C***********************************************************************
C   CALCULATE AND WRITE CHI AND ETA ONTO DISK FILE                      
C***********************************************************************
      DO 270 J=1,NFD                                                    
      DO 270 L=1,NR                                                     
      CHI(J,L)=QABS(J,L)                                                
      ETA(J,L)=QABS(J,L)*BTDAV(J,L)                                     
  270 CONTINUE                                                          
      CALL BRANWT (NCED,DUMMY(NDF3P1),NB2D)                             
C***********************************************************************
C   WRITE CHICG ONTO DISK FILE                                          
C***********************************************************************
      CALL BRANWT (NCGD,DUMMY(NDF6P1),NB1D)                             
C***********************************************************************
C   PRINT OUT PHYSICAL PROPERTIES OF MODEL                              
C***********************************************************************
      WRITE (6,666) (J,WLAMDA(J),FREQD(J),WFD(J),FLUXCD(J),             
     1               FLUXSD(J),TAU0(J),J=1,NFD)                         
  666 FORMAT (1H1,1X,'IF',4X,'MICRON',7X,'FREQD',7X,'WFD',8X,'FLUXCD',  
     1 6X,'FLUXSD',7X,'TAU0'//(1X,I3,1P6E12.4))                         
      WRITE (6,777)                                                     
  777 FORMAT (1H1,1X,'IR',10X,'R',13X,'RHOD',11X,'TD',12X,              
     1 'TOF')                                                           
      WRITE (6,888) (L,R(L),RHOD(L),TD(L),TOF(L),L=1,NR)                
  888 FORMAT (1H9,I3,5X,1PE10.3,5X,1PE10.3,5X,1PE10.3,                  
     1 5X,1PE10.3)                                                      
C***********************************************************************
C   COMPUTE QUADRATURE WEIGHTS FOR THE SOLUTION OF                      
C   MOMENT AND RAY EQUATIONS                                            
C***********************************************************************
      CALL QUDWTS                                                       
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE QUDWTS CALCULATES AND STORES THE QUADRATURE              
C   WEIGHTS FOR THE MOMENT AND RAY EQUATIONS. WEIGHTS FOR EVALUATING THE
C   MOMENT INTEGRALS ARE COMPUTED BY CALLING WTPLSP FOR THE SPHERICAL   
C   CASE AND WTCYL FOR THE CYLINDRICAL CASE                             
C***********************************************************************
      SUBROUTINE QUDWTS                                                 
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1,                     
     6           NQWT61=NQWT5+NQWT,NQWT99=12*NQWT,NQWT91=9*NQWT+1)      
      PARAMETER (NQG=4,NQGT=NQWT2+NQG,NQGT1=NQGT+NQG,                   
     1           NQGT2=NQGT1+NQG,NCQW=NQGT2+NQG-1,NTHETA=20)            
      COMMON QW(NQWT0),X(NPMAX),WJ1(NPMAX),WH1(NPMAX),WK1(NPMAX)        
      DIMENSION WZ1(NQWT),WZ2(NQWT),WZ3(NQWT),WJ(NQWT),WK(NQWT),        
     1          WB(NPMAX),WC(NC1),XMUSQ(NQWT),P2R3(NQWT),               
     2          WZETA1(ND),WZETA2(ND),WH(NQWT),WY(NQWT),XL(NPMAX)       
      DIMENSION WJO(NQG),WHO(NQG),WKO(NQG),WYO(NQG)                     
      EQUIVALENCE (QW(1),WZ1(1)),(QW(NQWT1),WZ2(1)),(QW(NQWT2),WZ3(1)), 
     1            (QW(NQWT3),WJ(1)),(QW(NQWT4),WK(1)),(QW(NQWT5),WB(1)),
     2            (QW(NQWT6),WC(1)),(QW(NQWT7),XMUSQ(1)),               
     3            (QW(NQWT8),P2R3(1)),(QW(NQWT9),WZETA1(1)),            
     4            (QW(NQWT10),WZETA2(1))                                
      EQUIVALENCE (CQW(1),WH(1)),(CQW(NQWT1),WY(1)),(CQW(NQWT2),WJO(1)),
     1            (CQW(NQGT),WHO(1)),(CQW(NQGT1),WKO(1)),               
     2            (CQW(NQGT2),WYO(1))                                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG),XMU(NTHETA)             
      NQW=NQWT0                                                         
C***********************************************************************
C   COMPUTE QUADRATURE WEIGHTS FOR COMBINED MOMENT EQUATIONS            
C***********************************************************************
      WR1(1)=R(2)-R(1)                                                  
      WR2(1)=0.5*WR1(1)*WR1(1)                                          
      XX=(R(1)**IGEOM)                                                  
      IF (R(1).EQ.0.0) XX=1.0                                           
      WR3(1)=0.5*(R(2)**IGEOM)/XX                                       
      IF (R(1).EQ.0.0) GO TO 5                                          
      XX=DLOG(R(2)/R(1))/(R(2)-R(1))                                    
      WZETA1(1)=R(2)*XX-1.0                                             
      WZETA2(1)=1.0-R(1)*XX                                             
      GO TO 6                                                           
    5 CONTINUE                                                          
      WZETA1(1)=0.0                                                     
      WZETA2(1)=0.5                                                     
    6 CONTINUE                                                          
      DO 10 L=2,N1                                                      
      B=R(L)-R(L-1)                                                     
      F=R(L+1)-R(L)                                                     
      R2=R(L)**IGEOM                                                    
      WR1(L)=F*R2                                                       
      WR2(L)=F*(R(L-1)**IGEOM)                                          
      WR3(L)=B*R2                                                       
      WR4(L)=B*(R(L+1)**IGEOM)                                          
      WR5(L)=B*F*(B+F)*R2                                               
      IF (IGEOM.EQ.0) GOTO 10                                           
      XX=DLOG(R(L+1)/R(L))/F                                            
      WZETA1(L)=R(L+1)*XX-1.0                                           
      WZETA2(L)=1.0-R(L)*XX                                             
   10 CONTINUE                                                          
      WR1(NR)=R(NR)-R(N1)                                               
      WR2(NR)=0.5*WR1(NR)*WR1(NR)                                       
      WR3(NR)=0.5*(R(N1)**IGEOM)/(R(NR)**IGEOM)                         
   12 CONTINUE                                                          
      IF (IGEOM.NE.0) GOTO 45                                           
C***********************************************************************
C   COMPUTE QUADRATURE WEIGHTS FOR THE PLANAR CASE                      
C***********************************************************************
      CALL WTPLSP (XMU,WJ1,WH1,WK1,NTHETA,0)                            
      DO 20 K=1,NTHETA                                                  
      WK(K)=WK1(K)                                                      
      WB(K)=WH1(K)                                                      
      WC(K)=WB(K)                                                       
      WJ(K)=WJ1(K)                                                      
      XMUSQ(K)=XMU(K)*XMU(K)                                            
   20 CONTINUE                                                          
      JZ=0                                                              
      DO 40 IP=1,NTHETA                                                 
C***********************************************************************
C   CALCULATE IMPACT PARAMETERS FOR PLANAR CASE                         
C***********************************************************************
      P(IP)=DSQRT(1.0-XMUSQ(IP))                                        
      XMUI=1.0/XMU(IP)                                                  
      JZ=JZ+1                                                           
      WZ1(JZ)=0.5*XMUI*(R(2)-R(1))                                      
      WZ2(JZ)=2.0*WZ1(JZ)*WZ1(JZ)                                       
      DO 30 L=2,N1                                                      
      JZ=JZ+1                                                           
      B=R(L)-R(L-1)                                                     
      F=R(L+1)-R(L)                                                     
      WZ1(JZ)=F                                                         
      WZ2(JZ)=B*F*(B+F)*XMUI*XMUI                                       
      WZ3(JZ)=B                                                         
   30 CONTINUE                                                          
      JZ=JZ+1                                                           
      WZ1(JZ)=0.5*XMUI*(R(NR)-R(N1))                                    
      WZ2(JZ)=2.0*WZ1(JZ)*WZ1(JZ)                                       
   40 CONTINUE                                                          
C***********************************************************************
C    WRITE QUADRATURE WEIGHTS FOR PLANAR CASE ONTO DISK                 
C***********************************************************************
      CALL BRANWT (NEFQW,QW(1),NQW)                                     
      RETURN                                                            
   45 CONTINUE                                                          
C***********************************************************************
C   CALCULATE IMPACT PARAMETERS FOR CYLINDRICAL AND SPHERICAL CASES     
C***********************************************************************
      IP0=0                                                             
      IF (IC.NE.0) IP0=NC                                               
      NP=IP0+NR                                                         
      IP1=IP0+1                                                         
      IP2=IP1+1                                                         
      NPS1=NP-1                                                         
      DO 50 IP=IP1,NP                                                   
      JP=IP-IP0                                                         
      P(IP)=R(JP)                                                       
   50 CONTINUE                                                          
      IF (IC.EQ.0) GO TO 65                                             
C***********************************************************************
C   CALCULATE IMPACT PARAMETERS THROUGH CORE                            
C***********************************************************************
      P(1)=0.0                                                          
      DP=R(2)-R(1)                                                      
      DO 60 JP=2,IP0                                                    
      IP=IP1-JP+1                                                       
      P(IP)=P(IP+1)-DP                                                  
      DP=2.1*DP                                                         
      IF (DP.GE.P(IP)) DP=0.5*P(IP)                                     
   60 CONTINUE                                                          
   65 CONTINUE                                                          
C***********************************************************************
C   COMPUTE AND ASSIGN WEIGHTS FOR MOMENT INTEGRALS                     
C***********************************************************************
      X(1)=0.0                                                          
      J=0                                                               
      IPMAX=IP0                                                         
      DO 120 L=1,NR                                                     
      IPMAX=IPMAX+1                                                     
      IF (IPMAX.EQ.1) GO TO 75                                          
      R2=R(L)*R(L)                                                      
      IPM1=IPMAX-1                                                      
      DO 70 IP=1,IPM1                                                   
      P2=P(IPMAX-IP)*P(IPMAX-IP)                                        
      IF (IGEOM.EQ.1) P2=P(IP+1)*P(IP+1)                                
      IF (IGEOM.EQ.1) X(IP+1)=DSQRT((P2/R2))                            
      IF (IGEOM.EQ.2) X(IP+1)=DSQRT(1.0-P2/R2)                          
   70 CONTINUE                                                          
   75 CONTINUE                                                          
C***********************************************************************
C   CALL SUBROUTINE TO COMPUTE MOMENT INTEGRAL WEIGHTS                  
C***********************************************************************
      IF (IGEOM.EQ.1) CALL WTCYL(X,WJ1,WH1,WK1,WY1,                     
     1    WJO,WHO,WKO,WYO,IPMAX)                                        
      IF (IGEOM.EQ.2) CALL WTPLSP (X,WJ1,WH1,WK1,IPMAX,2)               
      J=J+1                                                             
      K=J                                                               
      DO 80 I=1,IP1                                                     
      LL=IPMAX-I+1                                                      
      WJ(K)=WJ1(LL)                                                     
      WK(K)=WK1(LL)                                                     
      IF (IGEOM.NE.1) GO TO 77                                          
      WH(K)=WK1(LL)                                                     
      WY(K)=WY1(LL)                                                     
   77 CONTINUE                                                          
      IF (L.EQ.1) WC(I)=WH1(LL)                                         
      K=K+NR                                                            
   80 CONTINUE                                                          
      IF (IPMAX.LE.IP1) GO TO 120                                       
      K=K-1                                                             
      DO 90 I=IP2,IPMAX                                                 
      LL=IPMAX-I+1                                                      
      WJ(K)=WJ1(LL)                                                     
      WK(K)=WK1(LL)                                                     
      IF (IGEOM.NE.1) GO TO 88                                          
      WH(K)=WH1(LL)                                                     
      WY(K)=WY1(LL)                                                     
   88 CONTINUE                                                          
      K=K+NR-(I-IP0)                                                    
   90 CONTINUE                                                          
      IF (L.NE.NR) GO TO 120                                            
      DO 110 I=1,IPMAX                                                  
      LL=IPMAX-I+1                                                      
      WB(I)=WH1(LL)                                                     
  110 CONTINUE                                                          
  120 CONTINUE                                                          
C***********************************************************************
C   COMPUTE WEIGHTS FOR COMBINED RAY EQUATIONS FOR                      
C   SPHERICAL AND CYLINDRICAL CASES                                     
C***********************************************************************
      IBC=1                                                             
  122 CONTINUE                                                          
      JZ=0                                                              
      IR0=1                                                             
      STH=1.0                                                           
      IF (IGEOM.EQ.1) STH=BETA(IBC)                                     
      DO 150 IP=1,NPS1                                                  
      IF (IP.GT.IP1) IR0=IR0+1                                          
      IR1=IR0+1                                                         
      P2=P(IP)*P(IP)                                                    
      DO 130 L=IR0,NR                                                   
      R2=R(L)*R(L)                                                      
      X(L)=DSQRT(R2-P2)                                                 
  130 CONTINUE                                                          
      JZ=JZ+1                                                           
      WZ1(JZ)=0.5*(X(IR1)-X(IR0))/STH                                   
      WZ2(JZ)=2.0*WZ1(JZ)*WZ1(JZ)                                       
      IF (R(IR0).EQ.0.0) GO TO 133                                      
      XMUSQ(JZ)=(X(IR0)/R(IR0))**2                                      
      P2R3(JZ)=P2/R(IR0)**3                                             
      GO TO 135                                                         
  133 CONTINUE                                                          
      XMUSQ(JZ)=1.0                                                     
      P2R3(JZ)=0.0                                                      
  135 CONTINUE                                                          
      IF (IP.EQ.NPS1) GO TO 144                                         
      DO 140 L=IR1,N1                                                   
      JZ=JZ+1                                                           
      B=(X(L)-X(L-1))/STH                                               
      F=(X(L+1)-X(L))/STH                                               
      WZ1(JZ)=F                                                         
      WZ2(JZ)=B*F*(B+F)                                                 
      WZ3(JZ)=B                                                         
      XMUSQ(JZ)=(X(L)/R(L))**2                                          
      P2R3(JZ)=P2/R(L)**3                                               
  140 CONTINUE                                                          
  144 CONTINUE                                                          
      JZ=JZ+1                                                           
      WZ1(JZ)=0.5*(X(NR)-X(N1))/STH                                     
      WZ2(JZ)=2.0*WZ1(JZ)*WZ1(JZ)                                       
      XMUSQ(JZ)=(X(NR)/R(NR))**2                                        
      P2R3(JZ)=P2/R(NR)**3                                              
  150 CONTINUE                                                          
C***********************************************************************
C   WRITE WEIGHTS TO DISK FILE                                          
C***********************************************************************
      WRITE (NEFQW) QW                                                  
      IBC=IBC+1                                                         
      IF (IGEOM.EQ.1.AND.IBC.LE.NQG) GOTO 122                           
      REWIND NEFQW                                                      
      IF (IGEOM.EQ.1) CALL BRANWT (NCYQW,CQW(1),NCQW)                   
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE WTCYL COMPUTES THE QUADRATURE WEIGHTS FOR                
C   THE MOMENT INTEGRALS IN THE CYLINDRICAL CASE                        
C   X     --- ARRAY OF ABSCISSAS                                        
C   N     --- PARAMETER SPECIFYING THE NUMBER OF INTEGRATION INTERVALS  
C   PH    --- RENORMALIZATION FACTOR FOR WH ARRAY                       
C   PJ    --- RENORMALIZATION FACTOR FOR WJ ARRAY                       
C   PK    --- RENORMALIZATION FACTOR FOR WK ARRAY                       
C   PY    --- RENORMALIZATION FACTOR FOR WY ARRAY                       
C   SUMH  --- SUM OF WH ARRAY ELEMENTS                                  
C   SUMJ  --- SUM OF WJ ARRAY ELEMENTS                                  
C   SUMK  --- SUM OF WK ARRAY ELEMENTS                                  
C   SUMY  --- SUM OF WY ARRAY ELEMENTS                                  
C   PHO   --- RENORMALIZATION FACTOR FOR WHO ARRAY                      
C   PJO   --- RENORMALIZATION FACTOR FOR WJO ARRAY                      
C   PKO   --- RENORMALIZATION FACTOR FOR WKO ARRAY                      
C   PYO   --- RENORMALIZATION FACTOR FOR WYO ARRAY                      
C   SUMHO --- SUM OF WHO ARRAY ELEMENTS                                 
C   SUMJO --- SUM OF WJO ARRAY ELEMENTS                                 
C   SUMKO --- SUM OF WKO ARRAY ELEMENTS                                 
C   SUMYO --- SUM OF WYO ARRAY ELEMENTS                                 
C***********************************************************************
      SUBROUTINE WTCYL (X,WJ,WH,WK,WY,WJO,WHO,WKO,WYO,N)                
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   
     1           NQWT2=NQWT1+NQWT)                                      
      PARAMETER (NQG=4,NQGT=NQWT2+NQG,NQGT1=NQGT+NQG,                   
     1           NQGT2=NQGT1+NQG,NCQW=NQGT2+NQG-1,NTHETA=20)            
      DIMENSION X(1),WJ(1),WH(1),WK(1),WY(NPMAX)                        
      DIMENSION PHI(NPMAX),WJI(NPMAX),WHI(NPMAX),WKI(NPMAX),WYI(NPMAX)  
      DIMENSION OMEGA(NQG),OMEG2(NQG)                                   
      DIMENSION WJO(NQG),WHO(NQG),WKO(NQG),WYO(NQG)                     
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG)                         
      NP1=N-1                                                           
      C3=1.0/3.0                                                        
      IF (N.GT.1) GO TO 5                                               
C***********************************************************************
C   SET PHI QUADRATURE WEIGHTS FOR N=1 TO RENORMALIZATION CONSTANTS     
C***********************************************************************
      WJ(1)=PI/2.0                                                      
      WH(1)=1.0                                                         
      WK(1)=PI/4.0                                                      
      WY(1)=1.0                                                         
      RETURN                                                            
    5 CONTINUE                                                          
C***********************************************************************
C   COMPUTE PHI QUADRATURE WEIGHTS FOR N>1                              
C***********************************************************************
      WJI(1)=0.0                                                        
      WHI(1)=0.0                                                        
      WKI(1)=0.0                                                        
      WYI(1)=0.0                                                        
      DO 10 L=1,N                                                       
      PHI(L)=DASIN(X(L))                                                
   10 CONTINUE                                                          
      DO 20 L=1,NP1                                                     
      B=X(L)                                                            
      F=X(L+1)                                                          
C***********************************************************************
C   COMPUTE PHI WEIGHTS FOR J INTEGRAL                                  
C***********************************************************************
      WJI(L)=WJI(L)+0.5*(PHI(L+1)-PHI(L))                               
      WJI(L+1)=0.5*(PHI(L+1)-PHI(L))                                    
C***********************************************************************
C   USE TRAPEZOIDAL RULE FOR WH AND WK                                  
C***********************************************************************
      WHI(L)=WHI(L)+0.5*(F-B)                                           
      WHI(L+1)=0.5*(F-B)                                                
      WKI(L)=WKI(L)+0.5*(DSQRT(1.0-B*B))*(F-B)                          
      WKI(L+1)=0.5*(DSQRT(1.0-F*F))*(F-B)                               
      IF (ISCA.EQ.0) GO TO 20                                           
C***********************************************************************
C   COMPUTE PHI WEIGHTS FOR Y INTEGRAL                                  
C***********************************************************************
      WYI(L)=WYI(L)+0.5*X(L)*(PHI(L+1)-PHI(L))                          
      WYI(L+1)=0.5*X(L+1)*(PHI(L+1)-PHI(L))                             
   20 CONTINUE                                                          
      DO 30 L=1,N                                                       
      WJ(L)=WJI(N+1-L)                                                  
      WH(L)=WHI(N+1-L)                                                  
      WK(L)=WKI(N+1-L)                                                  
      IF (ISCA.EQ.0) GO TO 30                                           
      WY(L)=WYI(N+1-L)                                                  
   30 CONTINUE                                                          
      SUMJ=0.0                                                          
      SUMH=0.0                                                          
      SUMK=0.0                                                          
      SUMY=0.0                                                          
      DO 40 L=1,N                                                       
      SUMJ=SUMJ+WJ(L)                                                   
      SUMH=SUMH+WH(L)                                                   
      SUMK=SUMK+WK(L)                                                   
      IF (ISCA.EQ.0) GO TO 40                                           
      SUMY=SUMY+WY(L)                                                   
   40 CONTINUE                                                          
      PJ=PI/(2.0*SUMJ)                                                  
      PH=1.0/SUMH                                                       
      PK=PI/(4.0*SUMK)                                                  
      IF (ISCA.EQ.0) GO TO 44                                           
      PY=1.0/SUMY                                                       
   44 CONTINUE                                                          
C***********************************************************************
C   RENORMALIZE PHI WEIGHTS                                             
C***********************************************************************
      DO 50 L=1,N                                                       
      WJ(L)=PJ*WJ(L)                                                    
      WH(L)=PH*WH(L)                                                    
      WK(L)=PK*WK(L)                                                    
      IF (ISCA.EQ.0) GO TO 50                                           
      WY(L)=PY*WY(L)                                                    
   50 CONTINUE                                                          
C***********************************************************************
C   OMEGA=COS(THETA) ARE THE ABSCISSAS FOR THE GAUSSIAN                 
C   QUADRATURES                                                         
C   BETA'S = SIN(THETA); USED IN RAY EQN SOLUTION                       
C***********************************************************************
      OMEGA(1)=0.0694318442                                             
      OMEGA(2)=0.3300094782                                             
      OMEGA(3)=0.6699905218                                             
      OMEGA(4)=0.9305681558                                             
      DO 80 L=1,NQG                                                     
      OMEG2(L)=OMEGA(L)*OMEGA(L)                                        
      BETA(L)=DSQRT(1.0-OMEG2(L))                                       
   80 CONTINUE                                                          
C***********************************************************************
C   WJO, WKO,WHO, AND WYO ARE GAUSSIAN QUADRATURES FOR                  
C   THE THETA INTEGRATION                                               
C***********************************************************************
      WJO(1)=0.1739274226                                               
      WJO(2)=0.3260725774                                               
      WJO(3)=0.3260725774                                               
      WJO(4)=0.1739274226                                               
      DO 100 L=1,NQG                                                    
      WHO(L)=BETA(L)*WJO(L)                                             
      WKO(L)=OMEG2(L)*WJO(L)                                            
      WYO(L)=OMEGA(L)*WJO(L)                                            
  100 CONTINUE                                                          
      SUMJO=0.0                                                         
      SUMHO=0.0                                                         
      SUMKO=0.0                                                         
      SUMYO=0.0                                                         
      DO 60 L=1,NQG                                                     
      SUMJO=SUMJO+WJO(L)                                                
      SUMHO=SUMHO+WHO(L)                                                
      SUMKO=SUMKO+WKO(L)                                                
      SUMYO=SUMYO+WYO(L)                                                
   60 CONTINUE                                                          
      PJO=1.0/SUMJO                                                     
      PHO=PI/(4*SUMHO)                                                  
      PKO=C3/SUMKO                                                      
      PYO=0.5/SUMYO                                                     
      DO 70 L=1,NQG                                                     
C***********************************************************************
C   RENORMALIZE THETA WEIGHTS                                           
C***********************************************************************
      WJO(L)=PJO*WJO(L)                                                 
      WHO(L)=PHO*WHO(L)                                                 
      WKO(L)=PKO*WKO(L)                                                 
      IF (ISCA.EQ.0) GO TO 70                                           
      WYO(L)=PYO*WYO(L)                                                 
   70 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE WTPLSP CALCULATES THE QUADRATURE WEIGHTS                 
C   FOR THE MOMENT INTEGRALS IN THE PLANAR AND SPHERICAL                
C   CASES                                                               
C   X     --- ARRAY OF ABSCISSAS                                        
C   N     --- PARAMETER SPECIFYING THE NUMBER OF INTEGRATION INTERVALS  
C   PH    --- RENORMALIZATION FACTOR FOR WH ARRAY                       
C   PJ    --- RENORMALIZATION FACTOR FOR WJ ARRAY                       
C   PK    --- RENORMALIZATION FACTOR FOR WK ARRAY                       
C   PY    --- RENORMALIZATION FACTOR FOR WY ARRAY                       
C   SUMH  --- SUM OF WH ARRAY ELEMENTS                                  
C   SUMJ  --- SUM OF WJ ARRAY ELEMENTS                                  
C   SUMK  --- SUM OF WK ARRAY ELEMENTS                                  
C   SUMY  --- SUM OF WY ARRAY ELEMENTS                                  
C***********************************************************************
      SUBROUTINE WTPLSP (X,WJ,WH,WK,N,IG)                               
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      DIMENSION X(1),WJ(1),WH(1),WK(1)                                  
      C3=1.0/3.0                                                        
      C6=1.0/6.0                                                        
      C12=1.0/12.0                                                      
      IF (IG.NE.0) GOTO 11                                              
C***********************************************************************
C   COMPUTE GAUSSIAN QUADRATURE WEIGHTS FOR MOMENT INTEGRALS IN         
C   PLANAR CASE                                                         
C***********************************************************************
      IF (N.EQ.20) GOTO 5                                               
      WRITE (6,111)N                                                    
  111 FORMAT(1H1,40X,'INPUT NTHETA ( = ',I2,' ) IS NOT EQUAL TO 20'/)   
      STOP                                                              
    5 CONTINUE                                                          
      X(1)=9.96869316259256D-01                                         
      X(2)=9.83577911866012D-01                                         
      X(3)=9.59963099538093D-01                                         
      X(4)=9.26522808214715D-01                                         
      X(5)=8.83969092344513D-01                                         
      X(6)=8.33208746518991D-01                                         
      X(7)=7.75323580615695D-01                                         
      X(8)=7.11547287818728D-01                                         
      X(9)=6.43239129897547D-01                                         
      X(10)=5.71854959066743D-01                                        
      X(11)=4.98916184517151D-01                                        
      X(12)=4.25977341817173D-01                                        
      X(13)=3.54592954222632D-01                                        
      X(14)=2.86284388984712D-01                                        
      X(15)=2.22507407964513D-01                                        
      X(16)=1.64621085368438D-01                                        
      X(17)=1.13858697085453D-01                                        
      X(18)=7.1300985081249D-02                                         
      X(19)=3.7851287849502D-02                                         
      X(20)=1.4204211159358D-02                                         
      WK(1)=7.975792736277D-03                                          
      WK(2)=1.7913752325739D-02                                         
      WK(3)=2.6382564429126D-02                                         
      WK(4)=3.2734644317573D-02                                         
      WK(5)=3.6587916553227D-02                                         
      WK(6)=3.7847390376922D-02                                         
      WK(7)=3.6697420472018D-02                                         
      WK(8)=3.3556431453475D-02                                         
      WK(9)=2.9002404745589D-02                                         
      WK(10)=2.3682290414246D-02                                        
      WK(11)=1.8220503418269D-02                                        
      WK(12)=1.3140919256662D-02                                        
      WK(13)=8.813556078243D-03                                         
      WK(14)=5.432090806123D-03                                         
      WK(15)=3.022489170907D-03                                         
      WK(16)=1.477444441971D-03                                         
      WK(17)=6.07045704267D-04                                          
      WK(18)=1.93888309618D-04                                          
      WK(19)=4.1039102087D-05                                           
      WK(20)=3.749220993D-06                                            
      DO 10 L=1,N                                                       
      WH(L)=WK(L)/X(L)                                                  
      WJ(L)=WH(L)/X(L)                                                  
   10 CONTINUE                                                          
      GOTO 25                                                           
   11 CONTINUE                                                          
C***********************************************************************
C   COMPUTE QUADRATURE WEIGHTS FOR MOMENT INTEGRALS IN                  
C   SHERICAL CASE                                                       
C***********************************************************************
      N1=N-1                                                            
      IF (N.GT.1) GO TO 15                                              
C***********************************************************************
C   SET WEIGHTS FOR N=1 TO RENORMALIZATION CONSTANTS                    
C***********************************************************************
      WJ(1)=1.0                                                         
      WH(1)=0.5                                                         
      WK(1)=C3                                                          
      RETURN                                                            
   15 CONTINUE                                                          
C***********************************************************************
C   COMPUTE WEIGHTS FOR N>1 USING TRAPEZOIDAL RULE                      
C***********************************************************************
      WJ(1)=0.0                                                         
      WH(1)=0.0                                                         
      WK(1)=0.0                                                         
      DO 20 L=1,N1                                                      
      B=X(L)                                                            
      F=X(L+1)                                                          
      BF=B*F                                                            
      B2=B*B                                                            
      B3=B2*B                                                           
      F2=F*F                                                            
      F3=F2*F                                                           
      FB2=F*B2                                                          
      F2B=F2*B                                                          
      WJ(L)=WJ(L)+0.5*(F-B)                                             
      WJ(L+1)=0.5*(F-B)                                                 
      WH(L)=WH(L)+C6*(F2+BF-2.0*B2)                                     
      WH(L+1)=C6*(2.0*F2-BF-B2)                                         
      WK(L)=WK(L)+C12*(F3+F2B+FB2-3.0*B3)                               
      WK(L+1)=C12*(3.0*F3-F2B-FB2-B3)                                   
   20 CONTINUE                                                          
   25 CONTINUE                                                          
      SUMJ=0.0                                                          
      SUMH=0.0                                                          
      SUMK=0.0                                                          
      DO 30 L=1,N                                                       
      SUMJ=SUMJ+WJ(L)                                                   
      SUMH=SUMH+WH(L)                                                   
      SUMK=SUMK+WK(L)                                                   
   30 CONTINUE                                                          
      PJ=1.0/SUMJ                                                       
      PH=0.5/SUMH                                                       
      PK=1.0/(3.0*SUMK)                                                 
C***********************************************************************
C   RENORMALIZE WEIGHTS                                                 
C***********************************************************************
      DO 40 L=1,N                                                       
      WJ(L)=PJ*WJ(L)                                                    
      WH(L)=PH*WH(L)                                                    
      WK(L)=PK*WK(L)                                                    
   40 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE GETTD SOLVES FOR THE CORRECTIONS TO THE                  
C   MEAN INTENSITY (AJ) AND THE DUST TEMPERATURE (TD).                  
C   AJ AND TD ARE UPDATED                                               
C   A  --- ELEMENTS OF W, DF(J)/DJ(I-1,J)                               
C   B  --- ELEMENTS OF W, DF(J)/DJ(I,J)                                 
C   C  --- ELEMENTS OF W, DF(J)/DJ(I+1,J)                               
C   D1 --- F                                                            
C   D2 --- U                                                            
C   V  --- V                                                            
C   P1 --- E                                                            
C   P2 --- G                                                            
C   W  --- P                                                            
C   Z  --- V*W(INVERSE)*F                                               
C   Q  --- Q                                                            
C   Y  --- V*W(INVERSE)*U                                               
C***********************************************************************
      SUBROUTINE GETTD                                                  
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          
      PARAMETER (ND5=5*ND,ND1P1=ND+1,ND2P1=2*ND+1,                      
     1           ND3P1=3*ND+1,ND4P1=4*ND+1)                             
      PARAMETER (NX=7*NDF+9*ND)                                         
      COMMON DUMMY(NX),DD1(ND),DD2(ND),DD3(ND),DD4(ND),DD5(ND),         
     1       DD6(ND),DD7(ND),BLOCK(ND5),W(ND,ND)                        
      DIMENSION A(ND),B(ND),C(ND),D(ND),F(ND),G(ND),H(ND),Q(ND),V(ND),  
     1          Y(ND),Z(ND),D1(ND),G1(ND),P1(ND),D2(ND),G2(ND),P2(ND)   
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),DX(NDF)                 
      EQUIVALENCE (DD1(1),D(1),P1(1)),(DD2(1),G(1),G2(1),P2(1)),        
     1            (DD3(1),B(1)),(DD4(1),C(1)),                          
     2            (DD5(1),Y(1),G1(1)),(DD6(1),Q(1),Z(1)),               
     3            (DD7(1),V(1))                                         
      EQUIVALENCE (DUMMY(1),AJ(1,1)),(DUMMY(NDF1P1),FK(1,1)),           
     1            (DUMMY(NDF2P1),ZETA(1,1)),(DUMMY(NDF3P1),DX(1))       
      EQUIVALENCE (BLOCK(1),A(1)),(BLOCK(ND1P1),D1(1)),                 
     1            (BLOCK(ND2P1),D2(1)),(BLOCK(ND3P1),F(1)),             
     2            (BLOCK(ND4P1),H(1))                                   
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
C***********************************************************************
C   FORM E, G, AND V ELEMENTS OF GRAND MATRIX                           
C***********************************************************************
      CALL JACOB1                                                       
      DO 10 L=1,NR                                                      
      Z(L)=0.0                                                          
      DO 10 I=1,NR                                                      
      W(L,I)=0.0                                                        
   10 CONTINUE                                                          
      DO 60 J=1,NFD                                                     
C***********************************************************************
C   FORM W, U, AND F ELEMENTS OF GRAND MATRIX                           
C***********************************************************************
      CALL JACOB2 (J,A,B,C,D1,D2,V)                                     
      A(1)=1.0/B(1)                                                     
      H(1)=-C(1)*A(1)                                                   
      G1(1)=D1(1)*A(1)                                                  
      DO 20 I=2,NR                                                      
      F(I)=1.0/(B(I)+A(I)*H(I-1))                                       
      H(I)=-C(I)*F(I)                                                   
      G1(I)=(D1(I)-A(I)*G1(I-1))*F(I)                                   
   20 CONTINUE                                                          
C***********************************************************************
C   WRITE A, D1, D2, F & H ONTO DISK FILE                               
C***********************************************************************
      WRITE (NEID) BLOCK                                                
      IF (J.EQ.NFD) REWIND NEID                                         
C***********************************************************************
C   CONPUTE V*W(INVERSE)*U, V*W(INVERSE)*F                              
C***********************************************************************
      Y(NR)=G1(NR)                                                      
      Z(NR)=Z(NR)+V(NR)*Y(NR)                                           
      DO 30 K=1,N1                                                      
      Y(NR-K)=G1(NR-K)+H(NR-K)*Y(NR-K+1)                                
      Z(NR-K)=Z(NR-K)+V(NR-K)*Y(NR-K)                                   
   30 CONTINUE                                                          
      DO 50 L=1,NR                                                      
      G2(1)=0.0                                                         
      IF (L.EQ.1) G2(1)=D2(1)*A(1)                                      
      DO 40 I=2,NR                                                      
      IF (I.EQ.L) G2(I)=(D2(I)-A(I)*G2(I-1))*F(I)                       
      IF (I.NE.L) G2(I)=-A(I)*G2(I-1)*F(I)                              
   40 CONTINUE                                                          
      Y(NR)=G2(NR)                                                      
      W(NR,L)=W(NR,L)-V(NR)*Y(NR)                                       
      DO 50 K=1,N1                                                      
      Y(NR-K)=G2(NR-K)+H(NR-K)*Y(NR-K+1)                                
      W(NR-K,L)=W(NR-K,L)-V(NR-K)*Y(NR-K)                               
   50 CONTINUE                                                          
   60 CONTINUE                                                          
C***********************************************************************
C   READ P1, P2 FROM DISK FILE                                          
C***********************************************************************
      CALL JACOB3 (P1,P2)                                               
      DO 70 I=1,NR                                                      
      Q(I)=P1(I)-Z(I)                                                   
      W(I,I)=P2(I)+W(I,I)                                               
   70 CONTINUE                                                          
C***********************************************************************
C   SOLVE SYSTEM OF EQUATIONS FOR J CORRECTIONS                         
C***********************************************************************
      CALL LINEQ1 (W,Q,G2,NR,ND)                                        
      I1=NR                                                             
      DO 110 J=1,NFD                                                    
C***********************************************************************
C   READ A,D1,D2,F & H FROM DISK FILE                                   
C***********************************************************************
      READ (NEID) BLOCK                                                 
      IF (J.EQ.NFD) REWIND NEID                                         
      D(1)=D1(1)-D2(1)*Q(1)                                             
      G(1)=D(1)*A(1)                                                    
      DO 80 I=2,NR                                                      
      D(I)=D1(I)-D2(I)*Q(I)                                             
      G(I)=(D(I)-A(I)*G(I-1))*F(I)                                      
   80 CONTINUE                                                          
      Y(NR)=G(NR)                                                       
      DX(I1)=Y(NR)                                                      
      DO 90 K=1,N1                                                      
      Y(NR-K)=G(NR-K)+H(NR-K)*Y(NR-K+1)                                 
      DX(I1-K)=Y(NR-K)                                                  
   90 CONTINUE                                                          
      I1=I1+NR                                                          
  110 CONTINUE                                                          
      ITCONT=1                                                          
      ITCONJ=1                                                          
C***********************************************************************
C   READ AJ,FK,ZETA, FROM DISK FILE                                     
C***********************************************************************
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 
      DO 120 L=1,NR                                                     
C***********************************************************************
C   COMPUTE DUST TEMPERATURE CORRECTIONS AND TEST FOR CONVERGENCE       
C***********************************************************************
      DTD(L)=Q(L)/(TD(L)+0.5*Q(L))                                      
      IF (Q(L).LE.(-TD(L))) Q(L)=-0.9*TD(L)                             
      IF (Q(L).GE.TD(L)) Q(L)=0.9*TD(L)                                 
      TD(L)=TD(L)+Q(L)                                                  
      IF (DABS(DTD(L)).GT.EPS) ITCONT=0                                 
      I=L-NR                                                            
      DO 120 J=1,NFD                                                    
C***********************************************************************
C   COMPUTE MEAN INTENSITY CORRECTIONS AND CHECK FOR CONVERGENCE        
C***********************************************************************
      I=I+NR                                                            
      XX=DX(I)/(AJ(J,L)+0.5*DX(I))                                      
      IF (DABS(XX).GT.DABS(DJD(L))) DJD(L)=XX                           
      IF (DABS(XX).GT.EPS) ITCONJ=0                                     
      IF (DX(I).LE.(-AJ(J,L))) DX(I)=-0.9*AJ(J,L)                       
      IF (DX(I).GE.AJ(J,L)) DX(I)=0.9*AJ(J,L)                           
      AJ(J,L)=AJ(J,L)+DX(I)                                             
  120 CONTINUE                                                          
C***********************************************************************
C   WRITE AJ,FK, AND ZETA TO DISK FILE                                  
C***********************************************************************
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE JACOB1 FORMS THE V, E, AND G ELEMENTS OF                 
C   THE GRAND ARRAY USED TO SOLVE FOR DTD AND DJD                       
C   WORD1 --- DUMMY ARRAY USED TO WRITE ONTO NJCBD                      
C   P1SUM --- ELEMENTS OF E                                             
C   P2SUM --- ELEMENTS OF G                                             
C   WORD2 --- DUMMY ARRAY USED TO WRITE ONTO NJCBD                      
C***********************************************************************
      SUBROUTINE JACOB1                                                 
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          
      PARAMETER (ND5=5*ND,NF5=5*NF,NX1=6*NDF+ND5+1,NX2=NX1+ND5,         
     1           NX3=NX2+NF5,NX4=NX3+NF5,NX5=NX4+NF5)                   
      PARAMETER (ND2=2*ND,ND7=7*ND,NDF7=7*NDF)                          
      COMMON DUMMY(NDF7),AJ1(ND),CHI1(ND),ETA1(ND),FKZETA(ND),W(ND),    
     1       DEDT(ND),V(ND),P1(ND),P2(ND)                               
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHICG(NF,ND),           
     1          QABS(NF,ND),BTDAV(NF,ND),BTDAVI(NF,ND),WORD1(ND7),      
     2          WORD2(ND2)                                              
      EQUIVALENCE (DUMMY(1),AJ(1,1)),(DUMMY(NDF1P1),FK(1,1)),           
     1            (DUMMY(NDF2P1),ZETA(1,1)),(DUMMY(NDF3P1),CHICG(1,1)), 
     2            (DUMMY(NDF4P1),QABS(1,1)),(DUMMY(NDF5P1),BTDAV(1,1)), 
     3            (DUMMY(NDF6P1),BTDAVI(1,1)),(AJ1(1),WORD1(1)),        
     4            (P1(1),WORD2(1))                                      
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         
C***********************************************************************
C   READ AJ, FK, ZETA FROM DISK FILE                                    
C***********************************************************************
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 
C***********************************************************************
C   READ CHICG FROM DISK FILE                                           
C***********************************************************************
      CALL BRANRD (NCGD,DUMMY(NDF3P1),NB1D)                             
C***********************************************************************
C   READ QABS FROM DISK FILE                                            
C***********************************************************************
      CALL BRANRD (NQASD,DUMMY(NDF4P1),NB1D)                            
C***********************************************************************
C   READ BTDAV FROM DISK FILE                                           
C***********************************************************************
      CALL BRANRD (NBTAD,DUMMY(NDF5P1),NB2D)                            
C***********************************************************************
C   CALCULATE DE/DT, DE/DJ, E                                           
C***********************************************************************
      DO 20 J=1,NFD                                                     
      DO 10 L=1,NR                                                      
      AJ1(L)=AJ(J,L)                                                    
      W(L)=ZETA(J,L)*CHICG(J,L)                                         
      FKZETA(L)=FK(J,L)*ZETA(J,L)                                       
      CHI1(L)=QABS(J,L)                                                 
      ETA1(L)=CHI1(L)*BTDAV(J,L)                                        
      DEDT(L)=CHI1(L)*BTDAVI(J,L)                                       
      V(L)=QABS(J,L)*WFD(J)                                             
   10 CONTINUE                                                          
C***********************************************************************
C   WRITE AJ1 ONTO DISK FILE                                            
C***********************************************************************
      WRITE (NJCBD) WORD1                                               
   20 CONTINUE                                                          
C***********************************************************************
C   CALCULATE E, DE/DT                                                  
C***********************************************************************
      DO 40 L=1,NR                                                      
      P1SUM=0.0                                                         
      P2SUM=0.0                                                         
      DO 30 J=1,NFD                                                     
      QW=QABS(J,L)*WFD(J)                                               
      P1SUM=P1SUM+QW*(AJ(J,L)-BTDAV(J,L))                               
      P2SUM=P2SUM+QW*BTDAVI(J,L)                                        
   30 CONTINUE                                                          
      P1(L)=-P1SUM                                                      
      P2(L)=-P2SUM                                                      
   40 CONTINUE                                                          
C***********************************************************************
C   WRITE P1, P2 TO DISK FILE                                           
C***********************************************************************
      WRITE (NJCBD) WORD2                                               
      REWIND NJCBD                                                      
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE JACOB2 FORMS THE F, U, AND W ELEMENTS                    
C   OF THE GRAND ARRAY USED TO SOLVE FOR DTD AND DJD                    
C   A    --- ELEMENTS OF A                                              
C   B    --- ELEMENTS OF B                                              
C   C    --- ELEMENTS OF C                                              
C   D1   --- ELEMENTS OF F                                              
C   D2   --- ELEMENTS OF U                                              
C   V    --- DE/DJ                                                      
C   VV   --- DE/DJ                                                      
C   WORD --- DUMMY ARRAY USED TO READ FILE NJCBD                        
C***********************************************************************
      SUBROUTINE JACOB2 (J,A,B,C,D1,D2,V)                               
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (ND7=7*ND)                                              
      DIMENSION A(1),B(1),C(1),D1(1),D2(1),V(1)                         
      COMMON AJ1(ND),CHI1(ND),ETA1(ND),FKZETA(ND),W(ND),DEDT(ND),VV(ND) 
      DIMENSION WORD(ND7)                                               
      EQUIVALENCE (AJ1(1),WORD(1))                                      
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        
      IF (J.EQ.1) CALL BCD (B1,B2,B1N,B2N)                              
C***********************************************************************
C   READ AJ1 FROM DISK FILE                                             
C***********************************************************************
      READ (NJCBD) WORD                                                 
      DO 10 L=1,NR                                                      
      V(L)=VV(L)                                                        
   10 CONTINUE                                                          
C***********************************************************************
C   CALCULATE A, B, C, F, U ARRAYS FOR INTERIOR POINTS                  
C***********************************************************************
      DO 20 L=2,N1                                                      
      X13=W(L-1)*W(L+1)                                                 
      T12=WR1(L)*X13+WR2(L)*W(L)*W(L+1)                                 
      T34=WR3(L)*X13+WR4(L)*W(L)*W(L-1)                                 
      T5=WR5(L)*X13*W(L)                                                
      A(L)=T12*FKZETA(L-1)                                              
      B(L)=-(T12+T34)*FKZETA(L)-T5*CHI1(L)                              
      C(L)=T34*FKZETA(L+1)                                              
      D1(L)=-(A(L)*AJ1(L-1)+B(L)*AJ1(L)+C(L)*AJ1(L+1)+T5*ETA1(L))       
      D2(L)=T5*DEDT(L)                                                  
   20 CONTINUE                                                          
C***********************************************************************
C   CALCULATE A, B, C, F, U ARRAYS AT BOUNDARIES                        
C***********************************************************************
      B11=B1(J)                                                         
      B12=B2(J)                                                         
      C11=W(1)*WR1(1)*(1.5*W(2)-WR3(1)*W(1))                            
      C12=WR2(1)*W(1)*W(2)                                              
      B(1)=W(2)*FKZETA(1)+B11*C11+C12*CHI1(1)                           
      C(1)=-W(2)*FKZETA(2)                                              
      D1(1)=-(B(1)*AJ1(1)+C(1)*AJ1(2)+C11*B12-C12*ETA1(1))              
      D2(1)=-C12*DEDT(1)                                                
      BN1=B1N(J)                                                        
      BN2=B2N(J)                                                        
      CN1=W(NR)*WR1(NR)*(1.5*W(N1)-WR3(NR)*W(NR))                       
      CN2=WR2(NR)*W(N1)*W(NR)                                           
      A(NR)=W(N1)*FKZETA(N1)                                            
      B(NR)=-W(N1)*FKZETA(NR)+CN1*BN1-CN2*CHI1(NR)                      
      D1(NR)=-(A(NR)*AJ1(N1)+B(NR)*AJ1(NR)+CN1*BN2+CN2*ETA1(NR))        
      D2(NR)=CN2*DEDT(NR)                                               
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE JACOB3 READS P1 AND P2 FROM DISK FILE                    
C   WORD --- DUMMY ARRAY FOR READING FILE NJCBD                         
C   P1   --- ARRAY OF E(J)                                              
C   P2   --- ARRAY OF DE(J)/DT                                          
C***********************************************************************
      SUBROUTINE JACOB3 (P1,P2)                                         
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (ND2=2*ND)                                              
      DIMENSION P1(1),P2(1)                                             
      COMMON WORD(ND2)                                                  
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      READ (NJCBD) WORD                                                 
      DO 10 L=1,NR                                                      
      P1(L)=WORD(L)                                                     
      P2(L)=WORD(NR+L)                                                  
   10 CONTINUE                                                          
      REWIND NJCBD                                                      
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE LINEQ1 SOLVES A SYSTEM OF LINEARIZED EQUATIONS           
C   BY OF GAUSSIAN ELIMINATION WITH PIVOTAL CONDENSATION                
C   A     --- N BY N ARRAY OF COEFFICIENTS                              
C   B     --- N DIMENSIONAL SOLUTION VECTOR                             
C   Z     --- DIVISION FACTORS, Z(I) = 1/A(I,I)                         
C   N     --- SIZE OF A ARRAY                                           
C   ND    --- MAXIMUM ALLOWED N                                         
C   ISING --- CONTROL PARAMETER, 2 - A IS SINGULAR, 1 - A NON-SINGULAR  
C***********************************************************************
      SUBROUTINE LINEQ1 (A,B,Z,N,ND)                                    
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      DIMENSION A(ND,ND),B(ND),Z(ND)                                    
      ISING=1                                                           
      NM1=N-1                                                           
      IF (NM1.EQ.0) GO TO 99                                            
C***********************************************************************
C   PERFORM PIVOTAL CONDENSATION                                        
C***********************************************************************
      DO 50 J=1,NM1                                                     
      J1=J+1                                                            
      LMAX=J                                                            
      RMAX=DABS(A(J,J))                                                 
      DO 10 K=J1,N                                                      
      RNEXT=DABS(A(K,J))                                                
      RMAX=RNEXT                                                        
      IF (RMAX.GE.RNEXT) GO TO 10                                       
      LMAX=K                                                            
   10 CONTINUE                                                          
      IF (LMAX.NE.J) GO TO 11                                           
      IF (A(J,J)) 22,77,22                                              
   11 CONTINUE                                                          
C***********************************************************************
C   PERFORM GAUSSIAN ELIMINATION ON DIAGONALLY DOMINANT MATRIX          
C***********************************************************************
      DO 20 L=J,N                                                       
      W=A(J,L)                                                          
      A(J,L)=A(LMAX,L)                                                  
      A(LMAX,L)=W                                                       
   20 CONTINUE                                                          
      W=B(J)                                                            
      B(J)=B(LMAX)                                                      
      B(LMAX)=W                                                         
C***********************************************************************
C   TEST FOR SINGULARITY OF COEFFICIENT MATRIX                          
C***********************************************************************
      IF (A(J,J)) 22,77,22                                              
   22 CONTINUE                                                          
      Z(J)=1.0/A(J,J)                                                   
      DO 40 K=J1,N                                                      
      IF (A(K,J)) 25,40,25                                              
   25 CONTINUE                                                          
      W=-Z(J)*A(K,J)                                                    
      DO 30 L=J1,N                                                      
      A(K,L)=W*A(J,L)+A(K,L)                                            
   30 CONTINUE                                                          
      B(K)=W*B(J)+B(K)                                                  
   40 CONTINUE                                                          
   50 CONTINUE                                                          
      IF (A(N,N)) 55,77,55                                              
   55 CONTINUE                                                          
      Z(N)=1.0/A(N,N)                                                   
      B(N)=Z(N)*B(N)                                                    
      DO 70 K=1,NM1                                                     
      J=N-K                                                             
      J1=J+1                                                            
      W=0.0                                                             
      DO 60 I=J1,N                                                      
      W=A(J,I)*B(I)+W                                                   
   60 CONTINUE                                                          
      B(J)=(B(J)-W)*Z(J)                                                
   70 CONTINUE                                                          
      RETURN                                                            
   77 CONTINUE                                                          
      ISING=2                                                           
      WRITE (6,111) ISING                                               
  111 FORMAT (1H1,20X,'COEFFICIENT MATRIX IS SINGULAR --- ISING = ',I1) 
      STOP                                                              
   99 CONTINUE                                                          
      IF (A(1,1).EQ.0.0) GO TO 77                                       
      B(1)=B(1)/A(1,1)                                                  
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE UPDATE CALCULATES THE TEMPERATURE DISTRIBUTION FOR       
C   EACH GRAIN TYPE, UPDATES THE EDDINGTON FLUX, AND THE EMISSIVITY     
C   AT EACH RADIAL AND FREQUENCY GRID POINT                             
C***********************************************************************
      SUBROUTINE UPDATE                                                 
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          
      PARAMETER (ND5=5*ND,NF5=5*NF,NY1=NDF4P1+ND5,NY2=NY1+ND5,          
     1           NY3=NY2+NF5,NY4=NY3+NF5)                               
      PARAMETER (NDF7=7*NDF)                                            
      COMMON DUMMY(NDF7)                                                
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),             
     1          ETA(NF,ND),CHICG(NF,ND),AH(NF,ND),QABS(NF,ND),          
     2          QSCA(NF,ND),BTDAV(NF,ND),BTDAVI(NF,ND),H(NF,ND),        
     3          ABUNDI(5,ND),TDI(5,ND),QABSI(NF,5),QSCAI(NF,5),         
     4          GBARI(NF,5)                                             
      EQUIVALENCE (DUMMY(1),AJ(1,1)),(DUMMY(NDF1P1),FK(1,1),QABS(1,1)), 
     1            (DUMMY(NDF2P1),ZETA(1,1),QSCA(1,1)),                  
     2            (DUMMY(NDF3P1),CHI(1,1),CHICG(1,1)),                  
     3            (DUMMY(NDF4P1),ETA(1,1),H(1,1)),                      
     4            (DUMMY(NDF5P1),BTDAV(1,1)),                           
     5            (DUMMY(NDF6P1),AH(1,1),BTDAVI(1,1)),                  
     6            (DUMMY(NDF4P1),ABUNDI(1,1)),(DUMMY(NY1),TDI(1,1)),    
     7            (DUMMY(NY2),QABSI(1,1)),(DUMMY(NY3),QSCAI(1,1)),      
     8            (DUMMY(NY4),GBARI(1,1))                               
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      ITMAXT=100                                                        
      IXX=-ITER-1                                                       
      IF (ITER.GE.9) IXX=-10                                            
      EPST=10.0**IXX                                                    
      IF (IMIX.GT.1) GO TO 11                                           
      DO 10 J=1,NFD                                                     
      DO 10 L=1,NR                                                      
      CALL GETBJ (FREQD(J),TD(L),2,BJ0,BJ1)                             
      BTDAV(J,L)=BJ0                                                    
      BTDAVI(J,L)=BJ1                                                   
   10 CONTINUE                                                          
      GO TO 66                                                          
   11 CONTINUE                                                          
C***********************************************************************
C   READ AJ FROM DISK FILE                                              
C***********************************************************************
      CALL BRANRD (NJFPD,DUMMY(1),NB1D)                                 
C***********************************************************************
C   READ ABUNDI, TDI, QABSI, QSCAI, GBARI FROM DISK FILE                
C***********************************************************************
      CALL BRANRD (NGPD,DUMMY(NDF4P1),NGP)                              
C***********************************************************************
C   CALCULATE TEMPERATURE FOR EACH GRAIN TYPE                           
C***********************************************************************
      DO 40 IG=1,IMIX                                                   
      DO 40 L=1,NR                                                      
      TT=TDI(IG,L)                                                      
      DO 30 IT=1,ITMAXT                                                 
      SNUM=0.0                                                          
      SDEN=0.0                                                          
      DO 20 J=1,NFD                                                     
      XX=QABSI(J,IG)*WFD(J)                                             
      CALL GETBJ (FREQD(J),TT,2,BJ0,BJ1)                                
C***********************************************************************
C   SET ENERGY BALANCE CONDITION                                        
C***********************************************************************
      SNUM=SNUM+XX*(BJ0-AJ(J,L))                                        
      SDEN=SDEN-XX*BJ1                                                  
   20 CONTINUE                                                          
C***********************************************************************
C   TEST FOR TEMPERATURE CONVERGENCE IN MULTIPLE GRAIN CASE             
C***********************************************************************
      DTDI=SNUM/SDEN                                                    
      ERROR=DABS(DTDI/(TT+0.5*DTDI))                                    
      IF (DTDI.GE.TT) DTDI=0.9*TT                                       
      IF (DTDI.LT.(-TT)) DTDI=-0.9*TT                                   
      TT=TT+DTDI                                                        
      IF (ERROR.LT.EPST) GO TO 33                                       
   30 CONTINUE                                                          
      WRITE (6,111) IG,L,TT,DTDI                                        
  111 FORMAT (/5X,'NO CONVERGENCE IN GETTDI --- IG = ',I2,2X,'IR = ',   
     1 I3,'TDI = ',1PE10.3,2X,'DTDI = ',1PE10.3)                        
      STOP                                                              
   33 CONTINUE                                                          
C***********************************************************************
C   UPDATE TEMPERATURE PROFILE                                          
C***********************************************************************
      TDI(IG,L)=TT                                                      
   40 CONTINUE                                                          
C***********************************************************************
C   WRITE ABUNDI, TDI, QABSI, QSCAI, GBARI ONTO DISK FILE               
C***********************************************************************
      CALL BRANWT (NGPD,DUMMY(NDF4P1),NGP)                              
C***********************************************************************
C   READ QABS FROM DISK FILE                                            
C***********************************************************************
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB1D)                            
      DO 60 J=1,NFD                                                     
      DO 60 L=1,NR                                                      
      SUM=0.0                                                           
C***********************************************************************
C   CALCULATE THERMAL EMISSION TERM                                     
C***********************************************************************
      DO 50 IG=1,IMIX                                                   
      CALL GETBJ (FREQD(J),TDI(IG,L),0,BJ0,BJ1)                         
      SUM=SUM+ABUNDI(IG,L)*QABSI(J,IG)*BJ0                              
   50 CONTINUE                                                          
      BTDAV(J,L)=R0*RHOD(L)*SUM/QABS(J,L)                               
C***********************************************************************
C   CALCULATE B, (DB/DT)/B                                              
C***********************************************************************
      CALL GETBJ (FREQD(J),TD(L),3,BJ0,BJ10)                            
      BTDAVI(J,L)=BTDAV(J,L)*BJ10                                       
   60 CONTINUE                                                          
   66 CONTINUE                                                          
C***********************************************************************
C   WRITE BTDAV, BTDAVI TO DISK FILE                                    
C***********************************************************************
      CALL BRANWT (NBTAD,DUMMY(NDF5P1),NB2D)                            
      IF (ISCA.EQ.0) GO TO 115                                          
C***********************************************************************
C   READ AJ, FK, ZETA FROM DISK FILE                                    
C***********************************************************************
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 
C***********************************************************************
C   READ CHICG FROM DISK FILE                                           
C***********************************************************************
      CALL BRANRD (NCGD,DUMMY(NDF3P1),NB1D)                             
C***********************************************************************
C   READ AH FROM DISK FILE                                              
C***********************************************************************
      CALL BRANRD (NAHD,DUMMY(NDF6P1),NB1D)                             
C***********************************************************************
C   UPDATE BOUNDARY CONSTANTS AND FLUX                                  
C***********************************************************************
      CALL BCD (B1,B2,B1N,B2N)                                          
      DO 70 J=1,NFD                                                     
      H(J,1)=-(B1(J)*AJ(J,1)+B2(J))                                     
      H(J,NR)=-(B1N(J)*AJ(J,NR)+B2N(J))                                 
   70 CONTINUE                                                          
C***********************************************************************
C   UPDATE FLUX FOR SPHERICAL AND CYLINDRICAL CASES                     
C***********************************************************************
      DO 80 L=2,N1                                                      
      WR12=WR1(L)*WR1(L)                                                
      WR32=WR3(L)*WR3(L)                                                
      WR5R2I=1.0/(WR5(L)*(R(L)**IGEOM))                                 
      WT1=WR12*WR5R2I                                                   
      WT2=(WR32-WR12)*WR5R2I                                            
      WT3=-WR32*WR5R2I                                                  
      DO 80 J=1,NFD                                                     
      FPJ1=FK(J,L-1)*ZETA(J,L-1)*AJ(J,L-1)                              
      FPJ2=FK(J,L)*ZETA(J,L)*AJ(J,L)                                    
      FPJ3=FK(J,L+1)*ZETA(J,L+1)*AJ(J,L+1)                              
      H(J,L)=(WT1*FPJ1+WT2*FPJ2+WT3*FPJ3)/(ZETA(J,L)*CHICG(J,L))        
   80 CONTINUE                                                          
      IF (ITER.EQ.1) GO TO 101                                          
      L1=1                                                              
C***********************************************************************
C  FOR IC=0 MONOCHROMATIC FLUX AT CENTER MUST VANISH BY SYMMETRY        
C***********************************************************************
      IF (IC.EQ.0) DHD(1)=0.0                                           
      IF (IC.EQ.0) L1=2                                                 
      DO 90 L=L1,NR                                                     
      DO 90 J=1,NFD                                                     
      XX=(AH(J,L)-H(J,L))/(0.5*(AH(J,L)+H(J,L)))                        
      DHD(L)=DMAX1(DABS(XX),DABS(DHD(L)))                               
   90 CONTINUE                                                          
  101 CONTINUE                                                          
C***********************************************************************
C   UPDATE AND WRITE AH ONTO DISK FILE                                  
C***********************************************************************
      DO 110 L=1,NR                                                     
      DO 110 J=1,NFD                                                    
      AH(J,L)=H(J,L)                                                    
  110 CONTINUE                                                          
      CALL BRANWT (NAHD,DUMMY(NDF6P1),NB1D)                             
  115 CONTINUE                                                          
C***********************************************************************
C   READ AJ FROM DISK FILE IF ISCA=0                                    
C***********************************************************************
      IF (ISCA.EQ.0) CALL BRANRD (NJFPD,DUMMY(1),NB1D)                  
C***********************************************************************
C   READ QABS FROM DISK FILE                                            
C***********************************************************************
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB2D)                            
C***********************************************************************
C   READ CHI FROM DISK FILE                                             
C***********************************************************************
      CALL BRANRD (NCED,DUMMY(NDF3P1),NB1D)                             
C***********************************************************************
C   UPDATE EMISSIVITY                                                   
C***********************************************************************
      DO 120 J=1,NFD                                                    
      DO 120 L=1,NR                                                     
      ETA(J,L)=QSCA(J,L)*AJ(J,L)+QABS(J,L)*BTDAV(J,L)                   
  120 CONTINUE                                                          
C***********************************************************************
C   WRITE CHI, ETA ONTO DISK FILE                                       
C***********************************************************************
      CALL BRANWT (NCED,DUMMY(NDF3P1),NB2D)                             
C***********************************************************************
C   SOLVE RAY EQUATIONS TO DETERMINE "INTENSITY" U, CONFIGURATION       
C   FUNCTION, AND ANISOTROPY FACTOR                                     
C***********************************************************************
      IF (IGEOM.EQ.0) CALL EDFTPL                                       
      IF (IGEOM.EQ.1) CALL EDFTCY                                       
      IF (IGEOM.EQ.2) CALL EDFTSP                                       
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE EDFTPL SOLVES THE RAY EQUATIONS TO DETERMINE THE         
C   INTENSITY-LIKE FUNCTION U, THE EMERGENT INTENSITY, THE ANISOTROPY   
C   FACTOR, THE BOUNDARY FACTORS, AND THE CONFIGURATION FUNCTION FOR    
C   THE PLANAR CASE                                                     
C***********************************************************************
      SUBROUTINE EDFTPL                                                 
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1,          
     3           NDF8M=NDF8P1-1)                                        
      PARAMETER (NFC1P1=NFC2+1,NFC2P1=2*NFC2+1,NFC3P1=3*NFC2+1,         
     1           NFC4P1=4*NFC2+1,NFC5P1=5*NFC2+1,                       
     2           NFC6P1=6*NFC2+1,NFC7P1=7*NFC2+1,                       
     3           NFC8P1=8*NFC2+1,NCEIC=NFC8P1+NDF)                      
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1)                     
      PARAMETER (NXY1=9*NDF+NFC2,NTHETA=20)                             
      COMMON QW(NQWT0),DUMMY(NXY1),AVB(NF),AVC(NF),DP(ND),Y(ND)         
      DIMENSION WZ1(NQWT),WZ2(NQWT),WZ3(NQWT),WJ(NQWT),                 
     1          WK(NQWT),WB(NPMAX),WC(NC1),XMUSQ(NQWT),P2R3(NQWT),      
     2          WZETA1(ND),WZETA2(ND)                                   
      DIMENSION FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),ETA(NF,ND),AH(NF,ND),  
     1          CGBAR(NF,ND),AH0(NF,ND),AH1(NF,ND),AVJ(NF,ND),          
     2          AVK(NF,ND),G(NF,ND),H(NF,ND),U(NF,ND),EI(NF,ND),        
     3          EIC(NF,NC2)                                             
      EQUIVALENCE (QW(1),WZ1(1)),(QW(NQWT1),WZ2(1)),(QW(NQWT2),WZ3(1)), 
     1            (QW(NQWT3),WJ(1)),(QW(NQWT4),WK(1)),(QW(NQWT5),WB(1)),
     2            (QW(NQWT6),WC(1)),(QW(NQWT7),XMUSQ(1)),               
     3            (QW(NQWT8),P2R3(1)),(QW(NQWT9),WZETA1(1)),            
     4            (QW(NQWT10),WZETA2(1))                                
      EQUIVALENCE (DUMMY(1),CHI(1,1)),(DUMMY(NDF1P1),FK(1,1),ETA(1,1)), 
     1            (DUMMY(NDF2P1),ZETA(1,1),AH(1,1),AH0(1,1)),           
     2            (DUMMY(NDF3P1),CGBAR(1,1),AH1(1,1)),                  
     3            (DUMMY(NDF4P1),AVJ(1,1)),(DUMMY(NDF5P1),AVK(1,1)),    
     4            (DUMMY(NDF6P1),G(1,1),U(1,1)),(DUMMY(NDF7P1),H(1,1)), 
     5            (DUMMY(NDF8P1),EI(1,1)),(DUMMY(NDF9P1),EIC(1,1))      
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      IF ((ITCONT*ITCONJ).NE.0) ITCON=1                                 
      IF (IEDFTR.EQ.0.AND.ITCON.EQ.0) GO TO 258                         
C***********************************************************************
C   READ CHI, ETA FROM DISK FILE                                        
C***********************************************************************
      CALL BRANRD (NCED,DUMMY(1),NB2D)                                  
C***********************************************************************
C   READ AH FROM DISK FILE                                              
C***********************************************************************
      CALL BRANRD (NAHD,DUMMY(NDF2P1),NB1D)                             
C***********************************************************************
C   READ CGBAR FROM DISK FILE                                           
C***********************************************************************
      CALL BRANRD (NGBD,DUMMY(NDF3P1),NB1D)                             
C***********************************************************************
C   READ QUADRATURE WEIGHTS FROM DISK FILE                              
C***********************************************************************
      CALL BRANRD (NEFQW,QW(1),NQW)                                     
C***********************************************************************
C   CALCULATE BOUNDARY CONSTANTS FOR RAY EQUATIONS                      
C***********************************************************************
      CALL EFBC (B1,B2,B1N,B2N)                                         
C***********************************************************************
C   INITIALIZE INTEGRALS AND ANISOTROPIC SCATTERING FUNCTION            
C***********************************************************************
      DO 10 L=1,NR                                                      
      DO 10,J=1,NFD                                                     
      AVJ(J,L)=0.0                                                      
      AVK(J,L)=0.0                                                      
      AH0(J,L)=3.0*CGBAR(J,L)*AH(J,L)/CHI(J,L)                          
   10 CONTINUE                                                          
      IF (ISCA.NE.0) GO TO 22                                           
C***********************************************************************
C   INITIALIZE ANISOTROPIC SCATTERING FUNCTION FOR ISCA=0               
C***********************************************************************
      DO 20 L=1,NR                                                      
      DO 20 J=1,NFD                                                     
      AH0(J,L)=0.0                                                      
      AH1(J,L)=0.0                                                      
   20 CONTINUE                                                          
      GO TO 42                                                          
   22 CONTINUE                                                          
C***********************************************************************
C   CALCULATE FLUX AT INNER AND OUTER BOUNDARIES                        
C***********************************************************************
      F=R(3)-R(2)                                                       
      B=R(2)-R(1)                                                       
      BF=B+F                                                            
      BFI=1.0/(B*F*BF)                                                  
      W1=-F*(BF+B)*BFI                                                  
      W2=BF*BF*BFI                                                      
      W3=-B*B*BFI                                                       
      F=R(NR)-R(N1)                                                     
      B=R(N1)-R(NR-2)                                                   
      BF=B+F                                                            
      BFI=1.0/(B*F*BF)                                                  
      W1N=F*F*BFI                                                       
      W2N=-BF*BF*BFI                                                    
      W3N=B*(BF+F)*BFI                                                  
      DO 30 J=1,NFD                                                     
      AH1(J,1)=W1*AH0(J,1)+W2*AH0(J,2)+W3*AH0(J,3)                      
      AH1(J,NR)=W1N*AH0(J,NR-2)+W2N*AH0(J,N1)+W3N*AH0(J,NR)             
   30 CONTINUE                                                          
C***********************************************************************
C   CALCULATE FLUX AT INTERIOR POINTS                                   
C***********************************************************************
      DO 40 L=2,N1                                                      
      WR12=WR1(L)*WR1(L)                                                
      WR32=WR3(L)*WR3(L)                                                
      WR5I=1.0/WR5(L)                                                   
      WT1=-WR12*WR5I                                                    
      WT2=(WR12-WR32)*WR5I                                              
      WT3=WR32*WR5I                                                     
      DO 40 J=1,NFD                                                     
      AH1(J,L)=WT1*AH0(J,L-1)+WT2*AH0(J,L)+WT3*AH0(J,L+1)               
   40 CONTINUE                                                          
   42 CONTINUE                                                          
C***********************************************************************
C   INITIALIZE H INTEGRALS                                              
C***********************************************************************
      DO 50 J=1,NFD                                                     
      AVB(J)=0.0                                                        
      AVC(J)=0.0                                                        
   50 CONTINUE                                                          
      IZ=0                                                              
C***********************************************************************
C   SOLVE RAY EQUATIONS TO DETERMINE THE "INTENSITY" U                  
C***********************************************************************
      DO 170 IP=1,NTHETA                                                
      IZ=IZ+1                                                           
      XMU=DSQRT(XMUSQ(IP))                                              
      DO 70 J=1,NFD                                                     
C***********************************************************************
C   CON1 AND CON2 COMPUTE SCATTERING COEFFICIENTS                       
C***********************************************************************
      CON1=AH0(J,1)*XMU                                                 
      CON2=XMUSQ(IP)*AH1(J,1)                                           
      C11=CHI(J,1)*(3.0*CHI(J,2)-CHI(J,1))*WZ1(IZ)                      
      C12=CHI(J,1)*CHI(J,2)*WZ2(IZ)                                     
      B1I=1.0/(CHI(J,2)+CHI(J,1)*C12+B1(J)*C11)                         
      G(J,1)=((ETA(J,1)-CON2)*C12-(B2(J)+CON1)*C11)*B1I                 
      H(J,1)=CHI(J,2)*B1I                                               
   70 CONTINUE                                                          
      DO 80 L=2,N1                                                      
      IZ=IZ+1                                                           
      DO 80 J=1,NFD                                                     
      CON2=XMUSQ(IP)*AH1(J,L)                                           
      X13=CHI(J,L-1)*CHI(J,L+1)                                         
      T=WZ2(IZ)*X13*CHI(J,L)                                            
      AL=WZ1(IZ)*(X13+CHI(J,L)*CHI(J,L+1))                              
      CL=WZ3(IZ)*(X13+CHI(J,L)*CHI(J,L-1))                              
      BL=-(AL+CL+T*CHI(J,L))                                            
      DL=-T*(ETA(J,L)-CON2)                                             
      EL=1.0/(BL+AL*H(J,L-1))                                           
      G(J,L)=(DL-AL*G(J,L-1))*EL                                        
      H(J,L)=-CL*EL                                                     
   80 CONTINUE                                                          
      IZ=IZ+1                                                           
      DO 90 J=1,NFD                                                     
      CON1=AH0(J,NR)*XMU                                                
      CON2=XMUSQ(IP)*AH1(J,NR)                                          
      CN1=CHI(J,NR)*(3.0*CHI(J,N1)-CHI(J,NR))*WZ1(IZ)                   
      CN2=CHI(J,N1)*CHI(J,NR)*WZ2(IZ)                                   
      AN=CHI(J,N1)                                                      
      BN=-CHI(J,N1)+B1N(J)*CN1-CHI(J,NR)*CN2                            
      DN=-((B2N(J)+CON1)*CN1+(ETA(J,NR)-CON2)*CN2)                      
      U(J,NR)=(DN-AN*G(J,N1))/(BN+AN*H(J,N1))                           
   90 CONTINUE                                                          
      LL=NR+1                                                           
      DO 110 L=2,NR                                                     
      DO 110 J=1,NFD                                                    
      U(J,LL-L)=G(J,LL-L)+H(J,LL-L)*U(J,LL-L+1)                         
  110 CONTINUE                                                          
C***********************************************************************
C   COMPUTE H INTEGRAL AT OUTER BOUNDARY                                
C***********************************************************************
      DO 130 J=1,NFD                                                    
      AVB(J)=AVB(J)+WB(IP)*U(J,NR)                                      
  130 CONTINUE                                                          
      IF (IC.EQ.0) GO TO 145                                            
C***********************************************************************
C   COMPUTE H INTEGRAL AT INNER BOUNDARY                                
C***********************************************************************
      DO 140 J=1,NFD                                                    
      AVC(J)=AVC(J)+WC(IP)*U(J,1)                                       
  140 CONTINUE                                                          
  145 CONTINUE                                                          
C***********************************************************************
C   COMPUTE J AND K INTEGRALS                                           
C***********************************************************************
      DO 150 L=1,NR                                                     
      DO 150 J=1,NFD                                                    
      AVJ(J,L)=AVJ(J,L)+WJ(IP)*U(J,L)                                   
      AVK(J,L)=AVK(J,L)+WK(IP)*U(J,L)                                   
  150 CONTINUE                                                          
      IF (ITCON.NE.1) GO TO 170                                         
C***********************************************************************
C   COMPUTE EMERGENT INTENSITIES IF CONVERGENCE HAS OCCURRED            
C***********************************************************************
      DO 160 J=1,NFD                                                    
      EI(J,IP)=U(J,NR)-(B1N(J)*U(J,NR)+B2N(J))                          
  160 CONTINUE                                                          
  170 CONTINUE                                                          
      IF (IEDFTR.EQ.0) GO TO 215                                        
C***********************************************************************
C   COMPUTE BOUNDARY FACTORS                                            
C***********************************************************************
      DO 190 J=1,NFD                                                    
      FCD(J)=AVC(J)/AVJ(J,1)                                            
      FBD(J)=AVB(J)/AVJ(J,NR)                                           
  190 CONTINUE                                                          
C***********************************************************************
C   COMPUTE ANISOTROPY FACTORS AND CONFIGURATION FUNCTION               
C***********************************************************************
      DO 210 L=1,NR                                                     
      DO 210 J=1,NFD                                                    
      FK(J,L)=AVK(J,L)/AVJ(J,L)                                         
      ZETA(J,L)=1.0                                                     
  210 CONTINUE                                                          
  215 CONTINUE                                                          
      IF (ITCON.EQ.0) GO TO 216                                         
C***********************************************************************
C   WRITE EI ONTO DISK FILE                                             
C***********************************************************************
      CALL BRANWT (NEID,DUMMY(NDF8P1),NB1D)                             
C***********************************************************************
C   WRITE EIC ONTO DISK FILE                                            
C***********************************************************************
  216 CONTINUE                                                          
      IF (IEDFTR.EQ.0) RETURN                                           
C***********************************************************************
C   WRITE AJ, FK, ZETA ONTO DISK FILE                                   
C***********************************************************************
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 
  258 CONTINUE                                                          
C***********************************************************************
C   UPDATE AJ, AH, ETA                                                  
C***********************************************************************
      CALL GETAJ                                                        
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE EDFTSP SOLVES THE RAY EQUATIONS TO DETERMINE THE         
C   INTENSITY-LIKE FUNCTION U, THE EMERGENT INTENSITY, THE ANISOTROPY   
C   FACTOR, THE BOUNDARY FACTORS, AND THE CONFIGURATION FUNCTION FOR    
C   THE SPHERICAL CASE                                                  
C***********************************************************************
      SUBROUTINE EDFTSP                                                 
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1)          
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1)                     
      PARAMETER (NXY1=9*NDF+NFC2)                                       
      COMMON QW(NQWT0),DUMMY(NXY1),AVB(NF),AVC(NF),DP(ND),Y(ND)         
      DIMENSION WZ1(NQWT),WZ2(NQWT),WZ3(NQWT),WJ(NQWT),                 
     1          WK(NQWT),WB(NPMAX),WC(NC1),XMUSQ(NQWT),P2R3(NQWT),      
     2          WZETA1(ND),WZETA2(ND)                                   
      DIMENSION FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),                       
     1          ETA(NF,ND),AH(NF,ND),CGBAR(NF,ND),EIC(NF,NC2),          
     2          AH0(NF,ND),AH1(NF,ND)                                   
      DIMENSION AVJ(NF,ND),AVK(NF,ND),G(NF,ND),H(NF,ND),U(NF,ND),       
     1          EI(NF,ND)                                               
      EQUIVALENCE (QW(1),WZ1(1)),(QW(NQWT1),WZ2(1)),(QW(NQWT2),WZ3(1)), 
     1            (QW(NQWT3),WJ(1)),(QW(NQWT4),WK(1)),(QW(NQWT5),WB(1)),
     2            (QW(NQWT6),WC(1)),(QW(NQWT7),XMUSQ(1)),               
     3            (QW(NQWT8),P2R3(1)),(QW(NQWT9),WZETA1(1)),            
     4            (QW(NQWT10),WZETA2(1))                                
      EQUIVALENCE (DUMMY(1),CHI(1,1)),(DUMMY(NDF1P1),FK(1,1),ETA(1,1)), 
     1            (DUMMY(NDF2P1),ZETA(1,1),AH(1,1),AH0(1,1)),           
     2            (DUMMY(NDF3P1),CGBAR(1,1),AH1(1,1)),                  
     3            (DUMMY(NDF4P1),AVJ(1,1)),(DUMMY(NDF5P1),AVK(1,1)),    
     4            (DUMMY(NDF6P1),G(1,1),U(1,1)),(DUMMY(NDF7P1),H(1,1)), 
     5            (DUMMY(NDF8P1),EI(1,1)),(DUMMY(NDF9P1),EIC(1,1))      
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      IF ((ITCONT*ITCONJ).NE.0) ITCON=1                                 
      IF (IEDFTR.EQ.0.AND.ITCON.EQ.0) GO TO 258                         
C***********************************************************************
C   READ CHI, ETA FROM DISK FILE                                        
C***********************************************************************
      CALL BRANRD (NCED,DUMMY(1),NB2D)                                  
C***********************************************************************
C   READ AH FROM DISK FILE                                              
C***********************************************************************
      CALL BRANRD (NAHD,DUMMY(NDF2P1),NB1D)                             
C***********************************************************************
C   READ CGBAR FROM DISK FILE                                           
C***********************************************************************
      CALL BRANRD (NGBD,DUMMY(NDF3P1),NB1D)                             
C***********************************************************************
C   READ QUADRATURE WEIGHTS FROM DISK FILE                              
C***********************************************************************
      CALL BRANRD (NEFQW,QW(1),NQW)                                     
C***********************************************************************
C   CALCULATE BOUNDARY CONSTANTS FOR RAY EQUATIONS                      
C***********************************************************************
      CALL EFBC (B1,B2,B1N,B2N)                                         
C***********************************************************************
C   INITIALIZE INTEGRALS AND ANISOTROPIC SCATTERING FUNCTION            
C***********************************************************************
      DO 10 L=1,NR                                                      
      DO 10,J=1,NFD                                                     
      AVJ(J,L)=0.0                                                      
      AVK(J,L)=0.0                                                      
      AH0(J,L)=3.0*CGBAR(J,L)*AH(J,L)/CHI(J,L)                          
   10 CONTINUE                                                          
      IF (ISCA.NE.0) GO TO 22                                           
C***********************************************************************
C   INITIALIZE ANISOTROPIC SCATTERING FUNCTION FOR ISCA=0               
C***********************************************************************
      DO 20 L=1,NR                                                      
      DO 20 J=1,NFD                                                     
      AH0(J,L)=0.0                                                      
      AH1(J,L)=0.0                                                      
   20 CONTINUE                                                          
      GO TO 42                                                          
   22 CONTINUE                                                          
C***********************************************************************
C   CALCULATE FLUX PROFILE                                              
C***********************************************************************
      F=R(3)-R(2)                                                       
      B=R(2)-R(1)                                                       
      BF=B+F                                                            
      BFI=1.0/(B*F*BF)                                                  
      W1=-F*(BF+B)*BFI                                                  
      W2=BF*BF*BFI                                                      
      W3=-B*B*BFI                                                       
      F=R(NR)-R(N1)                                                     
      B=R(N1)-R(NR-2)                                                   
      BF=B+F                                                            
      BFI=1.0/(B*F*BF)                                                  
      W1N=F*F*BFI                                                       
      W2N=-BF*BF*BFI                                                    
      W3N=B*(BF+F)*BFI                                                  
      DO 30 J=1,NFD                                                     
      AH1(J,1)=W1*AH0(J,1)+W2*AH0(J,2)+W3*AH0(J,3)                      
      AH1(J,NR)=W1N*AH0(J,NR-2)+W2N*AH0(J,N1)+W3N*AH0(J,NR)             
   30 CONTINUE                                                          
C***********************************************************************
C   CALCULATE FLUX AT INTERIOR POINTS FOR SPHERICAL GEOMETRY            
C***********************************************************************
      DO 40 L=2,N1                                                      
      WR12=WR1(L)*WR1(L)                                                
      WR32=WR3(L)*WR3(L)                                                
      WR5R2I=1.0/(WR5(L)*R(L)*R(L))                                     
      WT1=-WR12*WR5R2I                                                  
      WT2=(WR12-WR32)*WR5R2I                                            
      WT3=WR32*WR5R2I                                                   
      DO 40 J=1,NFD                                                     
      AH1(J,L)=WT1*AH0(J,L-1)+WT2*AH0(J,L)+WT3*AH0(J,L+1)               
   40 CONTINUE                                                          
   42 CONTINUE                                                          
C***********************************************************************
C   INITIALIZE H INTEGRALS                                              
C***********************************************************************
      DO 50 J=1,NFD                                                     
      AVB(J)=0.0                                                        
      AVC(J)=0.0                                                        
   50 CONTINUE                                                          
      MB=-NR                                                            
      IZ=0                                                              
C***********************************************************************
C   SOLVE RAY EQUATIONS TO DETERMINE THE "INTENSITY" U                  
C***********************************************************************
      DO 170 IP=1,NP                                                    
      IF (IP-IP1) 54,54,55                                              
   54 CONTINUE                                                          
      IR0=1                                                             
      JR=0                                                              
      MW=NR                                                             
      GO TO 66                                                          
   55 CONTINUE                                                          
      IR0=IR0+1                                                         
      JR=IP-IP1                                                         
      MW=MW-1                                                           
      DO 60 J=1,NFD                                                     
      B1(J)=0.0                                                         
      B2(J)=0.0                                                         
   60 CONTINUE                                                          
   66 CONTINUE                                                          
      IR1=IR0+1                                                         
      MB=MB+MW                                                          
      IF (IP.EQ.NP) GO TO 115                                           
      IZ=IZ+1                                                           
      XMU=DSQRT(XMUSQ(IZ))                                              
      DO 70 J=1,NFD                                                     
C***********************************************************************
C   CON1 AND CON2 COMPUTE SCATTERING COEFFICIENTS                       
C***********************************************************************
      CON1=AH0(J,IR0)*XMU                                               
      CON2=XMUSQ(IZ)*AH1(J,IR0)+AH0(J,IR0)*P2R3(IZ)                     
      C11=CHI(J,IR0)*(3.0*CHI(J,IR1)-CHI(J,IR0))*WZ1(IZ)                
      C12=CHI(J,IR0)*CHI(J,IR1)*WZ2(IZ)                                 
      B1I=1.0/(CHI(J,IR1)+CHI(J,IR0)*C12+B1(J)*C11)                     
      G(J,IR0)=((ETA(J,IR0)-CON2)*C12-(B2(J)+CON1)*C11)*B1I             
      H(J,IR0)=CHI(J,IR1)*B1I                                           
   70 CONTINUE                                                          
      IF (IR0.EQ.N1) GO TO 88                                           
      DO 80 L=IR1,N1                                                    
      IZ=IZ+1                                                           
      DO 80 J=1,NFD                                                     
      CON2=XMUSQ(IZ)*AH1(J,L)+AH0(J,L)*P2R3(IZ)                         
      X13=CHI(J,L-1)*CHI(J,L+1)                                         
      T=WZ2(IZ)*X13*CHI(J,L)                                            
      AL=WZ1(IZ)*(X13+CHI(J,L)*CHI(J,L+1))                              
      CL=WZ3(IZ)*(X13+CHI(J,L)*CHI(J,L-1))                              
      BL=-(AL+CL+T*CHI(J,L))                                            
      DL=-T*(ETA(J,L)-CON2)                                             
      EL=1.0/(BL+AL*H(J,L-1))                                           
      G(J,L)=(DL-AL*G(J,L-1))*EL                                        
      H(J,L)=-CL*EL                                                     
   80 CONTINUE                                                          
   88 CONTINUE                                                          
      IZ=IZ+1                                                           
      XMU=DSQRT(XMUSQ(IZ))                                              
      DO 90 J=1,NFD                                                     
      CON1=AH0(J,NR)*XMU                                                
      CON2=XMUSQ(IZ)*AH1(J,NR)+AH0(J,NR)*P2R3(IZ)                       
      CN1=CHI(J,NR)*(3.0*CHI(J,N1)-CHI(J,NR))*WZ1(IZ)                   
      CN2=CHI(J,N1)*CHI(J,NR)*WZ2(IZ)                                   
      AN=CHI(J,N1)                                                      
      BN=-CHI(J,N1)+B1N(J)*CN1-CHI(J,NR)*CN2                            
      DN=-((B2N(J)+CON1)*CN1+(ETA(J,NR)-CON2)*CN2)                      
      U(J,NR)=(DN-AN*G(J,N1))/(BN+AN*H(J,N1))                           
   90 CONTINUE                                                          
      LL=NR+IR0                                                         
      DO 110 L=IR1,NR                                                   
      DO 110 J=1,NFD                                                    
      U(J,LL-L)=G(J,LL-L)+H(J,LL-L)*U(J,LL-L+1)                         
  110 CONTINUE                                                          
      GO TO 125                                                         
  115 CONTINUE                                                          
      DO 120 J=1,NFD                                                    
      U(J,NR)=B2N(J)                                                    
  120 CONTINUE                                                          
  125 CONTINUE                                                          
C***********************************************************************
C   COMPUTE H INTEGRAL AT OUTER BOUNDARY                                
C***********************************************************************
      DO 130 J=1,NFD                                                    
      AVB(J)=AVB(J)+WB(IP)*U(J,NR)                                      
  130 CONTINUE                                                          
      IF (IC.EQ.0.OR.IP.GT.IP1) GO TO 145                               
C***********************************************************************
C   COMPUTE H INTEGRAL AT INNER BOUNDARY                                
C***********************************************************************
      DO 140 J=1,NFD                                                    
      AVC(J)=AVC(J)+WC(IP)*U(J,IR0)                                     
  140 CONTINUE                                                          
  145 CONTINUE                                                          
      JR=JR+1                                                           
      WJT=WJ(JR+MB)                                                     
      WKT=WK(JR+MB)                                                     
C***********************************************************************
C   COMPUTE J AND K INTEGRALS                                           
C***********************************************************************
      DO 150 J=1,NFD                                                    
      AVJ(J,JR)=AVJ(J,JR)+WJT*U(J,JR)                                   
      AVK(J,JR)=AVK(J,JR)+WKT*U(J,JR)                                   
  150 CONTINUE                                                          
      IF (JR.LT.NR) GO TO 145                                           
      IF (ITCON.NE.1) GO TO 170                                         
C***********************************************************************
C   COMPUTE EMERGENT INTENSITIES IF CONVERGENCE HAS OCCURRED            
C***********************************************************************
      DO 160 J=1,NFD                                                    
      UVNR=U(J,NR)-(B1N(J)*U(J,NR)+B2N(J))                              
      IF (IC.EQ.0) EI(J,IP)=UVNR                                        
      IF (IC.EQ.0) GO TO 160                                            
      IF (IP.EQ.1) EI(J,1)=UVNR                                         
      IF (IP.LE.(IP1+1)) EIC(J,IP)=UVNR                                 
      IF (IP.GE.IP1.AND.IP.LT.NP) EI(J,IP-IP0+1)=UVNR                   
  160 CONTINUE                                                          
  170 CONTINUE                                                          
      IF (ITCON.NE.1.OR.IC.NE.0) GO TO 185                              
      DO 180 J=1,NFD                                                    
      EI(J,NR)=B2N(J)                                                   
  180 CONTINUE                                                          
  185 CONTINUE                                                          
      IF (IEDFTR.EQ.0) GO TO 215                                        
C***********************************************************************
C   COMPUTE BOUNDARY FACTORS                                            
C***********************************************************************
      DO 190 J=1,NFD                                                    
      FCD(J)=AVC(J)/AVJ(J,1)                                            
      FBD(J)=AVB(J)/AVJ(J,NR)                                           
  190 CONTINUE                                                          
C***********************************************************************
C   COMPUTE ANISOTROPY FACTORS                                          
C***********************************************************************
      DO 210 L=1,NR                                                     
      DO 210 J=1,NFD                                                    
      FK(J,L)=AVK(J,L)/AVJ(J,L)                                         
  210 CONTINUE                                                          
  215 CONTINUE                                                          
      IF (ITCON.EQ.0) GO TO 216                                         
C***********************************************************************
C   WRITE EI ONTO DISK FILE                                             
C***********************************************************************
      CALL BRANWT (NEID,DUMMY(NDF8P1),NB1D)                             
C***********************************************************************
C   WRITE EIC ONTO DISK FILE                                            
C***********************************************************************
      IF (IC.NE.0) CALL BRANWT (NEICD,DUMMY(NDF9P1),NFC2)               
  216 CONTINUE                                                          
      IF (IEDFTR.EQ.0) RETURN                                           
C***********************************************************************
C   COMPUTE CONFIGURATION FUNCTION                                      
C***********************************************************************
      DO 250 J=1,NFD                                                    
      DO 220 L=1,NR                                                     
      Y(L)=3.0-1.0/FK(J,L)                                              
  220 CONTINUE                                                          
      DO 230 L=1,N1                                                     
      DP(L)=WZETA1(L)*Y(L)+WZETA2(L)*Y(L+1)                             
  230 CONTINUE                                                          
      Y(1)=0.0                                                          
      DO 240 L=2,NR                                                     
      Y(L)=Y(L-1)+DP(L-1)                                               
  240 CONTINUE                                                          
      DO 250 L=1,NR                                                     
      ZETA(J,L)=DEXP(Y(L))                                              
  250 CONTINUE                                                          
C***********************************************************************
C   WRITE AJ, FK, ZETA ONTO DISK FILE                                   
C***********************************************************************
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 
  258 CONTINUE                                                          
C***********************************************************************
C   UPDATE AJ, AH, ETA                                                  
C***********************************************************************
      CALL GETAJ                                                        
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE EDFTCY SOLVES THE RAY EQUATIONS TO DETERMINE THE         
C   INTENSITY-LIKE FUNCTION U, THE EMERGENT INTENSITY, THE ANISOTROPY   
C   FACTOR, THE BOUNDARY FACTORS, AND THE CONFIGURATION FUNCTION FOR    
C   THE CYLINDRICAL CASE                                                
C***********************************************************************
      SUBROUTINE EDFTCY                                                 
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1,          
     3           NDF8M=NDF8P1-1)                                        
      PARAMETER (NFC1P1=NFC2+1,NFC2P1=2*NFC2+1,NFC3P1=3*NFC2+1,         
     1           NFC4P1=4*NFC2+1,NFC5P1=5*NFC2+1,                       
     2           NFC6P1=6*NFC2+1,NFC7P1=7*NFC2+1,                       
     3           NFC8P1=8*NFC2+1,NCEIC=NFC8P1+NDF)                      
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1,                     
     6           NQWT61=NQWT5+NQWT,NQWT99=12*NQWT,NQWT91=9*NQWT+1)      
      PARAMETER (NXY1=9*NDF+NFC2,NTHETA=20)                             
      PARAMETER (NQG=4,NQGT=NQWT2+NQG,NQGT1=NQGT+NQG,                   
     1           NQGT2=NQGT1+NQG,NCQW=NQGT2+NQG-1)                      
      COMMON QW(NQWT0),DUMMY(NXY1),AVB(NF),AVC(NF),DP(ND),Y(ND)         
      DIMENSION WZ1(NQWT),WZ2(NQWT),WZ3(NQWT),WJ(NQWT),                 
     1          WK(NQWT),WB(NPMAX),WC(NC1),XMUSQ(NQWT),P2R3(NQWT),      
     2          WZETA1(ND),WZETA2(ND),WY(NQWT),WH(NQWT)                 
      DIMENSION FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),ETA(NF,ND),AH(NF,ND),  
     1          CGBAR(NF,ND),AH0(NF,ND),AH1(NF,ND),AVP(NF,ND),          
     2          AVY(NF,ND),AY0(NF,ND),GY(NF,ND),HY(NF,ND),UY(NF,ND)     
      DIMENSION AVJ(NF,ND),AVK(NF,ND),AVQ(NF,ND),G(NF,ND),H(NF,ND),     
     1          U(NF,ND),QRAT(NF,ND),EI(NF,ND),EIC(NF,NC2),AP0(NF,ND)   
      DIMENSION WJO(NQG),WHO(NQG),WKO(NQG),WYO(NQG)                     
      EQUIVALENCE (QW(1),WZ1(1)),(QW(NQWT1),WZ2(1)),(QW(NQWT2),WZ3(1)), 
     1            (QW(NQWT3),WJ(1)),(QW(NQWT4),WK(1)),(QW(NQWT5),WB(1)),
     2            (QW(NQWT6),WC(1)),(QW(NQWT7),XMUSQ(1)),               
     3            (QW(NQWT8),P2R3(1)),(QW(NQWT9),WZETA1(1)),            
     4            (QW(NQWT10),WZETA2(1))                                
      EQUIVALENCE (CQW(1),WH(1)),(CQW(NQWT1),WY(1)),(CQW(NQWT2),WJO(1)),
     1            (CQW(NQGT),WHO(1)),(CQW(NQGT1),WKO(1)),               
     2            (CQW(NQGT2),WYO(1))                                   
      EQUIVALENCE (DUMMY(1),CHI(1,1)),(DUMMY(NDF1P1),FK(1,1),ETA(1,1)), 
     1            (DUMMY(NDF2P1),ZETA(1,1),AH(1,1),AH0(1,1)),           
     2            (DUMMY(NDF3P1),CGBAR(1,1),AH1(1,1)),                  
     3            (DUMMY(NDF4P1),AVJ(1,1)),(DUMMY(NDF5P1),AVK(1,1)),    
     4            (DUMMY(NDF6P1),G(1,1),U(1,1)),(DUMMY(NDF7P1),H(1,1)), 
     5            (DUMMY(NDF8P1),EI(1,1)),(DUMMY(NDF9P1),EIC(1,1))      
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG)                         
      SAVE AVP,AVY                                                      
      IF ((ITCONT*ITCONJ).NE.0) ITCON=1                                 
      IF (IEDFTR.EQ.0.AND.ITCON.EQ.0) GO TO 266                         
C***********************************************************************
C   READ CHI, ETA FROM DISK FILE                                        
C***********************************************************************
      CALL BRANRD (NCED,DUMMY(1),NB2D)                                  
C***********************************************************************
C   READ AH FROM DISK FILE                                              
C***********************************************************************
      CALL BRANRD (NAHD,DUMMY(NDF2P1),NB1D)                             
C***********************************************************************
C   READ CGBAR FROM DISK FILE                                           
C***********************************************************************
      CALL BRANRD (NGBD,DUMMY(NDF3P1),NB1D)                             
C***********************************************************************
C   READ QUADRATURE WEIGHTS FROM DISK FILE                              
C***********************************************************************
      CALL BRANRD (NCYQW,CQW(1),NCQW)                                   
C***********************************************************************
C   CALCULATE BOUNDARY CONSTANTS FOR RAY EQUATIONS                      
C***********************************************************************
      CALL EFBC (B1,B2,B1N,B2N)                                         
      PI=3.141592653589793E0                                            
      TPII=2.0/PI                                                       
      IF (ITER.NE.1) GOTO 15                                            
      DO 10 L=1,NR                                                      
      DO 10 J=1,NFD                                                     
      AVY(J,L)=0.0                                                      
      AVP(J,L)=0.0                                                      
   10 CONTINUE                                                          
   15 CONTINUE                                                          
C***********************************************************************
C   INITIALIZE INTEGRALS AND ANISOTROPIC SCATTERING FUNCTION            
C***********************************************************************
    1 DO 20 L=1,NR                                                      
      DO 20,J=1,NFD                                                     
      AVJ(J,L)=0.0                                                      
      AVK(J,L)=0.0                                                      
      AH0(J,L)=3.0*CGBAR(J,L)*AH(J,L)/CHI(J,L)                          
      AY0(J,L)=3.0*CGBAR(J,L)*AVY(J,L)                                  
      AP0(J,L)=3.0*CGBAR(J,L)*AVP(J,L)                                  
      AVQ(J,L)=0.0                                                      
      AVY(J,L)=0.0                                                      
      AVP(J,L)=0.0                                                      
      IF (ITER.EQ.1.OR.IEDFTR.EQ.0) QRAT(J,L)=2.0/3.0                   
   20 CONTINUE                                                          
      IF (ISCA.NE.0) GO TO 33                                           
C***********************************************************************
C   INITIALIZE ANISOTROPIC SCATTERING FUNCTION FOR ISCA=0               
C***********************************************************************
      DO 30 L=1,NR                                                      
      DO 30 J=1,NFD                                                     
      AH0(J,L)=0.0                                                      
      AH1(J,L)=0.0                                                      
      AY0(J,L)=0.0                                                      
      AP0(J,L)=0.0                                                      
   30 CONTINUE                                                          
      GO TO 42                                                          
   33 CONTINUE                                                          
C***********************************************************************
C   CALCULATE FLUX PROFILE                                              
C***********************************************************************
      F=R(3)-R(2)                                                       
      B=R(2)-R(1)                                                       
      BF=B+F                                                            
      BFI=1.0/(B*F*BF)                                                  
      W1=-F*(BF+B)*BFI                                                  
      W2=BF*BF*BFI                                                      
      W3=-B*B*BFI                                                       
      F=R(NR)-R(N1)                                                     
      B=R(N1)-R(NR-2)                                                   
      BF=B+F                                                            
      BFI=1.0/(B*F*BF)                                                  
      W1N=F*F*BFI                                                       
      W2N=-BF*BF*BFI                                                    
      W3N=B*(BF+F)*BFI                                                  
      DO 35 J=1,NFD                                                     
      AH1(J,1)=W1*AH0(J,1)+W2*AH0(J,2)+W3*AH0(J,3)                      
      AH1(J,NR)=W1N*AH0(J,NR-2)+W2N*AH0(J,N1)+W3N*AH0(J,NR)             
   35 CONTINUE                                                          
C***********************************************************************
C   CALCULATE FLUX AT INTERIOR POINTS FOR SPHERICAL GEOMETRY            
C***********************************************************************
      DO 40 L=2,N1                                                      
      WR12=WR1(L)*WR1(L)                                                
      WR32=WR3(L)*WR3(L)                                                
      WR5R2I=1.0/(WR5(L)*(R(L)**IGEOM))                                 
      WT1=-WR12*WR5R2I                                                  
      WT2=(WR12-WR32)*WR5R2I                                            
      WT3=WR32*WR5R2I                                                   
      DO 40 J=1,NFD                                                     
      AH1(J,L)=WT1*AH0(J,L-1)+WT2*AH0(J,L)+WT3*AH0(J,L+1)               
   40 CONTINUE                                                          
   42 CONTINUE                                                          
C***********************************************************************
C   INITIALIZE H INTEGRALS                                              
C***********************************************************************
      DO 50 J=1,NFD                                                     
      AVB(J)=0.0                                                        
      AVC(J)=0.0                                                        
   50 CONTINUE                                                          
      IBC=1                                                             
   52 CONTINUE                                                          
      SBET=DSQRT(1.0-BETA(IBC)*BETA(IBC))                               
      BTSQ=BETA(IBC)*BETA(IBC)                                          
      READ (NEFQW) QW                                                   
      MB=-NR                                                            
      IZ=0                                                              
C***********************************************************************
C   SOLVE RAY EQUATIONS TO DETERMINE THE "INTENSITY" U                  
C***********************************************************************
      DO 170 IP=1,NP                                                    
      IF (IP-IP1) 54,54,55                                              
   54 CONTINUE                                                          
      IR0=1                                                             
      JR=0                                                              
      MW=NR                                                             
      GO TO 66                                                          
   55 CONTINUE                                                          
      IR0=IR0+1                                                         
      JR=IP-IP1                                                         
      MW=MW-1                                                           
      DO 60 J=1,NFD                                                     
      B1(J)=0.0                                                         
      B2(J)=0.0                                                         
   60 CONTINUE                                                          
   66 CONTINUE                                                          
      IR1=IR0+1                                                         
      MB=MB+MW                                                          
      IF (IP.EQ.NP) GO TO 115                                           
      IZ=IZ+1                                                           
      XMU=DSQRT(XMUSQ(IZ))                                              
      SMU=DSQRT(1.0-XMU*XMU)                                            
      DO 70 J=1,NFD                                                     
C***********************************************************************
C   CON0, CON1, AND CON2 COMPUTE SCATTERING COEFFICIENTS                
C***********************************************************************
      CON0=SBET*AP0(J,IR0)+BETA(IBC)*SMU*AY0(J,IR0)                     
      CON1=AH0(J,IR0)*XMU*BETA(IBC)                                     
      CON2=BTSQ*(XMUSQ(IZ)*AH1(J,IR0)+AH0(J,IR0)*P2R3(IZ))              
      C11=CHI(J,IR0)*(3.0*CHI(J,IR1)-CHI(J,IR0))*WZ1(IZ)                
      C12=CHI(J,IR0)*CHI(J,IR1)*WZ2(IZ)                                 
      B1I=1.0/(CHI(J,IR1)+CHI(J,IR0)*C12+B1(J)*C11)                     
      G(J,IR0)=((ETA(J,IR0)+CON0-CON2)*C12-(B2(J)+CON1)*C11)*B1I        
      H(J,IR0)=CHI(J,IR1)*B1I                                           
   70 CONTINUE                                                          
      IF (IR0.EQ.N1) GO TO 88                                           
      DO 80 L=IR1,N1                                                    
      IZ=IZ+1                                                           
      DO 80 J=1,NFD                                                     
      CON2=BTSQ*(XMUSQ(IZ)*AH1(J,L)+AH0(J,L)*P2R3(IZ))                  
      CON0=SBET*AP0(J,L)+BETA(IBC)*SMU*AY0(J,L)                         
      X13=CHI(J,L-1)*CHI(J,L+1)                                         
      T=WZ2(IZ)*X13*CHI(J,L)                                            
      AL=WZ1(IZ)*(X13+CHI(J,L)*CHI(J,L+1))                              
      CL=WZ3(IZ)*(X13+CHI(J,L)*CHI(J,L-1))                              
      BL=-(AL+CL+T*CHI(J,L))                                            
      DL=-T*(ETA(J,L)+CON0-CON2)                                        
      EL=1.0/(BL+AL*H(J,L-1))                                           
      G(J,L)=(DL-AL*G(J,L-1))*EL                                        
      H(J,L)=-CL*EL                                                     
   80 CONTINUE                                                          
   88 CONTINUE                                                          
      IZ=IZ+1                                                           
      XMU=DSQRT(XMUSQ(IZ))                                              
      DO 90 J=1,NFD                                                     
      CON0=SBET*AP0(J,NR)+BETA(IBC)*SMU*AY0(J,NR)                       
      CON1=AH0(J,NR)*XMU*BETA(IBC)                                      
      CON2=BTSQ*(XMUSQ(IZ)*AH1(J,NR)+AH0(J,NR)*P2R3(IZ))                
      CN1=CHI(J,NR)*(3.0*CHI(J,N1)-CHI(J,NR))*WZ1(IZ)                   
      CN2=CHI(J,N1)*CHI(J,NR)*WZ2(IZ)                                   
      AN=CHI(J,N1)                                                      
      BN=-CHI(J,N1)+B1N(J)*CN1-CHI(J,NR)*CN2                            
      DN=-((B2N(J)+CON1)*CN1+(ETA(J,NR)+CON0-CON2)*CN2)                 
      U(J,NR)=(DN-AN*G(J,N1))/(BN+AN*H(J,N1))                           
   90 CONTINUE                                                          
      LL=NR+IR0                                                         
      DO 110 L=IR1,NR                                                   
      DO 110 J=1,NFD                                                    
      U(J,LL-L)=G(J,LL-L)+H(J,LL-L)*U(J,LL-L+1)                         
  110 CONTINUE                                                          
      GO TO 125                                                         
  115 CONTINUE                                                          
      DO 120 J=1,NFD                                                    
      U(J,NR)=B2N(J)                                                    
  120 CONTINUE                                                          
  125 CONTINUE                                                          
C***********************************************************************
C   COMPUTE H INTEGRAL AT OUTER BOUNDARY                                
C***********************************************************************
      DO 130 J=1,NFD                                                    
      AVB(J)=AVB(J)+WB(IP)*U(J,NR)*WHO(IBC)*TPII                        
  130 CONTINUE                                                          
      IF (IC.EQ.0) GO TO 145                                            
      IF (IP.GT.IP1) GO TO 145                                          
C***********************************************************************
C   COMPUTE H INTEGRAL AT INNER BOUNDARY                                
C***********************************************************************
      DO 140 J=1,NFD                                                    
      AVC(J)=AVC(J)+WC(IP)*U(J,IR0)*WHO(IBC)*TPII                       
  140 CONTINUE                                                          
  145 CONTINUE                                                          
      JR=JR+1                                                           
      WJT=WJ(JR+MB)                                                     
      WKT=WK(JR+MB)                                                     
      WYT=WY(JR+MB)                                                     
C***********************************************************************
C   COMPUTE J AND K INTEGRALS                                           
C***********************************************************************
      DO 150 J=1,NFD                                                    
      AVJ(J,JR)=AVJ(J,JR)+WJT*U(J,JR)*WJO(IBC)*TPII                     
      AVK(J,JR)=AVK(J,JR)+WKT*U(J,JR)*WJO(IBC)*TPII-                    
     1          TPII*WKT*U(J,JR)*WKO(IBC)                               
      AVQ(J,JR)=AVQ(J,JR)+TPII*WJT*U(J,JR)*(WJO(IBC)-WKO(IBC))          
      IF (ISCA.EQ.0) GOTO 150                                           
      AVY(J,JR)=AVY(J,JR)+WYT*U(J,JR)*WHO(IBC)*TPII                     
      AVP(J,JR)=AVP(J,JR)+WJT*U(J,JR)*WYO(IBC)*TPII                     
  150 CONTINUE                                                          
      IF (JR.LT.NR) GO TO 145                                           
      IF (ITCON.NE.1) GO TO 170                                         
C***********************************************************************
C   COMPUTE EMERGENT INTENSITIES IF CONVERGENCE HAS OCCURRED            
C***********************************************************************
      DO 160 J=1,NFD                                                    
      UVNR=U(J,NR)-(B1N(J)*U(J,NR)+B2N(J))                              
      IF (IC.EQ.0) EI(J,IP)=UVNR                                        
      IF (IC.EQ.0) GO TO 160                                            
      IF (IP.EQ.1) EI(J,1)=UVNR                                         
      IF (IP.LE.(IP1+1)) EIC(J,IP)=UVNR                                 
      IF (IP.GE.IP1.AND.IP.LT.NP) EI(J,IP-IP0+1)=UVNR                   
  160 CONTINUE                                                          
  170 CONTINUE                                                          
      DO 180 J=1,NFD                                                    
      DO 180 L=1,NR                                                     
      IF (IBC.EQ.NQG) QRAT(J,L)=AVQ(J,L)/AVJ(J,L)                       
      IF (IBC.EQ.NQG.AND.ITER.GE.ITMAX.AND.ITCON.NE.1)                  
     1    WRITE (NEICD) QRAT(J,L)                                       
      IF (L.GT.1.OR.IC.NE.0.OR.ITCON.NE.1) GOTO 175                     
      EI(J,NR)=B2N(J)                                                   
C***********************************************************************
C   WRITE EI ONTO DISK FILE                                             
C***********************************************************************
  175 CONTINUE                                                          
      IF (ITCON.NE.1) GOTO 180                                          
      WRITE (NEID) EI(J,L)                                              
C***********************************************************************
C   WRITE EIC ONTO DISK FILE                                            
C***********************************************************************
      IF (IC.NE.1.OR.L.GT.NC2) GOTO 180                                 
      WRITE (NEICD) EIC(J,L)                                            
  180 CONTINUE                                                          
      IF (ITCON.NE.1) GOTO 195                                          
      DO 190 J=1,NFD                                                    
      DO 190 L=1,NR                                                     
      IF (IBC.EQ.NQG) WRITE (NEICD) QRAT(J,L)                           
  190 CONTINUE                                                          
  195 CONTINUE                                                          
      IBC=IBC+1                                                         
      IF (IBC.LE.NQG) GOTO 52                                           
      REWIND NEFQW                                                      
      REWIND NEID                                                       
      REWIND NEICD                                                      
      IF (IEDFTR.EQ.0) GO TO 225                                        
C***********************************************************************
C   COMPUTE BOUNDARY FACTORS                                            
C***********************************************************************
      DO 210 J=1,NFD                                                    
      FCD(J)=AVC(J)/AVJ(J,1)                                            
      FBD(J)=AVB(J)/AVJ(J,NR)                                           
  210 CONTINUE                                                          
C***********************************************************************
C   COMPUTE ANISOTROPY FACTORS                                          
C***********************************************************************
      DO 220 L=1,NR                                                     
      DO 220 J=1,NFD                                                    
      FK(J,L)=AVK(J,L)/AVJ(J,L)                                         
  220 CONTINUE                                                          
  225 CONTINUE                                                          
      IF (IEDFTR.EQ.0) RETURN                                           
C***********************************************************************
C   COMPUTE CONFIGURATION FUNCTION                                      
C***********************************************************************
      DO 260 J=1,NFD                                                    
      DO 230 L=1,NR                                                     
      Y(L)=(2.0*FK(J,L)-QRAT(J,L))/FK(J,L)                              
  230 CONTINUE                                                          
      DO 240 L=1,N1                                                     
      DP(L)=WZETA1(L)*Y(L)+WZETA2(L)*Y(L+1)                             
  240 CONTINUE                                                          
      Y(1)=0.0                                                          
      DO 250 L=2,NR                                                     
      Y(L)=Y(L-1)+DP(L-1)                                               
  250 CONTINUE                                                          
      DO 260 L=1,NR                                                     
      ZETA(J,L)=DEXP(Y(L))                                              
  260 CONTINUE                                                          
C***********************************************************************
C   WRITE AJ, FK, ZETA ONTO DISK FILE                                   
C***********************************************************************
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 
  266 CONTINUE                                                          
C***********************************************************************
C   UPDATE AJ, AH, ETA                                                  
C***********************************************************************
      CALL GETAJ                                                        
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE EFBC CALCULATES THE CONSTANTS IN THE BOUNDARY            
C   CONDITIONS FOR THE RAY EQUATIONS                                    
C   B1,B2 ---  ALPHA AND BETA AT CLOUD SURFACE                          
C   C1,C2 ---  ALPHA AND BETA AT CLOUD CENTER                           
C***********************************************************************
      SUBROUTINE EFBC (C1,C2,B1,B2)                                     
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      DIMENSION C1(1),C2(1),B1(1),B2(1)                                 
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      CON=-4.0*AJ0I                                                     
      DO 10 J=1,NFD                                                     
      B1(J)=-1.0                                                        
      B2(J)=-CON*FLUXSD(J)                                              
   10 CONTINUE                                                          
      IF (IC.EQ.0) GO TO 22                                             
      IF (NH.EQ.1) GO TO 33                                             
   15 DO 20 J=1,NFD                                                     
      C1(J)=1.0                                                         
      C2(J)=CON*FLUXCD(J)                                               
   20 CONTINUE                                                          
      RETURN                                                            
   22 CONTINUE                                                          
      DO 30 J=1,NFD                                                     
      C1(J)=0.0                                                         
      C2(J)=0.0                                                         
   30 CONTINUE                                                          
      RETURN                                                            
   33 CONTINUE                                                          
      DO 40 J=1,NFD                                                     
      C1(J)=0.0                                                         
      C2(J)=-2.0*AJ0I*FLUXCD(J)                                         
   40 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   SUBROUTINE GETAJ UPDATES THE MEAN INTENSITY AJ. IF CONVERGENCE      
C   HAS NOT BEEN REACHED, THE FLUX AND EMISSIVITY ARE UPDATED           
C***********************************************************************
      SUBROUTINE GETAJ                                                  
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NDF7=7*NDF)                                            
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1)          
      COMMON DUMMY(NDF7)                                                
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),             
     1          ETA(NF,ND),AH(NF,ND),CHICG(NF,ND),BTDAV(NF,ND),         
     2          QABS(NF,ND),QSCA(NF,ND),Q(NF,ND),H(NF,ND)               
      EQUIVALENCE (DUMMY(1),AJ(1,1),Q(1,1)),                            
     1            (DUMMY(NDF1P1),FK(1,1),QABS(1,1)),                    
     2            (DUMMY(NDF2P1),ZETA(1,1),QSCA(1,1)),                  
     3            (DUMMY(NDF3P1),CHI(1,1)),                             
     2            (DUMMY(NDF4P1),ETA(1,1)),                             
     3            (DUMMY(NDF5P1),CHICG(1,1),BTDAV(1,1),AH(1,1)),        
     4            (DUMMY(NDF6P1),H(1,1))                                
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/GRIDWT/WR1(ND),WR2(ND),WR3(ND),WR4(ND),WR5(ND)             
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SPARE/B1(NF),B2(NF),B1N(NF),B2N(NF)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
C***********************************************************************
C   READ AJ,FK,ZETA FROM DISK FILE                                      
C***********************************************************************
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 
C***********************************************************************
C   READ CHI, ETA FROM DISK FILE                                        
C***********************************************************************
      CALL BRANRD (NCED,DUMMY(NDF3P1),NB2D)                             
C***********************************************************************
C   READ CHICG FROM DISK FILE                                           
C***********************************************************************
      CALL BRANRD (NCGD,DUMMY(NDF5P1),NB1D)                             
C***********************************************************************
C   CALCULATE BOUNDARY CONSTANTS FOR COMBINED MOMENT EQUATIONS          
C***********************************************************************
      CALL BCD (B1,B2,B1N,B2N)                                          
C***********************************************************************
C   CALCULATE AJ BY GAUSSIAN ELIMINATION                                
C***********************************************************************
      DO 10 J=1,NFD                                                     
      W1=ZETA(J,1)*CHICG(J,1)                                           
      W2=ZETA(J,2)*CHICG(J,2)                                           
      C11=W1*WR1(1)*(1.5*W2-WR3(1)*W1)                                  
      C12=WR2(1)*W1*W2                                                  
      B1I=1.0/(W2*FK(J,1)*ZETA(J,1)+B1(J)*C11+C12*CHI(J,1))             
      Q(J,1)=-(C11*B2(J)-C12*ETA(J,1))*B1I                              
      H(J,1)=W2*FK(J,2)*ZETA(J,2)*B1I                                   
   10 CONTINUE                                                          
      DO 20 L=2,N1                                                      
      DO 20 J=1,NFD                                                     
      W1=ZETA(J,L-1)*CHICG(J,L-1)                                       
      W2=ZETA(J,L)*CHICG(J,L)                                           
      W3=ZETA(J,L+1)*CHICG(J,L+1)                                       
      X13=W1*W3                                                         
      T12=WR1(L)*X13+WR2(L)*W2*W3                                       
      T34=WR3(L)*X13+WR4(L)*W2*W1                                       
      T5=WR5(L)*X13*W2                                                  
      AL=T12*FK(J,L-1)*ZETA(J,L-1)                                      
      BL=-(T12+T34)*FK(J,L)*ZETA(J,L)-T5*CHI(J,L)                       
      CL=T34*FK(J,L+1)*ZETA(J,L+1)                                      
      DL=-T5*ETA(J,L)                                                   
      EL=1.0/(BL+AL*H(J,L-1))                                           
      Q(J,L)=(DL-AL*Q(J,L-1))*EL                                        
      H(J,L)=-CL*EL                                                     
   20 CONTINUE                                                          
      DO 30 J=1,NFD                                                     
      WN=ZETA(J,NR)*CHICG(J,NR)                                         
      WN1=ZETA(J,N1)*CHICG(J,N1)                                        
      CN1=WN*WR1(NR)*(1.5*WN1-WR3(NR)*WN)                               
      CN2=WR2(NR)*WN1*WN                                                
      AN=WN1*FK(J,N1)*ZETA(J,N1)                                        
      BN=-WN1*FK(J,NR)*ZETA(J,NR)+CN1*B1N(J)-CN2*CHI(J,NR)              
      DN=-(CN1*B2N(J)+CN2*ETA(J,NR))                                    
      Q(J,NR)=(DN-AN*Q(J,N1))/(BN+AN*H(J,N1))                           
   30 CONTINUE                                                          
      DO 40 L=1,N1                                                      
      DO 40 J=1,NFD                                                     
      Q(J,NR-L)=Q(J,NR-L)+H(J,NR-L)*Q(J,NR-L+1)                         
   40 CONTINUE                                                          
C***********************************************************************
C   WRITE AJ, FK, AND ZETA ONTO DISK FILE                               
C***********************************************************************
      CALL BRANWT (NJFPD,DUMMY(1),NB3D)                                 
      IF (ITER.NE.0) GO TO 55                                           
C***********************************************************************
C   READ QABS AND QSCA FROM DISK FILE                                   
C***********************************************************************
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB2D)                            
C***********************************************************************
C   UPDATE CHI                                                          
C***********************************************************************
      DO 50 J=1,NFD                                                     
      DO 50 L=1,NR                                                      
      CHI(J,L)=QABS(J,L)+QSCA(J,L)                                      
   50 CONTINUE                                                          
C***********************************************************************
C   WRITE CHI TO DISK FILE                                              
C***********************************************************************
      CALL BRANWT (NCED,DUMMY(NDF3P1),NB2D)                             
   55 CONTINUE                                                          
      IF (ITCON.EQ.0) RETURN                                            
C***********************************************************************
C   UPDATE AH                                                           
C***********************************************************************
      DO 60 J=1,NFD                                                     
      AH(J,1)=-(B1(J)*Q(J,1)+B2(J))                                     
      AH(J,NR)=-(B1N(J)*Q(J,NR)+B2N(J))                                 
   60 CONTINUE                                                          
      DO 70 L=2,N1                                                      
      WR12=WR1(L)*WR1(L)                                                
      WR32=WR3(L)*WR3(L)                                                
      WR5R2I=1.0/(WR5(L)*(R(L)**IGEOM))                                 
      WT1=WR12*WR5R2I                                                   
      WT2=(WR32-WR12)*WR5R2I                                            
      WT3=-WR32*WR5R2I                                                  
      DO 70 J=1,NFD                                                     
      FPQ1=FK(J,L-1)*ZETA(J,L-1)*Q(J,L-1)                               
      FPQ2=FK(J,L)*ZETA(J,L)*Q(J,L)                                     
      FPQ3=FK(J,L+1)*ZETA(J,L+1)*Q(J,L+1)                               
      AH(J,L)=(WT1*FPQ1+WT2*FPQ2+WT3*FPQ3)/(ZETA(J,L)*CHICG(J,L))       
   70 CONTINUE                                                          
C***********************************************************************
C   WRITE AH TO DISK FILE                                               
C***********************************************************************
      CALL BRANWT (NAHD,DUMMY(NDF5P1),NB1D)                             
C***********************************************************************
C   READ QABS AND QSCA FROM DISK FILE                                   
C***********************************************************************
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB2D)                            
C***********************************************************************
C   READ BTDAV FROM DISK FILE                                           
C***********************************************************************
      CALL BRANRD (NBTAD,DUMMY(NDF5P1),NB1D)                            
C***********************************************************************
C   UPDATE ETA                                                          
C***********************************************************************
      DO 80 J=1,NFD                                                     
      DO 80 L=1,NR                                                      
      ETA(J,L)=QSCA(J,L)*AJ(J,L)+QABS(J,L)*BTDAV(J,L)                   
   80 CONTINUE                                                          
C***********************************************************************
C   WRITE ETA ONTO DISK FILE                                            
C***********************************************************************
      CALL BRANWT (NCED,DUMMY(NDF3P1),NB2D)                             
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
C***********************************************************************
C    SUBROUTINE OUTPUT PRINTS THE RESULTS OF EACH ITERATION AND         
C    COMPUTES AND PRINTS OUT THE FINAL MODEL RESULTS                    
C***********************************************************************
      SUBROUTINE OUTPUT                                                 
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1,          
     3           NDF8M=NDF8P1-1)                                        
      PARAMETER (NFC1P1=NFC2+1,NFC2P1=2*NFC2+1,NFC3P1=3*NFC2+1,         
     1           NFC4P1=4*NFC2+1,NFC5P1=5*NFC2+1,                       
     2           NFC6P1=6*NFC2+1,NFC7P1=7*NFC2+1,                       
     3           NFC8P1=8*NFC2+1,NCEIC=NFC8P1+NDF)                      
      PARAMETER (NQWT=ND*NC+ND*(ND+1)/2,NQWT1=NQWT+1,                   
     1           NQWT2=NQWT1+NQWT,NQWT3=NQWT2+NQWT,                     
     2           NQWT4=NQWT3+NQWT,NQWT5=NQWT4+NQWT,                     
     3           NQWT6=NQWT5+NPMAX,NQWT7=NQWT6+NC+1,                    
     4           NQWT8=NQWT7+NQWT,NQWT9=NQWT8+NQWT,                     
     5           NQWT10=NQWT9+ND,NQWT0=NQWT10+ND-1)                     
      PARAMETER (ND5=5*ND,NF5=5*NF,NY1=NDF4P1+ND5,NY2=NY1+ND5,          
     1           NY3=NY2+NF5,NY4=NY3+NF5)                               
      PARAMETER (NXY=6*NDF+NFC2,NTHETA=20)                              
      PARAMETER (NQG=4,NQGT=NQWT2+NQG,NQGT1=NQGT+NQG,                   
     1           NQGT2=NQGT1+NQG,NCQW=NQGT2+NQG-1)                      
      COMMON DUMMY(NXY),TAU1(ND),SOURCE(ND),PLANKJ(ND),AJ1(ND),         
     1       FK1(ND),ZETA1(ND),CHI1(ND),ETA1(ND),AH1(ND),TOF(ND),       
     2       ALUM(NF),XU(ND),PP(ND),AHOUTC(NF),AHOUTS(NF),              
     3       BDAHC(NF),BDAHS(NF),TB(ND),BK(NF)                          
      DIMENSION AJ(NF,ND),FK(NF,ND),ZETA(NF,ND),CHI(NF,ND),             
     1          ETA(NF,ND),AH(NF,ND),QABS(NF,ND),BTDAV(NF,ND),          
     2          EI(NF,ND),EITB(NF,ND),EIC(NF,NC2),XMU(NTHETA)           
      DIMENSION HEATD(ND),COOLD(ND),AVFLUX(ND)                          
      DIMENSION ABUNDI(5,ND),TDI(5,ND),QABSI(NF,5),QSCAI(NF,5),         
     1          GBARI(NF,5),COOLDI(5,ND),HEATDI(5,ND)                   
      DIMENSION QRAT(NF,ND),AVJ(NF,ND)                                  
      DIMENSION QRA1(ND)                                                
      EQUIVALENCE (DUMMY(1),AJ(1,1)),                                   
     1            (DUMMY(NDF1P1),FK(1,1),EI(1,1),QABS(1,1)),            
     2            (DUMMY(NDF2P1),ZETA(1,1),EITB(1,1),BTDAV(1,1)),       
     3            (DUMMY(NDF3P1),CHI(1,1)),(DUMMY(NDF4P1),ETA(1,1)),    
     4            (DUMMY(NDF5P1),AH(1,1)),(DUMMY(NDF6P1),EIC(1,1))      
      EQUIVALENCE (ZETA1(1),COOLD(1)),(ETA1(1),HEATD(1)),               
     2            (CHI1(1),AVFLUX(1))                                   
      EQUIVALENCE (DUMMY(NDF4P1),ABUNDI(1,1)),(DUMMY(NY1),TDI(1,1)),    
     1            (DUMMY(NY2),QABSI(1,1)),                              
     2            (DUMMY(NY3),QSCAI(1,1),COOLDI(1,1)),                  
     3            (DUMMY(NY4),GBARI(1,1))                               
      COMMON/BOUND/FLUXCD(NF),FLUXSD(NF),FBD(NF),FCD(NF)                
      COMMON/CHECK/DTD(ND),DJD(ND),DHD(ND)                              
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      
      COMMON/CYCLE/EPS,ITER,ITMAX,ITCON,ITCONT,ITCONJ                   
      COMMON/FILE/NEFQW,NJFPD,NCED,NAHD,NEID,NEICD,NQASD,NGBD,NBTAD,    
     1            NCGD,NJCBD,NGPD,NB1D,NB2D,NB3D,NQW,NGP,NCYQW          
      COMMON/MODEL/RMAX,RMIN,RHOCS,RHOBAR,TAUOF,IOF                     
      COMMON/NORM/R0,R0I,AJ0,AJ0I,ETA0,ETA0I                            
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      COMMON/WGR/WY1(NPMAX),CQW(NCQW),BETA(NQG)                         
      IF (IGEOM.EQ.0) NP=NTHETA                                         
C***********************************************************************
C   WRITE THE ITERATION RESULTS TO THE OUTPUT FILE                      
C***********************************************************************
      WRITE (6,441) ITER,ITCONT,ITCONJ                                  
  441 FORMAT (1H1,5X,'ITERATION ',I2,10X,'ITCONT = ',I1,10X,'ITCONJ = ',
     1 I1/2X,'IR',10X,'R',13X,'TD',13X,'DTD',12X,'DJD',12X,'DHD')       
      WRITE (6,442) (L,R(L),TD(L),DTD(L),DJD(L),DHD(L),L=1,NR)          
  442 FORMAT (1H9,I3,1P5E15.4)                                          
      DO 10 L=1,NR                                                      
      DJD(L)=0.0                                                        
      DHD(L)=0.0                                                        
   10 CONTINUE                                                          
      IF (ITCON) 11,11,12                                               
   11 CONTINUE                                                          
      IF (ITER.GE.ITMAX) WRITE (6,443) ITER                             
  443 FORMAT (1H1,20(/),45X,'NO CONVERGENCE AFTER ',I2,' ITERATIONS')   
      IF (ITER.GE.ITMAX) GO TO 15                                       
      RETURN                                                            
   12 CONTINUE                                                          
      WRITE (6,444) ITER                                                
  444 FORMAT (1H1,20(/),44X,'SUCCESSFUL CONVERGENCE AFTER ',I2,         
     1 ' ITERATIONS'///40X,'FINAL RESULTS FOR THERMAL PROPERTIES OF DIDC
     2 MODEL')                                                          
   15 CONTINUE                                                          
      IF (ITCON.NE.1.OR.IGEOM.NE.1.OR.IC.EQ.0) GOTO 35                  
      DO 30 IBC=1,NQG                                                   
      DO 20 J=1,NFD                                                     
      DO 20 L=1,NC2                                                     
      READ (NEICD) EIC(J,L)                                             
   20 CONTINUE                                                          
   30 CONTINUE                                                          
   35 CONTINUE                                                          
      READ (5,445)ICONV                                                 
  445 FORMAT(7X,I1)                                                     
      HC=AJ0/CTH                                                        
      FOURPI=4.0*PI                                                     
      IF (ITCON.EQ.0) GO TO 44                                          
      PCON=180.0/PI                                                     
      DO 40 IP=1,N1                                                     
      IF (IC.EQ.0) PP(IP)=DASIN(R(IP))*PCON                             
      IF (IC.NE.0) PP(IP+1)=DASIN(R(IP))*PCON                           
   40 CONTINUE                                                          
      IF (IC.EQ.0) PP(NR)=90.0                                          
      IF (IC.NE.0) PP(1)=0.0                                            
   44 CONTINUE                                                          
C***********************************************************************
C   READ AJ, FK, ZETA FROM DISK FILE                                    
C***********************************************************************
      CALL BRANRD (NJFPD,DUMMY(1),NB3D)                                 
C***********************************************************************
C   READ CHI, ETA FROM DISK FILE                                        
C***********************************************************************
      CALL BRANRD (NCED,DUMMY(NDF3P1),NB2D)                             
C***********************************************************************
C   READ AH, FROM DISK FILE                                             
C***********************************************************************
      CALL BRANRD (NAHD,DUMMY(NDF5P1),NB1D)                             
      IF (IGEOM.EQ.2) CALL BRANRD (NEICD,DUMMY(NDF6P1),NFC2)            
      DO 70 J=1,NFD                                                     
      TF=HK*FREQD(J)                                                    
      HFC=HC*FREQD(J)**3                                                
C***********************************************************************
C   CALCULATE BRIGHTNESS TEMPERATURE                                    
C***********************************************************************
      DO 50 L=1,NR                                                      
      IF (IGEOM.EQ.1) READ (NEICD) QRAT(J,L)                            
      AJ1(L)=AJ(J,L)*AJ0                                                
      IF (AJ1(L).LE.0.0) TB(L)=0.0                                      
      IF (AJ1(L).LE.0.0) GO TO 46                                       
      TB(L)=TF/(DLOG(HFC/AJ1(L)+1.0))                                   
   46 CONTINUE                                                          
      FK1(L)=FK(J,L)                                                    
      ZETA1(L)=ZETA(J,L)                                                
      IF (IGEOM.EQ.1) QRA1(L)=QRAT(J,L)                                 
      CHI1(L)=CHI(J,L)*R0I                                              
      ETA1(L)=ETA(J,L)*ETA0I                                            
      AH1(L)=AH(J,L)*AJ0                                                
C***********************************************************************
C   CALCULATE  FLUX AT BOUNDARIES AND THE SOURCE FUNCTION               
C***********************************************************************
      IF (L.EQ.1) BDAHC(J)=AH1(L)                                       
      IF (L.EQ.NR) BDAHS(J)=AH1(L)                                      
      SOURCE(L)=ETA1(L)/CHI1(L)                                         
C***********************************************************************
C   CALCULATE PLANCK FUNCTION FOR THE GRAINS                            
C***********************************************************************
      CALL GETBJ (FREQD(J),TD(L),0,BJ0,BJ1)                             
      PLANKJ(L)=AJ0*BJ0                                                 
   50 CONTINUE                                                          
C***********************************************************************
C   CALCULATE  OPTICAL DEPTH PROFILE                                    
C***********************************************************************
      CALL GETAU (R,CHI1,TAU1,NR)                                       
      DO 60 L=1,NR                                                      
      TAU1(L)=TAU1(L)*R0                                                
      IF (J.EQ.IOF) TOF(L)=TAU1(L)                                      
   60 CONTINUE                                                          
      IF (IOUT.EQ.0) GO TO 65                                           
      IF (IGEOM.EQ.1) GO TO 63                                          
C***********************************************************************
C   WRITE BRIGHTNESS TEMPERATURE, ANISOTROPY FACTORS, BOUNDARY FACTORS, 
C   OPTICAL DEPTH, DUST TEMPERATURE AND MEAN INTENSITY FOR EACH         
C   FREQUENCY AND RADIAL GRID POINT                                     
C***********************************************************************
      WRITE (6,551) J,FREQD(J),WLAMDA(J),TF,FBD(J),FCD(J)               
  551 FORMAT (1H1,11H**FREQUENCY,I3,2H**,2X,'FREQ =',1PE10.3,' HERTZ',  
     1 2X,'LAMBDA=',1PE10.3,' MICRON',2X,'TF=',1PE10.3,' DEGREE',2X,    
     2 'FBD=',1PE10.3,2X,'FCD=',1PE10.3/2X,'IR',6X,'TB',8X,             
     3  'TAU',9X,'TD',8X,'CHI',8X,'ETA',9X,'FK',8X,'ZETA',9X,'AJ',9X,   
     4  'BJ',7X,'SOURCE',7X,'AH')                                       
      WRITE (6,552) (L,TB(L),TAU1(L),TD(L),CHI1(L),ETA1(L),             
     1 FK1(L),ZETA1(L),AJ1(L),PLANKJ(L),SOURCE(L),AH1(L),L=1,NR)        
  552 FORMAT (1H9,I3,1P11E11.3)                                         
      GO TO 65                                                          
   63 CONTINUE                                                          
C***********************************************************************
C   WRITE THE ABOVE FOR THE CYLINDRICAL CASE                            
C***********************************************************************
      WRITE (6,553) J,FREQD(J),WLAMDA(J),TF,FBD(J),FCD(J)               
  553 FORMAT (1H1,11H**FREQUENCY,I3,2H**,2X,'FREQ =',1PE10.3,' HERTZ',  
     1 2X,'LAMBDA=',1PE10.3,' MICRON',2X,'TF=',1PE10.3,' DEGREE',2X,    
     2 'FBD=',1PE10.3,2X,'FCD=',1PE10.3/2X,'IR',6X,'TB',8X,             
     3  'TAU',8X,'CHI',8X,'ETA',9X,'FK',7X,'QRAT',8X,'ZETA',9X,'AJ',9X, 
     4  'BJ',7X,'SOURCE',7X,'AH')                                       
      WRITE (6,552) (L,TB(L),TAU1(L),CHI1(L),ETA1(L),FK1(L),            
     1 QRA1(L),ZETA1(L),AJ1(L),PLANKJ(L),SOURCE(L),AH1(L),L=1,NR)       
   65 CONTINUE                                                          
C***********************************************************************
C   CALCULATE FLUX AT BOUNDARIES AND THE OBSERVED FLUX                  
C***********************************************************************
      AHOUTC(J)=FLUXCD(J)-BDAHC(J)                                      
      AHOUTS(J)=FLUXSD(J)+BDAHS(J)                                      
      ALUM(J)=FOURPI*AHOUTS(J)                                          
   70 CONTINUE                                                          
      REWIND NEICD                                                      
      CCOOLD=FOURPI*AJ0*R0I                                             
      NBC=1                                                             
      IF (IGEOM.EQ.1.AND.IEMRG.NE.0) NBC=NQG                            
C***********************************************************************
C   WRITE CONTINUUM RADIATION PROPERTIES TO OUTPUT                      
C***********************************************************************
      WRITE (6,554) (J,FREQD(J),WLAMDA(J),BDAHC(J),FLUXCD(J),AHOUTC(J)  
     1 ,FCD(J),BDAHS(J),FLUXSD(J),AHOUTS(J),FBD(J),ALUM(J),             
     2 J=1,NFD)                                                         
  554 FORMAT (1H1,30X,'PROPERTIES OF CONTINUUM RADIATION AT INNER AND OU
     1TER BOUNDARIES'/2X,'IF',5X,'FREQD',5X,'MICRON',5X,'BDAHC',6X,     
     2 'FLUXCD',    5X,'AHOUTC',6X,'FCD',7X,'BDAHS',6X,'FLUXSD',5X,     
     3 'AHOUTS',6X,'FBD',6X,'OBS FLUX'/(1X,I3,1X,1P11E11.3))            
      IF (IGEOM.EQ.1.AND.IEMRG.EQ.0) THET=DASIN(BETA(1))*PCON           
C***********************************************************************
C   READ EMERGENT INTENSITIES THROUGH CORE FROM THE DISK FILE           
C***********************************************************************
      IF (ITCON.EQ.0) GO TO 177                                         
      DO 170 IBC=1,NBC                                                  
      THET=DASIN(BETA(IBC))*PCON                                        
      DO 90 J=1,NFD                                                     
      BDAHS(J)=FOURPI*FLUXSD(J)                                         
      BDAHC(J)=FOURPI*FLUXCD(J)                                         
      DO 90 IP=1,NR                                                     
      IF (IGEOM.EQ.1) READ (NEID) EI(J,IP)                              
      IF (IP.GT.NC2.OR.IC.EQ.0) GOTO 90                                 
      IF (IGEOM.EQ.1) READ (NEICD) EIC(J,IP)                            
      EIC(J,IP)=AJ0*EIC(J,IP)                                           
   90 CONTINUE                                                          
      IF (IC.EQ.0.OR.IGEOM.EQ.0) GO TO 126                              
      IF (IGEOM.EQ.1) WRITE (6,666) THET                                
  666 FORMAT (2X,'THETA = ',1PE10.3)                                    
C***********************************************************************
C   WRITE EMERGENT INTENSITIES THROUGH CORE AND EMERGENT FLUX           
C   TO OUTPUT                                                           
C***********************************************************************
      WRITE (6,771) (J,WLAMDA(J),(EIC(J,IP),IP=1,NC,2),EIC(J,NC1),      
     1              EIC(J,NC2),BDAHC(J),BDAHS(J),ALUM(J),J=1,NFD)       
  771 FORMAT (1H1,45X,'EMERGENT INTENSITIES AND FLUXES'/1X,'IF',        
     1  5X,'MICRON',5X,'EIC(1)',5X,'EIC(3)',5X,'EIC(5)',5X,'EIC(7)',    
     2 5X,'EIC(9)',4X,'EIC(10)',4X,'EIC(11)',5X,'FLUXCD',5X,'FLUXSD',4X,
     3 'OBS FLUX'/(I3,1X,1P11E11.3))                                    
  126 CONTINUE                                                          
C***********************************************************************
C   READ EMERGENT INTENSITIES OUTSIDE CORE FROM THE DISK FILE           
C***********************************************************************
      IF (IGEOM.NE.1) CALL BRANRD (NEID,DUMMY(NDF1P1),NB1D)             
C***********************************************************************
C   CALCULATE ANGULAR DISTRIBUTION OF EMERGENT INTENSITIES              
C***********************************************************************
      WRITE (6,773)                                                     
  773 FORMAT (1H1,30X,'ANGULAR DISTRIBUTION OF EMERGENT INTENSITY FOR DU
     1ST COMPONENT'/)                                                   
      XUCON=PI/180.0                                                    
      NZ=NR                                                             
      IF (IGEOM.EQ.0) NZ=NTHETA                                         
      DO 150 IP=1,NZ                                                    
      IF (IGEOM.NE.0) XU(IP)=DCOS(PP(IP)*XUCON)                         
      IF (IGEOM.EQ.0) XU(IP)=XMU(IP)                                    
      IF (IGEOM.EQ.0) PP(IP)=DACOS(XU(IP))                              
      DO 140 J=1,NFD                                                    
      EI(J,IP)=AJ0*EI(J,IP)                                             
      IF (EI(J,IP).EQ.0.0) EITB(J,IP)=0.0                               
      IF (EI(J,IP).EQ.0.0) GO TO 140                                    
      TF=HK*FREQD(J)                                                    
      HFC=HC*FREQD(J)**3                                                
      XX=HFC/EI(J,IP)+1.0                                               
      IF (XX.LE.0.0) EITB(J,IP)=0.0                                     
      IF (XX.LE.0.0) GO TO 140                                          
      EITB(J,IP)=TF/DLOG(XX)                                            
  140 CONTINUE                                                          
      IF (IC.NE.0) GO TO 143                                            
      TOFP=TOF(IP)                                                      
      RP=R(IP)                                                          
      GO TO 147                                                         
  143 CONTINUE                                                          
      IF (IP.GT.1) GO TO 145                                            
      TOFP=TOF(IP)                                                      
      RP=0.0                                                            
      GO TO 147                                                         
  145 CONTINUE                                                          
      TOFP=TOF(IP-1)                                                    
      RP=R(IP-1)                                                        
  147 CONTINUE                                                          
C***********************************************************************
C   WRITE ANGULAR DISTRIBUTION OF EMERGENT INTENSITIES                  
C   TO OUTPUT                                                           
C***********************************************************************
      IF (IGEOM.EQ.1) GO TO 149                                         
      WRITE (6,775) IP,TOFP,XU(IP),PP(IP),(EI(J,IP),                    
     1              EITB(J,IP),J=1,NFD)                                 
  775 FORMAT (5X,'IP = ',I3,4X,'TAU = ',1PE10.3,5X,                     
     1 'XMU = ',1PE10.3,5X,'THETA = ',1PE10.3/(1X,'EMERG INT',1X,       
     2 5(1PE10.3,1X,'(',1PE10.3,')',1X)))                               
      GO TO 150                                                         
  149 CONTINUE                                                          
      WRITE (6,776) IP,TOFP,RP,XU(IP),PP(IP),(EI(J,IP),                 
     1              EITB(J,IP),J=1,NFD)                                 
  776 FORMAT (5X,'IP = ',I3,4X,'TAU = ',1PE10.3,5X,'R = ',1PE10.3,5X,   
     1 'XMU = ',1PE10.3,5X,' PHI  = ',1PE10.3/(1X,'EMERG INT',1X,       
     2 5(1PE10.3,1X,'(',1PE10.3,')',1X)))                               
  150 CONTINUE                                                          
      DO 160 L1=1,NFD,10                                                
      L2=MIN0(NFD,L1+9)                                                 
      WRITE (6,887) (WLAMDA(J),J=L1,L2)                                 
  887 FORMAT (1H1,30X,'ANGULAR DISTRIBUTION OF EMERGENT INTENSITY AT SEL
     1ECTED WAVELENGTHS'/3X,'I',7X,'R',5X,10('(',1PE9.2,')')/)          
      DO 160 L=1,NZ                                                     
      RP=R(L)                                                           
      IF (IGEOM.EQ.0) RP=P(L)                                           
      WRITE (6,888) L,RP,(EI(J,L),J=L1,L2)                              
  888 FORMAT (1H9,I3,1X,1P11E11.3)                                      
  160 CONTINUE                                                          
C***********************************************************************
C   CALL CONVOL TO CALCULATE CONVOLUTION INTEGRAL                       
C***********************************************************************
      IF (ICONV.EQ.1) CALL CONVOL(EI,EIC)                               
  170 CONTINUE                                                          
  177 CONTINUE                                                          
      IF (IGEOM.EQ.1) REWIND NEICD                                      
      IF (IGEOM.EQ.1) REWIND NEID                                       
C***********************************************************************
C   READ QABS AND QSCA FROM DISK FILE                                   
C***********************************************************************
      CALL BRANRD (NQASD,DUMMY(NDF1P1),NB1D)                            
C***********************************************************************
C   READ BTDAV FROM DISK FILE                                           
C***********************************************************************
      CALL BRANRD (NBTAD,DUMMY(NDF2P1),NB1D)                            
C***********************************************************************
C   READ GRAIN CHARACTERISTICS FROM DISK FILE                           
C***********************************************************************
      CALL BRANRD (NGPD,DUMMY(NDF4P1),NGP)                              
      DO 220 L=1,NR                                                     
      SUMAH=0.0                                                         
      SUMHD=0.0                                                         
      SUMCD=0.0                                                         
      DO 180 J=1,NFD                                                    
      XDUM=WFD(J)*AH(J,L)                                               
      SUMAH=SUMAH+XDUM                                                  
      XX=QABS(J,L)*WFD(J)                                               
      SUMCD=SUMCD+XX*BTDAV(J,L)                                         
      SUMHD=SUMHD+XX*AJ(J,L)                                            
  180 CONTINUE                                                          
C***********************************************************************
C   CALCULATE NET FLUX                                                  
C***********************************************************************
      AVFLUX(L)=SUMAH*AJ0                                               
      IF (IC.NE.0.AND.IGEOM.EQ.2) AVFLUX(L)=AVFLUX(L)*(R0*R(L))**2      
      IF (IC.NE.0.AND.IGEOM.EQ.1) AVFLUX(L)=AVFLUX(L)*(R0*R(L))         
C***********************************************************************
C   CALCULATE HEATING AND COOLING RATES                                 
C***********************************************************************
      COOLD(L)=CCOOLD*SUMCD                                             
      HEATD(L)=CCOOLD*SUMHD                                             
      DO 210 IG=1,IMIX                                                  
      SUMHDI=0.0                                                        
      SUMCDI=0.0                                                        
C***********************************************************************
C   CALCULATE HEATING AND COOLING RATES FOR EACH GRAIN TYPE             
C***********************************************************************
      IF (IMIX.EQ.1) TDI(IG,L)=TD(L)                                    
      DO 190 J=1,NFD                                                    
      XX=QABSI(J,IG)*WFD(J)                                             
      CALL GETBJ (FREQD(J),TDI(IG,L),0,BJ0,BJ1)                         
      SUMCDI=SUMCDI+XX*BJ0                                              
      SUMHDI=SUMHDI+XX*AJ(J,L)                                          
  190 CONTINUE                                                          
      XX=CCOOLD*RHOD(L)*R0*ABUNDI(IG,L)                                 
      COOLDI(IG,L)=XX*SUMCDI                                            
      HEATDI(IG,L)=XX*SUMHDI                                            
  210 CONTINUE                                                          
  220 CONTINUE                                                          
C***********************************************************************
C   WRITE CLOUD TEMPERATURE CHARACTERISTICS AND NET FLUX TO             
C   OUTPUT                                                              
C***********************************************************************
      WRITE (6,996)                                                     
  996 FORMAT (1H1,1X,'IR',7X,'R',8X,'RHOD',6X,'TAUOF',7X,'TD',          
     1 7X,'AVFLUX',6X,'COOLD',6X,'HEATD')                               
      WRITE (6,997) (L,R(L),RHOD(L),TOF(L),TD(L),AVFLUX(L),             
     1COOLD(L),HEATD(L),L=1,NR)                                         
  997 FORMAT (1H9,I3,1X,1P7E11.3)                                       
C***********************************************************************
C   WRITE CLOUD TEMPERATURE CHARACTERISTICS AND NET FLUX                
C   FOR EACH GRAIN TYPE TO OUTPUT                                       
C***********************************************************************
      DO 230 K1=1,IMIX,2                                                
      K2=MIN0(IMIX,K1+1)                                                
      WRITE (6,998) (K,K,K,K,K=K1,K2)                                   
  998 FORMAT (1H1,1X,'IR',6X,'R',5X,2(1X,'ABUNDI(',I2,')',2X,'TDI(',    
     1 I2,')',3X,'COOLDI(',I2,')',1X,'HEATDI(',I2,')'))                 
      DO 230 L=1,NR                                                     
      WRITE (6,999) L,R(L),(ABUNDI(K,L),TDI(K,L),COOLDI(K,L),           
     1              HEATDI(K,L),K=K1,K2)                                
  999 FORMAT (1H9,I3,1X,1P9E11.3)                                       
  230 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                        
C***********************************************************************
C   THE SUBROUTINE CONVOL CALCULATES THE CONVOLUTION INTEGRAL           
C   AT POINTS ON THE SURFACE OF THE CLOUD                               
C   AR     --- BEAM RADIAL GRID                                         
C   ANG    --- BEAM ANGULAR GRID                                        
C   CONV   --- CONVOLUTION INTEGRAL                                     
C   SUMAR  --- SUM OF RADIAL WEIGHTS                                    
C   SUMANG --- SUM OF ANGULAR WEIGHTS                                   
C   PANG   --- NORMALIZATION FACTOR FOR ANGULAR WEIGHTS                 
C   WAR    --- TRAPEZOIDAL QUADRATURE WEIGHTS FOR RADIAL INTEGRATION    
C   WTANG  --- TRAPEZOIDAL QUADRATURE WEIGHTS FOR ANGULAR INTEGRATION   
C   SIGMA  --- HALF WIDTH OF BEAM PATTERN                               
C***********************************************************************
      SUBROUTINE CONVOL(E,EC)                                           
      IMPLICIT REAL*8(A-H,O-Z),INTEGER*4(I-N)                           
      PARAMETER (ND=100,NF=60,NC=9,NPMAX=ND+NC,NDF=ND*NF)               
      PARAMETER (NC1=NC+1,NC2=NC+2,NFC2=NF*NC2)                         
      PARAMETER (NDF1P1=NDF+1,NDF2P1=2*NDF+1,NDF3P1=3*NDF+1,            
     1           NDF4P1=4*NDF+1,NDF5P1=5*NDF+1,NDF6P1=6*NDF+1,          
     2           NDF7P1=7*NDF+1,NDF8P1=8*NDF+1,NDF9P1=9*NDF+1,          
     3           NDF8M=NDF8P1-1)                                        
      PARAMETER (NFC1P1=NFC2+1,NFC2P1=2*NFC2+1,NFC3P1=3*NFC2+1,         
     1           NFC4P1=4*NFC2+1,NFC5P1=5*NFC2+1,                       
     2           NFC6P1=6*NFC2+1,NFC7P1=7*NFC2+1,                       
     3           NFC8P1=8*NFC2+1,NCEIC=NFC8P1+NDF)                      
      PARAMETER (NXY=6*NDF+NFC2,NANG=20,NP2MAX=2*NPMAX,                 
     1           NTHETA=20)                                             
      DIMENSION SIGMA(NF),WTANG(NANG),XMU(NTHETA),WAR(NP2MAX),ANG(NANG),
     1          E(NF,ND),EC(NF,NC2),CONV(NF,10),AR(30),EINT1(NF,NPMAX)  
      COMMON/CONST/PI,CV,PLANK,BOLTZ,PARSEC,HK,CTH                      
      COMMON/PARM/NR,NFD,N1,NP,IP0,IP1,NPS1,IMIX                        
      COMMON/PFREQ/FREQD(NF),WFD(NF),WLAMDA(NF)                         
      COMMON/SOLN/TD(ND),RHOD(ND),R(ND),P(NPMAX)                        
      COMMON/SWITCH/IEDFTR,IOUT,IMODEL,IC,IB,ISCA,NH,IGEOM,IEMRG        
      NP1=NP-1                                                          
C***********************************************************************
C   READ BEAM WIDTH FOR EACH FREQUENCY FROM THE INPUT DATA SET          
C***********************************************************************
      READ (5,300)(SIGMA(J),J=1,NFD)                                    
  300 FORMAT (1P7E10.3,10X)                                             
      IF (IC.EQ.0.OR.IGEOM.EQ.0) GO TO 35                               
      DO 30 J=1,NFD                                                     
      DO 10 L=1,NC                                                      
      EINT1(J,L)=EC(J,L)                                                
   10 CONTINUE                                                          
      DO 20 LL=1,NR                                                     
      EINT1(J,LL+NC)=E(J,LL)                                            
   20 CONTINUE                                                          
   30 CONTINUE                                                          
      GO TO 45                                                          
   35 CONTINUE                                                          
      DO 40 J=1,NFD                                                     
      DO 40 L=1,NR                                                      
      EINT1(J,L)=E(J,L)                                                 
   40 CONTINUE                                                          
   45 CONTINUE                                                          
      NANG1=NANG-1                                                      
      CON1=2.0*PI/(NANG-1)                                              
      DO 50 K=1,NANG                                                    
      ANG(K)=(K-1)*CON1                                                 
   50 CONTINUE                                                          
C***********************************************************************
C   CALCULATE WEIGHTS FOR THE ANGULAR INTEGRATION                       
C***********************************************************************
      WTANG(1)=0.0                                                      
      DO 60 IANG2=1,NANG                                                
      IF (IANG2.EQ.NANG) GO TO 65                                       
      B=ANG(IANG2)                                                      
      F=ANG(IANG2+1)                                                    
      WTANG(IANG2)=WTANG(IANG2)+0.5*(F-B)                               
      WTANG(IANG2+1)=0.5*(F-B)                                          
   60 CONTINUE                                                          
   65 CONTINUE                                                          
      SUMANG=0.0                                                        
      DO 70 IANG2=1,NANG                                                
      SUMANG=SUMANG+WTANG(IANG2)                                        
   70 CONTINUE                                                          
      PANG=2.0*PI/SUMANG                                                
      DO 80 IANG2=1,NANG                                                
      WTANG(IANG2)=PANG*WTANG(IANG2)                                    
   80 CONTINUE                                                          
      CON3=P(NP)/9.0                                                    
      DO 200 MN=1,10                                                    
      DO 90 J=1,NFD                                                     
      DO 90 L=1,10                                                      
      CONV(J,L)=0.0                                                     
   90 CONTINUE                                                          
      Y=DFLOAT(MN-1)*CON3                                               
      DO 180 J=1,NFD                                                    
      CON4=1.0/(2.0*SIGMA(J)*SIGMA(J))                                  
      CON5=CON4/PI                                                      
C***********************************************************************
C   CALCULATE RADIAL GRID ACROSS THE BEAM                               
C***********************************************************************
      CON2=4.0*SIGMA(J)/29.0                                            
      DO 100 L=1,30                                                     
      AR(L)=DFLOAT(L-1)*CON2                                            
  100 CONTINUE                                                          
C***********************************************************************
C   CALCULATE RADIAL INTEGRATION WEIGHTS                                
C***********************************************************************
      WAR(1)=0.0                                                        
      DO 110 L=1,29                                                     
      B1=AR(L)                                                          
      F1=AR(L+1)                                                        
      B1F=B1*F1                                                         
      B12=B1*B1                                                         
      F12=F1*F1                                                         
      WAR(L)=WAR(L)+(F12+B1F-2.0*B12)/6.0                               
      WAR(L+1)=(2.0*F12-B1F-B12)/6.0                                    
  110 CONTINUE                                                          
      SUMR=0.0                                                          
      DO 120 L=1,30                                                     
      SUMR=SUMR+WAR(L)                                                  
  120 CONTINUE                                                          
      PAR=0.5*(4.0*SIGMA(J))**2.0                                       
      PR=PAR/SUMR                                                       
      DO 130 L=1,30                                                     
      WAR(L)=PR*WAR(L)                                                  
  130 CONTINUE                                                          
      DO 170 M=1,10                                                     
      X=DFLOAT(M-1)*CON3                                                
      D=DSQRT(X*X+Y*Y)                                                  
      PHI=PI/2.0                                                        
      IF (X.EQ.0.0.AND.Y.EQ.0.0) PHI=0.0                                
      IF (X.EQ.0.0.OR.Y.EQ.0.0) GOTO 135                                
      PHI=DATAN(Y/X)                                                    
  135 CONTINUE                                                          
      DO 160 IANG=1,NANG                                                
      CONVR=0.0                                                         
      DO 150 L=1,30                                                     
      IF (IGEOM.EQ.1) GO TO 136                                         
C***********************************************************************
C   CALCULATE IMPACT PARAMETER ACROSS THE CLOUD FOR PLANAR AND          
C   SPHERICAL GEOMETRIES                                                
C***********************************************************************
      ARG=PI-ANG(IANG)+PHI                                              
      IF (ANG(IANG).GT.PI) ARG=ANG(IANG)-PI-PHI                         
      PV=DSQRT(D*D+AR(L)*AR(L)-2.0*D*AR(L)*DCOS(ARG))                   
      GO TO 137                                                         
  136 CONTINUE                                                          
C***********************************************************************
C   CALCULATE IMPACT PARAMETER ACROSS THE CLOUD FOR CYLINDRICAL         
C   GEOMETRIES                                                          
C***********************************************************************
      PV=DABS(AR(L)*DCOS(PI-ANG(IANG))-D)                               
      IF (ANG(IANG).LT.PI/2) PV=D+AR(L)*DCOS(ANG(IANG))                 
  137 CONTINUE                                                          
      IF (PV.GT.P(NP)) GO TO 150                                        
      DO 140 LM=1,NP1                                                   
      IF (PV.GE.P(LM).AND.PV.LT.P(LM+1)) LQ=LM                          
  140 CONTINUE                                                          
      IF (PV.LT.P(1)) EINT=EINT1(J,1)                                   
      IF (PV.LT.P(1)) GOTO 145                                          
C***********************************************************************
C   INTERPOLATE EMERGENT INTENSITIES                                    
C***********************************************************************
      SLOPE=(EINT1(J,LQ+1)-EINT1(J,LQ))/(P(LQ+1)-P(LQ))                 
      EINT=EINT1(J,LQ)-SLOPE*P(LQ)+SLOPE*PV                             
C***********************************************************************
C   CALCULATE CONVOLUTION INTEGRAL                                      
C***********************************************************************
  145 ZX=-((AR(L)*AR(L))*CON4)                                          
      Z=DEXP(ZX)*CON5                                                   
      ZY=EINT*WAR(L)*Z                                                  
      CONVR=CONVR+ZY                                                    
  150 CONTINUE                                                          
      CONV(J,M)=CONV(J,M)+WTANG(IANG)*CONVR                             
  160 CONTINUE                                                          
  170 CONTINUE                                                          
  180 CONTINUE                                                          
C***********************************************************************
C   WRITE CONVOLUTION INTEGRALS TO OUTPUT FILE                          
C***********************************************************************
      WRITE (6,111) Y                                                   
  111 FORMAT (1H1,25X,'CONVOLVED INTENSITIES, AT GIVEN X AND Y  FROM CLO
     1UD CENTER Y = ',F5.3)                                             
      WRITE (6,222)                                                     
  222 FORMAT (1H9,1X,'IF',4X,'MICRON',3X,'SIGMA',3X,'X = 0.0',3X,       
     1'X=0.111',3X,'X=0.222',3X,'X=0.333',3X,'X=0.444',3X,'X=0.556',    
     23X,'X=0.667',3X,'X=0.778',3X,'X=0.889',3X,'X=1.000'/)             
      DO 190 J=1,NFD                                                    
      WRITE (6,333) J,WLAMDA(J),SIGMA(J),(CONV(J,M),M=1,10)             
  333 FORMAT (1H9,I3,1P12E10.3)                                         
  190 CONTINUE                                                          
      IF (IGEOM.NE.2) RETURN                                            
  200 CONTINUE                                                          
      RETURN                                                            
      END                                                        
