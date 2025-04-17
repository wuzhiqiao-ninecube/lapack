*> \brief \b QBLAT1
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       PROGRAM QBLAT1
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    Test program for the REAL*16 Level 1 BLAS.
*>
*>    Based upon the original BLAS test routine together with:
*>    F06EAF Example Program Text
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup single_blas_testing
*
*  =====================================================================
      PROGRAM QBLAT1
*
*  -- Reference BLAS test routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          ICASE, INCX, INCY, N
      LOGICAL          PASS
*     .. Local Scalars ..
      REAL*16          SFAC
      INTEGER          IC
*     .. External Subroutines ..
      EXTERNAL         CHECK0, CHECK1, CHECK2, CHECK3, HEADER
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, PASS
*     .. Data statements ..
      DATA             SFAC/9.765625Q-4/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      DO 20 IC = 1, 13
         ICASE = IC
         CALL HEADER
*
*        .. Initialize  PASS,  INCX,  and INCY for a new case. ..
*        .. the value 9999 for INCX or INCY will appear in the ..
*        .. detailed  output, if any, for cases  that do not involve ..
*        .. these parameters ..
*
         PASS = .TRUE.
         INCX = 9999
         INCY = 9999
         IF (ICASE.EQ.3 .OR. ICASE.EQ.11) THEN
            CALL CHECK0(SFAC)
         ELSE IF (ICASE.EQ.7 .OR. ICASE.EQ.8 .OR. ICASE.EQ.9 .OR.
     +            ICASE.EQ.10) THEN
            CALL CHECK1(SFAC)
         ELSE IF (ICASE.EQ.1 .OR. ICASE.EQ.2 .OR. ICASE.EQ.5 .OR.
     +            ICASE.EQ.6 .OR. ICASE.EQ.12 .OR. ICASE.EQ.13) THEN
            CALL CHECK2(SFAC)
         ELSE IF (ICASE.EQ.4) THEN
            CALL CHECK3(SFAC)
         END IF
*        -- Print
         IF (PASS) WRITE (NOUT,99998)
   20 CONTINUE
      STOP
*
99999 FORMAT (' Real BLAS Test Program Results',/1X)
99998 FORMAT ('                                 ----- PASS -----')
*
*     End of QBLAT1
*
      END
      SUBROUTINE HEADER
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          ICASE, INCX, INCY, N
      LOGICAL          PASS
*     .. Local Arrays ..
      CHARACTER*6      L(13)
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, PASS
*     .. Data statements ..
      DATA             L(1)/' QDOT '/
      DATA             L(2)/'QAXPY '/
      DATA             L(3)/'QROTG '/
      DATA             L(4)/' QROT '/
      DATA             L(5)/'QCOPY '/
      DATA             L(6)/'QSWAP '/
      DATA             L(7)/'QNRM2 '/
      DATA             L(8)/'QASUM '/
      DATA             L(9)/'QSCAL '/
      DATA             L(10)/'IQAMAX'/
      DATA             L(11)/'QROTMG'/
      DATA             L(12)/'QROTM '/
      DATA             L(13)/'QDSDOT'/
*     .. Executable Statements ..
      WRITE (NOUT,99999) ICASE, L(ICASE)
      RETURN
*
99999 FORMAT (/' Test of subprogram number',I3,12X,A6)
*
*     End of HEADER
*
      END
      SUBROUTINE CHECK0(SFAC)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      REAL*16           SFAC
*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, N
      LOGICAL           PASS
*     .. Local Scalars ..
      REAL*16           D12, SA, SB, SC, SS
      INTEGER           I, K
*     .. Local Arrays ..
      REAL*16           DA1(8), DATRUE(8), DB1(8), DBTRUE(8), DC1(8),
     +                  DS1(8), DAB(4,9), DTEMP(9), DTRUE(9,9)
*     .. External Subroutines ..
      EXTERNAL          QROTG, QROTMG, STEST, STEST1
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, PASS
*     .. Data statements ..
      DATA              DA1/0.3Q0, 0.4Q0, -0.3Q0, -0.4Q0, -0.3Q0, 0.0Q0,
     +                  0.0Q0, 1.0Q0/
      DATA              DB1/0.4Q0, 0.3Q0, 0.4Q0, 0.3Q0, -0.4Q0, 0.0Q0,
     +                  1.0Q0, 0.0Q0/
      DATA              DC1/0.6Q0, 0.8Q0, -0.6Q0, 0.8Q0, 0.6Q0, 1.0Q0,
     +                  0.0Q0, 1.0Q0/
      DATA              DS1/0.8Q0, 0.6Q0, 0.8Q0, -0.6Q0, 0.8Q0, 0.0Q0,
     +                  1.0Q0, 0.0Q0/
      DATA              DATRUE/0.5Q0, 0.5Q0, 0.5Q0, -0.5Q0, -0.5Q0,
     +                  0.0Q0, 1.0Q0, 1.0Q0/
      DATA              DBTRUE/0.0Q0, 0.6Q0, 0.0Q0, -0.6Q0, 0.0Q0,
     +                  0.0Q0, 1.0Q0, 0.0Q0/
*     INPUT FOR MODIFIED GIVENS
      DATA DAB/ .1Q0,.3Q0,1.2Q0,.2Q0,
     A          .7Q0, .2Q0, .6Q0, 4.2Q0,
     B          0.Q0,0.Q0,0.Q0,0.Q0,
     C          4.Q0, -1.Q0, 2.Q0, 4.Q0,
     D          6.Q-10, 2.Q-2, 1.Q5, 10.Q0,
     E          4.Q10, 2.Q-2, 1.Q-5, 10.Q0,
     F          2.Q-10, 4.Q-2, 1.Q5, 10.Q0,
     G          2.Q10, 4.Q-2, 1.Q-5, 10.Q0,
     H          4.Q0, -2.Q0, 8.Q0, 4.Q0    /
*    TRUE RESULTS FOR MODIFIED GIVENS
      DATA DTRUE/0.Q0,0.Q0, 1.3Q0, .2Q0, 0.Q0,0.Q0,0.Q0, .5Q0, 0.Q0,
     A           0.Q0,0.Q0, 4.5Q0, 4.2Q0, 1.Q0, .5Q0, 0.Q0,0.Q0,0.Q0,
     B           0.Q0,0.Q0,0.Q0,0.Q0, -2.Q0, 0.Q0,0.Q0,0.Q0,0.Q0,
     C           0.Q0,0.Q0,0.Q0, 4.Q0, -1.Q0, 0.Q0,0.Q0,0.Q0,0.Q0,
     D           0.Q0, 15.Q-3, 0.Q0, 10.Q0, -1.Q0, 0.Q0, -1.Q-4,
     E           0.Q0, 1.Q0,
     F           0.Q0,0.Q0, 6144.Q-5, 10.Q0, -1.Q0, 4096.Q0, -1.Q6,
     G           0.Q0, 1.Q0,
     H           0.Q0,0.Q0,15.Q0,10.Q0,-1.Q0, 5.Q-5, 0.Q0,1.Q0,0.Q0,
     I           0.Q0,0.Q0, 15.Q0, 10.Q0, -1. Q0, 5.Q5, -4096.Q0,
     J           1.Q0, 4096.Q-6,
     K           0.Q0,0.Q0, 7.Q0, 4.Q0, 0.Q0,0.Q0, -.5Q0, -.25Q0, 0.Q0/
*                   4096 = 2 ** 12
      DATA D12  /4096.Q0/
      DTRUE(1,1) = 12.Q0 / 130.Q0
      DTRUE(2,1) = 36.Q0 / 130.Q0
      DTRUE(7,1) = -1.Q0 / 6.Q0
      DTRUE(1,2) = 14.Q0 / 75.Q0
      DTRUE(2,2) = 49.Q0 / 75.Q0
      DTRUE(9,2) = 1.Q0 / 7.Q0
      DTRUE(1,5) = 45.Q-11 * (D12 * D12)
      DTRUE(3,5) = 4.Q5 / (3.Q0 * D12)
      DTRUE(6,5) = 1.Q0 / D12
      DTRUE(8,5) = 1.Q4 / (3.Q0 * D12)
      DTRUE(1,6) = 4.Q10 / (1.5Q0 * D12 * D12)
      DTRUE(2,6) = 2.Q-2 / 1.5Q0
      DTRUE(8,6) = 5.Q-7 * D12
      DTRUE(1,7) = 4.Q0 / 150.Q0
      DTRUE(2,7) = (2.Q-10 / 1.5Q0) * (D12 * D12)
      DTRUE(7,7) = -DTRUE(6,5)
      DTRUE(9,7) = 1.Q4 / D12
      DTRUE(1,8) = DTRUE(1,7)
      DTRUE(2,8) = 2.Q10 / (1.5Q0 * D12 * D12)
      DTRUE(1,9) = 32.Q0 / 7.Q0
      DTRUE(2,9) = -16.Q0 / 7.Q0
*     .. Executable Statements ..
*
*     Compute true values which cannot be prestored
*     in decimal notation
*
      DBTRUE(1) = 1.0Q0/0.6Q0
      DBTRUE(3) = -1.0Q0/0.6Q0
      DBTRUE(5) = 1.0Q0/0.6Q0
*
      DO 20 K = 1, 8
*        .. Set N=K for identification in output if any ..
         N = K
         IF (ICASE.EQ.3) THEN
*           .. SROTG ..
            IF (K.GT.8) GO TO 40
            SA = DA1(K)
            SB = DB1(K)
            CALL QROTG(SA,SB,SC,SS)
            CALL STEST1(SA,DATRUE(K),DATRUE(K),SFAC)
            CALL STEST1(SB,DBTRUE(K),DBTRUE(K),SFAC)
            CALL STEST1(SC,DC1(K),DC1(K),SFAC)
            CALL STEST1(SS,DS1(K),DS1(K),SFAC)
         ELSEIF (ICASE.EQ.11) THEN
*           .. QROTMG ..
            DO I=1,4
               DTEMP(I)= DAB(I,K)
               DTEMP(I+4) = 0.0
            END DO
            DTEMP(9) = 0.0
            CALL QROTMG(DTEMP(1),DTEMP(2),DTEMP(3),DTEMP(4),DTEMP(5))
            CALL STEST(9,DTEMP,DTRUE(1,K),DTRUE(1,K),SFAC)
         ELSE
            WRITE (NOUT,*) ' Shouldn''t be here in CHECK0'
            STOP
         END IF
   20 CONTINUE
   40 RETURN
*
*     End of CHECK0
*
      END
      SUBROUTINE CHECK1(SFAC)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      REAL*16           SFAC
*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, N
      LOGICAL           PASS
*     .. Local Scalars ..
      INTEGER           I, IX, LEN, NP1
*     .. Local Arrays ..
      REAL*16           DTRUE1(5), DTRUE3(5), DTRUE5(8,5,2), DV(8,5,2),
     +                  DVR(8), SA(10), STEMP(1), STRUE(8), SX(8),
     +                  SXR(15)
      INTEGER           ITRUE2(5), ITRUEC(5)
*     .. External Functions ..
      REAL*16           QASUM, QNRM2
      INTEGER           IQAMAX
      EXTERNAL          QASUM, QNRM2, IQAMAX
*     .. External Subroutines ..
      EXTERNAL          ITEST1, QSCAL, STEST, STEST1
*     .. Intrinsic Functions ..
      INTRINSIC         MAX
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, PASS
*     .. Data statements ..
      DATA              SA/0.3Q0, -1.0Q0, 0.0Q0, 1.0Q0, 0.3Q0, 0.3Q0,
     +                  0.3Q0, 0.3Q0, 0.3Q0, 0.3Q0/
      DATA              DV/0.1Q0, 2.0Q0, 2.0Q0, 2.0Q0, 2.0Q0, 2.0Q0,
     +                  2.0Q0, 2.0Q0, 0.3Q0, 3.0Q0, 3.0Q0, 3.0Q0, 3.0Q0,
     +                  3.0Q0, 3.0Q0, 3.0Q0, 0.3Q0, -0.4Q0, 4.0Q0,
     +                  4.0Q0, 4.0Q0, 4.0Q0, 4.0Q0, 4.0Q0, 0.2Q0,
     +                  -0.6Q0, 0.3Q0, 5.0Q0, 5.0Q0, 5.0Q0, 5.0Q0,
     +                  5.0Q0, 0.1Q0, -0.3Q0, 0.5Q0, -0.1Q0, 6.0Q0,
     +                  6.0Q0, 6.0Q0, 6.0Q0, 0.1Q0, 8.0Q0, 8.0Q0, 8.0Q0,
     +                  8.0Q0, 8.0Q0, 8.0Q0, 8.0Q0, 0.3Q0, 9.0Q0, 9.0Q0,
     +                  9.0Q0, 9.0Q0, 9.0Q0, 9.0Q0, 9.0Q0, 0.3Q0, 2.0Q0,
     +                  -0.4Q0, 2.0Q0, 2.0Q0, 2.0Q0, 2.0Q0, 2.0Q0,
     +                  0.2Q0, 3.0Q0, -0.6Q0, 5.0Q0, 0.3Q0, 2.0Q0,
     +                  2.0Q0, 2.0Q0, 0.1Q0, 4.0Q0, -0.3Q0, 6.0Q0,
     +                  -0.5Q0, 7.0Q0, -0.1Q0, 3.0Q0/
      DATA              DVR/8.0Q0, -7.0Q0, 9.0Q0, 5.0Q0, 9.0Q0, 8.0Q0,
     +                  7.0Q0, 7.0Q0/
      DATA              DTRUE1/0.0Q0, 0.3Q0, 0.5Q0, 0.7Q0, 0.6Q0/
      DATA              DTRUE3/0.0Q0, 0.3Q0, 0.7Q0, 1.1Q0, 1.0Q0/
      DATA              DTRUE5/0.10Q0, 2.0Q0, 2.0Q0, 2.0Q0, 2.0Q0,
     +                  2.0Q0, 2.0Q0, 2.0Q0, -0.3Q0, 3.0Q0, 3.0Q0,
     +                  3.0Q0, 3.0Q0, 3.0Q0, 3.0Q0, 3.0Q0, 0.0Q0, 0.0Q0,
     +                  4.0Q0, 4.0Q0, 4.0Q0, 4.0Q0, 4.0Q0, 4.0Q0,
     +                  0.20Q0, -0.60Q0, 0.30Q0, 5.0Q0, 5.0Q0, 5.0Q0,
     +                  5.0Q0, 5.0Q0, 0.03Q0, -0.09Q0, 0.15Q0, -0.03Q0,
     +                  6.0Q0, 6.0Q0, 6.0Q0, 6.0Q0, 0.10Q0, 8.0Q0,
     +                  8.0Q0, 8.0Q0, 8.0Q0, 8.0Q0, 8.0Q0, 8.0Q0,
     +                  0.09Q0, 9.0Q0, 9.0Q0, 9.0Q0, 9.0Q0, 9.0Q0,
     +                  9.0Q0, 9.0Q0, 0.09Q0, 2.0Q0, -0.12Q0, 2.0Q0,
     +                  2.0Q0, 2.0Q0, 2.0Q0, 2.0Q0, 0.06Q0, 3.0Q0,
     +                  -0.18Q0, 5.0Q0, 0.09Q0, 2.0Q0, 2.0Q0, 2.0Q0,
     +                  0.03Q0, 4.0Q0, -0.09Q0, 6.0Q0, -0.15Q0, 7.0Q0,
     +                  -0.03Q0, 3.0Q0/
      DATA              ITRUE2/0, 1, 2, 2, 3/
      DATA              ITRUEC/0, 1, 1, 1, 1/
*     .. Executable Statements ..
      DO 80 INCX = 1, 2
         DO 60 NP1 = 1, 5
            N = NP1 - 1
            LEN = 2*MAX(N,1)
*           .. Set vector arguments ..
            DO 20 I = 1, LEN
               SX(I) = DV(I,NP1,INCX)
   20       CONTINUE
*
            IF (ICASE.EQ.7) THEN
*              .. QNRM2 ..
               STEMP(1) = DTRUE1(NP1)
               CALL STEST1(QNRM2(N,SX,INCX),STEMP(1),STEMP,SFAC)
            ELSE IF (ICASE.EQ.8) THEN
*              .. QASUM ..
               STEMP(1) = DTRUE3(NP1)
               CALL STEST1(QASUM(N,SX,INCX),STEMP(1),STEMP,SFAC)
            ELSE IF (ICASE.EQ.9) THEN
*              .. QSCAL ..
               CALL QSCAL(N,SA((INCX-1)*5+NP1),SX,INCX)
               DO 40 I = 1, LEN
                  STRUE(I) = DTRUE5(I,NP1,INCX)
   40          CONTINUE
               CALL STEST(LEN,SX,STRUE,STRUE,SFAC)
            ELSE IF (ICASE.EQ.10) THEN
*              .. IQAMAX ..
               CALL ITEST1(IQAMAX(N,SX,INCX),ITRUE2(NP1))
               DO 100 I = 1, LEN
                  SX(I) = 42.0Q0
  100          CONTINUE
               CALL ITEST1(IQAMAX(N,SX,INCX),ITRUEC(NP1))
            ELSE
               WRITE (NOUT,*) ' Shouldn''t be here in CHECK1'
               STOP
            END IF
   60    CONTINUE
         IF (ICASE.EQ.10) THEN
            N = 8
            IX = 1
            DO 120 I = 1, N
               SXR(IX) = DVR(I)
               IX = IX + INCX
  120       CONTINUE
            CALL ITEST1(IQAMAX(N,SXR,INCX),3)
         END IF
   80 CONTINUE
      RETURN
*
*     End of CHECK1
*
      END
      SUBROUTINE CHECK2(SFAC)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      REAL*16           SFAC
*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, N
      LOGICAL           PASS
*     .. Local Scalars ..
      REAL*16           SA
      INTEGER           I, J, KI, KN, KNI, KPAR, KSIZE, LENX, LENY,
     $                  LINCX, LINCY, MX, MY
*     .. Local Arrays ..
      REAL*16           DT10X(7,4,4), DT10Y(7,4,4), DT7(4,4),
     $                  DT8(7,4,4), DX1(7),
     $                  DY1(7), SSIZE1(4), SSIZE2(14,2), SSIZE3(4),
     $                  SSIZE(7), STX(7), STY(7), SX(7), SY(7),
     $                  DPAR(5,4), DT19X(7,4,16),DT19XA(7,4,4),
     $                  DT19XB(7,4,4), DT19XC(7,4,4),DT19XD(7,4,4),
     $                  DT19Y(7,4,16), DT19YA(7,4,4),DT19YB(7,4,4),
     $                  DT19YC(7,4,4), DT19YD(7,4,4), DTEMP(5),
     $                  ST7B(4,4), STY0(1), SX0(1), SY0(1)
      INTEGER           INCXS(4), INCYS(4), LENS(4,2), NS(4)
*     .. External Functions ..
      REAL*16           QDOT, QDSDOT
      EXTERNAL          QDOT, QDSDOT
*     .. External Subroutines ..
      EXTERNAL          QAXPY, QCOPY, QROTM, QSWAP, STEST, STEST1
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, MIN
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, PASS
*     .. Data statements ..
      EQUIVALENCE (DT19X(1,1,1),DT19XA(1,1,1)),(DT19X(1,1,5),
     A   DT19XB(1,1,1)),(DT19X(1,1,9),DT19XC(1,1,1)),
     B   (DT19X(1,1,13),DT19XD(1,1,1))
      EQUIVALENCE (DT19Y(1,1,1),DT19YA(1,1,1)),(DT19Y(1,1,5),
     A   DT19YB(1,1,1)),(DT19Y(1,1,9),DT19YC(1,1,1)),
     B   (DT19Y(1,1,13),DT19YD(1,1,1))

      DATA              SA/0.3Q0/
      DATA              INCXS/1, 2, -2, -1/
      DATA              INCYS/1, -2, 1, -2/
      DATA              LENS/1, 1, 2, 4, 1, 1, 3, 7/
      DATA              NS/0, 1, 2, 4/
      DATA              DX1/0.6Q0, 0.1Q0, -0.5Q0, 0.8Q0, 0.9Q0, -0.3Q0,
     +                  -0.4Q0/
      DATA              DY1/0.5Q0, -0.9Q0, 0.3Q0, 0.7Q0, -0.6Q0, 0.2Q0,
     +                  0.8Q0/
      DATA              DT7/0.0Q0, 0.30Q0, 0.21Q0, 0.62Q0, 0.0Q0,
     +                  0.30Q0, -0.07Q0, 0.85Q0, 0.0Q0, 0.30Q0, -0.79Q0,
     +                  -0.74Q0, 0.0Q0, 0.30Q0, 0.33Q0, 1.27Q0/
      DATA              ST7B/.1Q0, .4Q0, .31Q0, .72Q0, .1Q0, .4Q0,
     +                  .03Q0, .95Q0,.1Q0, .4Q0, -.69Q0, -.64Q0,
     +                  .1Q0, .4Q0, .43Q0, 1.37Q0/
      DATA              DT8/0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.68Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.68Q0, -0.87Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.68Q0, -0.87Q0, 0.15Q0,
     +                  0.94Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.5Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.68Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.35Q0, -0.9Q0, 0.48Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.38Q0, -0.9Q0, 0.57Q0, 0.7Q0, -0.75Q0,
     +                  0.2Q0, 0.98Q0, 0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.68Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.35Q0, -0.72Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.38Q0,
     +                  -0.63Q0, 0.15Q0, 0.88Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.68Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.68Q0, -0.9Q0, 0.33Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.68Q0, -0.9Q0, 0.33Q0, 0.7Q0,
     +                  -0.75Q0, 0.2Q0, 1.04Q0/
      DATA              DT10X/0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.5Q0, -0.9Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.5Q0, -0.9Q0, 0.3Q0, 0.7Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.3Q0, 0.1Q0, 0.5Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.8Q0, 0.1Q0, -0.6Q0,
     +                  0.8Q0, 0.3Q0, -0.3Q0, 0.5Q0, 0.6Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.5Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, -0.9Q0,
     +                  0.1Q0, 0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.7Q0,
     +                  0.1Q0, 0.3Q0, 0.8Q0, -0.9Q0, -0.3Q0, 0.5Q0,
     +                  0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.5Q0, 0.3Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.5Q0, 0.3Q0, -0.6Q0, 0.8Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0/
      DATA              DT10Y/0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.6Q0, 0.1Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.6Q0, 0.1Q0, -0.5Q0, 0.8Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, -0.5Q0, -0.9Q0, 0.6Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, -0.4Q0, -0.9Q0, 0.9Q0,
     +                  0.7Q0, -0.5Q0, 0.2Q0, 0.6Q0, 0.5Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.6Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, -0.5Q0,
     +                  0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  -0.4Q0, 0.9Q0, -0.5Q0, 0.6Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.6Q0, -0.9Q0, 0.1Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.6Q0, -0.9Q0, 0.1Q0, 0.7Q0,
     +                  -0.5Q0, 0.2Q0, 0.8Q0/
      DATA              SSIZE1/0.0Q0, 0.3Q0, 1.6Q0, 3.2Q0/
      DATA              SSIZE2/0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0,
     +                  1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0,
     +                  1.17Q0, 1.17Q0, 1.17Q0/
      DATA              SSIZE3/ .1Q0, .4Q0, 1.7Q0, 3.3Q0 /
*
*                         FOR DROTM
*
      DATA DPAR/-2.Q0,  0.Q0,0.Q0,0.Q0,0.Q0,
     A          -1.Q0,  2.Q0, -3.Q0, -4.Q0,  5.Q0,
     B           0.Q0,  0.Q0,  2.Q0, -3.Q0,  0.Q0,
     C           1.Q0,  5.Q0,  2.Q0,  0.Q0, -4.Q0/
*                        TRUE X RESULTS F0R ROTATIONS DROTM
      DATA DT19XA/.6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     A            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     B            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     C            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     D            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     E           -.8Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     F           -.9Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     G           3.5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     H            .6Q0,   .1Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     I           -.8Q0,  3.8Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     J           -.9Q0,  2.8Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     K           3.5Q0,  -.4Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     L            .6Q0,   .1Q0,  -.5Q0,   .8Q0,          0.Q0,0.Q0,0.Q0,
     M           -.8Q0,  3.8Q0, -2.2Q0, -1.2Q0,          0.Q0,0.Q0,0.Q0,
     N           -.9Q0,  2.8Q0, -1.4Q0, -1.3Q0,          0.Q0,0.Q0,0.Q0,
     O           3.5Q0,  -.4Q0, -2.2Q0,  4.7Q0,          0.Q0,0.Q0,0.Q0/
*
      DATA DT19XB/.6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     A            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     B            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     C            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     D            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     E           -.8Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     F           -.9Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     G           3.5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     H            .6Q0,   .1Q0,  -.5Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     I           0.Q0,    .1Q0, -3.0Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     J           -.3Q0,   .1Q0, -2.0Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     K           3.3Q0,   .1Q0, -2.0Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     L            .6Q0,   .1Q0,  -.5Q0,   .8Q0,   .9Q0,  -.3Q0,  -.4Q0,
     M          -2.0Q0,   .1Q0,  1.4Q0,   .8Q0,   .6Q0,  -.3Q0, -2.8Q0,
     N          -1.8Q0,   .1Q0,  1.3Q0,   .8Q0,  0.Q0,   -.3Q0, -1.9Q0,
     O           3.8Q0,   .1Q0, -3.1Q0,   .8Q0,  4.8Q0,  -.3Q0, -1.5Q0 /
*
      DATA DT19XC/.6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     A            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     B            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     C            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     D            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     E           -.8Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     F           -.9Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     G           3.5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     H            .6Q0,   .1Q0,  -.5Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     I           4.8Q0,   .1Q0, -3.0Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     J           3.3Q0,   .1Q0, -2.0Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     K           2.1Q0,   .1Q0, -2.0Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     L            .6Q0,   .1Q0,  -.5Q0,   .8Q0,   .9Q0,  -.3Q0,  -.4Q0,
     M          -1.6Q0,   .1Q0, -2.2Q0,   .8Q0,  5.4Q0,  -.3Q0, -2.8Q0,
     N          -1.5Q0,   .1Q0, -1.4Q0,   .8Q0,  3.6Q0,  -.3Q0, -1.9Q0,
     O           3.7Q0,   .1Q0, -2.2Q0,   .8Q0,  3.6Q0,  -.3Q0, -1.5Q0 /
*
      DATA DT19XD/.6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     A            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     B            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     C            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     D            .6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     E           -.8Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     F           -.9Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     G           3.5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     H            .6Q0,   .1Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     I           -.8Q0, -1.0Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     J           -.9Q0,  -.8Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     K           3.5Q0,   .8Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     L            .6Q0,   .1Q0,  -.5Q0,   .8Q0,          0.Q0,0.Q0,0.Q0,
     M           -.8Q0, -1.0Q0,  1.4Q0, -1.6Q0,          0.Q0,0.Q0,0.Q0,
     N           -.9Q0,  -.8Q0,  1.3Q0, -1.6Q0,          0.Q0,0.Q0,0.Q0,
     O           3.5Q0,   .8Q0, -3.1Q0,  4.8Q0,          0.Q0,0.Q0,0.Q0/
*                        TRUE Y RESULTS FOR ROTATIONS DROTM
      DATA DT19YA/.5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     A            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     B            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     C            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     D            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     E            .7Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     F           1.7Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     G          -2.6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     H            .5Q0,  -.9Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     I            .7Q0, -4.8Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     J           1.7Q0,  -.7Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     K          -2.6Q0,  3.5Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     L            .5Q0,  -.9Q0,   .3Q0,   .7Q0,          0.Q0,0.Q0,0.Q0,
     M            .7Q0, -4.8Q0,  3.0Q0,  1.1Q0,          0.Q0,0.Q0,0.Q0,
     N           1.7Q0,  -.7Q0,  -.7Q0,  2.3Q0,          0.Q0,0.Q0,0.Q0,
     O          -2.6Q0,  3.5Q0,  -.7Q0, -3.6Q0,          0.Q0,0.Q0,0.Q0/
*
      DATA DT19YB/.5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     A            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     B            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     C            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     D            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     E            .7Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     F           1.7Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     G          -2.6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     H            .5Q0,  -.9Q0,   .3Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     I           4.0Q0,  -.9Q0,  -.3Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     J           -.5Q0,  -.9Q0,  1.5Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     K          -1.5Q0,  -.9Q0, -1.8Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     L            .5Q0,  -.9Q0,   .3Q0,   .7Q0,  -.6Q0,   .2Q0,   .8Q0,
     M           3.7Q0,  -.9Q0, -1.2Q0,   .7Q0, -1.5Q0,   .2Q0,  2.2Q0,
     N           -.3Q0,  -.9Q0,  2.1Q0,   .7Q0, -1.6Q0,   .2Q0,  2.0Q0,
     O          -1.6Q0,  -.9Q0, -2.1Q0,   .7Q0,  2.9Q0,   .2Q0, -3.8Q0 /
*
      DATA DT19YC/.5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     A            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     B            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     C            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     D            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     E            .7Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     F           1.7Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     G          -2.6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     H            .5Q0,  -.9Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     I           4.0Q0, -6.3Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     J           -.5Q0,   .3Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     K          -1.5Q0,  3.0Q0,             0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     L            .5Q0,  -.9Q0,   .3Q0,   .7Q0,          0.Q0,0.Q0,0.Q0,
     M           3.7Q0, -7.2Q0,  3.0Q0,  1.7Q0,          0.Q0,0.Q0,0.Q0,
     N           -.3Q0,   .9Q0,  -.7Q0,  1.9Q0,          0.Q0,0.Q0,0.Q0,
     O          -1.6Q0,  2.7Q0,  -.7Q0, -3.4Q0,          0.Q0,0.Q0,0.Q0/
*
      DATA DT19YD/.5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     A            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     B            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     C            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     D            .5Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     E            .7Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     F           1.7Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     G          -2.6Q0,                  0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,0.Q0,
     H            .5Q0,  -.9Q0,   .3Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     I            .7Q0,  -.9Q0,  1.2Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     J           1.7Q0,  -.9Q0,   .5Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     K          -2.6Q0,  -.9Q0, -1.3Q0,             0.Q0,0.Q0,0.Q0,0.Q0,
     L            .5Q0,  -.9Q0,   .3Q0,   .7Q0,  -.6Q0,   .2Q0,   .8Q0,
     M            .7Q0,  -.9Q0,  1.2Q0,   .7Q0, -1.5Q0,   .2Q0,  1.6Q0,
     N           1.7Q0,  -.9Q0,   .5Q0,   .7Q0, -1.6Q0,   .2Q0,  2.4Q0,
     O          -2.6Q0,  -.9Q0, -1.3Q0,   .7Q0,  2.9Q0,   .2Q0, -4.0Q0 /
*
*     .. Executable Statements ..
*
      DO 120 KI = 1, 4
         INCX = INCXS(KI)
         INCY = INCYS(KI)
         MX = ABS(INCX)
         MY = ABS(INCY)
*
         DO 100 KN = 1, 4
            N = NS(KN)
            KSIZE = MIN(2,KN)
            LENX = LENS(KN,MX)
            LENY = LENS(KN,MY)
*           .. Initialize all argument arrays ..
            DO 20 I = 1, 7
               SX(I) = DX1(I)
               SY(I) = DY1(I)
   20       CONTINUE
*
            IF (ICASE.EQ.1) THEN
*              .. QDOT ..
               CALL STEST1(QDOT(N,SX,INCX,SY,INCY),DT7(KN,KI),SSIZE1(KN)
     +                     ,SFAC)
            ELSE IF (ICASE.EQ.2) THEN
*              .. QAXPY ..
               CALL QAXPY(N,SA,SX,INCX,SY,INCY)
               DO 40 J = 1, LENY
                  STY(J) = DT8(J,KN,KI)
   40          CONTINUE
               CALL STEST(LENY,SY,STY,SSIZE2(1,KSIZE),SFAC)
            ELSE IF (ICASE.EQ.5) THEN
*              .. QCOPY ..
               DO 60 I = 1, 7
                  STY(I) = DT10Y(I,KN,KI)
   60          CONTINUE
               CALL QCOPY(N,SX,INCX,SY,INCY)
               CALL STEST(LENY,SY,STY,SSIZE2(1,1),1.0Q0)
               IF (KI.EQ.1) THEN
                  SX0(1) = 42.0Q0
                  SY0(1) = 43.0Q0
                  IF (N.EQ.0) THEN
                     STY0(1) = SY0(1)
                  ELSE
                     STY0(1) = SX0(1)
                  END IF
                  LINCX = INCX
                  INCX = 0
                  LINCY = INCY
                  INCY = 0
                  CALL QCOPY(N,SX0,INCX,SY0,INCY)
                  CALL STEST(1,SY0,STY0,SSIZE2(1,1),1.0Q0)
                  INCX = LINCX
                  INCY = LINCY
               END IF
            ELSE IF (ICASE.EQ.6) THEN
*              .. QSWAP ..
               CALL QSWAP(N,SX,INCX,SY,INCY)
               DO 80 I = 1, 7
                  STX(I) = DT10X(I,KN,KI)
                  STY(I) = DT10Y(I,KN,KI)
   80          CONTINUE
               CALL STEST(LENX,SX,STX,SSIZE2(1,1),1.0Q0)
               CALL STEST(LENY,SY,STY,SSIZE2(1,1),1.0Q0)
            ELSEIF (ICASE.EQ.12) THEN
*              .. QROTM ..
               KNI=KN+4*(KI-1)
               DO KPAR=1,4
                  DO I=1,7
                     SX(I) = DX1(I)
                     SY(I) = DY1(I)
                     STX(I)= DT19X(I,KPAR,KNI)
                     STY(I)= DT19Y(I,KPAR,KNI)
                  END DO
*
                  DO I=1,5
                     DTEMP(I) = DPAR(I,KPAR)
                  END DO
*
                  DO  I=1,LENX
                     SSIZE(I)=STX(I)
                  END DO
*                   SEE REMARK ABOVE ABOUT DT11X(1,2,7)
*                       AND DT11X(5,3,8).
                  IF ((KPAR .EQ. 2) .AND. (KNI .EQ. 7))
     $               SSIZE(1) = 2.4Q0
                  IF ((KPAR .EQ. 3) .AND. (KNI .EQ. 8))
     $               SSIZE(5) = 1.8Q0
*
                  CALL   QROTM(N,SX,INCX,SY,INCY,DTEMP)
                  CALL   STEST(LENX,SX,STX,SSIZE,SFAC)
                  CALL   STEST(LENY,SY,STY,STY,SFAC)
               END DO
            ELSEIF (ICASE.EQ.13) THEN
*              .. QDSROT ..
               CALL STEST1 (QDSDOT(N,.1Q0,SX,INCX,SY,INCY),
     $                 ST7B(KN,KI),SSIZE3(KN),SFAC)
            ELSE
               WRITE (NOUT,*) ' Shouldn''t be here in CHECK2'
               STOP
            END IF
  100    CONTINUE
  120 CONTINUE
      RETURN
*
*     End of CHECK2
*
      END
      SUBROUTINE CHECK3(SFAC)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      REAL*16           SFAC
*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, N
      LOGICAL           PASS
*     .. Local Scalars ..
      REAL*16           SC, SS
      INTEGER           I, K, KI, KN, KSIZE, LENX, LENY, MX, MY
*     .. Local Arrays ..
      REAL*16           COPYX(5), COPYY(5), DT9X(7,4,4), DT9Y(7,4,4),
     +                  DX1(7), DY1(7), MWPC(11), MWPS(11), MWPSTX(5),
     +                  MWPSTY(5), MWPTX(11,5), MWPTY(11,5), MWPX(5),
     +                  MWPY(5), SSIZE2(14,2), STX(7), STY(7), SX(7),
     +                  SY(7)
      INTEGER           INCXS(4), INCYS(4), LENS(4,2), MWPINX(11),
     +                  MWPINY(11), MWPN(11), NS(4)
*     .. External Subroutines ..
      EXTERNAL          QROT, STEST
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, MIN
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, PASS
*     .. Data statements ..
      DATA              INCXS/1, 2, -2, -1/
      DATA              INCYS/1, -2, 1, -2/
      DATA              LENS/1, 1, 2, 4, 1, 1, 3, 7/
      DATA              NS/0, 1, 2, 4/
      DATA              DX1/0.6Q0, 0.1Q0, -0.5Q0, 0.8Q0, 0.9Q0, -0.3Q0,
     +                  -0.4Q0/
      DATA              DY1/0.5Q0, -0.9Q0, 0.3Q0, 0.7Q0, -0.6Q0, 0.2Q0,
     +                  0.8Q0/
      DATA              SC, SS/0.8Q0, 0.6Q0/
      DATA              DT9X/0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.78Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.78Q0, -0.46Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.78Q0, -0.46Q0, -0.22Q0,
     +                  1.06Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.6Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.78Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.66Q0, 0.1Q0, -0.1Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.96Q0, 0.1Q0, -0.76Q0, 0.8Q0, 0.90Q0,
     +                  -0.3Q0, -0.02Q0, 0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.78Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, -0.06Q0, 0.1Q0,
     +                  -0.1Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.90Q0,
     +                  0.1Q0, -0.22Q0, 0.8Q0, 0.18Q0, -0.3Q0, -0.02Q0,
     +                  0.6Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.78Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.78Q0, 0.26Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.78Q0, 0.26Q0, -0.76Q0, 1.12Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0/
      DATA              DT9Y/0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.04Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.04Q0, -0.78Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.04Q0, -0.78Q0, 0.54Q0,
     +                  0.08Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.5Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.04Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.7Q0,
     +                  -0.9Q0, -0.12Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.64Q0, -0.9Q0, -0.30Q0, 0.7Q0, -0.18Q0, 0.2Q0,
     +                  0.28Q0, 0.5Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.04Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.7Q0, -1.08Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.64Q0, -1.26Q0,
     +                  0.54Q0, 0.20Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.5Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.04Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.04Q0, -0.9Q0, 0.18Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.04Q0, -0.9Q0, 0.18Q0, 0.7Q0,
     +                  -0.18Q0, 0.2Q0, 0.16Q0/
      DATA              SSIZE2/0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0, 0.0Q0,
     +                  0.0Q0, 1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0,
     +                  1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0, 1.17Q0,
     +                  1.17Q0, 1.17Q0, 1.17Q0/
*     .. Executable Statements ..
*
      DO 60 KI = 1, 4
         INCX = INCXS(KI)
         INCY = INCYS(KI)
         MX = ABS(INCX)
         MY = ABS(INCY)
*
         DO 40 KN = 1, 4
            N = NS(KN)
            KSIZE = MIN(2,KN)
            LENX = LENS(KN,MX)
            LENY = LENS(KN,MY)
*
            IF (ICASE.EQ.4) THEN
*              .. SROT ..
               DO 20 I = 1, 7
                  SX(I) = DX1(I)
                  SY(I) = DY1(I)
                  STX(I) = DT9X(I,KN,KI)
                  STY(I) = DT9Y(I,KN,KI)
   20          CONTINUE
               CALL QROT(N,SX,INCX,SY,INCY,SC,SS)
               CALL STEST(LENX,SX,STX,SSIZE2(1,KSIZE),SFAC)
               CALL STEST(LENY,SY,STY,SSIZE2(1,KSIZE),SFAC)
            ELSE
               WRITE (NOUT,*) ' Shouldn''t be here in CHECK3'
               STOP
            END IF
   40    CONTINUE
   60 CONTINUE
*
      MWPC(1) = 1
      DO 80 I = 2, 11
         MWPC(I) = 0
   80 CONTINUE
      MWPS(1) = 0
      DO 100 I = 2, 6
         MWPS(I) = 1
  100 CONTINUE
      DO 120 I = 7, 11
         MWPS(I) = -1
  120 CONTINUE
      MWPINX(1) = 1
      MWPINX(2) = 1
      MWPINX(3) = 1
      MWPINX(4) = -1
      MWPINX(5) = 1
      MWPINX(6) = -1
      MWPINX(7) = 1
      MWPINX(8) = 1
      MWPINX(9) = -1
      MWPINX(10) = 1
      MWPINX(11) = -1
      MWPINY(1) = 1
      MWPINY(2) = 1
      MWPINY(3) = -1
      MWPINY(4) = -1
      MWPINY(5) = 2
      MWPINY(6) = 1
      MWPINY(7) = 1
      MWPINY(8) = -1
      MWPINY(9) = -1
      MWPINY(10) = 2
      MWPINY(11) = 1
      DO 140 I = 1, 11
         MWPN(I) = 5
  140 CONTINUE
      MWPN(5) = 3
      MWPN(10) = 3
      DO 160 I = 1, 5
         MWPX(I) = I
         MWPY(I) = I
         MWPTX(1,I) = I
         MWPTY(1,I) = I
         MWPTX(2,I) = I
         MWPTY(2,I) = -I
         MWPTX(3,I) = 6 - I
         MWPTY(3,I) = I - 6
         MWPTX(4,I) = I
         MWPTY(4,I) = -I
         MWPTX(6,I) = 6 - I
         MWPTY(6,I) = I - 6
         MWPTX(7,I) = -I
         MWPTY(7,I) = I
         MWPTX(8,I) = I - 6
         MWPTY(8,I) = 6 - I
         MWPTX(9,I) = -I
         MWPTY(9,I) = I
         MWPTX(11,I) = I - 6
         MWPTY(11,I) = 6 - I
  160 CONTINUE
      MWPTX(5,1) = 1
      MWPTX(5,2) = 3
      MWPTX(5,3) = 5
      MWPTX(5,4) = 4
      MWPTX(5,5) = 5
      MWPTY(5,1) = -1
      MWPTY(5,2) = 2
      MWPTY(5,3) = -2
      MWPTY(5,4) = 4
      MWPTY(5,5) = -3
      MWPTX(10,1) = -1
      MWPTX(10,2) = -3
      MWPTX(10,3) = -5
      MWPTX(10,4) = 4
      MWPTX(10,5) = 5
      MWPTY(10,1) = 1
      MWPTY(10,2) = 2
      MWPTY(10,3) = 2
      MWPTY(10,4) = 4
      MWPTY(10,5) = 3
      DO 200 I = 1, 11
         INCX = MWPINX(I)
         INCY = MWPINY(I)
         DO 180 K = 1, 5
            COPYX(K) = MWPX(K)
            COPYY(K) = MWPY(K)
            MWPSTX(K) = MWPTX(I,K)
            MWPSTY(K) = MWPTY(I,K)
  180    CONTINUE
         CALL QROT(MWPN(I),COPYX,INCX,COPYY,INCY,MWPC(I),MWPS(I))
         CALL STEST(5,COPYX,MWPSTX,MWPSTX,SFAC)
         CALL STEST(5,COPYY,MWPSTY,MWPSTY,SFAC)
  200 CONTINUE
      RETURN
*
*     End of CHECK3
*
      END
      SUBROUTINE STEST(LEN,SCOMP,STRUE,SSIZE,SFAC)
*     ********************************* STEST **************************
*
*     THIS SUBR COMPARES ARRAYS  SCOMP() AND STRUE() OF LENGTH LEN TO
*     SEE IF THE TERM BY TERM DIFFERENCES, MULTIPLIED BY SFAC, ARE
*     NEGLIGIBLE.
*
*     C. L. LAWSON, JPL, 1974 DEC 10
*
*     .. Parameters ..
      INTEGER          NOUT
      REAL*16          ZERO
      PARAMETER        (NOUT=6, ZERO=0.0Q0)
*     .. Scalar Arguments ..
      REAL*16          SFAC
      INTEGER          LEN
*     .. Array Arguments ..
      REAL*16          SCOMP(LEN), SSIZE(LEN), STRUE(LEN)
*     .. Scalars in Common ..
      INTEGER          ICASE, INCX, INCY, N
      LOGICAL          PASS
*     .. Local Scalars ..
      REAL*16          SD
      REAL*16          TEMP1,TEMP2
      INTEGER          I
*     .. External Functions ..
      REAL*16          SDIFF
      EXTERNAL         SDIFF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS,EPSILON,REAL
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, PASS
*     .. Executable Statements ..
*
      DO 40 I = 1, LEN
         SD = SCOMP(I) - STRUE(I)
         TEMP1 = ABS(SFAC*SD)
         TEMP2 = ABS(SSIZE(I))*EPSILON(ZERO) + EPSILON(ZERO)
         IF (TEMP1 .LE. TEMP2)
     +       GO TO 40
*
*                             HERE    SCOMP(I) IS NOT CLOSE TO STRUE(I).
*
         IF ( .NOT. PASS) GO TO 20
*                             PRINT FAIL MESSAGE AND HEADER.
         PASS = .FALSE.
         WRITE (NOUT,99999)
         WRITE (NOUT,99998)
   20    WRITE (NOUT,99997) ICASE, N, INCX, INCY, I, SCOMP(I),
     +     STRUE(I), SD, SSIZE(I)
   40 CONTINUE
      RETURN
*
99999 FORMAT ('                                       FAIL')
99998 FORMAT (/' CASE  N INCX INCY  I                            ',
     +       ' COMP(I)                             TRUE(I)  DIFFERENCE',
     +       '     SIZE(I)',/1X)
99997 FORMAT (1X,I4,I3,2I5,I3,2E36.8,2E12.4)
*
*     End of STEST
*
      END
      SUBROUTINE STEST1(SCOMP1,STRUE1,SSIZE,SFAC)
*     ************************* STEST1 *****************************
*
*     THIS IS AN INTERFACE SUBROUTINE TO ACCOMMODATE THE FORTRAN
*     REQUIREMENT THAT WHEN A DUMMY ARGUMENT IS AN ARRAY, THE
*     ACTUAL ARGUMENT MUST ALSO BE AN ARRAY OR AN ARRAY ELEMENT.
*
*     C.L. LAWSON, JPL, 1978 DEC 6
*
*     .. Scalar Arguments ..
      REAL*16           SCOMP1, SFAC, STRUE1
*     .. Array Arguments ..
      REAL*16           SSIZE(*)
*     .. Local Arrays ..
      REAL*16           SCOMP(1), STRUE(1)
*     .. External Subroutines ..
      EXTERNAL          STEST
*     .. Executable Statements ..
*
      SCOMP(1) = SCOMP1
      STRUE(1) = STRUE1
      CALL STEST(1,SCOMP,STRUE,SSIZE,SFAC)
*
      RETURN
*
*     End of STEST1
*
      END
      REAL*16          FUNCTION SDIFF(SA,SB)
*     ********************************* SDIFF **************************
*     COMPUTES DIFFERENCE OF TWO NUMBERS.  C. L. LAWSON, JPL 1974 FEB 15
*
*     .. Scalar Arguments ..
      REAL*16                         SA, SB
*     .. Executable Statements ..
      SDIFF = SA - SB
      RETURN
*
*     End of SDIFF
*
      END
      SUBROUTINE ITEST1(ICOMP,ITRUE)
*     ********************************* ITEST1 *************************
*
*     THIS SUBROUTINE COMPARES THE VARIABLES ICOMP AND ITRUE FOR
*     EQUALITY.
*     C. L. LAWSON, JPL, 1974 DEC 10
*
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      INTEGER           ICOMP, ITRUE
*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, N
      LOGICAL           PASS
*     .. Local Scalars ..
      INTEGER           ID
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, PASS
*     .. Executable Statements ..
*
      IF (ICOMP.EQ.ITRUE) GO TO 40
*
*                            HERE ICOMP IS NOT EQUAL TO ITRUE.
*
      IF ( .NOT. PASS) GO TO 20
*                             PRINT FAIL MESSAGE AND HEADER.
      PASS = .FALSE.
      WRITE (NOUT,99999)
      WRITE (NOUT,99998)
   20 ID = ICOMP - ITRUE
      WRITE (NOUT,99997) ICASE, N, INCX, INCY, ICOMP, ITRUE, ID
   40 CONTINUE
      RETURN
*
99999 FORMAT ('                                       FAIL')
99998 FORMAT (/' CASE  N INCX INCY                               ',
     +       ' COMP                                TRUE     DIFFERENCE',
     +       /1X)
99997 FORMAT (1X,I4,I3,2I5,2I36,I12)
*
*     End of ITEST1
*
      END
