*> \brief \b CBLAT1
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       PROGRAM QCBLAT1
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    Test program for the COMPLEX*32 Level 1 BLAS.
*>    Based upon the original BLAS test routine together with:
*>
*>    F06GAF Example Program Text
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
*> \ingroup COMPLEX*32_blas_testing
*
*  =====================================================================
      PROGRAM QCBLAT1
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
      INTEGER          ICASE, INCX, INCY, MODE, N
      LOGICAL          PASS
*     .. Local Scalars ..
      REAL*16             SFAC
      INTEGER          IC
*     .. External Subroutines ..
      EXTERNAL         CHECK1, CHECK2, HEADER
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Data statements ..
      DATA             SFAC/9.765625Q-4/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      DO 20 IC = 1, 10
         ICASE = IC
         CALL HEADER
*
*        Initialize PASS, INCX, INCY, and MODE for a new case.
*        The value 9999 for INCX, INCY or MODE will appear in the
*        detailed  output, if any, for cases that do not involve
*        these parameters.
*
         PASS = .TRUE.
         INCX = 9999
         INCY = 9999
         MODE = 9999
         IF (ICASE.LE.5) THEN
            CALL CHECK2(SFAC)
         ELSE IF (ICASE.GE.6) THEN
            CALL CHECK1(SFAC)
         END IF
*        -- Print
         IF (PASS) WRITE (NOUT,99998)
   20 CONTINUE
      STOP
*
99999 FORMAT (' COMPLEX*32 BLAS Test Program Results',/1X)
99998 FORMAT ('                                    ----- PASS -----')
*
*     End of QCBLAT1
*
      END
      SUBROUTINE HEADER
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          ICASE, INCX, INCY, MODE, N
      LOGICAL          PASS
*     .. Local Arrays ..
      CHARACTER*6      L(10)
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Data statements ..
      DATA             L(1)/'XDOTC '/
      DATA             L(2)/'XDOTU '/
      DATA             L(3)/'XAXPY '/
      DATA             L(4)/'XCOPY '/
      DATA             L(5)/'XSWAP '/
      DATA             L(6)/'QXNRM2'/
      DATA             L(7)/'QXASUM'/
      DATA             L(8)/'XSCAL '/
      DATA             L(9)/'XQSCAL'/
      DATA             L(10)/'IXAMAX'/
*     .. Executable Statements ..
      WRITE (NOUT,99999) ICASE, L(ICASE)
      RETURN
*
99999 FORMAT (/' Test of subprogram number',I3,12X,A6)
*
*     End of HEADER
*
      END
      SUBROUTINE CHECK1(SFAC)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      REAL*16              SFAC
*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, MODE, N
      LOGICAL           PASS
*     .. Local Scalars ..
      COMPLEX*32        CA
      REAL*16           SA
      INTEGER           I, IX, J, LEN, NP1
*     .. Local Arrays ..
      COMPLEX*32        CTRUE5(8,5,2), CTRUE6(8,5,2), CV(8,5,2), CVR(8),
     +                  CX(8), CXR(15), MWPCS(5), MWPCT(5)
      REAL*16           STRUE2(5), STRUE4(5)
      INTEGER           ITRUE3(5), ITRUEC(5)
*     .. External Functions ..
      REAL*16           QXASUM, QXNRM2
      INTEGER           IXAMAX
      EXTERNAL          QXASUM, QXNRM2, IXAMAX
*     .. External Subroutines ..
      EXTERNAL          XSCAL, XQSCAL, CTEST, ITEST1, STEST1
*     .. Intrinsic Functions ..
      INTRINSIC         MAX
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Data statements ..
      DATA              SA, CA/0.3Q0, (0.4Q0,-0.7Q0)/
      DATA              ((CV(I,J,1),I=1,8),J=1,5)/(0.1Q0,0.1Q0),
     +                  (1.0Q0,2.0Q0), (1.0Q0,2.0Q0), (1.0Q0,2.0Q0),
     +                  (1.0Q0,2.0Q0), (1.0Q0,2.0Q0), (1.0Q0,2.0Q0),
     +                  (1.0Q0,2.0Q0), (0.3Q0,-0.4Q0), (3.0Q0,4.0Q0),
     +                  (3.0Q0,4.0Q0), (3.0Q0,4.0Q0), (3.0Q0,4.0Q0),
     +                  (3.0Q0,4.0Q0), (3.0Q0,4.0Q0), (3.0Q0,4.0Q0),
     +                  (0.1Q0,-0.3Q0), (0.5Q0,-0.1Q0), (5.0Q0,6.0Q0),
     +                  (5.0Q0,6.0Q0), (5.0Q0,6.0Q0), (5.0Q0,6.0Q0),
     +                  (5.0Q0,6.0Q0), (5.0Q0,6.0Q0), (0.1Q0,0.1Q0),
     +                  (-0.6Q0,0.1Q0), (0.1Q0,-0.3Q0), (7.0Q0,8.0Q0),
     +                  (7.0Q0,8.0Q0), (7.0Q0,8.0Q0), (7.0Q0,8.0Q0),
     +                  (7.0Q0,8.0Q0), (0.3Q0,0.1Q0), (0.5Q0,0.0Q0),
     +                  (0.0Q0,0.5Q0), (0.0Q0,0.2Q0), (2.0Q0,3.0Q0),
     +                  (2.0Q0,3.0Q0), (2.0Q0,3.0Q0), (2.0Q0,3.0Q0)/
      DATA              ((CV(I,J,2),I=1,8),J=1,5)/(0.1Q0,0.1Q0),
     +                  (4.0Q0,5.0Q0), (4.0Q0,5.0Q0), (4.0Q0,5.0Q0),
     +                  (4.0Q0,5.0Q0), (4.0Q0,5.0Q0), (4.0Q0,5.0Q0),
     +                  (4.0Q0,5.0Q0), (0.3Q0,-0.4Q0), (6.0Q0,7.0Q0),
     +                  (6.0Q0,7.0Q0), (6.0Q0,7.0Q0), (6.0Q0,7.0Q0),
     +                  (6.0Q0,7.0Q0), (6.0Q0,7.0Q0), (6.0Q0,7.0Q0),
     +                  (0.1Q0,-0.3Q0), (8.0Q0,9.0Q0), (0.5Q0,-0.1Q0),
     +                  (2.0Q0,5.0Q0), (2.0Q0,5.0Q0), (2.0Q0,5.0Q0),
     +                  (2.0Q0,5.0Q0), (2.0Q0,5.0Q0), (0.1Q0,0.1Q0),
     +                  (3.0Q0,6.0Q0), (-0.6Q0,0.1Q0), (4.0Q0,7.0Q0),
     +                  (0.1Q0,-0.3Q0), (7.0Q0,2.0Q0), (7.0Q0,2.0Q0),
     +                  (7.0Q0,2.0Q0), (0.3Q0,0.1Q0), (5.0Q0,8.0Q0),
     +                  (0.5Q0,0.0Q0), (6.0Q0,9.0Q0), (0.0Q0,0.5Q0),
     +                  (8.0Q0,3.0Q0), (0.0Q0,0.2Q0), (9.0Q0,4.0Q0)/
      DATA              CVR/(8.0Q0,8.0Q0), (-7.0Q0,-7.0Q0),
     +                  (9.0Q0,9.0Q0), (5.0Q0,5.0Q0), (9.0Q0,9.0Q0),
     +                  (8.0Q0,8.0Q0), (7.0Q0,7.0Q0), (7.0Q0,7.0Q0)/
      DATA              STRUE2/0.0Q0, 0.5Q0, 0.6Q0, 0.7Q0, 0.8Q0/
      DATA              STRUE4/0.0Q0, 0.7Q0, 1.0Q0, 1.3Q0, 1.6Q0/
      DATA              ((CTRUE5(I,J,1),I=1,8),J=1,5)/(0.1Q0,0.1Q0),
     +                  (1.0Q0,2.0Q0), (1.0Q0,2.0Q0), (1.0Q0,2.0Q0),
     +                  (1.0Q0,2.0Q0), (1.0Q0,2.0Q0), (1.0Q0,2.0Q0),
     +                  (1.0Q0,2.0Q0), (-0.16Q0,-0.37Q0), (3.0Q0,4.0Q0),
     +                  (3.0Q0,4.0Q0), (3.0Q0,4.0Q0), (3.0Q0,4.0Q0),
     +                  (3.0Q0,4.0Q0), (3.0Q0,4.0Q0), (3.0Q0,4.0Q0),
     +                  (-0.17Q0,-0.19Q0), (0.13Q0,-0.39Q0),
     +                  (5.0Q0,6.0Q0), (5.0Q0,6.0Q0), (5.0Q0,6.0Q0),
     +                  (5.0Q0,6.0Q0), (5.0Q0,6.0Q0), (5.0Q0,6.0Q0),
     +                  (0.11Q0,-0.03Q0), (-0.17Q0,0.46Q0),
     +                  (-0.17Q0,-0.19Q0), (7.0Q0,8.0Q0), (7.0Q0,8.0Q0),
     +                  (7.0Q0,8.0Q0), (7.0Q0,8.0Q0), (7.0Q0,8.0Q0),
     +                  (0.19Q0,-0.17Q0), (0.20Q0,-0.35Q0),
     +                  (0.35Q0,0.20Q0), (0.14Q0,0.08Q0),
     +                  (2.0Q0,3.0Q0), (2.0Q0,3.0Q0), (2.0Q0,3.0Q0),
     +                  (2.0Q0,3.0Q0)/
      DATA              ((CTRUE5(I,J,2),I=1,8),J=1,5)/(0.1Q0,0.1Q0),
     +                  (4.0Q0,5.0Q0), (4.0Q0,5.0Q0), (4.0Q0,5.0Q0),
     +                  (4.0Q0,5.0Q0), (4.0Q0,5.0Q0), (4.0Q0,5.0Q0),
     +                  (4.0Q0,5.0Q0), (-0.16Q0,-0.37Q0), (6.0Q0,7.0Q0),
     +                  (6.0Q0,7.0Q0), (6.0Q0,7.0Q0), (6.0Q0,7.0Q0),
     +                  (6.0Q0,7.0Q0), (6.0Q0,7.0Q0), (6.0Q0,7.0Q0),
     +                  (-0.17Q0,-0.19Q0), (8.0Q0,9.0Q0),
     +                  (0.13Q0,-0.39Q0), (2.0Q0,5.0Q0), (2.0Q0,5.0Q0),
     +                  (2.0Q0,5.0Q0), (2.0Q0,5.0Q0), (2.0Q0,5.0Q0),
     +                  (0.11Q0,-0.03Q0), (3.0Q0,6.0Q0),
     +                  (-0.17Q0,0.46Q0), (4.0Q0,7.0Q0),
     +                  (-0.17Q0,-0.19Q0), (7.0Q0,2.0Q0), (7.0Q0,2.0Q0),
     +                  (7.0Q0,2.0Q0), (0.19Q0,-0.17Q0), (5.0Q0,8.0Q0),
     +                  (0.20Q0,-0.35Q0), (6.0Q0,9.0Q0),
     +                  (0.35Q0,0.20Q0), (8.0Q0,3.0Q0),
     +                  (0.14Q0,0.08Q0), (9.0Q0,4.0Q0)/
      DATA              ((CTRUE6(I,J,1),I=1,8),J=1,5)/(0.1Q0,0.1Q0),
     +                  (1.0Q0,2.0Q0), (1.0Q0,2.0Q0), (1.0Q0,2.0Q0),
     +                  (1.0Q0,2.0Q0), (1.0Q0,2.0Q0), (1.0Q0,2.0Q0),
     +                  (1.0Q0,2.0Q0), (0.09Q0,-0.12Q0), (3.0Q0,4.0Q0),
     +                  (3.0Q0,4.0Q0), (3.0Q0,4.0Q0), (3.0Q0,4.0Q0),
     +                  (3.0Q0,4.0Q0), (3.0Q0,4.0Q0), (3.0Q0,4.0Q0),
     +                  (0.03Q0,-0.09Q0), (0.15Q0,-0.03Q0),
     +                  (5.0Q0,6.0Q0), (5.0Q0,6.0Q0), (5.0Q0,6.0Q0),
     +                  (5.0Q0,6.0Q0), (5.0Q0,6.0Q0), (5.0Q0,6.0Q0),
     +                  (0.03Q0,0.03Q0), (-0.18Q0,0.03Q0),
     +                  (0.03Q0,-0.09Q0), (7.0Q0,8.0Q0), (7.0Q0,8.0Q0),
     +                  (7.0Q0,8.0Q0), (7.0Q0,8.0Q0), (7.0Q0,8.0Q0),
     +                  (0.09Q0,0.03Q0), (0.15Q0,0.00Q0),
     +                  (0.00Q0,0.15Q0), (0.00Q0,0.06Q0), (2.0Q0,3.0Q0),
     +                  (2.0Q0,3.0Q0), (2.0Q0,3.0Q0), (2.0Q0,3.0Q0)/
      DATA              ((CTRUE6(I,J,2),I=1,8),J=1,5)/(0.1Q0,0.1Q0),
     +                  (4.0Q0,5.0Q0), (4.0Q0,5.0Q0), (4.0Q0,5.0Q0),
     +                  (4.0Q0,5.0Q0), (4.0Q0,5.0Q0), (4.0Q0,5.0Q0),
     +                  (4.0Q0,5.0Q0), (0.09Q0,-0.12Q0), (6.0Q0,7.0Q0),
     +                  (6.0Q0,7.0Q0), (6.0Q0,7.0Q0), (6.0Q0,7.0Q0),
     +                  (6.0Q0,7.0Q0), (6.0Q0,7.0Q0), (6.0Q0,7.0Q0),
     +                  (0.03Q0,-0.09Q0), (8.0Q0,9.0Q0),
     +                  (0.15Q0,-0.03Q0), (2.0Q0,5.0Q0), (2.0Q0,5.0Q0),
     +                  (2.0Q0,5.0Q0), (2.0Q0,5.0Q0), (2.0Q0,5.0Q0),
     +                  (0.03Q0,0.03Q0), (3.0Q0,6.0Q0),
     +                  (-0.18Q0,0.03Q0), (4.0Q0,7.0Q0),
     +                  (0.03Q0,-0.09Q0), (7.0Q0,2.0Q0), (7.0Q0,2.0Q0),
     +                  (7.0Q0,2.0Q0), (0.09Q0,0.03Q0), (5.0Q0,8.0Q0),
     +                  (0.15Q0,0.00Q0), (6.0Q0,9.0Q0), (0.00Q0,0.15Q0),
     +                  (8.0Q0,3.0Q0), (0.00Q0,0.06Q0), (9.0Q0,4.0Q0)/
      DATA              ITRUE3/0, 1, 2, 2, 2/
      DATA              ITRUEC/0, 1, 1, 1, 1/
*     .. Executable Statements ..
      DO 60 INCX = 1, 2
         DO 40 NP1 = 1, 5
            N = NP1 - 1
            LEN = 2*MAX(N,1)
*           .. Set vector arguments ..
            DO 20 I = 1, LEN
               CX(I) = CV(I,NP1,INCX)
   20       CONTINUE
            IF (ICASE.EQ.6) THEN
*              .. QXNRM2 ..
               CALL STEST1(QXNRM2(N,CX,INCX),STRUE2(NP1),STRUE2(NP1),
     +                     SFAC)
            ELSE IF (ICASE.EQ.7) THEN
*              .. QXASUM ..
               CALL STEST1(QXASUM(N,CX,INCX),STRUE4(NP1),STRUE4(NP1),
     +                     SFAC)
            ELSE IF (ICASE.EQ.8) THEN
*              .. XSCAL ..
               CALL XSCAL(N,CA,CX,INCX)
               CALL CTEST(LEN,CX,CTRUE5(1,NP1,INCX),CTRUE5(1,NP1,INCX),
     +                    SFAC)
            ELSE IF (ICASE.EQ.9) THEN
*              .. XQSCAL ..
               CALL XQSCAL(N,SA,CX,INCX)
               CALL CTEST(LEN,CX,CTRUE6(1,NP1,INCX),CTRUE6(1,NP1,INCX),
     +                    SFAC)
            ELSE IF (ICASE.EQ.10) THEN
*              .. IXAMAX ..
               CALL ITEST1(IXAMAX(N,CX,INCX),ITRUE3(NP1))
               DO 160 I = 1, LEN
                  CX(I) = (42.0Q0,43.0Q0)
  160          CONTINUE
               CALL ITEST1(IXAMAX(N,CX,INCX),ITRUEC(NP1))
            ELSE
               WRITE (NOUT,*) ' Shouldn''t be here in CHECK1'
               STOP
            END IF
*
   40    CONTINUE
         IF (ICASE.EQ.10) THEN
            N = 8
            IX = 1
            DO 180 I = 1, N
               CXR(IX) = CVR(I)
               IX = IX + INCX
  180       CONTINUE
            CALL ITEST1(IXAMAX(N,CXR,INCX),3)
         END IF
   60 CONTINUE
*
      INCX = 1
      IF (ICASE.EQ.8) THEN
*        XSCAL
*        Add a test for alpha equal to zero.
         CA = (0.0Q0,0.0Q0)
         DO 80 I = 1, 5
            MWPCT(I) = (0.0Q0,0.0Q0)
            MWPCS(I) = (1.0Q0,1.0Q0)
   80    CONTINUE
         CALL XSCAL(5,CA,CX,INCX)
         CALL CTEST(5,CX,MWPCT,MWPCS,SFAC)
      ELSE IF (ICASE.EQ.9) THEN
*        XQSCAL
*        Add a test for alpha equal to zero.
         SA = 0.0Q0
         DO 100 I = 1, 5
            MWPCT(I) = (0.0Q0,0.0Q0)
            MWPCS(I) = (1.0Q0,1.0Q0)
  100    CONTINUE
         CALL XQSCAL(5,SA,CX,INCX)
         CALL CTEST(5,CX,MWPCT,MWPCS,SFAC)
*        Add a test for alpha equal to one.
         SA = 1.0Q0
         DO 120 I = 1, 5
            MWPCT(I) = CX(I)
            MWPCS(I) = CX(I)
  120    CONTINUE
         CALL XQSCAL(5,SA,CX,INCX)
         CALL CTEST(5,CX,MWPCT,MWPCS,SFAC)
*        Add a test for alpha equal to minus one.
         SA = -1.0Q0
         DO 140 I = 1, 5
            MWPCT(I) = -CX(I)
            MWPCS(I) = -CX(I)
  140    CONTINUE
         CALL XQSCAL(5,SA,CX,INCX)
         CALL CTEST(5,CX,MWPCT,MWPCS,SFAC)
      END IF
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
      REAL*16              SFAC
*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, MODE, N
      LOGICAL           PASS
*     .. Local Scalars ..
      COMPLEX*32           CA
      INTEGER           I, J, KI, KN, KSIZE, LENX, LENY, LINCX, LINCY,
     +                  MX, MY
*     .. Local Arrays ..
      COMPLEX*32           CDOT(1), CSIZE1(4), CSIZE2(7,2), CSIZE3(14),
     +                  CT10X(7,4,4), CT10Y(7,4,4), CT6(4,4), CT7(4,4),
     +                  CT8(7,4,4), CTY0(1), CX(7), CX0(1), CX1(7),
     +                  CY(7), CY0(1), CY1(7)
      INTEGER           INCXS(4), INCYS(4), LENS(4,2), NS(4)
*     .. External Functions ..
      COMPLEX*32           XDOTC, XDOTU
      EXTERNAL          XDOTC, XDOTU
*     .. External Subroutines ..
      EXTERNAL          XAXPY, XCOPY, XSWAP, CTEST
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, MIN
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Data statements ..
      DATA              CA/(0.4Q0,-0.7Q0)/
      DATA              INCXS/1, 2, -2, -1/
      DATA              INCYS/1, -2, 1, -2/
      DATA              LENS/1, 1, 2, 4, 1, 1, 3, 7/
      DATA              NS/0, 1, 2, 4/
      DATA              CX1/(0.7Q0,-0.8Q0), (-0.4Q0,-0.7Q0),
     +                  (-0.1Q0,-0.9Q0), (0.2Q0,-0.8Q0),
     +                  (-0.9Q0,-0.4Q0), (0.1Q0,0.4Q0), (-0.6Q0,0.6Q0)/
      DATA              CY1/(0.6Q0,-0.6Q0), (-0.9Q0,0.5Q0),
     +                  (0.7Q0,-0.6Q0), (0.1Q0,-0.5Q0), (-0.1Q0,-0.2Q0),
     +                  (-0.5Q0,-0.3Q0), (0.8Q0,-0.7Q0)/
      DATA              ((CT8(I,J,1),I=1,7),J=1,4)/(0.6Q0,-0.6Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.32Q0,-1.41Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.32Q0,-1.41Q0),
     +                  (-1.55Q0,0.5Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.32Q0,-1.41Q0), (-1.55Q0,0.5Q0),
     +                  (0.03Q0,-0.89Q0), (-0.38Q0,-0.96Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0)/
      DATA              ((CT8(I,J,2),I=1,7),J=1,4)/(0.6Q0,-0.6Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.32Q0,-1.41Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (-0.07Q0,-0.89Q0),
     +                  (-0.9Q0,0.5Q0), (0.42Q0,-1.41Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.78Q0,0.06Q0), (-0.9Q0,0.5Q0),
     +                  (0.06Q0,-0.13Q0), (0.1Q0,-0.5Q0),
     +                  (-0.77Q0,-0.49Q0), (-0.5Q0,-0.3Q0),
     +                  (0.52Q0,-1.51Q0)/
      DATA              ((CT8(I,J,3),I=1,7),J=1,4)/(0.6Q0,-0.6Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.32Q0,-1.41Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (-0.07Q0,-0.89Q0),
     +                  (-1.18Q0,-0.31Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.78Q0,0.06Q0), (-1.54Q0,0.97Q0),
     +                  (0.03Q0,-0.89Q0), (-0.18Q0,-1.31Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0)/
      DATA              ((CT8(I,J,4),I=1,7),J=1,4)/(0.6Q0,-0.6Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.32Q0,-1.41Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.32Q0,-1.41Q0), (-0.9Q0,0.5Q0),
     +                  (0.05Q0,-0.6Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.32Q0,-1.41Q0),
     +                  (-0.9Q0,0.5Q0), (0.05Q0,-0.6Q0), (0.1Q0,-0.5Q0),
     +                  (-0.77Q0,-0.49Q0), (-0.5Q0,-0.3Q0),
     +                  (0.32Q0,-1.16Q0)/
      DATA              CT7/(0.0Q0,0.0Q0), (-0.06Q0,-0.90Q0),
     +                  (0.65Q0,-0.47Q0), (-0.34Q0,-1.22Q0),
     +                  (0.0Q0,0.0Q0), (-0.06Q0,-0.90Q0),
     +                  (-0.59Q0,-1.46Q0), (-1.04Q0,-0.04Q0),
     +                  (0.0Q0,0.0Q0), (-0.06Q0,-0.90Q0),
     +                  (-0.83Q0,0.59Q0), (0.07Q0,-0.37Q0),
     +                  (0.0Q0,0.0Q0), (-0.06Q0,-0.90Q0),
     +                  (-0.76Q0,-1.15Q0), (-1.33Q0,-1.82Q0)/
      DATA              CT6/(0.0Q0,0.0Q0), (0.90Q0,0.06Q0),
     +                  (0.91Q0,-0.77Q0), (1.80Q0,-0.10Q0),
     +                  (0.0Q0,0.0Q0), (0.90Q0,0.06Q0), (1.45Q0,0.74Q0),
     +                  (0.20Q0,0.90Q0), (0.0Q0,0.0Q0), (0.90Q0,0.06Q0),
     +                  (-0.55Q0,0.23Q0), (0.83Q0,-0.39Q0),
     +                  (0.0Q0,0.0Q0), (0.90Q0,0.06Q0), (1.04Q0,0.79Q0),
     +                  (1.95Q0,1.22Q0)/
      DATA              ((CT10X(I,J,1),I=1,7),J=1,4)/(0.7Q0,-0.8Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.6Q0,-0.6Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.6Q0,-0.6Q0), (-0.9Q0,0.5Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.6Q0,-0.6Q0),
     +                  (-0.9Q0,0.5Q0), (0.7Q0,-0.6Q0), (0.1Q0,-0.5Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0)/
      DATA              ((CT10X(I,J,2),I=1,7),J=1,4)/(0.7Q0,-0.8Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.6Q0,-0.6Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.7Q0,-0.6Q0), (-0.4Q0,-0.7Q0),
     +                  (0.6Q0,-0.6Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.8Q0,-0.7Q0),
     +                  (-0.4Q0,-0.7Q0), (-0.1Q0,-0.2Q0),
     +                  (0.2Q0,-0.8Q0), (0.7Q0,-0.6Q0), (0.1Q0,0.4Q0),
     +                  (0.6Q0,-0.6Q0)/
      DATA              ((CT10X(I,J,3),I=1,7),J=1,4)/(0.7Q0,-0.8Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.6Q0,-0.6Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (-0.9Q0,0.5Q0), (-0.4Q0,-0.7Q0),
     +                  (0.6Q0,-0.6Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.1Q0,-0.5Q0),
     +                  (-0.4Q0,-0.7Q0), (0.7Q0,-0.6Q0), (0.2Q0,-0.8Q0),
     +                  (-0.9Q0,0.5Q0), (0.1Q0,0.4Q0), (0.6Q0,-0.6Q0)/
      DATA              ((CT10X(I,J,4),I=1,7),J=1,4)/(0.7Q0,-0.8Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.6Q0,-0.6Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.6Q0,-0.6Q0), (0.7Q0,-0.6Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.6Q0,-0.6Q0),
     +                  (0.7Q0,-0.6Q0), (-0.1Q0,-0.2Q0), (0.8Q0,-0.7Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0)/
      DATA              ((CT10Y(I,J,1),I=1,7),J=1,4)/(0.6Q0,-0.6Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.7Q0,-0.8Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.7Q0,-0.8Q0), (-0.4Q0,-0.7Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.7Q0,-0.8Q0),
     +                  (-0.4Q0,-0.7Q0), (-0.1Q0,-0.9Q0),
     +                  (0.2Q0,-0.8Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0)/
      DATA              ((CT10Y(I,J,2),I=1,7),J=1,4)/(0.6Q0,-0.6Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.7Q0,-0.8Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (-0.1Q0,-0.9Q0), (-0.9Q0,0.5Q0),
     +                  (0.7Q0,-0.8Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (-0.6Q0,0.6Q0),
     +                  (-0.9Q0,0.5Q0), (-0.9Q0,-0.4Q0), (0.1Q0,-0.5Q0),
     +                  (-0.1Q0,-0.9Q0), (-0.5Q0,-0.3Q0),
     +                  (0.7Q0,-0.8Q0)/
      DATA              ((CT10Y(I,J,3),I=1,7),J=1,4)/(0.6Q0,-0.6Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.7Q0,-0.8Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (-0.1Q0,-0.9Q0), (0.7Q0,-0.8Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (-0.6Q0,0.6Q0),
     +                  (-0.9Q0,-0.4Q0), (-0.1Q0,-0.9Q0),
     +                  (0.7Q0,-0.8Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0)/
      DATA              ((CT10Y(I,J,4),I=1,7),J=1,4)/(0.6Q0,-0.6Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.7Q0,-0.8Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.7Q0,-0.8Q0), (-0.9Q0,0.5Q0),
     +                  (-0.4Q0,-0.7Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.7Q0,-0.8Q0),
     +                  (-0.9Q0,0.5Q0), (-0.4Q0,-0.7Q0), (0.1Q0,-0.5Q0),
     +                  (-0.1Q0,-0.9Q0), (-0.5Q0,-0.3Q0),
     +                  (0.2Q0,-0.8Q0)/
      DATA              CSIZE1/(0.0Q0,0.0Q0), (0.9Q0,0.9Q0),
     +                  (1.63Q0,1.73Q0), (2.90Q0,2.78Q0)/
      DATA              CSIZE3/(0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (1.17Q0,1.17Q0),
     +                  (1.17Q0,1.17Q0), (1.17Q0,1.17Q0),
     +                  (1.17Q0,1.17Q0), (1.17Q0,1.17Q0),
     +                  (1.17Q0,1.17Q0), (1.17Q0,1.17Q0)/
      DATA              CSIZE2/(0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (0.0Q0,0.0Q0),
     +                  (0.0Q0,0.0Q0), (0.0Q0,0.0Q0), (1.54Q0,1.54Q0),
     +                  (1.54Q0,1.54Q0), (1.54Q0,1.54Q0),
     +                  (1.54Q0,1.54Q0), (1.54Q0,1.54Q0),
     +                  (1.54Q0,1.54Q0), (1.54Q0,1.54Q0)/
*     .. Executable Statements ..
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
*           .. initialize all argument arrays ..
            DO 20 I = 1, 7
               CX(I) = CX1(I)
               CY(I) = CY1(I)
   20       CONTINUE
            IF (ICASE.EQ.1) THEN
*              .. XDOTC ..
               CDOT(1) = XDOTC(N,CX,INCX,CY,INCY)
               CALL CTEST(1,CDOT,CT6(KN,KI),CSIZE1(KN),SFAC)
            ELSE IF (ICASE.EQ.2) THEN
*              .. XDOTU ..
               CDOT(1) = XDOTU(N,CX,INCX,CY,INCY)
               CALL CTEST(1,CDOT,CT7(KN,KI),CSIZE1(KN),SFAC)
            ELSE IF (ICASE.EQ.3) THEN
*              .. XAXPY ..
               CALL XAXPY(N,CA,CX,INCX,CY,INCY)
               CALL CTEST(LENY,CY,CT8(1,KN,KI),CSIZE2(1,KSIZE),SFAC)
            ELSE IF (ICASE.EQ.4) THEN
*              .. XCOPY ..
               CALL XCOPY(N,CX,INCX,CY,INCY)
               CALL CTEST(LENY,CY,CT10Y(1,KN,KI),CSIZE3,1.0Q0)
               IF (KI.EQ.1) THEN
                  CX0(1) = (42.0Q0,43.0Q0)
                  CY0(1) = (44.0Q0,45.0Q0)
                  IF (N.EQ.0) THEN
                     CTY0(1) = CY0(1)
                  ELSE
                     CTY0(1) = CX0(1)
                  END IF
                  LINCX = INCX
                  INCX = 0
                  LINCY = INCY
                  INCY = 0
                  CALL XCOPY(N,CX0,INCX,CY0,INCY)
                  CALL CTEST(1,CY0,CTY0,CSIZE3,1.0Q0)
                  INCX = LINCX
                  INCY = LINCY
               END IF
            ELSE IF (ICASE.EQ.5) THEN
*              .. XSWAP ..
               CALL XSWAP(N,CX,INCX,CY,INCY)
               CALL CTEST(LENX,CX,CT10X(1,KN,KI),CSIZE3,1.0Q0)
               CALL CTEST(LENY,CY,CT10Y(1,KN,KI),CSIZE3,1.0Q0)
            ELSE
               WRITE (NOUT,*) ' Shouldn''t be here in CHECK2'
               STOP
            END IF
*
   40    CONTINUE
   60 CONTINUE
      RETURN
*
*     End of CHECK2
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
      REAL*16             ZERO
      PARAMETER        (NOUT=6, ZERO=0.0Q0)
*     .. Scalar Arguments ..
      REAL*16             SFAC
      INTEGER          LEN
*     .. Array Arguments ..
      REAL*16             SCOMP(LEN), SSIZE(LEN), STRUE(LEN)
*     .. Scalars in Common ..
      INTEGER          ICASE, INCX, INCY, MODE, N
      LOGICAL          PASS
*     .. Local Scalars ..
      REAL*16             SD
      INTEGER          I
*     .. External Functions ..
      REAL*16             SDIFF
      EXTERNAL         SDIFF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Executable Statements ..
*
      DO 40 I = 1, LEN
         SD = SCOMP(I) - STRUE(I)
         IF (ABS(SFAC*SD) .LE. ABS(SSIZE(I))*EPSILON(ZERO))
     +       GO TO 40
*
*                             HERE    SCOMP(I) IS NOT CLOSE TO STRUE(I).
*
         IF ( .NOT. PASS) GO TO 20
*                             PRINT FAIL MESSAGE AND HEADER.
         PASS = .FALSE.
         WRITE (NOUT,99999)
         WRITE (NOUT,99998)
   20    WRITE (NOUT,99997) ICASE, N, INCX, INCY, MODE, I, SCOMP(I),
     +     STRUE(I), SD, SSIZE(I)
   40 CONTINUE
      RETURN
*
99999 FORMAT ('                                       FAIL')
99998 FORMAT (/' CASE  N INCX INCY MODE  I                            ',
     +       ' COMP(I)                             TRUE(I)  DIFFERENCE',
     +       '     SIZE(I)',/1X)
99997 FORMAT (1X,I4,I3,3I5,I3,2E36.8,2E12.4)
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
      REAL*16              SCOMP1, SFAC, STRUE1
*     .. Array Arguments ..
      REAL*16              SSIZE(*)
*     .. Local Arrays ..
      REAL*16              SCOMP(1), STRUE(1)
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
      REAL*16             FUNCTION SDIFF(SA,SB)
*     ********************************* SDIFF **************************
*     COMPUTES DIFFERENCE OF TWO NUMBERS.  C. L. LAWSON, JPL 1974 FEB 15
*
*     .. Scalar Arguments ..
      REAL*16                            SA, SB
*     .. Executable Statements ..
      SDIFF = SA - SB
      RETURN
*
*     End of SDIFF
*
      END
      SUBROUTINE CTEST(LEN,CCOMP,CTRUE,CSIZE,SFAC)
*     **************************** CTEST *****************************
*
*     C.L. LAWSON, JPL, 1978 DEC 6
*
*     .. Scalar Arguments ..
      REAL*16             SFAC
      INTEGER          LEN
*     .. Array Arguments ..
      COMPLEX*32          CCOMP(LEN), CSIZE(LEN), CTRUE(LEN)
*     .. Local Scalars ..
      INTEGER          I
*     .. Local Arrays ..
      REAL*16             SCOMP(20), SSIZE(20), STRUE(20)
*     .. External Subroutines ..
      EXTERNAL         STEST
*     .. Intrinsic Functions ..
      INTRINSIC        IMAG, REAL
*     .. Executable Statements ..
      DO 20 I = 1, LEN
         SCOMP(2*I-1) = REAL(CCOMP(I),16)
         SCOMP(2*I) = IMAG(CCOMP(I))
         STRUE(2*I-1) = REAL(CTRUE(I),16)
         STRUE(2*I) = IMAG(CTRUE(I))
         SSIZE(2*I-1) = REAL(CSIZE(I),16)
         SSIZE(2*I) = IMAG(CSIZE(I))
   20 CONTINUE
*
      CALL STEST(2*LEN,SCOMP,STRUE,SSIZE,SFAC)
      RETURN
*
*     End of CTEST
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
      INTEGER           ICASE, INCX, INCY, MODE, N
      LOGICAL           PASS
*     .. Local Scalars ..
      INTEGER           ID
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Executable Statements ..
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
      WRITE (NOUT,99997) ICASE, N, INCX, INCY, MODE, ICOMP, ITRUE, ID
   40 CONTINUE
      RETURN
*
99999 FORMAT ('                                       FAIL')
99998 FORMAT (/' CASE  N INCX INCY MODE                               ',
     +       ' COMP                                TRUE     DIFFERENCE',
     +       /1X)
99997 FORMAT (1X,I4,I3,3I5,2I36,I12)
*
*     End of ITEST1
*
      END
