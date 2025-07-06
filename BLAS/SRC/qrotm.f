*> \brief \b QROTM
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE QROTM(N,QX,INCX,QY,INCY,QPARAM)
*
*       .. Scalar Arguments ..
*       INTEGER INCX,INCY,N
*       ..
*       .. Array Arguments ..
*       REAL*16 QPARAM(5),QX(*),QY(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
*>
*>    (QX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF QX ARE IN
*>    (QX**T)
*>
*>    QX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
*>    LX = (-INCX)*N, AND SIMILARLY FOR QY USING LY AND INCY.
*>    WITH QPARAM(1)=QFLAG, H HAS ONE OF THE FOLLOWING FORMS..
*>
*>    QFLAG=-1.Q0     QFLAG=0.Q0        QFLAG=1.Q0     QFLAG=-2.Q0
*>
*>      (QH11  QH12)    (1.Q0  QH12)    (QH11  1.Q0)    (1.Q0  0.Q0)
*>    H=(          )    (          )    (          )    (          )
*>      (QH21  QH22),   (QH21  1.Q0),   (-1.Q0 QH22),   (0.Q0  1.Q0).
*>    SEE  QROTMG FOR A DESCRIPTION OF DATA STORAGE IN QPARAM.
*>
*>    IF DFLAG IS NOT ONE OF THE LISTED ABOVE, THE BEHAVIOR IS UNDEFINED.
*>    NANS IN DFLAG MAY NOT PROPAGATE TO THE OUTPUT.
*>
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>         number of elements in input vector(s)
*> \endverbatim
*>
*> \param[in,out] QX
*> \verbatim
*>          QX is REAL*16 array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>         storage spacing between elements of QX
*> \endverbatim
*>
*> \param[in,out] QY
*> \verbatim
*>          QY is REAL*16 array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>         storage spacing between elements of QY
*> \endverbatim
*>
*> \param[in] QPARAM
*> \verbatim
*>          QPARAM is REAL*16 array, dimension (5)
*>     QPARAM(1)=QFLAG
*>     QPARAM(2)=QH11
*>     QPARAM(3)=QH21
*>     QPARAM(4)=QH12
*>     QPARAM(5)=QH22
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
*> \ingroup rotm
*
*  =====================================================================
      SUBROUTINE QROTM(N,QX,INCX,QY,INCY,QPARAM)
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      REAL*16 QPARAM(5),QX(*),QY(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      REAL*16 QFLAG,QH11,QH12,QH21,QH22,TWO,W,Z,ZERO
      INTEGER I,KX,KY,NSTEPS
*     ..
*     .. Data statements ..
      DATA ZERO,TWO/0.Q0,2.Q0/
*     ..
*
      QFLAG = QPARAM(1)
      IF (N.LE.0 .OR. (QFLAG+TWO.EQ.ZERO)) RETURN
      IF (INCX.EQ.INCY.AND.INCX.GT.0) THEN
*
         NSTEPS = N*INCX
         IF (QFLAG.LT.ZERO) THEN
            QH11 = QPARAM(2)
            QH12 = QPARAM(4)
            QH21 = QPARAM(3)
            QH22 = QPARAM(5)
            DO I = 1,NSTEPS,INCX
               W = QX(I)
               Z = QY(I)
               QX(I) = W*QH11 + Z*QH12
               QY(I) = W*QH21 + Z*QH22
            END DO
         ELSE IF (QFLAG.EQ.ZERO) THEN
            QH12 = QPARAM(4)
            QH21 = QPARAM(3)
            DO I = 1,NSTEPS,INCX
               W = QX(I)
               Z = QY(I)
               QX(I) = W + Z*QH12
               QY(I) = W*QH21 + Z
            END DO
         ELSE
            QH11 = QPARAM(2)
            QH22 = QPARAM(5)
            DO I = 1,NSTEPS,INCX
               W = QX(I)
               Z = QY(I)
               QX(I) = W*QH11 + Z
               QY(I) = -W + QH22*Z
            END DO
         END IF
      ELSE
         KX = 1
         KY = 1
         IF (INCX.LT.0) KX = 1 + (1-N)*INCX
         IF (INCY.LT.0) KY = 1 + (1-N)*INCY
*
         IF (QFLAG.LT.ZERO) THEN
            QH11 = QPARAM(2)
            QH12 = QPARAM(4)
            QH21 = QPARAM(3)
            QH22 = QPARAM(5)
            DO I = 1,N
               W = QX(KX)
               Z = QY(KY)
               QX(KX) = W*QH11 + Z*QH12
               QY(KY) = W*QH21 + Z*QH22
               KX = KX + INCX
               KY = KY + INCY
            END DO
         ELSE IF (QFLAG.EQ.ZERO) THEN
            QH12 = QPARAM(4)
            QH21 = QPARAM(3)
            DO I = 1,N
               W = QX(KX)
               Z = QY(KY)
               QX(KX) = W + Z*QH12
               QY(KY) = W*QH21 + Z
               KX = KX + INCX
               KY = KY + INCY
            END DO
         ELSE
             QH11 = QPARAM(2)
             QH22 = QPARAM(5)
             DO I = 1,N
                W = QX(KX)
                Z = QY(KY)
                QX(KX) = W*QH11 + Z
                QY(KY) = -W + QH22*Z
                KX = KX + INCX
                KY = KY + INCY
            END DO
         END IF
      END IF
      RETURN
*
*     End of QROTM
*
      END
