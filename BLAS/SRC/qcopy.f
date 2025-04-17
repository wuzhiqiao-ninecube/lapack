*> \brief \b QCOPY
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE QCOPY(N,QX,INCX,QY,INCY)
*
*       .. Scalar Arguments ..
*       INTEGER INCX,INCY,N
*       ..
*       .. Array Arguments ..
*       REAL*16 QX(*),QY(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    QCOPY copies a vector, x, to a vector, y.
*>    uses unrolled loops for increments equal to 1.
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
*> \param[in] QX
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
*> \param[out] QY
*> \verbatim
*>          QY is REAL*16 array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>         storage spacing between elements of QY
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
*> \ingroup copy
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, linpack, 3/11/78.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE QCOPY(N,QX,INCX,QY,INCY)
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      REAL*16 QX(*),QY(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I,IX,IY,M,MP1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MOD
*     ..
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*        code for both increments equal to 1
*
*
*        clean-up loop
*
         M = MOD(N,7)
         IF (M.NE.0) THEN
            DO I = 1,M
               QY(I) = QX(I)
            END DO
            IF (N.LT.7) RETURN
         END IF
         MP1 = M + 1
         DO I = MP1,N,7
            QY(I) = QX(I)
            QY(I+1) = QX(I+1)
            QY(I+2) = QX(I+2)
            QY(I+3) = QX(I+3)
            QY(I+4) = QX(I+4)
            QY(I+5) = QX(I+5)
            QY(I+6) = QX(I+6)
         END DO
      ELSE
*
*        code for unequal increments or equal increments
*          not equal to 1
*
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            QY(IY) = QX(IX)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      RETURN
*
*     End of QCOPY
*
      END
