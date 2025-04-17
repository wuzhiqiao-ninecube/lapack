*> \brief \b QDOT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       REAL*16 FUNCTION QDOT(N,QX,INCX,QY,INCY)
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
*>    QDOT forms the dot product of two vectors.
*>    uses unrolled loops for increments equal to one.
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
*> \param[in] QY
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
*> \ingroup dot
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
      REAL*16 FUNCTION QDOT(N,QX,INCX,QY,INCY)
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
      REAL*16 QTEMP
      INTEGER I,IX,IY,M,MP1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MOD
*     ..
      QDOT = 0.0Q0
      QTEMP = 0.0Q0
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*        code for both increments equal to 1
*
*
*        clean-up loop
*
         M = MOD(N,5)
         IF (M.NE.0) THEN
            DO I = 1,M
               QTEMP = QTEMP + QX(I)*QY(I)
            END DO
            IF (N.LT.5) THEN
               QDOT=QTEMP
            RETURN
            END IF
         END IF
         MP1 = M + 1
         DO I = MP1,N,5
          QTEMP = QTEMP + QX(I)*QY(I) + QX(I+1)*QY(I+1) +
     $            QX(I+2)*QY(I+2) + QX(I+3)*QY(I+3) + QX(I+4)*QY(I+4)
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
            QTEMP = QTEMP + QX(IX)*QY(IY)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      QDOT = QTEMP
      RETURN
*
*     End of QDOT
*
      END
