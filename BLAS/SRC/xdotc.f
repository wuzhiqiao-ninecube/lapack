*> \brief \b XDOTC
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       COMPLEX*32 FUNCTION XDOTC(N,XX,INCX,XY,INCY)
*
*       .. Scalar Arguments ..
*       INTEGER INCX,INCY,N
*       ..
*       .. Array Arguments ..
*       COMPLEX*32 XX(*),XY(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> XDOTC forms the dot product of two complex vectors
*>      XDOTC = X^H * Y
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
*> \param[in] XX
*> \verbatim
*>          XX is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>         storage spacing between elements of XX
*> \endverbatim
*>
*> \param[in] XY
*> \verbatim
*>          XY is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>         storage spacing between elements of CY
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd
*> \author NINECUBE Ltd.
*
*> \ingroup complex_blas_level1
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, linpack,  3/11/78.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      COMPLEX*32 FUNCTION XDOTC(N,XX,INCX,XY,INCY)
      IMPLICIT NONE
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      COMPLEX*32 XX(*),XY(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      COMPLEX*32 CTEMP
      INTEGER I,IX,IY
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC CONJG
*     ..
      XTEMP = (0.0Q0,0.0Q0)
      XDOTC = (0.0Q0,0.0Q0)
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*        code for both increments equal to 1
*
         DO I = 1,N
            XTEMP = XTEMP + CONJG(XX(I))*XY(I)
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
            XTEMP = XTEMP + CONJG(XX(IX))*XY(IY)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      XDOTC = XTEMP
      RETURN
*
*     End of XDOTC
*
      END
