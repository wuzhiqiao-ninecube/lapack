*> \brief \b XSCAL
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE XSCAL(N,XA,XX,INCX)
*
*       .. Scalar Arguments ..
*       COMPLEX XA
*       INTEGER INCX,N
*       ..
*       .. Array Arguments ..
*       COMPLEX XX(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    XSCAL scales a vector by a constant.
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
*> \param[in] CA
*> \verbatim
*>          XA is COMPLEX*32
*>           On entry, CA specifies the scalar alpha.
*> \endverbatim
*>
*> \param[in,out] XX
*> \verbatim
*>          XX is COMPLEX*32 array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>         storage spacing between elements of CX
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
*> \ingroup scal
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, linpack,  3/11/78.
*>     modified 3/93 to return if incx .le. 0.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE XSCAL(N,XA,XX,INCX)
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      COMPLEX*32 XA
      INTEGER INCX,N
*     ..
*     .. Array Arguments ..
      COMPLEX*32 XX(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I,NINCX
*     ..
*     .. Parameters ..
      COMPLEX ONE
      PARAMETER (ONE= (1.0Q+0,0.0Q+0))
*     ..
      IF (N.LE.0 .OR. INCX.LE.0 .OR. CA.EQ.ONE) RETURN
      IF (INCX.EQ.1) THEN
*
*        code for increment equal to 1
*
         DO I = 1,N
            CX(I) = CA*CX(I)
         END DO
      ELSE
*
*        code for increment not equal to 1
*
         NINCX = N*INCX
         DO I = 1,NINCX,INCX
            CX(I) = CA*CX(I)
         END DO
      END IF
      RETURN
*
*     End of XSCAL
*
      END
