*> \brief \b XQROT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE XQROT( N, XX, INCX, XY, INCY, C, S )
*
*       .. Scalar Arguments ..
*       INTEGER            INCX, INCY, N
*       REAL*16            C, S
*       ..
*       .. Array Arguments ..
*       COMPLEX*32         XX( * ), XY( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> Applies a plane rotation, where the cos and sin (c and s) are real
*> and the vectors cx and cy are complex.
*> jack dongarra, linpack, 3/11/78.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>           On entry, N specifies the order of the vectors cx and cy.
*>           N must be at least zero.
*> \endverbatim
*>
*> \param[in,out] XX
*> \verbatim
*>          XX is COMPLEX*32 array, dimension at least
*>           ( 1 + ( N - 1 )*abs( INCX ) ).
*>           Before entry, the incremented array XX must contain the n
*>           element vector cx. On exit, XX is overwritten by the updated
*>           vector cx.
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>           On entry, INCX specifies the increment for the elements of
*>           XX. INCX must not be zero.
*> \endverbatim
*>
*> \param[in,out] XY
*> \verbatim
*>          XY is COMPLEX*32 array, dimension at least
*>           ( 1 + ( N - 1 )*abs( INCY ) ).
*>           Before entry, the incremented array XY must contain the n
*>           element vector cy. On exit, XY is overwritten by the updated
*>           vector cy.
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>           On entry, INCY specifies the increment for the elements of
*>           XY. INCY must not be zero.
*> \endverbatim
*>
*> \param[in] C
*> \verbatim
*>          C is REAL*16
*>           On entry, C specifies the cosine, cos.
*> \endverbatim
*>
*> \param[in] S
*> \verbatim
*>          S is REAL*16
*>           On entry, S specifies the sine, sin.
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
*> \ingroup rot
*
*  =====================================================================
      SUBROUTINE XQROT( N, XX, INCX, XY, INCY, C, S )
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            INCX, INCY, N
      REAL*16            C, S
*     ..
*     .. Array Arguments ..
      COMPLEX*32         XX( * ), XY( * )
*     ..
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IX, IY
      COMPLEX*32         CTEMP
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.0 )
     $   RETURN
      IF( INCX.EQ.1 .AND. INCY.EQ.1 ) THEN
*
*        code for both increments equal to 1
*
         DO I = 1, N
            CTEMP = C*XX( I ) + S*XY( I )
            XY( I ) = C*XY( I ) - S*XX( I )
            XX( I ) = CTEMP
         END DO
      ELSE
*
*        code for unequal increments or equal increments not equal
*          to 1
*
         IX = 1
         IY = 1
         IF( INCX.LT.0 )
     $      IX = ( -N+1 )*INCX + 1
         IF( INCY.LT.0 )
     $      IY = ( -N+1 )*INCY + 1
         DO I = 1, N
            CTEMP = C*XX( IX ) + S*XY( IY )
            XY( IY ) = C*XY( IY ) - S*XX( IX )
            XX( IX ) = CTEMP
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      RETURN
*
*     End of XQROT
*
      END
