*> \brief \b IXAMAX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       INTEGER FUNCTION IXAMAX(N,XX,INCX)
*
*       .. Scalar Arguments ..
*       INTEGER INCX,N
*       ..
*       .. Array Arguments ..
*       COMPLEX*32 XX(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    IXAMAX finds the index of the first element having maximum |Re(.)| + |Im(.)|
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
*>          XX is COMPLEX*32 array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>         storage spacing between elements of XX
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
*> \ingroup iamax
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, 1/15/85.
*>     modified 3/93 to return if incx .le. 0.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      INTEGER FUNCTION IXAMAX(N,XX,INCX)
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER INCX,N
*     ..
*     .. Array Arguments ..
      COMPLEX*32 XX(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      REAL*16 QMAX
      INTEGER I,IX
*     ..
*     .. External Functions ..
      REAL*16 QCABS1
      EXTERNAL QCABS1
*     ..
      IXAMAX = 0
      IF (N.LT.1 .OR. INCX.LE.0) RETURN
      IXAMAX = 1
      IF (N.EQ.1) RETURN
      IF (INCX.EQ.1) THEN
*
*        code for increment equal to 1
*
         QMAX = QCABS1(XX(1))
         DO I = 2,N
            IF (QCABS1(XX(I)).GT.QMAX) THEN
               IXAMAX = I
               QMAX = QCABS1(XX(I))
            END IF
         END DO
      ELSE
*
*        code for increment not equal to 1
*
         IX = 1
         QMAX = QCABS1(XX(1))
         IX = IX + INCX
         DO I = 2,N
            IF (QCABS1(XX(IX)).GT.QMAX) THEN
               IXAMAX = I
               QMAX = QCABS1(XX(IX))
            END IF
            IX = IX + INCX
         END DO
      END IF
      RETURN
*
*     End of IXAMAX
*
      END
