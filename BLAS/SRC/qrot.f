*> \brief \b QROT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE QROT(N,QX,INCX,QY,INCY,C,S)
*
*       .. Scalar Arguments ..
*       REAL*16 C,S
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
*>   QROT applies a plane rotation.
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
*> \param[in] C
*> \verbatim
*>          C is REAL*16
*> \endverbatim
*>
*> \param[in] S
*> \verbatim
*>          S is REAL*16
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
      SUBROUTINE QROT(N,QX,INCX,QY,INCY,C,S)
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      REAL*16 C,S
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
      INTEGER I,IX,IY
*     ..
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*       code for both increments equal to 1
*
         DO I = 1,N
            QTEMP = C*QX(I) + S*QY(I)
            QY(I) = C*QY(I) - S*QX(I)
            QX(I) = QTEMP
         END DO
      ELSE
*
*       code for unequal increments or equal increments not equal
*         to 1
*
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            QTEMP = C*QX(IX) + S*QY(IY)
            QY(IY) = C*QY(IY) - S*QX(IX)
            QX(IX) = QTEMP
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      RETURN
*
*     End of QROT
*
      END
