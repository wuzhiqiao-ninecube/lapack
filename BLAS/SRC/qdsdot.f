*> \brief \b QDSDOT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       REAL*16 FUNCTION QDSDOT(N,SB,SX,INCX,SY,INCY)
*
*       .. Scalar Arguments ..
*       REAL*16 SB
*       INTEGER INCX,INCY,N
*       ..
*       .. Array Arguments ..
*       REAL*16 SX(*),SY(*)
*       ..
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>   Compute the inner product of two vectors with extended
*>   precision accumulation.
*>
*>   Returns Q.P. result with dot product accumulated in D.P.
*>   QDSDOT = SB + sum for I = 0 to N-1 of SX(LX+I*INCX)*SY(LY+I*INCY),
*>   where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
*>   defined in a similar way using INCY.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          number of elements in input vector(s)
*> \endverbatim
*>
*> \param[in] SB
*> \verbatim
*>          SB is REAL*16
*>          quad precision scalar to be added to inner product
*> \endverbatim
*>
*> \param[in] SX
*> \verbatim
*>          SX is REAL*16 array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*>          quad precision vector with N elements
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>          storage spacing between elements of SX
*> \endverbatim
*>
*> \param[in] SY
*> \verbatim
*>          SY is REAL*16 array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*>          quad precision vector with N elements
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>          storage spacing between elements of SY
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Lawson, C. L., (JPL), Hanson, R. J., (SNLA),
*> \author Kincaid, D. R., (U. of Texas), Krogh, F. T., (JPL)
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup quad_blas_level1
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>    REFERENCES
*>
*>    C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
*>    Krogh, Basic linear algebra subprograms for Fortran
*>    usage, Algorithm No. 539, Transactions on Mathematical
*>    Software 5, 3 (September 1979), pp. 308-323.
*>
*>    REVISION HISTORY  (YYMMDD)
*>
*>    791001  DATE WRITTEN
*>    890531  Changed all specific intrinsics to generic.  (WRB)
*>    890831  Modified array declarations.  (WRB)
*>    890831  REVISION DATE from Version 3.2
*>    891214  Prologue converted to Version 4.0 format.  (BAB)
*>    920310  Corrected definition of LX in DESCRIPTION.  (WRB)
*>    920501  Reformatted the REFERENCES section.  (WRB)
*>    070118  Reformat to LAPACK coding style
*> \endverbatim
*>
*  =====================================================================
      REAL*16 FUNCTION QDSDOT(N,SB,SX,INCX,SY,INCY)
      IMPLICIT NONE
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      REAL*16 SB
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      REAL*16 SX(*),SY(*)
*     .. Local Scalars ..
      REAL*16 DSDOT
      INTEGER I,KX,KY,NS
*     ..
      DSDOT = SB
      IF (N.LE.0) THEN
         QDSDOT = DSDOT
         RETURN
      END IF
      IF (INCX.EQ.INCY .AND. INCX.GT.0) THEN
*
*     Code for equal and positive increments.
*
         NS = N*INCX
         DO I = 1,NS,INCX
            DSDOT = DSDOT + SX(I)*SY(I)
         END DO
      ELSE
*
*     Code for unequal or nonpositive increments.
*
         KX = 1
         KY = 1
         IF (INCX.LT.0) KX = 1 + (1-N)*INCX
         IF (INCY.LT.0) KY = 1 + (1-N)*INCY
         DO I = 1,N
            DSDOT = DSDOT + SX(KX)*SY(KY)
            KX = KX + INCX
            KY = KY + INCY
         END DO
      END IF
      QDSDOT = DSDOT
      RETURN
*
*     End of QDSDOT
*
      END
