*> \brief \b DQDDOT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       REAL*16 FUNCTION DQDDOT(N,DB,DX,INCX,DY,INCY)
*
*       .. Scalar Arguments ..
*       REAL*16 SB
*       INTEGER INCX,INCY,N
*       ..
*       .. Array Arguments ..
*       REAL*16 DX(*),DY(*)
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
*>   Returns D.P. result with dot product accumulated in Q.P.
*>   QDSDOT = DB + sum for I = 0 to N-1 of DX(LX+I*INCX)*DY(LY+I*INCY),
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
*> \param[in] DB
*> \verbatim
*>          DB is  DOUBLE PRECISION
*>          double precision scalar to be added to inner product
*> \endverbatim
*>
*> \param[in] DX
*> \verbatim
*>          DX is  DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*>          double precision vector with N elements
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>          storage spacing between elements of SX
*> \endverbatim
*>
*> \param[in] DY
*> \verbatim
*>          DY is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*>          double precision vector with N elements
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
*> \ingroup dot
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
      DOUBLE PRECISION FUNCTION DQDDOT(N,DB,DX,INCX,DY,INCY)
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION DB
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION DX(*),DY(*)
*     .. Local Scalars ..
      REAL*16 QDDOT
      INTEGER I,KX,KY,NS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC DBLE, REAL
*     ..
      QDDOT = DB
      IF (N.LE.0) THEN
         DQDDOT = DBLE(QDDOT)
         RETURN
      END IF
      IF (INCX.EQ.INCY .AND. INCX.GT.0) THEN
*
*     Code for equal and positive increments.
*
         NS = N*INCX
         DO I = 1,NS,INCX
            QDDOT = QDDOT + REAL(SX(I),16)*REAL(SY(I),16)
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
            QDDOT = QDDOT + REAL(SX(KX),16)*REAL(SY(KY),16)
            KX = KX + INCX
            KY = KY + INCY
         END DO
      END IF
      DQDDOT = DBLE(QDDOT)
      RETURN
*
*     End of DQDDOT
*
      END
