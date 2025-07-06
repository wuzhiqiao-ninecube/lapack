*> \brief \b QDDOT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       DOUBLE PRECISION FUNCTION QDDOT(N,SX,INCX,SY,INCY)
*
*       .. Scalar Arguments ..
*       INTEGER INCX,INCY,N
*       ..
*       .. Array Arguments ..
*       REAL SX(*),SY(*)
*       ..
*
*    AUTHORS
*    =======
*    Lawson, C. L., (JPL), Hanson, R. J., (SNLA),
*    Kincaid, D. R., (U. of Texas), Krogh, F. T., (JPL)
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> Compute the inner product of two vectors with extended
*> precision accumulation and result.
*>
*> Returns D.P. dot product accumulated in D.P., for S.P. SX and DY
*> QDDOT = sum for I = 0 to N-1 of  SX(LX+I*INCX) * DY(LY+I*INCY),
*> where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
*> defined in a similar way using INCY.
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
*> \param[in] SX
*> \verbatim
*>          SX is REAL array, dimension(N)
*>         single precision vector with N elements
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
*>          DY is REAL array, dimension(N)
*>         single precision vector with N elements
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>         storage spacing between elements of DY
*> \endverbatim
*>
*> \result QDDOT
*> \verbatim
*>          QDDOT is DOUBLE PRECISION
*>         QDDOT  double precision dot product (zero if N.LE.0)
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
*> \endverbatim
*
*> \par References:
*  ================
*>
*> \verbatim
*>
*>
*>  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
*>  Krogh, Basic linear algebra subprograms for Fortran
*>  usage, Algorithm No. 539, Transactions on Mathematical
*>  Software 5, 3 (September 1979), pp. 308-323.
*>
*>  REVISION HISTORY  (YYMMDD)
*>
*>  791001  DATE WRITTEN
*>  890831  Modified array declarations.  (WRB)
*>  890831  REVISION DATE from Version 3.2
*>  891214  Prologue converted to Version 4.0 format.  (BAB)
*>  920310  Corrected definition of LX in DESCRIPTION.  (WRB)
*>  920501  Reformatted the REFERENCES section.  (WRB)
*>  070118  Reformat to LAPACK style (JL)
*> \endverbatim
*>
*  =====================================================================
      DOUBLE PRECISION FUNCTION QDDOT(N,DX,INCX,DY,INCY)
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      REAL DX(*),DY(*)
*     ..
*
*  Authors:
*  ========
*  Lawson, C. L., (JPL), Hanson, R. J., (SNLA),
*  Kincaid, D. R., (U. of Texas), Krogh, F. T., (JPL)
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I,KX,KY,NS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC REAL
*     ..
      QDDOT = 0.0Q0
      IF (N.LE.0) RETURN
      IF (INCX.EQ.INCY .AND. INCX.GT.0) THEN
*
*     Code for equal, positive, non-unit increments.
*
         NS = N*INCX
         DO I = 1,NS,INCX
            QDDOT = QDDOT + REAL(DX(I),16)*REAL(DY(I),16)
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
            QDDOT = QDDOT + REAL(DX(KX),16)*REAL(DY(KY),16)
            KX = KX + INCX
            KY = KY + INCY
         END DO
      END IF
      RETURN
*
*     End of QDDOT
*
      END
