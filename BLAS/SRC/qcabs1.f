*> \brief \b QCABS1
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       REAL*16 FUNCTION QCABS1(Z)
*
*       .. Scalar Arguments ..
*       COMPLEX*32 Z
*       ..
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> QCABS1 computes |Re(.)| + |Im(.)| of a double complex number
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] Z
*> \verbatim
*>          Z is COMPLEX*32
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
*> \ingroup double_blas_level1
*
*  =====================================================================
      REAL*16 FUNCTION QCABS1(Z) 
      IMPLICIT NONE     
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      COMPLEX*32 Z
*     ..
*     ..
*  =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC ABS,REAL,IMAG
*
      QCABS1 = ABS(REAL(Z, 16)) + ABS(IMAG(Z))
      RETURN
*
*     End of QCABS1
*
      END
