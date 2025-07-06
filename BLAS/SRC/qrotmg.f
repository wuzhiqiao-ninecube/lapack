*> \brief \b QROTMG
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE QROTMG(QD1,QD2,QX1,QY1,QPARAM)
*
*       .. Scalar Arguments ..
*       REAL*16 QD1,QD2,QX1,QY1
*       ..
*       .. Array Arguments ..
*       REAL*16 QPARAM(5)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS
*>    THE SECOND COMPONENT OF THE 2-VECTOR  (SQRT(QD1)*QX1,SQRT(QD2)*>    QY2)**T.
*>    WITH QPARAM(1)=QFLAG, H HAS ONE OF THE FOLLOWING FORMS..
*>
*>    QFLAG=-1.Q0     QFLAG=0.Q0        QFLAG=1.Q0     QFLAG=-2.Q0
*>
*>      (QH11  QH12)    (1.Q0  QH12)    (QH11  1.Q0)    (1.Q0  0.Q0)
*>    H=(          )    (          )    (          )    (          )
*>      (QH21  QH22),   (QH21  1.Q0),   (-1.Q0 QH22),   (0.Q0  1.Q0).
*>    LOCATIONS 2-4 OF QPARAM CONTAIN QH11,QH21,QH12, AND QH22
*>    RESPECTIVELY. (VALUES OF 1.Q0, -1.Q0, OR 0.Q0 IMPLIED BY THE
*>    VALUE OF QPARAM(1) ARE NOT STORED IN QPARAM.)
*>
*>    THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
*>    INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
*>    OF QD1 AND QD2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
*>
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in,out] QD1
*> \verbatim
*>          QD1 is REAL*16
*> \endverbatim
*>
*> \param[in,out] QD2
*> \verbatim
*>          QD2 is REAL*16
*> \endverbatim
*>
*> \param[in,out] QX1
*> \verbatim
*>          QX1 is REAL*16
*> \endverbatim
*>
*> \param[in] QY1
*> \verbatim
*>          QY1 is REAL*16
*> \endverbatim
*>
*> \param[out] QPARAM
*> \verbatim
*>          QPARAM is REAL*16 array, dimension (5)
*>     QPARAM(1)=QFLAG
*>     QPARAM(2)=QH11
*>     QPARAM(3)=QH21
*>     QPARAM(4)=QH12
*>     QPARAM(5)=QH22
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
*> \ingroup rotmg
*
*  =====================================================================
      SUBROUTINE QROTMG(QD1,QD2,QX1,QY1,QPARAM)
*
*  -- Reference BLAS level1 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      REAL*16 QD1,QD2,QX1,QY1
*     ..
*     .. Array Arguments ..
      REAL*16 QPARAM(5)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      REAL*16 QFLAG,QH11,QH12,QH21,QH22,QP1,QP2,QQ1,QQ2,QTEMP,
     $     QU,GAM,GAMSQ,ONE,RGAMSQ,TWO,ZERO
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC ABS
*     ..
*     .. Data statements ..
*
      DATA ZERO,ONE,TWO/0.Q0,1.Q0,2.Q0/
      DATA GAM,GAMSQ,RGAMSQ/4096.Q0,1.67772Q7,5.96046Q-8/
*     ..

      IF (QD1.LT.ZERO) THEN
*        GO ZERO-H-D-AND-QX1..
         QFLAG = -ONE
         QH11 = ZERO
         QH12 = ZERO
         QH21 = ZERO
         QH22 = ZERO
*
         QD1 = ZERO
         QD2 = ZERO
         QX1 = ZERO
      ELSE
*        CASE-QD1-NONNEGATIVE
         QP2 = QD2*QY1
         IF (QP2.EQ.ZERO) THEN
            QFLAG = -TWO
            QPARAM(1) = QFLAG
            RETURN
         END IF
*        REGULAR-CASE..
         QP1 = QD1*QX1
         QQ2 = QP2*QY1
         QQ1 = QP1*QX1
*
         IF (ABS(QQ1).GT.ABS(QQ2)) THEN
            QH21 = -QY1/QX1
            QH12 = QP2/QP1
*
            QU = ONE - QH12*QH21
*
           IF (QU.GT.ZERO) THEN
             QFLAG = ZERO
             QD1 = QD1/QU
             QD2 = QD2/QU
             QX1 = QX1*QU
           ELSE
*            This code path if here for safety. We do not expect this
*            condition to ever hold except in edge cases with rounding
*            errors. See DOI: 10.1145/355841.355847
             QFLAG = -ONE
             QH11 = ZERO
             QH12 = ZERO
             QH21 = ZERO
             QH22 = ZERO
*
             QD1 = ZERO
             QD2 = ZERO
             QX1 = ZERO
           END IF
         ELSE

            IF (QQ2.LT.ZERO) THEN
*              GO ZERO-H-D-AND-QX1..
               QFLAG = -ONE
               QH11 = ZERO
               QH12 = ZERO
               QH21 = ZERO
               QH22 = ZERO
*
               QD1 = ZERO
               QD2 = ZERO
               QX1 = ZERO
            ELSE
               QFLAG = ONE
               QH11 = QP1/QP2
               QH22 = QX1/QY1
               QU = ONE + QH11*QH22
               QTEMP = QD2/QU
               QD2 = QD1/QU
               QD1 = QTEMP
               QX1 = QY1*QU
            END IF
         END IF

*     PROCEDURE..SCALE-CHECK
         IF (QD1.NE.ZERO) THEN
            DO WHILE ((QD1.LE.RGAMSQ) .OR. (QD1.GE.GAMSQ))
               IF (QFLAG.EQ.ZERO) THEN
                  QH11 = ONE
                  QH22 = ONE
                  QFLAG = -ONE
               ELSE
                  QH21 = -ONE
                  QH12 = ONE
                  QFLAG = -ONE
               END IF
               IF (QD1.LE.RGAMSQ) THEN
                  QD1 = QD1*GAM**2
                  QX1 = QX1/GAM
                  QH11 = QH11/GAM
                  QH12 = QH12/GAM
               ELSE
                  QD1 = QD1/GAM**2
                  QX1 = QX1*GAM
                  QH11 = QH11*GAM
                  QH12 = QH12*GAM
               END IF
            ENDDO
         END IF

         IF (QD2.NE.ZERO) THEN
            DO WHILE ( (ABS(QD2).LE.RGAMSQ) .OR. (ABS(QD2).GE.GAMSQ) )
               IF (QFLAG.EQ.ZERO) THEN
                  QH11 = ONE
                  QH22 = ONE
                  QFLAG = -ONE
               ELSE
                  QH21 = -ONE
                  QH12 = ONE
                  QFLAG = -ONE
               END IF
               IF (ABS(QD2).LE.RGAMSQ) THEN
                  QD2 = QD2*GAM**2
                  QH21 = QH21/GAM
                  QH22 = QH22/GAM
               ELSE
                  QD2 = QD2/GAM**2
                  QH21 = QH21*GAM
                  QH22 = QH22*GAM
               END IF
            END DO
         END IF

      END IF

      IF (QFLAG.LT.ZERO) THEN
         QPARAM(2) = QH11
         QPARAM(3) = QH21
         QPARAM(4) = QH12
         QPARAM(5) = QH22
      ELSE IF (QFLAG.EQ.ZERO) THEN
         QPARAM(3) = QH21
         QPARAM(4) = QH12
      ELSE
         QPARAM(2) = QH11
         QPARAM(5) = QH22
      END IF

      QPARAM(1) = QFLAG
      RETURN
*
*     End of QROTMG
*
      END
