SUBROUTINE MXPTMA(KLX,KVX,KVXS,KIX,KIXS,PA,PBI,PCI,PBS,PCS,PX,PY)

!**** *MXPTMA*   - Multiplication of a pentadiagonal matrix by a matrix.

!     Purpose.    Multiplication of a pentadiagonal matrix by a matrix.
!     --------

!    Example, for KLX=5,KVX=2,KIX=1.

!    I PA (1) PBS(1) PCS(1)   0      0    I   I PX(1,1) PX(2,1) I
!    I PBI(1) PA (2) PBS(2) PCS(2)   0    I   I PX(1,2) PX(2,2) I
!    I PCI(1) PBI(2) PA (3) PBS(3) PCS(3) I * I PX(1,3) PX(2,3) I
!    I   0    PCI(2) PBI(3) PA (4) PBS(4) I   I PX(1,4) PX(2,4) I
!    I   0      0    PCI(3) PBI(4) PA (5) I   I PX(1,5) PX(2,5) I

!      I PY(1,1) PY(2,1) I
!      I PY(1,2) PY(2,2) I
!    = I PY(1,3) PY(2,3) I
!      I PY(1,4) PY(2,4) I
!      I PY(1,5) PY(2,5) I

!**   Interface.
!     ----------
!        *CALL* *MXPTMA(KLX,KVX,KVXS,KIX,PA,PBI,PCI,PBS,PCS,PX,PY)

!        Explicit arguments :
!        --------------------
!         KLX:         - Dimension of the matrix.                   (input)
!         KVX,KIX:     - Number of vectors to be multiplied is      (input)
!                        KVX*KIX.
!         KVXS:        - Surdimension corresponding to KVX.         (input)
!         PA:          - Diagonal of the matrix.                    (input)
!         PBI,PCI:     - Lower diagonals of the matrix.             (input)
!         PBS,PCS:     - Upper diagonals of the matrix.             (input)
!         PX:          - Initial vector:                            (input)
!         PY:          - Final vector:                              (output)

!        Implicit arguments :
!        --------------------
!        none.

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!        None.

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        K. YESSAD: OCTOBER 1993.

!     Modifications.
!     --------------
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLX 
INTEGER(KIND=JPIM),INTENT(IN)    :: KVXS  
INTEGER(KIND=JPIM),INTENT(IN)    :: KVX
INTEGER(KIND=JPIM),INTENT(IN)    :: KIX
INTEGER(KIND=JPIM),INTENT(IN)    :: KIXS
REAL(KIND=JPRB)   ,INTENT(IN)    :: PA(KLX) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBI(KLX) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCI(KLX) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBS(KLX) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCS(KLX) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PX(KIXS,KLX,KVXS) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PY(KIXS,KLX,KVXS) 
!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JI, JL, JV
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MXPTMA',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

!*       1.    COMPUTATION OF PY.
!              ------------------

!$OMP PARALLEL PRIVATE(JL,JV,JI)
IF (KLX >= 4) THEN
  !$OMP DO
  DO JV=1,KVX
    DO JI=1,KIX
      PY(JI,1,JV) = PA (1)*PX(JI,1,JV)+PBS(1)*PX(JI,2,JV)+PCS(1)*PX(JI,3,JV)
      PY(JI,2,JV) = PBI(1)*PX(JI,1,JV)&
       & +PA (2)*PX(JI,2,JV)&
       & +PBS(2)*PX(JI,3,JV)&
       & +PCS(2)*PX(JI,4,JV)  
    ENDDO
  ENDDO

  !$OMP DO 
  DO JV=1,KVX
    DO JI=1,KIX
      DO JL=3,KLX-2 
        PY(JI,JL,JV) = PCI(JL-2)*PX(JI,JL-2,JV)&
         & +PBI(JL-1)*PX(JI,JL-1,JV)&
         & +PA (JL  )*PX(JI,JL,JV)&
         & +PBS(JL  )*PX(JI,JL+1,JV)&
         & +PCS(JL  )*PX(JI,JL+2,JV)  
      ENDDO
    ENDDO
  ENDDO

  !$OMP DO
  DO JV=1,KVX
    DO JI=1,KIX
      PY(JI,KLX-1,JV) = PCI(KLX-3)*PX(JI,KLX-3,JV)&
       & +PBI(KLX-2)*PX(JI,KLX-2,JV)&
       & +PA (KLX-1)*PX(JI,KLX-1,JV)&
       & +PBS(KLX-1)*PX(JI,KLX,JV)  
      PY(JI,KLX,JV) = PCI(KLX-2)*PX(JI,KLX-2,JV)&
       & +PBI(KLX-1)*PX(JI,KLX-1,JV)&
       & +PA (KLX  )*PX(JI,KLX,JV)  
    ENDDO
  ENDDO

ELSEIF (KLX == 3) THEN
  !$OMP DO
  DO JV=1,KVX
    DO JI=1,KIX
      PY(JI,1,JV) = PA (1)*PX(JI,1,JV)+PBS(1)*PX(JI,2,JV)+PCS(1)*PX(JI,3,JV)
      PY(JI,2,JV) = PBI(1)*PX(JI,1,JV)+PA (2)*PX(JI,2,JV)+PBS(2)*PX(JI,3,JV)
      PY(JI,3,JV) = PCI(1)*PX(JI,1,JV)+PBI(2)*PX(JI,2,JV)+PA (3)*PX(JI,3,JV)
    ENDDO
  ENDDO

ELSEIF (KLX == 2) THEN
  !$OMP DO
  DO JV=1,KVX
    DO JI=1,KIX
      PY(JI,1,JV) = PA (1)*PX(JI,1,JV)+PBS(1)*PX(JI,2,JV)
      PY(JI,2,JV) = PBI(1)*PX(JI,1,JV)+PA (2)*PX(JI,2,JV)
    ENDDO
  ENDDO

ELSEIF (KLX == 1) THEN
  !$OMP DO
  DO JV=1,KVX
    DO JI=1,KIX
      PY(JI,1,JV) = PA (1)*PX(JI,1,JV)
    ENDDO
  ENDDO

ENDIF
!$OMP END PARALLEL

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('MXPTMA',1,ZHOOK_HANDLE)
END SUBROUTINE MXPTMA

