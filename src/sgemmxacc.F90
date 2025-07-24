#if defined(_OPENACC)
SUBROUTINE SGEMMXACC(PA,KA,KAD,PB,KB,KBD,PC,KC,KCA,KAR,KAC,KBC)

!**   Interface.
!     ----------
!        CALL SGEMMXACC(PA,KA,KAD,PB,KB,KBD,PC,KC,KCA,KAR,KAC,KBC)

!        Explicit arguments : See SGEMMX documentaion.
!        --------------------
!         PA     - input matrix PA                                     (in)
!         KA     - memory jump between two lines in PA (generally 1)   (in)
!         KAD    - memory jump between two columns in PA               (in)
!         PB     - input matrix PB                                     (in)
!         KB     - memory jump between two lines in PB (generally 1)   (in)
!         KBD    - memory jump between two columns in PB               (in)
!         PC     - output matrix PC                                    (out)
!         KC     - memory jump between two lines in PC (generally 1)   (in)
!         KCA    - memory jump between two columns in PC               (in)
!         KAR    - number of useful lines of PA                        (in)
!         KAC    - number of useful columns of PA (and lines of PB)    (in)
!         KBC    - number of useful columns of PB                      (in)

!        Implicit arguments :
!        --------------------

!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB, JPRD
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
USE OML_MOD   ,ONLY : OML_MAX_THREADS, OML_IN_PARALLEL

USE OPENACC
USE CUBLAS

!     ------------------------------------------------------------------

IMPLICIT NONE

REAL(KIND=JPRB)   ,INTENT(IN)    :: PA(KAR,KAC) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KAD 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PB(KBC,KAC) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KB 
INTEGER(KIND=JPIM),INTENT(IN)    :: KBD 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PC(KBC,KAR) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KC 
INTEGER(KIND=JPIM),INTENT(IN)    :: KCA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KAR 
INTEGER(KIND=JPIM),INTENT(IN)    :: KAC 
INTEGER(KIND=JPIM),INTENT(IN)    :: KBC 

!     ------------------------------------------------------------------

REAL(KIND=JPRB)   :: PCTR(KAR,KBC)
REAL(KIND=JPRB)   :: PBTR(KAC,KBC)
INTEGER(KIND=JPIM):: JL,JS
LOGICAL           :: LLACC

REAL(KIND=JPRB), PARAMETER :: ALPHA=1.0
REAL(KIND=JPRB), PARAMETER :: BETA=0.0

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SGEMMXACC',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------


!$ACC DATA PRESENT(PA,PB,PC)
!$ACC HOST_DATA USE_DEVICE(PA,PB,PC)
#if defined(PARKIND1_SINGLE)
!!  CALL CUBLASSGEMM('N','N',KAR,KBC,KAC,ALPHA,&
!!        &PA(1,1),KAD,PB(1,1),KBD,BETA,PC(1,1),KCA)
  CALL CUBLASSGEMM('N','T',KBC,KAR,KAC,ALPHA,&
        &PB(1,1),KBC,PA(1,1),KAR,BETA,PC(1,1),KBC)
#else
!!  CALL CUBLASDGEMM('N','N',KAR,KBC,KAC,ALPHA,&
!!        &PA(1,1),KAD,PB(1,1),KBD,BETA,PC(1,1),KCA)
  CALL CUBLASDGEMM('N','T',KBC,KAR,KAC,ALPHA,&
        &PB(1,1),KBC,PA(1,1),KAR,BETA,PC(1,1),KBC)

#endif
!$ACC END HOST_DATA
!$ACC WAIT
!$ACC END DATA

IF (LHOOK) CALL DR_HOOK('SGEMMXACC',1,ZHOOK_HANDLE)
END SUBROUTINE SGEMMXACC
#else
SUBROUTINE SGEMMXACC(PA,KA,KAD,PB,KB,KBD,PC,KC,KCA,KAR,KAC,KBC)

!**   Interface.
!     ----------
!        CALL SGEMMXACC(PA,KA,KAD,PB,KB,KBD,PC,KC,KCA,KAR,KAC,KBC)

!        Explicit arguments : See SGEMMX documentaion.
!        --------------------
!         PA     - input matrix PA                                     (in)
!         KA     - memory jump between two lines in PA (generally 1)   (in)
!         KAD    - memory jump between two columns in PA               (in)
!         PB     - input matrix PB                                     (in)
!         KB     - memory jump between two lines in PB (generally 1)   (in)
!         KBD    - memory jump between two columns in PB               (in)
!         PC     - output matrix PC                                    (out)
!         KC     - memory jump between two lines in PC (generally 1)   (in)
!         KCA    - memory jump between two columns in PC               (in)
!         KAR    - number of useful lines of PA                        (in)
!         KAC    - number of useful columns of PA (and lines of PB)    (in)
!         KBC    - number of useful columns of PB                      (in)

!        Implicit arguments :
!        --------------------

!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB, JPRD
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
USE OML_MOD   ,ONLY : OML_MAX_THREADS, OML_IN_PARALLEL

!     ------------------------------------------------------------------

IMPLICIT NONE

REAL(KIND=JPRB)   ,INTENT(IN)    :: PA(KAR,KAC) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KAD 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PB(KBC,KAC) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KB 
INTEGER(KIND=JPIM),INTENT(IN)    :: KBD 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PC(KBC,KAR) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KC 
INTEGER(KIND=JPIM),INTENT(IN)    :: KCA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KAR 
INTEGER(KIND=JPIM),INTENT(IN)    :: KAC 
INTEGER(KIND=JPIM),INTENT(IN)    :: KBC 

!     ------------------------------------------------------------------

REAL(KIND=JPRB)   :: PCTR(KAR,KBC)
REAL(KIND=JPRB)   :: PBTR(KAC,KBC)
INTEGER(KIND=JPIM):: JL,JS
LOGICAL           :: LLACC

REAL(KIND=JPRB), PARAMETER :: ALPHA=1.0
REAL(KIND=JPRB), PARAMETER :: BETA=0.0

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SGEMMXACC',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------
#if defined(PARKIND1_SINGLE)
!!  CALL SGEMM('N','N',KAR,KBC,KAC,ALPHA,&
!!        &PA(1,1),KAD,PB(1,1),KBD,BETA,PC(1,1),KCA)
  CALL SGEMM('N','T',KBC,KAR,KAC,ALPHA,&
        &PB(1,1),KBC,PA(1,1),KAR,BETA,PC(1,1),KBC)
#else
!!  CALL DGEMM('N','N',KAR,KBC,KAC,ALPHA,&
!!        &PA(1,1),KAD,PB(1,1),KBD,BETA,PC(1,1),KCA)
  CALL DGEMM('N','T',KBC,KAR,KAC,ALPHA,&
        &PB(1,1),KBC,PA(1,1),KAR,BETA,PC(1,1),KBC)
#endif

IF (LHOOK) CALL DR_HOOK('SGEMMXACC',1,ZHOOK_HANDLE)
END SUBROUTINE SGEMMXACC

#endif
