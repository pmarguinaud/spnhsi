INTERFACE
SUBROUTINE TRIDIA2 (KN,KLON,KST,KEND,PM,PRHS,PSOL)

! -----------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK

! -----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KN
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KST
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND
REAL(KIND=JPRB)   ,INTENT(IN)    :: PM(KLON,KN,-1:1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRHS(KLON,KN)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSOL(KLON,KN)

! -----------------------------------------------------------------------------

END SUBROUTINE TRIDIA2
END INTERFACE
