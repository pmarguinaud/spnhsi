SUBROUTINE TRIDIA2 (KN,KLON,KST,KEND,PM,PRHS,PSOL)

!$ACDC singleblock

!**** *TRIDIA*   SOLVES A NUMBER OF TRIDIAGONAL LINEAR SYSTEMS

!                Solves PM(js)*PSOL = PRHS(js) for js=kst,kend
!                (PM depends on "js", cf. former TRIDIALCZ)

!     Remark: this routine does something similar to routine SGTSL, but:
!      - it has been optimised to invert several tridiagonal linear systems
!        (SGTSL inverts only one tridiagonal linear system).
!      - it does not the additional tests and calculations done in SGTSL
!        (output quantity "kinfo").
!      - it is F90-norm compliant.

!        Explicit arguments :
!        --------------------
!            KN         : Dimension of the systems.                    (in)
!            KLON       : Number of systems to be solved.              (in)
!            KST        : First system to be solved.                   (in)
!            KEND       : Last system to be solved.                    (in)
!            PM         : Tridiagonal matrices of the systems          (inout)
!            PRHS       : RHS of the systems                           (in)
!            PSOL       : Solutions of the systems                     (out)

!     Author.
!     -------
!           OLIVIER TALAGRAND  -  16 SEPTEMBER 1988

!     Modifications.
!     --------------
!  MODIFICATION: ROBERTO BUIZZA - 29 JUL 92
!  K. Yessad (Oct 2008): merge of TRIDIALCZ and TRIDIAVSPL.
! -----------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

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

REAL(KIND=JPRB) :: ZRHS(KLON,KN)
REAL(KIND=JPRB) :: ZM(KLON,KN,-1:1)
INTEGER(KIND=JPIM) :: J, JROF

REAL(KIND=JPRB) :: ZDEN
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

! -----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TRIDIA2',0,ZHOOK_HANDLE)
! -----------------------------------------------------------------------------

!     1.   INITIALIZATIONS.
!          ----------------

!     1.1 FILL IN EXTREMITIES OF SECONDARY DIAGONALS

ZM=PM

!$ACDC PARALLEL {

ZM(KST:KEND,1,-1) = 0.0_JPRB
ZM(KST:KEND,KN,1) = 0.0_JPRB

!$ACDC }

!     1.2 Copy PRHS

!$ACDC PARALLEL {

ZRHS(KST:KEND,1:KN)=PRHS(KST:KEND,1:KN)

!$ACDC }

! -----------------------------------------------------------------------------

!     2.   ASCENDING LOOP.
!          ---------------

!$ACDC PARALLEL {

DO JROF = KST,KEND
  PSOL(JROF,1) = -ZM(JROF,1,1)/ZM(JROF,1,0)
  ZRHS(JROF,1) =  ZRHS(JROF,1)/ZM(JROF,1,0)
ENDDO

!$ACDC }

!$ACDC PARALLEL {

DO J = 2,KN
  DO JROF = KST,KEND
    ZDEN = 1.0_JPRB/(ZM(JROF,J,-1)*PSOL(JROF,J-1) + ZM(JROF,J,0))
    PSOL(JROF,J) = -ZM(JROF,J,1)*ZDEN
    ZRHS(JROF,J) = (ZRHS(JROF,J) - ZRHS(JROF,J-1)*ZM(JROF,J,-1))*ZDEN
  ENDDO
ENDDO

!$ACDC }

! -----------------------------------------------------------------------------

!     3.   DESCENDING LOOP.
!          ----------------

!$ACDC PARALLEL {

PSOL(KST:KEND,KN)=ZRHS(KST:KEND,KN)
DO J = KN-1,1,-1
  DO JROF = KST,KEND
    PSOL(JROF,J) = ZRHS(JROF,J) + PSOL(JROF,J)*PSOL(JROF,J+1)
  ENDDO
ENDDO

!$ACDC }

! -----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TRIDIA2',1,ZHOOK_HANDLE)
END SUBROUTINE TRIDIA2
