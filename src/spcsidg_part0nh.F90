SUBROUTINE SPCSIDG_PART0NH (YDGEOMETRY, YDDYN, KSTA, KEND, PSRHS,PLAPIN,&
     & KM,KOFF)

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE YOMDYN       , ONLY : TDYN
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMMP0       , ONLY : MYSETV

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(TDYN)        ,INTENT(IN)    :: YDDYN
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTA
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND
INTEGER(KIND=JPIM),INTENT(IN)    :: KM
REAL(KIND=JPRB),   INTENT(INOUT) :: PSRHS  (KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB),   INTENT(IN)    :: PLAPIN  (:)
INTEGER(KIND=JPIM),INTENT(IN)    :: KOFF 


INTEGER(KIND=JPIM) :: JSP, JLEV

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SPCSIDG_PART0NH',0,ZHOOK_HANDLE)

ASSOCIATE(NFLEVG=>YDGEOMETRY%YRDIMV%NFLEVG)
          
IF (KM>0) THEN
  !$OMP PARALLEL DO PRIVATE(JSP,JLEV)
  DO JLEV=1,NFLEVG
    DO JSP=KSTA,KEND
      PSRHS(JSP,JLEV)=PLAPIN(KOFF+JSP)*PSRHS(JSP,JLEV)
    ENDDO
  ENDDO
  !$OMP END PARALLEL DO
ENDIF

END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('SPCSIDG_PART0NH',1,ZHOOK_HANDLE)
END SUBROUTINE SPCSIDG_PART0NH

