SUBROUTINE SPCIMPF_HSI2(&
 ! --- INPUT -----------------------------------------------------------------
 & YDGEOMETRY,YDRIP,YDDYN,KMLOCSTA,KMLOCEND,KSTA,KEND,PR2D,&
 ! --- OUTPUT ----------------------------------------------------------------
 & PSRHS2_INC)  

!**** *SPCIMPFSOLVE* - CALL SIMPLICO FROM S-SPACE

!     Purpose.
!     --------

!**   Interface.
!     ----------
!        *CALL* *SPCIMPFSOLVE(..)

!        Explicit arguments :  LDONEM  - T if only one m if processed
!                              PSDIVP  - right hand side
!                              PSPDIVP - solution

!     Method.
!     -------

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        Tomas Wilhelmsson  *ECMWF*
!        21-09-2009 Original with extracted code from spcsi.F90

!     Modifications.
!     --------------
!      T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!      K. Yessad (July 2014): Move some variables.
!      O. Marsden (May 2016): Remove redundant geometry argument
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1     , ONLY : JPIM, JPRB
USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST       , ONLY : ROMEGA
USE YOMDYN       , ONLY : TDYN
USE YOMRIP       , ONLY : TRIP

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(TDYN)        ,INTENT(IN)    :: YDDYN
TYPE(TRIP)        ,INTENT(IN)    :: YDRIP
INTEGER(KIND=JPIM),INTENT(IN)    :: KMLOCSTA
INTEGER(KIND=JPIM),INTENT(IN)    :: KMLOCEND
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTA
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PR2D (KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSRHS2_INC(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZALPHA (0:YDGEOMETRY%YRDIM%NSMAX+1)
REAL(KIND=JPRB) :: ZDENIM (0:YDGEOMETRY%YRDIM%NSMAX+1)
REAL(KIND=JPRB) :: ZEPSI  (0:YDGEOMETRY%YRDIM%NSMAX)
REAL(KIND=JPRB) :: ZFPLUS (0:YDGEOMETRY%YRDIM%NSMAX+1)
REAL(KIND=JPRB) :: ZFMINUS(0:YDGEOMETRY%YRDIM%NSMAX+1)

INTEGER(KIND=JPIM) :: ILO, IM, ISTA, IEND, JL, JMLOC

REAL(KIND=JPRB) :: ZAL, ZBDT, ZEM, ZEN, ZF
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

#include "si_mxptco.h"

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SPCIMPF_HSI2',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,YDGEM=>YDGEOMETRY%YRGEM, YDMP=>YDGEOMETRY%YRMP, &
 & YDLAP=>YDGEOMETRY%YRLAP)
ASSOCIATE(NSMAX=>YDDIM%NSMAX, NSPEC2=>YDDIM%NSPEC2, NUMP=>YDDIM%NUMP, &
 & NFLEVL=>YDDIMV%NFLEVL, NFLSUR=>YDDIMV%NFLSUR, &
 & RBT=>YDDYN%RBT, &
 & TDT=>YDRIP%TDT,NSPSTAF=>YDMP%NSPSTAF)
!     ------------------------------------------------------------------

!     ------------------------------------------------------------------

!*       1.    SEMI-IMPLICIT SPECTRAL COMPUTATIONS.
!              ------------------------------------

!*        1.1  Preliminary initialisations.

ZBDT=0.5_JPRB*TDT*RBT

DO JMLOC=KMLOCSTA,KMLOCEND

!*        1.2  Main calculations.


  IM=YDLAP%MYMS(JMLOC)
  ISTA=NSPSTAF(IM)
  IEND=ISTA+2*(NSMAX+1-IM)-1

  !*             Set up helper arrays for implicit Coriolis.

  ZEM=REAL(IM,JPRB)
  ZAL=2.0_JPRB*ZBDT*ROMEGA*ZEM
  ILO=IM
  IF (IM == 0) THEN
    ZALPHA(0)=0.0_JPRB
    ZDENIM(0)=0.0_JPRB
    ZEPSI(0)=0.0_JPRB
    ILO=1
  ENDIF
  DO JL=ILO,NSMAX
    ZEN=REAL(JL,JPRB)
    ZALPHA(JL)=ZAL/(ZEN*(ZEN+1.0_JPRB))
    ZDENIM(JL)=1.0_JPRB/(1.0_JPRB+ZALPHA(JL)**2)
    ZEPSI(JL)=SQRT((ZEN*ZEN-ZEM*ZEM)/(4.0_JPRB*ZEN*ZEN-1.0_JPRB))
  ENDDO
  ZALPHA(NSMAX+1)=0.0_JPRB
  ZDENIM(NSMAX+1)=0.0_JPRB

  IF (IM == 0) THEN
    ZFPLUS(0)=0.0_JPRB
    ZFMINUS(0)=0.0_JPRB
  ENDIF
  ZF=2.0_JPRB*ZBDT*ROMEGA
  DO JL=ILO,NSMAX-1
    ZEN=REAL(JL,JPRB)
    ZFPLUS(JL)=ZF*ZEN*ZEPSI(JL+1)/(ZEN+1.0_JPRB)
    ZFMINUS(JL)=ZF*(ZEN+1.0_JPRB)*ZEPSI(JL)/ZEN
  ENDDO
  ZEN=REAL(NSMAX,JPRB)
  ZFPLUS(NSMAX)=0.0_JPRB
  ZFMINUS(NSMAX)=ZF*(ZEN+1.0_JPRB)*ZEPSI(NSMAX)/ZEN
  ZFPLUS(NSMAX+1)=0.0_JPRB
  ZFMINUS(NSMAX+1)=0.0_JPRB

  IF (KMLOCSTA==KMLOCEND) THEN

    CALL SI_MXPTCO(IM,NSMAX,YDGEOMETRY%YRDIMV%NFLEVG,YDGEOMETRY%YRDIMV%NFLEVG,ZF,&
     & ZALPHA(IM),ZDENIM(IM),ZEPSI(IM),&
     & PR2D,PSRHS2_INC)

  ELSE

    CALL SI_MXPTCO(IM,NSMAX,YDGEOMETRY%YRDIMV%NFLEVG,YDGEOMETRY%YRDIMV%NFLEVG,ZF,&
     & ZALPHA(IM),ZDENIM(IM),ZEPSI(IM),&
     & PR2D(ISTA:IEND,:),PSRHS2_INC(ISTA:IEND,:))

  ENDIF

ENDDO

!     ------------------------------------------------------------------

!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SPCIMPF_HSI2',1,ZHOOK_HANDLE)
END SUBROUTINE SPCIMPF_HSI2
