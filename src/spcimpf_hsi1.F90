SUBROUTINE SPCIMPF_HSI1(&
 ! --- INPUT -----------------------------------------------------------------
 & YDGEOMETRY,YDRIP,YDDYN,KMLOCSTA,KMLOCEND,KSTA,KEND,&
 ! --- OUTPUT ----------------------------------------------------------------
 & PSDIV,PSPVORG)  

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
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSDIV (KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSPVORG(KSTA:KEND,YDGEOMETRY%YRDIMV%NFLEVG)

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZALPHA (0:YDGEOMETRY%YRDIM%NSMAX+1)
REAL(KIND=JPRB) :: ZDENIM (0:YDGEOMETRY%YRDIM%NSMAX+1)
REAL(KIND=JPRB) :: ZEPSI  (0:YDGEOMETRY%YRDIM%NSMAX)
REAL(KIND=JPRB) :: ZFPLUS (0:YDGEOMETRY%YRDIM%NSMAX+1)
REAL(KIND=JPRB) :: ZFMINUS(0:YDGEOMETRY%YRDIM%NSMAX+1)

INTEGER(KIND=JPIM) :: ILO, IM, ISTA, IEND, JL, JMLOC,IRSP,IMSP,JN,JLEV

REAL(KIND=JPRB) :: ZAL, ZBDT, ZBDT2, ZEM, ZEN, ZF,ZTEMP
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SPCIMPF_HSI1',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,YDGEM=>YDGEOMETRY%YRGEM, YDMP=>YDGEOMETRY%YRMP, &
 & YDLAP=>YDGEOMETRY%YRLAP)
ASSOCIATE(NSMAX=>YDDIM%NSMAX, NSPEC2=>YDDIM%NSPEC2, NUMP=>YDDIM%NUMP, &
 & NFLEVL=>YDDIMV%NFLEVL, NFLSUR=>YDDIMV%NFLSUR, &
 & RBT=>YDDYN%RBT, &
 & NFLEVG=>YDDIMV%NFLEVG, &
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

      IF (IM > 0) THEN
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JN,JLEV,IRSP,IMSP,ZTEMP)
          DO JLEV=1,NFLEVG
            DO JN=IM,NSMAX
            IRSP=ISTA+(JN-IM)*2
            IMSP=IRSP+1
            ZTEMP=ZDENIM(JN)*(PSPVORG(IMSP,JLEV)&
             & +ZALPHA(JN)*PSPVORG(IRSP,JLEV))
            PSPVORG(IRSP,JLEV)=ZDENIM(JN)*(PSPVORG(IRSP,JLEV)&
             & -ZALPHA(JN)*PSPVORG(IMSP,JLEV))
            PSPVORG(IMSP,JLEV)=ZTEMP
          ENDDO
        ENDDO
!$OMP END PARALLEL DO
      ENDIF

      !        Add [F] * result to rhs of Helmholtz equation

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JN,JLEV,IRSP,IMSP)
        DO JLEV=1,NFLEVG
          DO JN=IM+1,NSMAX
          IRSP=ISTA+(JN-IM)*2
          IMSP=IRSP+1
          PSDIV(IRSP,JLEV)=PSDIV(IRSP,JLEV)&
           & + ZFMINUS(JN)*PSPVORG(IRSP-2,JLEV)
          PSDIV(IMSP,JLEV)=PSDIV(IMSP,JLEV)&
           & + ZFMINUS(JN)*PSPVORG(IMSP-2,JLEV)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JN,JLEV,IRSP,IMSP)
        DO JLEV=1,NFLEVG
          DO JN=IM,NSMAX-1
          IRSP=ISTA+(JN-IM)*2
          IMSP=IRSP+1
          PSDIV(IRSP,JLEV)=PSDIV(IRSP,JLEV)+ZFPLUS(JN)*PSPVORG(IRSP+2,JLEV)
          PSDIV(IMSP,JLEV)=PSDIV(IMSP,JLEV)+ZFPLUS(JN)*PSPVORG(IMSP+2,JLEV)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO

ENDDO

!     ------------------------------------------------------------------

!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SPCIMPF_HSI1',1,ZHOOK_HANDLE)
END SUBROUTINE SPCIMPF_HSI1
