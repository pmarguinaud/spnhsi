! (C) Copyright 1989- ECMWF.
! (C) Copyright 1989- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE YOMRIP

!$ACDC methods

USE PARKIND1 , ONLY : JPIM, JPRB, JPRD
USE YOEOZOC  , ONLY : TEOZOC
USE YOECMIP  , ONLY : TECMIP
USE YOERADGHG, ONLY : TRADGHG
USE YOEAERC_TEGEN, ONLY : TEAERC_TEGEN
USE YOEAERC  , ONLY : TEAERC
USE REGLATLON_FIELD_MIX, ONLY : REGLATLON_FIELD

IMPLICIT NONE

SAVE

!     --------------------------------------------------------------------------------
!     Date and timestep related variables.
!     Values may be different for the different models run under the OOPS layer.
!     In particuliar, we find there all date and timestep variables updated in UPDTIM.
!     --------------------------------------------------------------------------------

TYPE :: TRIP

!*    Numbering of timesteps

!     NSTART : first timestep of model
!     NSTOP  : last timestep of model
!     CSTOP  : character string defining stopping criteria for run

!     NFOST  : NUMBER OF FIRST-ORDER TIME STEPS AT THE START OF THE RUN
!              IN ORDER TO AVOID TRAJECTORIES GOING OUT OF THE ATMOSPHERE.

!     ------------------------------------------------------------------

!*    Real time related variables (updated in UPDTIM)

!     NSTADD : NUMBER OF DAYS SINCE START OF THE MODEL
!     NSTASS : NUMBER OF SECONDS since start of model modulo(86400)
!     RSTATI : NUMBER OF SECONDS SINCE START OF THE MODEL
!     RTIMTR : ABSOLUTE TIME OF THE MODEL, in seconds since 2000-01-01 12 UTC

!     RHGMT  : GMT TIME OF THE MODEL  (BETWEEN 0 AND 86400)
!     REQTIM : EQUATION OF TIME

!     ------------------------------------------------------------------

!*    Sun related variables (updated in UPDTIM)
!     RSOVR  : TRUE SOLAR TIME (GMT+EQUATION OF TIME)

!     RDEASO : DISTANCE EARTH-SUN
!     RDECLI : DECLINATION
!     RWSOVR : IN RADIANS, TRUE SOLAR TIME (GMT+EQUATION OF TIME)
!              THIS ANGLE IS ALSO PI - (LONGITUDE OF THE POINT
!              WHERE THE SUN IS AT ZENITH)
!     RIP0   : I0 WEIGHTED BY THE DISTANCE EARTH-SUN

!     RCODEC : COSINE OF THE DECLINATION
!     RSIDEC :   SINE OF THE DECLINATION

!     RCOVSR : COSINE OF TRUE SOLAR TIME
!     RSIVSR :   SINE OF TRUE SOLAR TIME

!     NEW VARIABLES FOR LMSE
!     RCODECN : COSINE OF THE DECLINATION FOR NEXT TIME STEP
!     RSIDECN :   SINE OF THE DECLINATION FOR NEXT TIME STEP

!     RCOVSRN : COSINE OF TRUE SOLAR TIME FOR NEXT TIME STEP
!     RSIVSRN :   SINE OF TRUE SOLAR TIME FOR NEXT TIME STEP

!     RCODECF : COSINE OF THE DECLINATION at the end of radiation step
!     RSIDECF :   SINE OF THE DECLINATION at the end of radiation step
!     RCOVSRF : COSINE OF TRUE SOLAR TIME at the end of radiation step
!     RSIVSRF :   SINE OF TRUE SOLAR TIME at the end of radiation step

!     ------------------------------------------------------------------

!*    Moon related variables (updated in UPDTIM)

!     RDECLU : LUNAR DECLINATION
!     RTMOLT : IN RADIANS, TRUE LUNAR TIME (GMT+EQUATION OF TIME)
!              THIS ANGLE IS ALSO PI - (LONGITUDE OF THE POINT
!              WHERE THE MOON IS AT ZENITH)
!     RJUSMP : JUlian date used for SUn and MOon computations.
!     RLUNDI : LUNar distance to Earth
!     RIP0LU : LUNAR I0 (DOWNWARD TOA LUNAR FLUX)

!     RCODECLU : COSINE OF THE LUNAR DECLINATION
!     RSIDECLU :   SINE OF THE LUNAR DECLINATION

!     RCOVSRLU : COSINE OF TRUE LUNAR TIME
!     RSIVSRLU :   SINE OF TRUE LUNAR TIME

!     ------------------------------------------------------------------

!*    Time step related variables

!     TSTEP  : length of the timestep in seconds
!     TDT    : For leap-frog scheme: 2*TSTEP except at the first time step where it is TSTEP
!              For a two-time level scheme (semi-Lagrangian), TDT is always TSTEP.

!     RDTSA  : TDT  /RA
!     RDTSA2 : RDTSA**2
!     RDTS62 : RDTSA**2/6
!     RDTS22 : RDTSA**2/2

!     RTDT   : TDT

!     ------------------------------------------------------------------

!*   Time-variable climatological distributions of gases / aerosols
!    YREOZOC : spectral distribution of ozone

INTEGER(KIND=JPIM) :: NSTART
INTEGER(KIND=JPIM) :: NSTOP
CHARACTER(LEN=8)   :: CSTOP !! added olivier
INTEGER(KIND=JPIM) :: NSTADD
INTEGER(KIND=JPIM) :: NSTASS
INTEGER(KIND=JPIM) :: NFOST
REAL(KIND=JPRB) :: RSTATI
REAL(KIND=JPRD) :: RTIMTR ! double precision JPRD required for simple precision runs.
REAL(KIND=JPRB) :: RHGMT
REAL(KIND=JPRB) :: REQTIM
REAL(KIND=JPRB) :: RSOVR
REAL(KIND=JPRB) :: RDEASO
REAL(KIND=JPRB) :: RDECLI
REAL(KIND=JPRB) :: RWSOVR
REAL(KIND=JPRB) :: RIP0
REAL(KIND=JPRB) :: RCODEC
REAL(KIND=JPRB) :: RSIDEC
REAL(KIND=JPRB) :: RCOVSR
REAL(KIND=JPRB) :: RSIVSR
REAL(KIND=JPRB) :: RCODECN
REAL(KIND=JPRB) :: RSIDECN
REAL(KIND=JPRB) :: RCOVSRN
REAL(KIND=JPRB) :: RSIVSRN
REAL(KIND=JPRB) :: RCODECF
REAL(KIND=JPRB) :: RSIDECF
REAL(KIND=JPRB) :: RCOVSRF
REAL(KIND=JPRB) :: RSIVSRF
REAL(KIND=JPRB) :: TSTEP
REAL(KIND=JPRB) :: TDT
REAL(KIND=JPRB) :: RDTSA
REAL(KIND=JPRB) :: RDTSA2
REAL(KIND=JPRB) :: RDTS62
REAL(KIND=JPRB) :: RDTS22
REAL(KIND=JPRB) :: RTDT
REAL(KIND=JPRB) :: RDECLU
REAL(KIND=JPRB) :: RTMOLT
REAL(KIND=JPRD) :: RJUSMP ! for this julian date, warning double precision JPRD mandatory for simple precision runs.
REAL(KIND=JPRB) :: RLUNDI
REAL(KIND=JPRB) :: RIP0LU
REAL(KIND=JPRB) :: RCODECLU
REAL(KIND=JPRB) :: RSIDECLU
REAL(KIND=JPRB) :: RCOVSRLU
REAL(KIND=JPRB) :: RSIVSRLU

!! things put here because their values are time-interpolated, i.e. non-constant
! defined in YOEOZOC
TYPE(TEOZOC)    :: YREOZOC
! defined in YOECMIP
TYPE(TECMIP)    :: YRECMIP
! defined in YOERADGHG
TYPE(TRADGHG)  :: YRERADGHG
! defined in YOEAERC / YOAERC_TEGEN
TYPE(TEAERC_TEGEN)    :: YREAERC_TEGEN
TYPE(TEAERC)          :: YREAERC
TYPE(REGLATLON_FIELD) :: RAERSO4
!-------------------------------------------------
! 2022-07-11, J.M. Piriou: Tchebychev polynomials for precise Sun and Moon position.
!-------------------------------------------------
INTEGER(KIND=JPIM) :: JPORDT=11
REAL(KIND=JPRD) :: GCOEFS_ASCENSION_DROITE(7585,0:10,2) ! right ascension.
REAL(KIND=JPRD) :: GCOEFS_DECLINAISON(7585,0:10,2) ! declination.
REAL(KIND=JPRD) :: GCOEFS_DISTANCE(7585,0:10,2) ! distance to Earth.
REAL(KIND=JPRD) :: GTDEBUT(7585,2) ! julian time at beginning of time interval.
REAL(KIND=JPRD) :: GTFIN(7585,2) ! julian time at end of time interval.
INTEGER(KIND=JPIM) :: NINTT_MOON ! number of time periods for Moon tabulation.
INTEGER(KIND=JPIM) :: NINTT_SUN ! number of time periods for Sun tabulation.
CHARACTER(LEN=200) :: CGSOLEC_SUN ! file name for SOLar Eclipse Coefficients for SUN position.
CHARACTER(LEN=200) :: CGSOLEC_MOON ! file name for SOLar Eclipse Coefficients for MOON position.

END TYPE TRIP

END MODULE YOMRIP
