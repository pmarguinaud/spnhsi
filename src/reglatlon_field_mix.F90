MODULE REGLATLON_FIELD_MIX

!$ACDC methods

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
IMPLICIT NONE
SAVE

TYPE REGLATLON_FIELD
  INTEGER(KIND=JPIM)       :: NLAT               ! # of latitudes   ; >=1
  INTEGER(KIND=JPIM)       :: NLON               ! # of longitudes  ; >=1
  REAL(KIND=JPRB)          :: DLAT               ! Latitude  increment (degrees), can be positve/negative
  REAL(KIND=JPRB)          :: DLON               ! Longitude increment (degrees), is assumed to be positve
  REAL(KIND=JPRB) ,POINTER :: PFLD(:,:)=>NULL()  ! Field contents as (lat,lon)
  REAL(KIND=JPRB) ,POINTER :: PLAT(:)  =>NULL()  ! List of NLAT  Latitudes (degrees)
  REAL(KIND=JPRB) ,POINTER :: PSIN(:)  =>NULL()  ! List of NLAT  SIN(Latitudes)
  REAL(KIND=JPRB) ,POINTER :: PLON(:)  =>NULL()  ! List of NLON Longitudes (degrees)
END TYPE REGLATLON_FIELD

END MODULE REGLATLON_FIELD_MIX
