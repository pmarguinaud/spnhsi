MODULE YOEAERC

!$ACDC methods

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

! ------------------------------------------------------------------
! Structure for storing aerosol climatology
TYPE :: TEAERC
  ! --- 2D climatology ---
  ! All 12 months are loaded at setup
  ! Vertically integrated mass (kg/m2) dimensioned lon,lat,mon,species
  REAL(KIND=JPRB), ALLOCATABLE :: COLUMN_MASS_MON(:,:,:,:)
  ! Aerosol scale height (m), dimensioned month,species
  REAL(KIND=JPRB), ALLOCATABLE :: SCALE_HEIGHT_MON(:,:)
  ! Vertically integrated mass for the current timestep, dimensioned
  ! lon,lat,species
  REAL(KIND=JPRB), ALLOCATABLE :: COLUMN_MASS(:,:,:)
  ! Cumulative mass fraction at half levels, i.e. the fraction of the
  ! column mass between top-of-atmosphere and a particular half level,
  ! dimensioned lev_model,species
  REAL(KIND=JPRB), ALLOCATABLE :: CUM_FRACTION(:,:)
  ! Keep track of aerosol type (e.g. DU, SU) because scale height is
  ! determined by this
  CHARACTER(2), ALLOCATABLE :: AER_TYPE(:)

  ! --- 4D climatology ---
  ! Only two months are ever held in memory
  ! Aerosol mass mixing ratio (kg/kg) dimensioned lon,lat,lev,species
  REAL(KIND=JPRB), ALLOCATABLE, DIMENSION(:,:,:,:) :: MMR_MON1
  REAL(KIND=JPRB), ALLOCATABLE, DIMENSION(:,:,:,:) :: MMR_MON2
  ! Corresponding pressure, dimensioned lon,lat,lev
  REAL(KIND=JPRB), POINTER, DIMENSION(:,:,:) :: PRESSURE_MON1
  REAL(KIND=JPRB), POINTER, DIMENSION(:,:,:) :: PRESSURE_MON2
  ! Aerosol mass mixing ratio (kg/kg) for the current timestep,
  ! dimensioned lon,lat,lev,species
  REAL(KIND=JPRB), ALLOCATABLE, DIMENSION(:,:,:,:) :: MMR
  ! Corresponding pressure, dimensioned lon,lat,lev
  REAL(KIND=JPRB), ALLOCATABLE, DIMENSION(:,:,:)   :: PRESSURE
  ! Some variables have long-term variability, represented by "epochs"
  ! (actually integer years)
  INTEGER(KIND=JPIM), ALLOCATABLE :: EPOCH(:)
  ! Year and month of the currently stored months, noting that the
  ! monthly values correspond ot the 15th of each month
  INTEGER(KIND=JPIM) :: YEAR1, YEAR2, MON1, MON2
  CHARACTER(LEN=512) :: FILE_NAME ! Save for future calls
  CHARACTER(LEN=128), ALLOCATABLE :: VAR_NAME(:)
  LOGICAL, ALLOCATABLE :: IS_SPECIES_4D(:) ! 4D or 3D
  ! 4D climatology also uses MMR and PRESSURE

  ! --- General ---
  ! Index to output aerosol mass-mixing-ratio array of each
  ! climatological species (in case we need to leave gaps for
  ! prognostic and parametric species)
  INTEGER(KIND=JPIM), ALLOCATABLE :: ISPECIES_INDEX(:)

  ! Number of latitude, longitude, levels, months and aerosol species
  INTEGER(KIND=JPIM) :: NLAT = 0, NLON = 0, NLEV = 0, NMON = 0, NAER = 0

  ! Initial latitude and longitude of look-up table, and increments,
  ! all in radians
  REAL(KIND=JPRB) :: LAT1 = 0.0_JPRB, LON1 = 0.0_JPRB
  REAL(KIND=JPRB) :: DLAT = 0.0_JPRB, DLON = 0.0_JPRB

  LOGICAL :: IS_INITIALISED = .FALSE.
  INTEGER(KIND=JPIM) :: DIMENSIONALITY = 2

END TYPE TEAERC

END MODULE YOEAERC

