MODULE TYPE_SPGEOM

!$ACDC methods

! Module for spectral geometry structures.

USE PARKIND1 , ONLY : JPIM, JPRB

!     ------------------------------------------------------------------

IMPLICIT NONE
PRIVATE

SAVE

! GMR     : coefficients for spectral multiplication by GM.
! SCGMAP  : coefficients for multiplication by (GM**2) in spectral space (global model).
! ESCGMAP : coefficients for multiplication by (GM**2) in spectral space (LAM model).

TYPE, PUBLIC :: TSPGEOM
REAL(KIND=JPRB),ALLOCATABLE :: GMR(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: SCGMAP(:,:)
REAL(KIND=JPRB) :: ESCGMAP(3)
END TYPE TSPGEOM

END MODULE TYPE_SPGEOM
