MODULE YOMCVER

!$ACDC methods

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK    ,DR_HOOK,  JPHOOK

IMPLICIT NONE

SAVE

! =============================================================================

TYPE TCVER
! ------ Vertical discretisation --------------------------------------------

! NDLNPR  : NDLNPR=0: conventional formulation of delta, i.e. ln(P(l)/P(l-1)).
!           NDLNPR=1: formulation of delta used in non hydrostatic model,
!                     i.e. (P(l)-P(l-1))/SQRT(P(l)*P(l-1)).
! RHYDR0  : value given to "alpha(1) = depth of log(Pi) between top and full level nr 1"
!           in case where general formula to compute "alpha" gives an infinite value
!           (used only if LVERTFE=F, NDLNPR=0).
!           This quantity is never used in the following cases:
!            LVERTFE=T.
!            LVERTFE=F with NDLNPR=1.
! LAPRXPK : way of computing full-levels pressures in primitive equation
!           hydrostatic model.
!           .T.: full levels are computed by PK=(PK+1/2 + PK-1/2)*0.5
!           .F.: full levels are computed by a more complicated formula
!                consistent with "alpha" in geopotential formula.
! REXP_VRAT : exponent of the vertical function S used for LBIGW

LOGICAL :: LAPRXPK
INTEGER(KIND=JPIM) :: NDLNPR
REAL(KIND=JPRB) :: RHYDR0
REAL(KIND=JPRB) :: REXP_VRAT

! ----- vertical discretisation, vertical boundaries:
! LREGETA   : .T.: for the interlayer L, ETA(L)=L/NFLEVG
!             .F.: for the interlayer L, ETA(L)=A(L)/P0+B(L)
! LVFE_REGETA: cf. LREGETA for "eta" used in VFE operators.
LOGICAL :: LREGETA
LOGICAL :: LVFE_REGETA


! * Variables related to vertical discretisation in finite elements:

! NVFE_TYPE     : Type of spline basis used for finite element vertical discretisation.
!               (1 = linear, 3 = cubic)
! NVFE_ORDER    : Order of spline used in VFE; NVFE_ORDER=NVFE_TYPE+1
! NVFE_INTERNALS: number of internals knots

! LVERTFE       : .T./.F. Finite element/conventional vertical discretisation.
! LVFE_LAPL_BC  : VFE for boundary cond. in vert. Laplacian term (NH model)

! VFE for vertical velocity (NH model):
! LVFE_GW       : T - invertible RINTGW/RDERGW used under key LGWADV, full levels gw.
! LVFE_GW_HALF  : T - invertible RINTGW/RDERGW used under key LGWADV, half levels gw.
! LVFE_GWMPA    : T - VFE for AROME physics vertical velocity

! LVFE_CHEB     : chebyshev nodes (dense distribution of levels near BCs in eta space)
! LVFE_ECMWF    : T if original ECMWF way to compute vertical integral and derivative
! LVFE_NOBC     : T no boundary conditions applied for vert.derivative (RDERI)
!                 F boundary conditions applied for vert.derivative (RDERB)
! LPERCENTILS   : used in SUVFE_KNOT to determine method for computing knots for basis functions
! LVFE_VERBOSE  : print several diagnostics or not
! CVFE_ETAH     : half levels eta definition
!           REGETA - regular distribution
!           MCONST - general definition
!           MCNTRL - explicitly prescribed m = dpi/eta at boundaries; density control
!           CHORDAL - using A and Bs
! RVFE_ALPHA    : Exponent that constrols definition of eta. 
!                 RVFE_ALPHA =  0   -  gives regular (the same like LVFE_REGETA)
!                 RVFE_ALPHA =  1   -  gives classic sigma (eta = sigma for pressure VP00)
! RVFE_BETA     : Exponent that constrols density of levels close to boundaries.
!                 RVFE_BETA  =  0   -  there is density transformation
!                 RVFE_BETA  =  1   -  chanyshev definition (when combined with RVFE_ALPHA=0.0)
! RVFE_ALPHA/RVFE_BETA : control definition of eta close to boundaries
!           RVFE_ALPHA/BETA =  0   -  gives regular (the same like LVFE_REGETA)
!           RVFE_ALPHA/BETA >  0   -  denser close to boundaries
!           RVFE_ALPHA/BETA <  0   -  denser close to inner domain (MCNTRL only)
! RVFE_KNOT_STRETCH : stretching of knots
!
! LCOMPATIBLE       : .T. compatible VFE discretisation of vertical pressure gradient
!                     with respect to VFD discretisation of vertical divergence term 
!                     (based on piecewise constant/linear FE basic functions) 
! LVFE_LAPL_HALF    : .T. VFE discretisation of the vertical Laplacian operator 
!                     of the SI linear NH model is consistent with its nonlinear
!                     VFE discrete counterpart 
! LVFE_FD_MIX       : .T. VFD discretisation of vertical gradient of pressure 
!                     under LVFERTE and LCOMPATIBLE 
! LREFINE_NHPRE_BBC : .T. refine treatment of surface nh pressure relying on 
!                     a consistent treatment of free-slip and rigid-bottom boundary condition. 
! ----------------------------------------------------------------------

INTEGER(KIND=JPIM) :: NVFE_TYPE
INTEGER(KIND=JPIM) :: NVFE_ORDER
INTEGER(KIND=JPIM) :: NVFE_INTERNALS
LOGICAL :: LVERTFE
LOGICAL :: LVFE_LAPL_BC
LOGICAL :: LVFE_GW
LOGICAL :: LVFE_GW_HALF
LOGICAL :: LVFE_GWMPA
LOGICAL :: LVFE_CHEB
LOGICAL :: LVFE_ECMWF
LOGICAL :: LVFE_NOBC
LOGICAL :: LVFE_VERBOSE
LOGICAL :: LVFE_NORMALIZE
LOGICAL :: LPERCENTILS
LOGICAL :: LVFE_COMPATIBLE
LOGICAL :: LVFE_FD_MIX
LOGICAL :: LVFE_LAPL_HALF
LOGICAL :: LCOMPATIBLE
LOGICAL :: LREFINE_NHPRE_BBC
REAL(KIND=JPRB) :: RVFE_ALPHA
REAL(KIND=JPRB) :: RVFE_BETA
REAL(KIND=JPRB) :: RVFE_KNOT_STRETCH
REAL(KIND=JPRB) :: RFAC1, RFAC2
CHARACTER(LEN=8) :: CVFE_ETAH
END TYPE TCVER

END MODULE YOMCVER
