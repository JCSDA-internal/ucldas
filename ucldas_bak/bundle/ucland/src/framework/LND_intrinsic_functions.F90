!> A module with intrinsic functions that are used by LND but are not supported
!!  by some compilers.
module LND_intrinsic_functions

! This file is part of UCLAND. See LICENSE.md for the license.

implicit none ; private

public :: invcosh

contains

!> Evaluate the inverse cosh, either using a math library or an
!! equivalent expression
function invcosh(x)
  real, intent(in) :: x !< The argument of the inverse of cosh.  NaNs will
                        !! occur if x<1, but there is no error checking
  real :: invcosh

#ifdef __INTEL_COMPILER
  invcosh = acosh(x)
#else
  invcosh = log(x+sqrt(x*x-1))
#endif

end function invcosh

end module LND_intrinsic_functions
