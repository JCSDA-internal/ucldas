!> Provides a few physical constants
module LND_constants

! This file is part of UCLAND. See LICENSE.md for the license.

use constants_mod, only : HLV, HLF

implicit none ; private

!> The constant offset for converting temperatures in Kelvin to Celsius
real, public, parameter :: CELSIUS_KELVIN_OFFSET = 273.15
public :: HLV, HLF

end module LND_constants
