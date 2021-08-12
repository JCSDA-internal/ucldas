!> Invokes unit tests in all modules that have them
module LND_unit_tests

! This file is part of UCLAND. See LICENSE.md for the license.

use LND_error_handler,              only : LND_error, FATAL, is_root_pe

use LND_string_functions,           only : string_functions_unit_tests
use LND_remapping,                  only : remapping_unit_tests
use LND_neutral_diffusion,          only : neutral_diffusion_unit_tests
use LND_diag_vkernels,              only : diag_vkernels_unit_tests
use LND_random,                     only : random_unit_tests
use LND_lateral_boundary_diffusion, only : near_boundary_unit_tests

implicit none ; private

public unit_tests

contains

!> Calls unit tests for other modules.
!! Note that if a unit test returns true, a FATAL error is triggered.
subroutine unit_tests(verbosity)
  ! Arguments
  integer, intent(in) :: verbosity !< The verbosity level
  ! Local variables
  logical :: verbose

  verbose = verbosity>=5

  if (is_root_pe()) then ! The following need only be tested on 1 PE
    if (string_functions_unit_tests(verbose)) call LND_error(FATAL, &
       "LND_unit_tests: string_functions_unit_tests FAILED")
    if (remapping_unit_tests(verbose)) call LND_error(FATAL, &
       "LND_unit_tests: remapping_unit_tests FAILED")
    if (neutral_diffusion_unit_tests(verbose)) call LND_error(FATAL, &
       "LND_unit_tests: neutralDiffusionUnitTests FAILED")
    if (diag_vkernels_unit_tests(verbose)) call LND_error(FATAL, &
       "LND_unit_tests: diag_vkernels_unit_tests FAILED")
    if (random_unit_tests(verbose)) call LND_error(FATAL, &
       "LND_unit_tests: random_unit_tests FAILED")
    if (near_boundary_unit_tests(verbose)) call LND_error(FATAL, &
       "LND_unit_tests: near_boundary_unit_tests FAILED")
  endif

end subroutine unit_tests

end module LND_unit_tests
