!> Provides a template for users to code updating the forcing fluxes.
module user_revise_forcing

! This file is part of UCLAND. See LICENSE.md for the license.

use LND_domains, only : pass_var, pass_vector, AGRID
use LND_error_handler, only : LND_error, FATAL, WARNING, is_root_pe
use LND_file_parser, only : get_param, log_version, param_file_type
use LND_forcing_type, only : forcing
use LND_grid, only : ocean_grid_type
use LND_io, only : file_exists, read_data
use LND_restart, only : register_restart_field, LND_restart_CS
use LND_time_manager, only : time_type, operator(+), operator(/)
use LND_tracer_flow_control, only : call_tracer_set_forcing
use LND_tracer_flow_control, only : tracer_flow_control_CS
use LND_variables, only : surface

implicit none ; private

public user_alter_forcing, user_revise_forcing_init

!> Control structure for user_revise_forcing
type, public :: user_revise_forcing_CS ; private
  real :: cdrag  !< The quadratic bottom drag coefficient.
end type user_revise_forcing_CS

! This include declares and sets the variable "version".
#include "version_variable.h"
  character(len=40) :: mdl = "user_revise_forcing" !< This module's name.
contains

!> This subroutine sets the surface wind stresses.
subroutine user_alter_forcing(sfc_state, fluxes, day, G, CS)
  type(surface),            intent(in)    :: sfc_state  !< A structure containing fields that
                                                    !! describe the surface state of the ocean.
  type(forcing),            intent(inout) :: fluxes !< A structure containing pointers to any
                                                    !! possible forcing fields. Unused fields
                                                    !! have NULL ptrs.
  type(time_type),          intent(in)    :: day    !< Time of the fluxes.
  type(ocean_grid_type),    intent(in)    :: G      !< The ocean's grid structure.
  type(user_revise_forcing_CS), pointer   :: CS     !< A pointer to the control structure
                                                    !! returned by a previous call to
                                                    !! surface_forcing_init.

end subroutine user_alter_forcing

!> Initialize the user_revise_forcing control structure
subroutine user_revise_forcing_init(param_file,CS)
  type(param_file_type), intent(in) :: param_file   !< A structure indicating the open file to
                                                    !! parse for model parameter values.
  type(user_revise_forcing_CS), pointer   :: CS     !< A pointer to the control structure
                                                    !! returned by a previous call to
                                                    !! surface_forcing_init.

  call log_version(param_file, mdl, version)

end subroutine user_revise_forcing_init

end module user_revise_forcing
