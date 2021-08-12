! (C) Copyright 2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Setup the model

module ucldas_model_mod_c

use iso_c_binding
use fckit_configuration_module, only: fckit_configuration
use datetime_mod, only: datetime, c_f_datetime
use duration_mod, only: duration, duration_seconds, assignment(=)
use ucldas_geom_mod, only: ucldas_geom
use ucldas_geom_mod_c, only: ucldas_geom_registry
use ucldas_state_mod
use ucldas_state_reg
use ucldas_model_mod, only: ucldas_model, ucldas_setup, ucldas_delete, ucldas_propagate, &
                          ucldas_initialize_integration, ucldas_finalize_integration

implicit none

private
public :: ucldas_model_registry

#define LISTED_TYPE ucldas_model

!> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

!> Global registry
type(registry_t) :: ucldas_model_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Linked list implementation
#include "oops/util/linkedList_c.f"

subroutine c_ucldas_setup(c_conf, c_key_geom, c_key_model) bind (c,name='ucldas_setup_f90')

  type(c_ptr),       intent(in) :: c_conf       !< pointer to object of class Config
  integer(c_int),    intent(in) :: c_key_geom   !< Geometry
  integer(c_int), intent(inout) :: c_key_model  !< Key to configuration data

  type(ucldas_model), pointer :: model
  type(ucldas_geom),  pointer :: geom

  type(duration) :: dtstep
  real(c_double), allocatable :: tocn_minmax(:), socn_minmax(:)
  type(fckit_configuration) :: f_conf
  character(len=:), allocatable :: str

  f_conf = fckit_configuration(c_conf)

  call ucldas_geom_registry%get(c_key_geom, geom)
  call ucldas_model_registry%init()
  call ucldas_model_registry%add(c_key_model)
  call ucldas_model_registry%get(c_key_model, model)

  ! Setup time step
  call f_conf%get_or_die("tstep", str)
  dtstep = trim(str)
  model%dt0 = duration_seconds(dtstep)

  ! Setup ucland advance or identity model
  call f_conf%get_or_die("advance_ucland", model%advance_ucland)

  ! Setup defaults for clamping values in the model
  if ( f_conf%has("tocn_minmax") ) then
    call f_conf%get_or_die("tocn_minmax", tocn_minmax)
    model%tocn_minmax = tocn_minmax
  else
    model%tocn_minmax=(/-999., -999./)
  endif
  if ( f_conf%has("socn_minmax") ) then
    call f_conf%get_or_die("socn_minmax", socn_minmax)
    model%socn_minmax = socn_minmax
  else
    model%socn_minmax=(/-999., -999./)
  endif

  ! Initialize ucland
  call ucldas_setup(model, geom)

  if (allocated(str)) deallocate(str)

  return
end subroutine c_ucldas_setup

! ------------------------------------------------------------------------------

!> Delete the model
subroutine c_ucldas_delete(c_key_conf) bind (c,name='ucldas_delete_f90')

  integer(c_int), intent(inout) :: c_key_conf  !< Key to configuration structure
  type(ucldas_model), pointer :: model

  call ucldas_model_registry%get(c_key_conf, model)
  call ucldas_delete(model)
  call ucldas_model_registry%remove(c_key_conf)

  return
end subroutine c_ucldas_delete

! ------------------------------------------------------------------------------
!> Prepare the model or integration
subroutine c_ucldas_initialize_integration(c_key_model, c_key_state) &
     & bind(c,name='ucldas_initialize_integration_f90')

  integer(c_int), intent(in) :: c_key_model  !< Configuration structure
  integer(c_int), intent(in) :: c_key_state  !< Model fields

  type(ucldas_model), pointer :: model
  type(ucldas_state),pointer :: flds

  call ucldas_state_registry%get(c_key_state, flds)
  call ucldas_model_registry%get(c_key_model, model)

  call ucldas_initialize_integration(model, flds)

  return
end subroutine c_ucldas_initialize_integration

! ------------------------------------------------------------------------------

!> Checkpoint model
subroutine c_ucldas_finalize_integration(c_key_model, c_key_state) &
           bind(c,name='ucldas_finalize_integration_f90')

  integer(c_int), intent(in) :: c_key_model  !< Configuration structure
  integer(c_int), intent(in) :: c_key_state  !< Model fields

  type(ucldas_model), pointer :: model
  type(ucldas_state),pointer :: flds

  call ucldas_state_registry%get(c_key_state, flds)
  call ucldas_model_registry%get(c_key_model, model)

  call ucldas_finalize_integration(model, flds)

  return
end subroutine c_ucldas_finalize_integration

! ------------------------------------------------------------------------------

!> Perform a timestep of the model
subroutine c_ucldas_propagate(c_key_model, c_key_state, c_key_date) bind(c,name='ucldas_propagate_f90')

  integer(c_int), intent(in) :: c_key_model  !< Config structure
  integer(c_int), intent(in) :: c_key_state  !< Model fields
  type(c_ptr), intent(inout) :: c_key_date   !< DateTime

  type(ucldas_model), pointer :: model
  type(ucldas_state),pointer :: flds
  type(datetime)            :: fldsdate

  call ucldas_model_registry%get(c_key_model, model)
  call ucldas_state_registry%get(c_key_state, flds)
  call c_f_datetime(c_key_date, fldsdate)

  call ucldas_propagate(model, flds, fldsdate)

  return
end subroutine c_ucldas_propagate

end module ucldas_model_mod_c
