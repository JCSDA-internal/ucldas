! (C) Copyright 2017-2020 UCAR.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module c_ucldas_horizfilt_mod
  use iso_c_binding
  use fckit_configuration_module, only: fckit_configuration
  use ucldas_horizfilt_mod
  use ucldas_geom_mod_c, only: ucldas_geom_registry
  use ucldas_geom_mod, only : ucldas_geom
  use ucldas_increment_mod
  use ucldas_increment_reg
  use ucldas_state_mod
  use ucldas_state_reg
  use oops_variables_mod

  implicit none

  private
  public :: ucldas_horizfilt_registry

#define LISTED_TYPE ucldas_horizfilt_type

  !> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

  !> Global registry
 type(registry_t) :: ucldas_horizfilt_registry

  ! ------------------------------------------------------------------------------
contains
  ! ------------------------------------------------------------------------------
  !> Linked list implementation
#include "oops/util/linkedList_c.f"
  ! ------------------------------------------------------------------------------

  ! ------------------------------------------------------------------------------
  !> Setup for the filtering operator

  subroutine c_ucldas_horizfilt_setup(c_key_self, &
                                    c_conf, &
                                    c_key_geom, &
                                    c_key_traj, &
                                    c_vars) &
          & bind (c,name='ucldas_horizfilt_setup_f90')
    integer(c_int), intent(inout) :: c_key_self   !< The filtering structure
    type(c_ptr),       intent(in) :: c_conf       !< The configuration
    integer(c_int),    intent(in) :: c_key_geom   !< Geometry
    integer(c_int),    intent(in) :: c_key_traj   !< Trajectory
    type(c_ptr),value, intent(in) :: c_vars       !< List of variables

    type(ucldas_horizfilt_type), pointer :: self
    type(ucldas_geom),           pointer :: geom
    type(ucldas_state),          pointer :: traj
    type(oops_variables)               :: vars

    call ucldas_geom_registry%get(c_key_geom, geom)
    call ucldas_state_registry%get(c_key_traj, traj)
    call ucldas_horizfilt_registry%init()
    call ucldas_horizfilt_registry%add(c_key_self)
    call ucldas_horizfilt_registry%get(c_key_self, self)
    vars = oops_variables(c_vars)
    call ucldas_horizfilt_setup(self, fckit_configuration(c_conf), geom, traj, vars)

  end subroutine c_ucldas_horizfilt_setup

  ! ------------------------------------------------------------------------------
  !> Delete filtering operator

  subroutine c_ucldas_horizfilt_delete(c_key_self) bind (c,name='ucldas_horizfilt_delete_f90')
    integer(c_int), intent(inout) :: c_key_self  !< The filtering structure

    type(ucldas_horizfilt_type),       pointer :: self

    call ucldas_horizfilt_registry%get(c_key_self,self)
    call ucldas_horizfilt_delete(self)
    call ucldas_horizfilt_registry%remove(c_key_self)

  end subroutine c_ucldas_horizfilt_delete

  ! ------------------------------------------------------------------------------
  !> Multiply

  subroutine c_ucldas_horizfilt_mult(c_key_self, c_key_in, c_key_out, c_key_geom) bind(c,name='ucldas_horizfilt_mult_f90')
    integer(c_int), intent(inout) :: c_key_self  !< The filtering structure
    integer(c_int), intent(in)    :: c_key_in    !<    "   to Increment in
    integer(c_int), intent(in)    :: c_key_out   !<    "   to Increment out
    integer(c_int), intent(in)    :: c_key_geom  !< Geometry

    type(ucldas_horizfilt_type), pointer :: self
    type(ucldas_increment),      pointer :: xin
    type(ucldas_increment),      pointer :: xout
    type(ucldas_geom),           pointer :: geom

    call ucldas_geom_registry%get(c_key_geom, geom)
    call ucldas_horizfilt_registry%get(c_key_self, self)
    call ucldas_increment_registry%get(c_key_in, xin)
    call ucldas_increment_registry%get(c_key_out, xout)

    call ucldas_horizfilt_mult(self, xin, xout, geom) !< xout = C.xout

  end subroutine c_ucldas_horizfilt_mult

  ! ------------------------------------------------------------------------------
  !> Multiply adjoint

  subroutine c_ucldas_horizfilt_mult_ad(c_key_self, c_key_in, c_key_out, c_key_geom) &
       bind(c,name='ucldas_horizfilt_multad_f90')
    integer(c_int), intent(inout) :: c_key_self  !< The filtering structure
    integer(c_int), intent(in)    :: c_key_in    !<    "   to Increment in
    integer(c_int), intent(in)    :: c_key_out   !<    "   to Increment out
    integer(c_int), intent(in)    :: c_key_geom  !< Geometry

    type(ucldas_horizfilt_type), pointer :: self
    type(ucldas_increment),      pointer :: xin
    type(ucldas_increment),      pointer :: xout
    type(ucldas_geom),           pointer :: geom

    call ucldas_geom_registry%get(c_key_geom, geom)
    call ucldas_horizfilt_registry%get(c_key_self, self)
    call ucldas_increment_registry%get(c_key_in, xin)
    call ucldas_increment_registry%get(c_key_out, xout)

    call ucldas_horizfilt_multad(self, xin, xout, geom) !< xout = C^T.xout

  end subroutine c_ucldas_horizfilt_mult_ad

end module c_ucldas_horizfilt_mod
