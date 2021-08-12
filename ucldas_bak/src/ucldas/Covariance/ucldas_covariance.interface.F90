! (C) Copyright 2017-2020 UCAR.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_covariance_mod_c

use iso_c_binding
use fckit_configuration_module, only: fckit_configuration
use oops_variables_mod
use ucldas_geom_mod, only : ucldas_geom
use ucldas_geom_mod_c, only : ucldas_geom_registry
use ucldas_increment_mod
use ucldas_increment_reg
use ucldas_state_mod
use ucldas_state_reg
use ucldas_covariance_mod, only: ucldas_cov, ucldas_cov_setup, ucldas_cov_delete, &
                               ucldas_cov_C_mult, ucldas_cov_sqrt_C_mult

implicit none

private
public :: ucldas_cov_registry

#define LISTED_TYPE ucldas_cov

!> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

!> Global registry
type(registry_t) :: ucldas_cov_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "oops/util/linkedList_c.f"
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
!> Setup for the UCLDAS model's background error covariance matrix

subroutine c_ucldas_b_setup(c_key_self, c_conf, c_key_geom, c_key_bkg, c_vars) &
     & bind (c,name='ucldas_b_setup_f90')
  integer(c_int), intent(inout) :: c_key_self   !< The background covariance structure
  type(c_ptr),       intent(in) :: c_conf       !< The configuration
  integer(c_int),    intent(in) :: c_key_geom   !< Geometry
  integer(c_int),    intent(in) :: c_key_bkg    !< Background
  type(c_ptr),value, intent(in) :: c_vars       !< List of variables

  type(ucldas_cov),   pointer :: self
  type(ucldas_geom),  pointer :: geom
  type(ucldas_state), pointer :: bkg
  type(oops_variables)      :: vars

  call ucldas_geom_registry%get(c_key_geom, geom)
  call ucldas_cov_registry%init()
  call ucldas_cov_registry%add(c_key_self)
  call ucldas_cov_registry%get(c_key_self, self)
  call ucldas_state_registry%get(c_key_bkg,bkg)
  vars = oops_variables(c_vars)
  call ucldas_cov_setup(self, fckit_configuration(c_conf), geom, bkg, vars)

end subroutine c_ucldas_b_setup

! ------------------------------------------------------------------------------
!> Delete for the UCLDAS model's background error covariance matrix

subroutine c_ucldas_b_delete(c_key_self) bind (c,name='ucldas_b_delete_f90')
  integer(c_int), intent(inout) :: c_key_self  !< The background covariance structure

  type(ucldas_cov),       pointer :: self

  call ucldas_cov_registry%get(c_key_self,self)
  call ucldas_cov_delete(self)
  call ucldas_cov_registry%remove(c_key_self)

end subroutine c_ucldas_b_delete

! ------------------------------------------------------------------------------

!> Multiply by covariance

subroutine c_ucldas_b_mult(c_key_self, c_key_in, c_key_out) bind(c,name='ucldas_b_mult_f90')
  integer(c_int), intent(inout) :: c_key_self  !< The background covariance structure
  integer(c_int), intent(in)    :: c_key_in    !<    "   to Increment in
  integer(c_int), intent(in)    :: c_key_out   !<    "   to Increment out

  type(ucldas_cov),       pointer :: self
  type(ucldas_increment), pointer :: xin
  type(ucldas_increment), pointer :: xout

  call ucldas_cov_registry%get(c_key_self, self)
  call ucldas_increment_registry%get(c_key_in, xin)
  call ucldas_increment_registry%get(c_key_out, xout)

  call xout%copy(xin)              !< xout = xin
  call ucldas_cov_C_mult(self, xout) !< xout = C.xout

end subroutine c_ucldas_b_mult


! ------------------------------------------------------------------------------

!> Generate randomized C^1/2 x increment

subroutine c_ucldas_b_randomize(c_key_self, c_key_out) bind(c,name='ucldas_b_randomize_f90')
  integer(c_int), intent(in) :: c_key_self  !< covar config structure
  integer(c_int), intent(in) :: c_key_out   !< Randomized increment

  type(ucldas_cov),       pointer :: self
  type(ucldas_increment), pointer :: xout

  call ucldas_cov_registry%get(c_key_self, self)
  call ucldas_increment_registry%get(c_key_out, xout)

  ! Randomize increment
  call ucldas_cov_sqrt_C_mult(self, xout) !< xout = C^1/2.xout

end subroutine c_ucldas_b_randomize

end module ucldas_covariance_mod_c
