! (C) Copyright 2017-2020 UCAR.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_localization_mod_c

use iso_c_binding
use ucldas_geom_mod, only: ucldas_geom
use ucldas_geom_mod_c, only: ucldas_geom_registry
use ucldas_fields_mod, only: ucldas_field
use ucldas_fields_mod_c, only: ucldas_fields_registry
use ucldas_covariance_mod, only: ucldas_cov
use ucldas_covariance_mod_c, only: ucldas_cov_registry

implicit none

private

!> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Linked list implementation
#include "oops/util/linkedList_c.f"

! ------------------------------------------------------------------------------
subroutine ucldas_localization_randomize(c_key_conf, c_key_xincr) &
  bind(c,name='ucldas_localization_randomize_f90')

  integer(c_int), intent(in) :: c_key_conf
  integer(c_int), intent(in) :: c_key_xincr

  type(ucldas_cov),   pointer :: conf   !< Config structure
  type(ucldas_field), pointer :: xincr

  call ucldas_cov_registry%get(c_key_conf,conf)
  call ucldas_field_registry%get(c_key_xincr,xincr)
  call abor1_ftn("localization: not implemented")

end subroutine ucldas_localization_randomize

! ------------------------------------------------------------------------------
subroutine ucldas_localization_mult(c_key_conf, c_key_xincr) &
  bind(c,name='ucldas_localization_mult_f90')

  integer(c_int), intent(in) :: c_key_conf
  integer(c_int), intent(in) :: c_key_xincr

  type(ucldas_cov),   pointer :: conf   !< Config structure
  type(ucldas_field), pointer :: xincr

  call ucldas_cov_registry%get(c_key_conf,conf)
  call ucldas_field_registry%get(c_key_xincr,xincr)
  call abor1_ftn("localization: not implemented")

end subroutine ucldas_localization_mult

! ------------------------------------------------------------------------------
subroutine ucldas_localization_setup(c_key_conf, c_model, c_key_geom) &
  bind(c,name='ucldas_localization_setup_f90')

  integer(c_int), intent(inout) :: c_key_conf
  type(c_ptr),    intent(in)    :: c_model    !< The configuration
  integer(c_int), intent(in)    :: c_key_geom !< Geometry

  type(ucldas_cov),  pointer :: conf !< covar structure
  type(ucldas_geom), pointer :: geom !< Geometry

  call abor1_ftn("localization: not implemented")

  call ucldas_cov_registry%init()
  call ucldas_cov_registry%add(c_key_conf)
  call ucldas_cov_registry%get(c_key_conf, conf)
  call ucldas_geom_registry%get(c_key_geom, geom)

  return
end subroutine ucldas_localization_setup

! ------------------------------------------------------------------------------
subroutine ucldas_localization_delete(c_key_self) &
  bind(c,name='ucldas_localization_delete_f90')

  integer(c_int), intent(inout) :: c_key_self

  type(ucldas_cov), pointer :: self

  call abor1_ftn("localization: not implemented")
  call ucldas_cov_registry%get(c_key_self, self)

end subroutine ucldas_localization_delete

end module ucldas_localization_mod_c
