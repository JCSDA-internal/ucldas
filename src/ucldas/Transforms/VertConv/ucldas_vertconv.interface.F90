! (C) Copyright 2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_vertconv_mod_c

use iso_c_binding
use fckit_configuration_module, only: fckit_configuration
use kinds, only: kind_real
use ucldas_geom_mod
use ucldas_geom_mod_c
use ucldas_increment_mod
use ucldas_increment_reg
use ucldas_state_mod
use ucldas_state_reg
use ucldas_vertconv_mod, only: ucldas_vertconv, ucldas_conv_setup, &
                             ucldas_conv, ucldas_conv_ad

implicit none

private
public :: ucldas_vertconv_registry

#define LISTED_TYPE ucldas_vertconv

!> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

!> Global registry
type(registry_t) :: ucldas_vertconv_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Linked list implementation
#include "oops/util/linkedList_c.f"

! ------------------------------------------------------------------------------
!> Constructor for Vertconv
subroutine c_ucldas_vertconv_setup(c_key_self, c_conf, c_key_bkg, c_key_geom) &
  bind(c,name='ucldas_vertconv_setup_f90')

  integer(c_int), intent(inout) :: c_key_self   !< The Vertconv structure
  type(c_ptr),       intent(in) :: c_conf       !< The configuration
  integer(c_int),    intent(in) :: c_key_bkg    !< background
  integer(c_int),    intent(in) :: c_key_geom   !< geometry

  type(ucldas_vertconv), pointer :: self
  type(ucldas_state), pointer :: bkg
  type(ucldas_geom), pointer :: geom

  call ucldas_vertconv_registry%init()
  call ucldas_vertconv_registry%add(c_key_self)
  call ucldas_vertconv_registry%get(c_key_self, self)
  call ucldas_state_registry%get(c_key_bkg, bkg)
  call ucldas_geom_registry%get(c_key_geom, geom)

  call ucldas_conv_setup (self, bkg, geom, fckit_configuration(c_conf))

end subroutine c_ucldas_vertconv_setup

! ------------------------------------------------------------------------------
!> Destructor for Vertconv
subroutine c_ucldas_vertconv_delete(c_key_self) bind(c,name='ucldas_vertconv_delete_f90')

  integer(c_int), intent(inout) :: c_key_self  !< The background covariance structure

  type(ucldas_vertconv), pointer :: self

  ! Deallocate background
  ! TODO
  ! Deallocate ocean depth array
  ! TODO

  call ucldas_vertconv_registry%get(c_key_self, self)

  if (associated(self%bkg)) nullify(self%bkg)

  call ucldas_vertconv_registry%remove(c_key_self)

end subroutine c_ucldas_vertconv_delete

! ------------------------------------------------------------------------------
!> Multiplication
subroutine c_ucldas_vertconv_mult_f90(c_key_a, c_key_m, c_key_self)&
  bind(c,name='ucldas_vertconv_mult_f90')

  integer(c_int), intent(in) :: c_key_a     !< Increment in
  integer(c_int), intent(in) :: c_key_m     !< Increment out
  integer(c_int), intent(in) :: c_key_self  !< config

  type(ucldas_increment), pointer :: dxa  ! in
  type(ucldas_increment), pointer :: dxm  ! out
  type(ucldas_vertconv),  pointer :: self

  call ucldas_increment_registry%get(c_key_a, dxa)
  call ucldas_increment_registry%get(c_key_m, dxm)
  call ucldas_vertconv_registry%get(c_key_self, self)

  !< Computes dxm = Vertconv dxa

  ! dxm = dxa
  call dxm%copy( dxa)

  ! Apply forward convolution operator to T & S
  call ucldas_conv(self, dxm, dxa)

end subroutine c_ucldas_vertconv_mult_f90

! ------------------------------------------------------------------------------
!> Multiplication adjoint
subroutine c_ucldas_vertconv_multad_f90(c_key_m, c_key_a, c_key_self)&
  bind(c,name='ucldas_vertconv_multad_f90')

  integer(c_int), intent(in) :: c_key_a     !< Increment out
  integer(c_int), intent(in) :: c_key_m     !< Increment in
  integer(c_int), intent(in) :: c_key_self  !< config

  type(ucldas_increment),   pointer :: dxa
  type(ucldas_increment),   pointer :: dxm
  type(ucldas_vertconv),    pointer :: self

  call ucldas_increment_registry%get(c_key_a,dxa)
  call ucldas_increment_registry%get(c_key_m,dxm)
  call ucldas_vertconv_registry%get(c_key_self, self)

  ! dxa = dxm
  call dxa%copy(dxm)

  ! Apply adjoint of convolution operator
  call ucldas_conv_ad(self, dxm, dxa)

end subroutine c_ucldas_vertconv_multad_f90

end module ucldas_vertconv_mod_c
