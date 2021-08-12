! (C) Copyright 2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_balance_mod_c

use iso_c_binding
use fckit_configuration_module, only: fckit_configuration
use ucldas_geom_mod
use ucldas_geom_mod_c
use ucldas_state_mod
use ucldas_state_reg
use ucldas_increment_mod
use ucldas_increment_reg
use ucldas_balance_mod, only: ucldas_balance_config, &
                            ucldas_balance_setup, ucldas_balance_delete, &
                            ucldas_balance_mult, ucldas_balance_multad, &
                            ucldas_balance_multinv, ucldas_balance_multinvad

implicit none

private
public :: ucldas_balance_registry

#define LISTED_TYPE ucldas_balance_config

!> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

!> Global registry
type(registry_t) :: ucldas_balance_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Linked list implementation
#include "oops/util/linkedList_c.f"

! ------------------------------------------------------------------------------
!> Constructor for D (standard deviation of background error)
subroutine c_ucldas_balance_setup(c_key_self, c_conf, c_key_traj, c_key_geom) &
  bind(c,name='ucldas_balance_setup_f90')

  integer(c_int), intent(inout) :: c_key_self   !< The D structure
  type(c_ptr),       intent(in) :: c_conf       !< The configuration
  integer(c_int), intent(in)    :: c_key_traj   !< Background field
  integer(c_int), intent(in)    :: c_key_geom   !< Geometry

  type(ucldas_state), pointer :: traj
  type(ucldas_balance_config), pointer :: self
  type(ucldas_geom), pointer :: geom

  call ucldas_balance_registry%init()
  call ucldas_balance_registry%add(c_key_self)
  call ucldas_balance_registry%get(c_key_self, self)
  call ucldas_state_registry%get(c_key_traj, traj)
  call ucldas_geom_registry%get(c_key_geom, geom)

  call ucldas_balance_setup(fckit_configuration(c_conf), self, traj, geom)

end subroutine c_ucldas_balance_setup

! ------------------------------------------------------------------------------
!> Destructor for D
subroutine c_ucldas_balance_delete(c_key_self) bind(c,name='ucldas_balance_delete_f90')

  integer(c_int), intent(inout) :: c_key_self

  type(ucldas_balance_config), pointer :: self

  call ucldas_balance_registry%get(c_key_self,self)
  call ucldas_balance_delete(self)
  call ucldas_balance_registry%remove(c_key_self)

end subroutine c_ucldas_balance_delete

! ------------------------------------------------------------------------------
!> Multiplication forward
subroutine c_ucldas_balance_mult_f90(c_key_self, c_key_a, c_key_m)&
  bind(c,name='ucldas_balance_mult_f90')

  integer(c_int), intent(in) :: c_key_a     !<    "   to Increment in
  integer(c_int), intent(in) :: c_key_m     !<    "   to Increment out
  integer(c_int), intent(in) :: c_key_self

  type(ucldas_increment), pointer :: dxa
  type(ucldas_increment), pointer :: dxm
  type(ucldas_balance_config), pointer :: self

  call ucldas_increment_registry%get(c_key_a,dxa)
  call ucldas_increment_registry%get(c_key_m,dxm)
  call ucldas_balance_registry%get(c_key_self,self)

  !< Computes dxm = K dxa
  call ucldas_balance_mult(self, dxa, dxm)

end subroutine c_ucldas_balance_mult_f90

! ------------------------------------------------------------------------------
!> Multiplication inverse
subroutine c_ucldas_balance_multinv_f90(c_key_self, c_key_m, c_key_a)&
  bind(c,name='ucldas_balance_multinv_f90')

  integer(c_int), intent(in) :: c_key_a     !<    "   to Increment in
  integer(c_int), intent(in) :: c_key_m     !<    "   to Increment out
  integer(c_int), intent(in) :: c_key_self

  type(ucldas_increment), pointer :: dxa
  type(ucldas_increment), pointer :: dxm
  type(ucldas_balance_config), pointer :: self

  call ucldas_increment_registry%get(c_key_a,dxa)
  call ucldas_increment_registry%get(c_key_m,dxm)
  call ucldas_balance_registry%get(c_key_self,self)

  !< Computes dxa = K^-1 dxm
  call ucldas_balance_multinv(self, dxa, dxm)

end subroutine c_ucldas_balance_multinv_f90

! ------------------------------------------------------------------------------
!> Multiplication adjoint
subroutine c_ucldas_balance_multad_f90(c_key_self, c_key_m, c_key_a)&
  bind(c,name='ucldas_balance_multad_f90')

  integer(c_int), intent(in) :: c_key_a     !<    "   to Increment in
  integer(c_int), intent(in) :: c_key_m     !<    "   to Increment out
  integer(c_int), intent(in) :: c_key_self

  type(ucldas_increment), pointer :: dxa
  type(ucldas_increment), pointer :: dxm
  type(ucldas_balance_config), pointer :: self

  call ucldas_increment_registry%get(c_key_a,dxa)
  call ucldas_increment_registry%get(c_key_m,dxm)
  call ucldas_balance_registry%get(c_key_self,self)

  !< Computes dxa = K^T dxm
  call ucldas_balance_multad(self, dxa, dxm)

end subroutine c_ucldas_balance_multad_f90

! ------------------------------------------------------------------------------
!> Multiplication inverse adjoint
subroutine c_ucldas_balance_multinvad_f90(c_key_self, c_key_a, c_key_m)&
  bind(c,name='ucldas_balance_multinvad_f90')

  integer(c_int), intent(in) :: c_key_a     !<    "   to Increment in
  integer(c_int), intent(in) :: c_key_m     !<    "   to Increment out
  integer(c_int), intent(in) :: c_key_self

  type(ucldas_increment), pointer :: dxa
  type(ucldas_increment), pointer :: dxm
  type(ucldas_balance_config), pointer :: self

  call ucldas_increment_registry%get(c_key_a,dxa)
  call ucldas_increment_registry%get(c_key_m,dxm)
  call ucldas_balance_registry%get(c_key_self,self)

  !< Computes dxm = (K^-1)^T dxa
  call ucldas_balance_multinvad(self, dxa, dxm)

end subroutine c_ucldas_balance_multinvad_f90

end module ucldas_balance_mod_c
