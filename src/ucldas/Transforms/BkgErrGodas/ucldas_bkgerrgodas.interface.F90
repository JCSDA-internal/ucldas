! (C) Copyright 2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_bkgerrgodas_mod_c

use iso_c_binding
use fckit_configuration_module, only: fckit_configuration
use ucldas_geom_mod
use ucldas_geom_mod_c
use ucldas_state_mod
use ucldas_state_reg
use ucldas_increment_mod
use ucldas_increment_reg
use ucldas_bkgerrgodas_mod, only: ucldas_bkgerrgodas_config, &
                                ucldas_bkgerrgodas_setup, ucldas_bkgerrgodas_mult

implicit none

private
public :: ucldas_bkgerrgodas_registry

#define LISTED_TYPE ucldas_bkgerrgodas_config

!> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

!> Global registry
type(registry_t) :: ucldas_bkgerrgodas_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Linked list implementation
#include "oops/util/linkedList_c.f"

! ------------------------------------------------------------------------------
!> Constructor for D (standard deviation of background error)
subroutine c_ucldas_bkgerrgodas_setup(c_key_self, c_conf, c_key_bkg, c_key_geom) &
  bind(c,name='ucldas_bkgerrgodas_setup_f90')

  integer(c_int), intent(inout) :: c_key_self   !< The D structure
  type(c_ptr),       intent(in) :: c_conf       !< The configuration
  integer(c_int), intent(in)    :: c_key_bkg    !< Background field
  integer(c_int), intent(in)    :: c_key_geom    !< Geometry

  type(ucldas_state), pointer :: bkg
  type(ucldas_geom), pointer :: geom
  type(ucldas_bkgerrgodas_config), pointer :: self

  call ucldas_bkgerrgodas_registry%init()
  call ucldas_bkgerrgodas_registry%add(c_key_self)
  call ucldas_bkgerrgodas_registry%get(c_key_self, self)
  call ucldas_state_registry%get(c_key_bkg, bkg)
  call ucldas_geom_registry%get(c_key_geom, geom)

  call ucldas_bkgerrgodas_setup(fckit_configuration(c_conf), self, bkg, geom)

end subroutine c_ucldas_bkgerrgodas_setup

! ------------------------------------------------------------------------------
!> Destructor for D
subroutine c_ucldas_bkgerrgodas_delete(c_key_self) bind(c,name='ucldas_bkgerrgodas_delete_f90')

  integer(c_int), intent(inout) :: c_key_self
  type(ucldas_bkgerrgodas_config), pointer :: self

  call ucldas_bkgerrgodas_registry%get(c_key_self, self)
  if (associated(self%bkg)) nullify(self%bkg)
  call self%std_bkgerr%delete()

  call ucldas_bkgerrgodas_registry%remove(c_key_self)

end subroutine c_ucldas_bkgerrgodas_delete

! ------------------------------------------------------------------------------
!> Multiplication forward and adjoint
subroutine c_ucldas_bkgerrgodas_mult_f90(c_key_self, c_key_a, c_key_m)&
  bind(c,name='ucldas_bkgerrgodas_mult_f90')

  integer(c_int), intent(in) :: c_key_a     !<    "   to Increment in
  integer(c_int), intent(in) :: c_key_m     !<    "   to Increment out
  integer(c_int), intent(in) :: c_key_self

  type(ucldas_increment), pointer :: dxa
  type(ucldas_increment), pointer :: dxm
  type(ucldas_bkgerrgodas_config), pointer :: self

  call ucldas_increment_registry%get(c_key_a,dxa)
  call ucldas_increment_registry%get(c_key_m,dxm)
  call ucldas_bkgerrgodas_registry%get(c_key_self,self)

  !< Computes dxm = D dxa
  call dxm%copy(dxa)
  call ucldas_bkgerrgodas_mult(self, dxa, dxm)

end subroutine c_ucldas_bkgerrgodas_mult_f90

end module ucldas_bkgerrgodas_mod_c
