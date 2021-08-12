!
! (C) Copyright 2019-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_geom_iter_interface

  use iso_c_binding
  use kinds
  use ucldas_geom_iter_mod
  use ucldas_geom_mod_c,  only : ucldas_geom_registry
  use ucldas_geom_mod, only: ucldas_geom

  implicit none

  private

contains

  ! ------------------------------------------------------------------------------
  !> Setup geometry iterator
  subroutine ucldas_geom_iter_setup_c(c_key_self, c_key_geom, c_iindex, c_jindex) bind(c, name='ucldas_geom_iter_setup_f90')

    ! Passed variables
    integer(c_int), intent(inout) :: c_key_self !< Geometry iterator
    integer(c_int), intent(   in) :: c_key_geom !< Geometry
    integer(c_int), intent(   in) :: c_iindex    !< Index
    integer(c_int), intent(   in) :: c_jindex    !< Index

    ! Local variables
    type(ucldas_geom_iter),     pointer :: self
    type(ucldas_geom),          pointer :: geom

    ! Interface
    call ucldas_geom_iter_registry%init()
    call ucldas_geom_iter_registry%add(c_key_self)
    call ucldas_geom_iter_registry%get(c_key_self, self)
    call ucldas_geom_registry%get(c_key_geom, geom)

    ! Call Fortran
    call ucldas_geom_iter_setup(self, geom, c_iindex, c_jindex)

  end subroutine ucldas_geom_iter_setup_c

  ! ------------------------------------------------------------------------------
  !> Clone geometry iterator
  subroutine ucldas_geom_iter_clone_c(c_key_self, c_key_other) bind(c, name='ucldas_geom_iter_clone_f90')

    ! Passed variables
    integer(c_int), intent(inout) :: c_key_self  !< Geometry iterator
    integer(c_int), intent(   in) :: c_key_other !< Other geometry iterator

    ! Local variables
    type(ucldas_geom_iter), pointer :: self, other

    ! Interface
    call ucldas_geom_iter_registry%get(c_key_other, other)
    call ucldas_geom_iter_registry%init()
    call ucldas_geom_iter_registry%add(c_key_self)
    call ucldas_geom_iter_registry%get(c_key_self, self)

    ! Call Fortran
    call ucldas_geom_iter_clone(self, other)

  end subroutine ucldas_geom_iter_clone_c

  ! ------------------------------------------------------------------------------
  !> Delete geometry iterator
  subroutine ucldas_geom_iter_delete_c(c_key_self) bind(c, name='ucldas_geom_iter_delete_f90')

      ! Passed variables
      integer(c_int), intent(inout) :: c_key_self !< Geometry iterator

      ! Clear interface
      call ucldas_geom_iter_registry%remove(c_key_self)

  end subroutine ucldas_geom_iter_delete_c

  ! ------------------------------------------------------------------------------
  !> Check geometry iterator equality
  subroutine ucldas_geom_iter_equals_c(c_key_self, c_key_other, c_equals) bind(c, name='ucldas_geom_iter_equals_f90')

    ! Passed variables
    integer(c_int), intent(inout) :: c_key_self  !< Geometry iterator
    integer(c_int), intent(   in) :: c_key_other !< Other geometry iterator
    integer(c_int), intent(inout) :: c_equals    !< Equality flag

    ! Local variables
    type(ucldas_geom_iter),pointer :: self,other

    ! Interface
    call ucldas_geom_iter_registry%get(c_key_self, self)
    call ucldas_geom_iter_registry%get(c_key_other, other)

    ! Call Fortran
    call ucldas_geom_iter_equals(self, other, c_equals)

  end subroutine ucldas_geom_iter_equals_c

  ! ------------------------------------------------------------------------------
  !> Get geometry iterator current lat/lon
  subroutine ucldas_geom_iter_current_c(c_key_self, c_lon, c_lat) bind(c, name='ucldas_geom_iter_current_f90')

    ! Passed variables
    integer(c_int), intent(   in) :: c_key_self !< Geometry iterator
    real(c_double), intent(inout) :: c_lat      !< Latitude
    real(c_double), intent(inout) :: c_lon      !< Longitude

    ! Local variables
    type(ucldas_geom_iter), pointer :: self

    ! Interface
    call ucldas_geom_iter_registry%get(c_key_self, self)

    ! Call Fortran
    call ucldas_geom_iter_current(self, c_lon, c_lat)

  end subroutine ucldas_geom_iter_current_c

  ! ------------------------------------------------------------------------------
  !> Update geometry iterator to next point
  subroutine ucldas_geom_iter_next_c(c_key_self) bind(c, name='ucldas_geom_iter_next_f90')

    ! Passed variables
    integer(c_int), intent(in) :: c_key_self !< Geometry iterator

    ! Local variables
    type(ucldas_geom_iter), pointer :: self

    ! Interface
    call ucldas_geom_iter_registry%get(c_key_self, self)

    ! Call Fortran
    call ucldas_geom_iter_next(self)

  end subroutine ucldas_geom_iter_next_c

end module ucldas_geom_iter_interface
