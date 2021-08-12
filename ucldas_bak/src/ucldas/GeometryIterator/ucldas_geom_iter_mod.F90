!
! (C) Copyright 2019-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_geom_iter_mod

  use iso_c_binding
  use kinds
  use ucldas_geom_mod, only: ucldas_geom

  implicit none

  private
  public :: ucldas_geom_iter
  public :: ucldas_geom_iter_registry
  public :: ucldas_geom_iter_setup, ucldas_geom_iter_clone, ucldas_geom_iter_equals
  public :: ucldas_geom_iter_current, ucldas_geom_iter_next

  type :: ucldas_geom_iter
    type(ucldas_geom), pointer :: geom => null() !< Geometry
    integer :: iind = 1  !< index e.g. lat(iind,jind)
    integer :: jind = 1  !< 
  end type ucldas_geom_iter

#define LISTED_TYPE ucldas_geom_iter

  !> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

  !> Global registry
  type(registry_t) :: ucldas_geom_iter_registry

contains

  ! ------------------------------------------------------------------------------
  ! Public
  ! ------------------------------------------------------------------------------

  !> Linked list implementation
#include "oops/util/linkedList_c.f"

  ! ------------------------------------------------------------------------------
  !> Setup for the geometry iterator
  subroutine ucldas_geom_iter_setup(self, geom, iind, jind)

    ! Passed variables
    type(ucldas_geom_iter),     intent(inout) :: self !< Geometry iterator
    type(ucldas_geom), pointer, intent(   in) :: geom !< Geometry
    integer,                  intent(   in) :: iind, jind  !< Index

    ! Associate geometry
    self%geom => geom

    ! Define iind/jind for local tile
    self%iind = iind
    self%jind = jind

  end subroutine ucldas_geom_iter_setup

  ! ------------------------------------------------------------------------------
  !> Clone for the geometry iterator
  subroutine ucldas_geom_iter_clone(self, other)

    ! Passed variables
    type(ucldas_geom_iter), intent(inout) :: self  !< Geometry iterator
    type(ucldas_geom_iter), intent(   in) :: other !< Other geometry iterator

    ! Associate geometry
    self%geom => other%geom

    ! Copy iind/jind
    self%iind = other%iind
    self%jind = other%jind

  end subroutine ucldas_geom_iter_clone

  ! ------------------------------------------------------------------------------
  !> Check for the geometry iterator equality
  subroutine ucldas_geom_iter_equals(self, other, equals)

    ! Passed variables
    type(ucldas_geom_iter), intent( in) :: self   !< Geometry iterator
    type(ucldas_geom_iter), intent( in) :: other  !< Other geometry iterator
    integer,            intent(out) :: equals !< Equality flag

    ! Initialization
    equals = 0

    ! Check equality
    if (associated(self%geom, other%geom) .and. (self%iind==other%iind) .and. (self%jind==other%jind)) equals = 1

  end subroutine ucldas_geom_iter_equals

  ! ------------------------------------------------------------------------------
  !> Get geometry iterator current lat/lon
  subroutine ucldas_geom_iter_current(self, lon, lat)

    ! Passed variables
    type(ucldas_geom_iter), intent( in) :: self !< Geometry iterator
    real(kind_real),    intent(out) :: lat  !< Latitude
    real(kind_real),    intent(out) :: lon  !< Longitude

    ! Check iind/jind
    if (self%iind == -1 .AND. self%jind == -1) then
      ! special case of {-1,-1} means end of the grid
      lat = self%geom%lat(self%geom%iec,self%geom%jec)
      lon = self%geom%lon(self%geom%iec,self%geom%jec) 
    elseif (self%iind < self%geom%isc .OR. self%iind > self%geom%iec .OR. &
            self%jind < self%geom%jsc .OR. self%jind > self%geom%jec) then
      ! outside of the grid
      call abor1_ftn('ucldas_geom_iter_current: iterator out of bounds')
    else
      ! inside of the grid
      lat = self%geom%lat(self%iind,self%jind)
      lon = self%geom%lon(self%iind,self%jind)
    endif

  end subroutine ucldas_geom_iter_current

  ! ------------------------------------------------------------------------------
  !> Update geometry iterator to next point
  subroutine ucldas_geom_iter_next(self)

    ! Passed variables
    type(ucldas_geom_iter), intent(inout) :: self !< Geometry iterator
    integer :: iind, jind

    iind = self%iind
    jind = self%jind

    ! do while ((iind.lt.self%geom%iec).and.(jind.lt.self%geom%jec))

      ! increment by 1
      if (iind.lt.self%geom%iec) then 
        iind = iind + 1
      elseif (iind.eq.self%geom%iec) then
        iind = self%geom%isc
        jind = jind + 1
      end if

     ! ! skip this point if it is on land
     ! if (self%geom%mask2d(iind,jind).lt.1) then 
     !   cycle
     ! else
     !   exit
     ! endif

    ! end do

    if (jind > self%geom%jec) then
        iind=-1
        jind=-1
    end if

    self%iind = iind
    self%jind = jind

  end subroutine ucldas_geom_iter_next
  ! ------------------------------------------------------------------------------

end module ucldas_geom_iter_mod
