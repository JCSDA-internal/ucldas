! (C) Copyright 2021-2021 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_fields_metadata_mod

use fckit_configuration_module, only: fckit_configuration, fckit_yamlconfiguration
use fckit_pathname_module, only : fckit_pathname

implicit none
private

public :: ucldas_field_metadata, ucldas_fields_metadata


! ------------------------------------------------------------------------------
! ucldas_field_metadata
! Holds all of the user configurable meta data associated with a single field
! ------------------------------------------------------------------------------
type :: ucldas_field_metadata
  character(len=:),  allocatable :: name !< internal name used only by ucldas code
  character(len=1)               :: grid     !< "h", "u" or "v"
  logical                        :: masked   !< should use land mask when interpolating
  character(len=:),  allocatable :: levels   !< "1", or "full_ocn"
  character(len=:),  allocatable :: getval_name !< variable name used by UFO
  character(len=:),  allocatable :: getval_name_surface  ! name used by UFO for the suface (if this is a 3D field)
  character(len=:),  allocatable :: io_file  !< the restart file domain (ocn, sfc, ice)
  character(len=:),  allocatable :: io_name  !< the name use in the restart IO
  character(len=:),  allocatable :: property  !< physical property of the field, "none" or "positive_definite"
  logical                        :: dummy_atm !< a meaningless dummy field, for the CRTM hacks
end type


! ------------------------------------------------------------------------------
! ucldas_fields_metadata
! Holds meta data for ALL possible fields (state, increment, other derived) in ucldas
! ------------------------------------------------------------------------------
type :: ucldas_fields_metadata

private
  type(ucldas_field_metadata), allocatable :: metadata(:)

contains
  procedure :: create => ucldas_fields_metadata_create
  procedure :: clone  => ucldas_fields_metadata_clone
  procedure :: get    => ucldas_fields_metadata_get
end type


! ------------------------------------------------------------------------------

contains

! ------------------------------------------------------------------------------

subroutine ucldas_fields_metadata_create(self, filename)
  class(ucldas_fields_metadata), intent(inout) :: self
  character(len=:), allocatable :: filename

  type(fckit_configuration)  :: conf
  type(fckit_Configuration), allocatable :: conf_list(:)

  integer :: i, j
  logical :: bool
  character(len=:), allocatable :: str

  ! parse all the metadata from a yaml configuration file
  conf = fckit_yamlconfiguration( fckit_pathname(filename))
  call conf%get_or_die("", conf_list)
  allocate(self%metadata(size(conf_list)))
  do i=1,size(self%metadata)

    call conf_list(i)%get_or_die("name", self%metadata(i)%name)

    if(.not. conf_list(i)%get("grid", str)) str = 'h'
    self%metadata(i)%grid = str

    if(.not. conf_list(i)%get("masked", bool)) bool = .true.
    self%metadata(i)%masked = bool

    if(.not. conf_list(i)%get("levels", str)) str = "1"
    self%metadata(i)%levels = str

    if(.not. conf_list(i)%get("getval name", str)) str=self%metadata(i)%name
    self%metadata(i)%getval_name = str

    if(.not. conf_list(i)%get("getval name surface", str)) str=""
    self%metadata(i)%getval_name_surface = str

    if(.not. conf_list(i)%get("io name", str)) str = ""
    self%metadata(i)%io_name = str

    if(.not. conf_list(i)%get("io file", str)) str = ""
    self%metadata(i)%io_file = str

    if(.not. conf_list(i)%get("property", str)) str = "none"
    self%metadata(i)%property = str

    if(.not. conf_list(i)%get("dummy_atm", bool)) bool = .false.
    self%metadata(i)%dummy_atm = bool
  end do

  ! check for duplicates
  do i=1,size(self%metadata)
    do j=i+1,size(self%metadata)
      if ( self%metadata(i)%name == self%metadata(j)%name .or. &
           self%metadata(i)%name == self%metadata(j)%getval_name .or. &
           self%metadata(i)%name == self%metadata(j)%getval_name_surface .or. &
           self%metadata(i)%getval_name == self%metadata(j)%name .or. &
           self%metadata(i)%getval_name == self%metadata(j)%getval_name .or. &
           self%metadata(i)%getval_name == self%metadata(j)%getval_name_surface .or. &
           ( self%metadata(i)%getval_name_surface /=  "" .and. ( &
             self%metadata(i)%getval_name_surface == self%metadata(j)%name .or. &
             self%metadata(i)%getval_name_surface == self%metadata(j)%getval_name ))) then
        str=repeat(" ",1024)
        write(str, *) "Duplicate field metadata: ", i, self%metadata(i)%name, &
                                                    j, self%metadata(j)%name
        call abor1_ftn(str)
      end if
    end do
  end do

end subroutine

! ------------------------------------------------------------------------------

subroutine ucldas_fields_metadata_clone(self, other)
  class(ucldas_fields_metadata), intent(in) :: self
  class(ucldas_fields_metadata), intent(out) :: other

  other%metadata = self%metadata

end subroutine

! ------------------------------------------------------------------------------

function ucldas_fields_metadata_get(self, name) result(field)
  class(ucldas_fields_metadata), intent(in) :: self
  character(len=:), allocatable :: name
  type(ucldas_field_metadata) :: field

  integer :: i

  ! find the field by any of its internal or getval names
  do i=1,size(self%metadata)
    if( trim(self%metadata(i)%name) == trim(name) .or. &
        trim(self%metadata(i)%getval_name) == trim(name) .or. &
        trim(self%metadata(i)%getval_name_surface) == trim(name) ) then
      field = self%metadata(i)
      return
    endif
  enddo

  call abor1_ftn("Unable to find field metadata for: " // name)

end function

! ------------------------------------------------------------------------------

end module
