! (C) Copyright 2020-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_increment_mod

use atlas_module, only: atlas_fieldset, atlas_field, atlas_real
use ucldas_fields_mod
use ucldas_convert_state_mod
use ucldas_geom_mod, only : ucldas_geom
use ucldas_geom_iter_mod, only : ucldas_geom_iter
use kinds, only: kind_real
use fckit_configuration_module, only: fckit_configuration
use random_mod, only: normal_distribution
use datetime_mod
use oops_variables_mod, only: oops_variables

implicit none
private

type, public, extends(ucldas_fields) :: ucldas_increment

contains
  ! get/set a single point
  procedure :: getpoint    => ucldas_increment_getpoint
  procedure :: setpoint    => ucldas_increment_setpoint

  ! atlas
  procedure :: set_atlas   => ucldas_increment_set_atlas
  procedure :: to_atlas    => ucldas_increment_to_atlas
  procedure :: from_atlas  => ucldas_increment_from_atlas

  ! misc
  procedure :: dirac       => ucldas_increment_dirac
  procedure :: random      => ucldas_increment_random
  procedure :: schur       => ucldas_increment_schur
  procedure :: convert     => ucldas_increment_change_resol

end type


contains

! ------------------------------------------------------------------------------
!> initialize fields with random normal distribution
subroutine ucldas_increment_random(self)
  class(ucldas_increment), intent(inout) :: self
  integer, parameter :: rseed = 1 ! constant for reproducability of tests
    ! NOTE: random seeds are not quite working the way expected,
    !  it is only set the first time normal_distribution() is called with a seed
  integer :: jz, i

  type(ucldas_field), pointer :: field

  ! set random values
  do i = 1, size(self%fields)
    field => self%fields(i)
    ! TODO remove this once increment / state are fully separated
    ! NOTE: can't randomize "hocn", testIncrementInterpAD fails
    if (field%name == 'hocn') cycle
    call normal_distribution(field%val,  0.0_kind_real, 1.0_kind_real, rseed)
  end do

  ! mask out land, set to zero
  do i=1,size(self%fields)
    field => self%fields(i)
    if (.not. associated(field%mask) ) cycle
    do jz=1,field%nz
      field%val(:,:,jz) = field%val(:,:,jz) * field%mask(:,:)
    end do
  end do

  ! update domains
  call self%update_halos()
end subroutine ucldas_increment_random


! ------------------------------------------------------------------------------
!> perform a shur product between two sets of fields
subroutine ucldas_increment_schur(self,rhs)
  class(ucldas_increment), intent(inout) :: self
  class(ucldas_increment),    intent(in) :: rhs
  integer :: i

  ! make sure fields are same shape
  call self%check_congruent(rhs)

  ! schur product
  do i=1,size(self%fields)
    self%fields(i)%val = self%fields(i)%val * rhs%fields(i)%val
  end do
end subroutine ucldas_increment_schur

! ------------------------------------------------------------------------------

subroutine ucldas_increment_getpoint(self, geoiter, values)
  class(ucldas_increment), intent(   in) :: self
  type(ucldas_geom_iter),  intent(   in) :: geoiter
  real(kind=kind_real),  intent(inout) :: values(:)

  integer :: ff, ii, nz
  type(ucldas_field), pointer :: field

  ! get values
  ! TODO generalize field names
  ii = 0
  do ff = 1, size(self%fields)
    field => self%fields(ff)
    select case(field%name)
    case("tocn", "socn", "ssh", "uocn", "vocn", "hocn", "cicen", "hicen", "hsnon", "chl", "biop")
      nz = field%nz
      values(ii+1:ii+nz) = field%val(geoiter%iind, geoiter%jind,:)
      ii = ii + nz
    end select
  end do
end subroutine ucldas_increment_getpoint

! ------------------------------------------------------------------------------

subroutine ucldas_increment_setpoint(self, geoiter, values)
  class(ucldas_increment), intent(inout) :: self
  type(ucldas_geom_iter),  intent(   in) :: geoiter
  real(kind=kind_real),  intent(   in) :: values(:)

  integer :: ff, ii, nz
  type(ucldas_field), pointer :: field

  ! Set values
  ! TODO generalize field names
  ii = 0
  do ff = 1, size(self%fields)
    field => self%fields(ff)
    select case(field%name)
    case("tocn", "socn", "ssh", "uocn", "vocn", "hocn", "cicen", "hicen", "hsnon", "chl", "biop")
      nz = field%nz
      field%val(geoiter%iind, geoiter%jind,:) = values(ii+1:ii+nz)
      ii = ii + nz
    end select
  end do
end subroutine ucldas_increment_setpoint


  ! ------------------------------------------------------------------------------
! TODO, generalize by removing the hardcoded int=>field_name
subroutine ucldas_increment_dirac(self, f_conf)
  class(ucldas_increment),        intent(inout) :: self
  type(fckit_configuration), value, intent(in):: f_conf   !< Configuration

  integer :: isc, iec, jsc, jec
  integer :: ndir,n, jz
  integer,allocatable :: ixdir(:),iydir(:),izdir(:),ifdir(:)

  type(ucldas_field), pointer :: field

  ! Get Diracs size
  ndir = f_conf%get_size("ixdir")
  if (( f_conf%get_size("iydir") /= ndir ) .or. &
      ( f_conf%get_size("izdir") /= ndir ) .or. &
      ( f_conf%get_size("ifdir") /= ndir )) &
      call abor1_ftn('ucldas_fields_dirac: inconsistent sizes for ixdir, iydir, izdir, and ifdir')

  ! Allocation
  allocate(ixdir(ndir))
  allocate(iydir(ndir))
  allocate(izdir(ndir))
  allocate(ifdir(ndir))

  ! Get Diracs positions
  call f_conf%get_or_die("ixdir", ixdir)
  call f_conf%get_or_die("iydir", iydir)
  call f_conf%get_or_die("izdir", izdir)
  call f_conf%get_or_die("ifdir", ifdir)

  ! get PE domain bounds
  isc = self%geom%isc ; iec = self%geom%iec
  jsc = self%geom%jsc ; jec = self%geom%jec

  ! Setup Diracs
  call self%zeros()
  do n=1,ndir
      ! skip this index if not in the bounds of this PE
      if (ixdir(n) > iec .or. ixdir(n) < isc) cycle
      if (iydir(n) > jec .or. iydir(n) < jsc) cycle

    field => null()
    select case(ifdir(n))
    case (1)
      call self%get("tocn", field)
    case (2)
      call self%get("socn", field)
    case (3)
      call self%get("ssh", field)
    case (4)
      call self%get("cicen", field)
    case (5)
      call self%get("hicen", field)
    case (6)
      call self%get("chl", field)
    case (7)
      call self%get("biop", field)
    case default
      ! TODO print error that out of range
    end select
    if (associated(field)) then
      jz = 1
      if (field%nz > 1) jz = izdir(n)
      field%val(ixdir(n),iydir(n),izdir(n)) = 1.0
    end if
  end do
end subroutine ucldas_increment_dirac


! ------------------------------------------------------------------------------
subroutine ucldas_increment_set_atlas(self, geom, vars, afieldset)
  class(ucldas_increment), intent(in)    :: self
  type(ucldas_geom),       intent(in)    :: geom
  type(oops_variables),  intent(in)    :: vars
  type(atlas_fieldset),  intent(inout) :: afieldset

  integer :: jvar, i, jz, nz
  logical :: var_found
  character(len=1024) :: fieldname
  type(ucldas_field), pointer :: field
  type(atlas_field) :: afield

  do jvar = 1,vars%nvars()
    var_found = .false.
    do i=1,size(self%fields)
      field => self%fields(i)
      if (trim(vars%variable(jvar))==trim(field%name)) then
        if (.not.afieldset%has_field(vars%variable(jvar))) then
          ! Variable dimension
          nz = field%nz
          if (nz==1) nz = 0

          ! Create field
          afield = geom%afunctionspace%create_field(name=vars%variable(jvar),kind=atlas_real(kind_real),levels=nz)

          ! Add field
          call afieldset%add(afield)

          ! Release pointer
          call afield%final()
        end if
        ! Set flag
        var_found = .true.
        exit
      end if
    end do
    if (.not.var_found) call abor1_ftn('variable '//trim(vars%variable(jvar))//' not found in increment')
  end do

end subroutine ucldas_increment_set_atlas


! ------------------------------------------------------------------------------
subroutine ucldas_increment_to_atlas(self, geom, vars, afieldset)
  class(ucldas_increment), intent(in)    :: self
  type(ucldas_geom),       intent(in)    :: geom
  type(oops_variables),  intent(in)    :: vars
  type(atlas_fieldset),  intent(inout) :: afieldset

  integer :: jvar, i, jz, nz
  real(kind=kind_real), pointer :: real_ptr_1(:), real_ptr_2(:,:)
  logical :: var_found
  character(len=1024) :: fieldname
  type(ucldas_field), pointer :: field
  type(atlas_field) :: afield

  do jvar = 1,vars%nvars()
    var_found = .false.
    do i=1,size(self%fields)
      field => self%fields(i)
      if (trim(vars%variable(jvar))==trim(field%name)) then
        ! Variable dimension
        nz = field%nz
        if (nz==1) nz = 0

        if (afieldset%has_field(vars%variable(jvar))) then
          ! Get field
          afield = afieldset%field(vars%variable(jvar))
        else
          ! Create field
          afield = geom%afunctionspace%create_field(name=vars%variable(jvar),kind=atlas_real(kind_real),levels=nz)

          ! Add field
          call afieldset%add(afield)
        end if

        ! Copy data
        if (nz==0) then
          call afield%data(real_ptr_1)
          real_ptr_1 = reshape(field%val(geom%isc:geom%iec,geom%jsc:geom%jec,1), &
        & (/(geom%iec-geom%isc+1)*(geom%jec-geom%jsc+1)/))
        else
          call afield%data(real_ptr_2)
          do jz=1,nz
            real_ptr_2(jz,:) = reshape(field%val(geom%isc:geom%iec,geom%jsc:geom%jec,jz), &
          & (/(geom%iec-geom%isc+1)*(geom%jec-geom%jsc+1)/))
          end do
        end if

        ! Release pointer
        call afield%final()

        ! Set flag
        var_found = .true.
        exit
      end if
    end do
  if (.not.var_found) call abor1_ftn('variable '//trim(vars%variable(jvar))//' not found in increment')
end do

end subroutine ucldas_increment_to_atlas


! ------------------------------------------------------------------------------
subroutine ucldas_increment_from_atlas(self, geom, vars, afieldset)
  class(ucldas_increment), intent(inout) :: self
  type(ucldas_geom),       intent(in)    :: geom
  type(oops_variables),  intent(in)    :: vars
  type(atlas_fieldset),  intent(in)    :: afieldset

  integer :: jvar, i, jz, nz
  real(kind=kind_real), pointer :: real_ptr_1(:), real_ptr_2(:,:)
  logical :: var_found
  character(len=1024) :: fieldname
  type(ucldas_field), pointer :: field
  type(atlas_field) :: afield

  ! Initialization
  call self%zeros()

  do jvar = 1,vars%nvars()
    var_found = .false.
    do i=1,size(self%fields)
      field => self%fields(i)
      if (trim(vars%variable(jvar))==trim(field%name)) then
        ! Variable dimension
        nz = field%nz
        if (nz==1) nz = 0

        ! Get field
        afield = afieldset%field(vars%variable(jvar))

        ! Copy data
        if (nz==0) then
          call afield%data(real_ptr_1)
          field%val(geom%isc:geom%iec,geom%jsc:geom%jec,1) = reshape(real_ptr_1, &
        & (/geom%iec-geom%isc+1,geom%jec-geom%jsc+1/))
        else
          call afield%data(real_ptr_2)
          do jz=1,nz
            field%val(geom%isc:geom%iec,geom%jsc:geom%jec,jz) = reshape(real_ptr_2(jz,:), &
          & (/geom%iec-geom%isc+1,geom%jec-geom%jsc+1/))
          end do
        end if

        ! Release pointer
        call afield%final()

        ! Set flag
        var_found = .true.
        exit
      end if
    end do
    if (.not.var_found) call abor1_ftn('variable '//trim(vars%variable(jvar))//' not found in increment')
  end do

end subroutine ucldas_increment_from_atlas

! ------------------------------------------------------------------------------
!> Change resolution
subroutine ucldas_increment_change_resol(self, rhs)
  class(ucldas_increment), intent(inout) :: self  ! target
  class(ucldas_increment),    intent(in) :: rhs   ! source

  integer :: n
  type(ucldas_convertstate_type) :: convert_state
  type(ucldas_field), pointer :: field1, field2, hocn1, hocn2

  call rhs%get("hocn", hocn1)
  call self%get("hocn", hocn2)

  call convert_state%setup(rhs%geom, self%geom, hocn1, hocn2)
  do n = 1, size(rhs%fields)
    if (trim(rhs%fields(n)%name)=="hocn") cycle ! skip layer thickness
    field1 => rhs%fields(n)
    call self%get(trim(field1%name),field2)
    call convert_state%change_resol2d(field1, field2, rhs%geom, self%geom)
  end do !n
  call convert_state%clean()
end subroutine ucldas_increment_change_resol

! ------------------------------------------------------------------------------

end module ucldas_increment_mod
