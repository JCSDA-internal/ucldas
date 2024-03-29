! (C) Copyright 2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Handle fields for the model

module ucldas_fields_mod

use fckit_configuration_module, only: fckit_configuration
use fckit_log_module, only: log, fckit_log
use fckit_mpi_module, only: fckit_mpi_comm, fckit_mpi_min, fckit_mpi_max, &
                            fckit_mpi_sum
use datetime_mod, only: datetime, datetime_set
use duration_mod, only: duration
use kinds, only: kind_real
use oops_variables_mod
use fms_mod,    only: read_data, write_data, set_domain
use fms_io_mod, only: fms_io_init, fms_io_exit, &
                      register_restart_field, restart_file_type, &
                      restore_state, query_initialized, &
                      free_restart_type, save_restart
use mpp_domains_mod, only : mpp_update_domains
use LND_remapping, only : remapping_CS, initialize_remapping, remapping_core_h, end_remapping
use ucldas_fields_metadata_mod
use ucldas_geom_mod, only : ucldas_geom
use ucldas_fieldsutils_mod, only: ucldas_genfilename, fldinfo
use ucldas_utils, only: ucldas_mld

use horiz_interp_mod, only : horiz_interp_type
use horiz_interp_spherical_mod, only : horiz_interp_spherical
use horiz_interp_spherical_mod, only : horiz_interp_spherical_new, horiz_interp_spherical_del
use tools_const, only: deg2rad

implicit none

private
public :: ucldas_fields, ucldas_field

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!> Holds all data and metadata related to a single field variable
type :: ucldas_field
  character(len=:),     allocatable :: name       !< the internally used name of the field
  integer                           :: nz         !< the number of levels
  real(kind=kind_real), allocatable :: val(:,:,:) !< the actual data
  real(kind=kind_real),     pointer :: mask(:,:) => null() !< field mask
  real(kind=kind_real),     pointer :: lon(:,:) => null()  !< field lon
  real(kind=kind_real),     pointer :: lat(:,:) => null()  !< field lat
  type(ucldas_field_metadata)         :: metadata   ! parameters for the field as determined
                                                  ! by the configuration yaml
contains
  procedure :: copy            => ucldas_field_copy
  procedure :: delete          => ucldas_field_delete

  procedure :: check_congruent => ucldas_field_check_congruent
  procedure :: update_halo     => ucldas_field_update_halo
  procedure :: stencil_interp  => ucldas_field_stencil_interp

end type ucldas_field

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!> Holds a collection of ucldas_field types, and the public suroutines
!> to manipulate them. Represents all the fields of a given state
!> or increment
type :: ucldas_fields
   type(ucldas_geom),  pointer :: geom           !< UCLAND Geometry
   type(ucldas_field), pointer :: fields(:) => null()

contains
  ! constructors / destructors
  procedure :: create => ucldas_fields_create
  procedure :: copy   => ucldas_fields_copy
  procedure :: delete => ucldas_fields_delete

  ! field getters/checkers
  procedure :: get    => ucldas_fields_get
  procedure :: has    => ucldas_fields_has
  procedure :: check_congruent => ucldas_fields_check_congruent
  procedure :: check_subset    => ucldas_fields_check_subset

  ! math
  procedure :: add      => ucldas_fields_add
  procedure :: axpy     => ucldas_fields_axpy
  procedure :: dot_prod => ucldas_fields_dotprod
  procedure :: gpnorm   => ucldas_fields_gpnorm
  procedure :: mul      => ucldas_fields_mul
  procedure :: sub      => ucldas_fields_sub
  procedure :: ones     => ucldas_fields_ones
  procedure :: zeros    => ucldas_fields_zeros

  ! IO
  procedure :: read      => ucldas_fields_read
  procedure :: write_file=> ucldas_fields_write_file
  procedure :: write_rst => ucldas_fields_write_rst

  ! misc
  procedure :: update_halos => ucldas_fields_update_halos
  procedure :: colocate  => ucldas_fields_colocate

  ! serialization
  procedure :: serial_size => ucldas_fields_serial_size
  procedure :: serialize   => ucldas_fields_serialize
  procedure :: deserialize => ucldas_fields_deserialize

end type ucldas_fields


contains

! ------------------------------------------------------------------------------
! ucldas_field subroutines
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
!> copy a field from rhs to self. Self must be allocated first
subroutine ucldas_field_copy(self, rhs)
  class(ucldas_field), intent(inout) :: self
  type(ucldas_field),  intent(in)    :: rhs

  call self%check_congruent(rhs)

  ! the only variable that should be different is %val
  self%val = rhs%val

  ! NOTE: the pointers (mask, lat, lon) will be different, but should NOT
  ! be changed to point to rhs pointers. Bad things happen
end subroutine ucldas_field_copy

! ------------------------------------------------------------------------------
!
subroutine ucldas_field_update_halo(self, geom)
  class(ucldas_field),     intent(inout) :: self
  type(ucldas_geom), pointer, intent(in) :: geom

  ! TODO have field keep a pointer to its relevant sections of geom
  call mpp_update_domains(self%val, geom%Domain%mpp_domain)
end subroutine ucldas_field_update_halo

! ------------------------------------------------------------------------------
!
subroutine ucldas_field_stencil_interp(self, geom, interp2d)
  class(ucldas_field),     intent(inout) :: self
  type(ucldas_geom), pointer, intent(in) :: geom
  type(horiz_interp_type),  intent(in) :: interp2d

  integer :: k
  real(kind=kind_real), allocatable :: val(:,:,:)

  allocate(val, mold=self%val)
  val = self%val
  do k = 1, self%nz
     call horiz_interp_spherical(interp2d, &
          & val(geom%isd:geom%ied, geom%jsd:geom%jed,k), &
          & self%val(geom%isc:geom%iec, geom%jsc:geom%jec,k))
  end do
  call self%update_halo(geom)
end subroutine ucldas_field_stencil_interp

! ------------------------------------------------------------------------------
! make sure the two fields are the same in terms of name, size, shape
subroutine ucldas_field_check_congruent(self, rhs)
  class(ucldas_field), intent(in) :: self
  type(ucldas_field),  intent(in) :: rhs
  integer :: i

  if ( self%nz /= rhs%nz ) call abor1_ftn("ucldas_field:  self%nz /= rhs%nz")
  if ( self%name /= rhs%name ) call abor1_ftn("ucldas_field:  self%name /= rhs%name")
  if ( size(shape(self%val)) /= size(shape(rhs%val)) ) &
    call abor1_ftn("ucldas_field: shape of self%val /= rhs%val")
  do i =1, size(shape(self%val))
    if (size(self%val, dim=i) /= size(rhs%val, dim=i)) &
      call abor1_ftn("ucldas_field: shape of self%val /= rhs%val")
  end do
end subroutine ucldas_field_check_congruent


! ------------------------------------------------------------------------------
!> delete the ucldas_field object
subroutine ucldas_field_delete(self)
  class(ucldas_field), intent(inout) :: self

  deallocate(self%val)
end subroutine


! ------------------------------------------------------------------------------
! ucldas_fields subroutines
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
!> for a given list of field names, initialize the properties of those fields
subroutine ucldas_fields_init_vars(self, vars)
  class(ucldas_fields),          intent(inout) :: self
  character(len=:), allocatable, intent(in) :: vars(:)

  integer :: i, nz

  allocate(self%fields(size(vars)))
  do i=1,size(vars)
    self%fields(i)%name = trim(vars(i))

    ! get the field metadata parameters that are read in from a config file
    self%fields(i)%metadata = self%geom%fields_metadata%get(self%fields(i)%name)

    ! Set grid location and masks
    select case(self%fields(i)%metadata%grid)
    case ('h')
      self%fields(i)%lon => self%geom%lon
      self%fields(i)%lat => self%geom%lat
      if (self%fields(i)%metadata%masked) &
        self%fields(i)%mask => self%geom%mask2d
    case ('u')
      self%fields(i)%lon => self%geom%lonu
      self%fields(i)%lat => self%geom%latu
      if (self%fields(i)%metadata%masked) &
        self%fields(i)%mask => self%geom%mask2du
    case ('v')
        self%fields(i)%lon => self%geom%lonv
        self%fields(i)%lat => self%geom%latv
        if (self%fields(i)%metadata%masked) &
          self%fields(i)%mask => self%geom%mask2dv
    case default
      call abor1_ftn('ucldas_fields::create(): Illegal grid '// &
                     self%fields(i)%metadata%grid // &
                     ' given for ' // self%fields(i)%name)
    end select

    ! determine number of levels
    if (self%fields(i)%name == self%fields(i)%metadata%getval_name_surface) then
      ! if this field is a surface getval, override the number of levels with 1
      nz = 1
    else
      select case(self%fields(i)%metadata%levels)
      case ('full_ocn')
        nz = self%geom%nzo
      case ('1') ! TODO, generalize to work with any number?
        nz = 1
      case default
        call abor1_ftn('ucldas_fields::create(): Illegal levels '//self%fields(i)%metadata%levels// &
                       ' given for ' // self%fields(i)%name)
      end select
    endif

    ! allocate space
    self%fields(i)%nz = nz
    allocate(self%fields(i)%val(&
      self%geom%isd:self%geom%ied, &
      self%geom%jsd:self%geom%jed, &
      nz ))

  end do
end subroutine


! ------------------------------------------------------------------------------
!> Create a new set of fields, allocate space for them, and initialize to zero
subroutine ucldas_fields_create(self, geom, vars)
  class(ucldas_fields),        intent(inout) :: self
  type(ucldas_geom),  pointer, intent(inout) :: geom
  type(oops_variables),      intent(inout) :: vars  !< list of field names to create

  character(len=:), allocatable :: vars_str(:)
  integer :: i

  ! make sure current object has not already been allocated
  if (associated(self%fields)) &
    call abor1_ftn("ucldas_fields::create(): object already allocated")

  ! associate geometry
  self%geom => geom

  ! initialize the variable parameters
  allocate(character(len=1024) :: vars_str(vars%nvars()))
  do i=1,vars%nvars()
    vars_str(i) = trim(vars%variable(i))
  end do
  call ucldas_fields_init_vars(self, vars_str)

  ! set everything to zero
  call self%zeros()
end subroutine ucldas_fields_create


! ------------------------------------------------------------------------------
!> delete all the fields
subroutine ucldas_fields_delete(self)
  class(ucldas_fields), intent(inout) :: self
  integer :: i

  ! clear the fields and nullify pointers
  nullify(self%geom)
  do i = 1, size(self%fields)
    call self%fields(i)%delete()
  end do
  deallocate(self%fields)
  nullify(self%fields)

end subroutine


! ------------------------------------------------------------------------------
!> Copy the contents of rhs to self. Self will be initialized with the variable
!> names in rhs if not already initialized
subroutine ucldas_fields_copy(self, rhs)
  class(ucldas_fields), intent(inout) :: self
  class(ucldas_fields),  intent(in)    :: rhs

  character(len=:), allocatable :: vars_str(:)
  integer :: i
  type(ucldas_field), pointer :: rhs_fld

  ! initialize the variables based on the names in rhs
  if (.not. associated(self%fields)) then
    self%geom => rhs%geom
    allocate(character(len=1024) :: vars_str(size(rhs%fields)))
    do i=1, size(vars_str)
      vars_str(i) = rhs%fields(i)%name
    end do
    call ucldas_fields_init_vars(self, vars_str)
  end if

  ! copy values from rhs to self, only if the variable exists
  !  in self
  do i=1,size(self%fields)
    call rhs%get(self%fields(i)%name, rhs_fld)
    call self%fields(i)%copy(rhs_fld)
  end do

end subroutine


! ------------------------------------------------------------------------------
!> get a pointer to the ucldas_field with the given name.
!> If no field exists with that name, the prorgam aborts
!> (use ucldas_fields%has() if you need to check for optional fields)
subroutine ucldas_fields_get(self, name, field)
  class(ucldas_fields),         intent(in) :: self
  character(len=*),           intent(in) :: name   !< name of field to find
  type(ucldas_field), pointer, intent(out) :: field  !< a pointer to the resulting field

  integer :: i

  ! find the field with the given name
  do i=1,size(self%fields)
    if (trim(name) == self%fields(i)%name) then
      field => self%fields(i)
      return
    end if
  end do

  ! oops, the field was not found
  call abor1_ftn("ucldas_fields::get():  cannot find field "//trim(name))
end subroutine


! ------------------------------------------------------------------------------
!> returns whether a field with the given name exists
function ucldas_fields_has(self, name) result(res)
  class(ucldas_fields), intent(in) :: self
  character(len=*),   intent(in) :: name

  logical :: res
  integer :: i

  res = .false.
  do i=1,size(self%fields)
    if (trim(name) == self%fields(i)%name) then
      res = .true.
      return
    end if
  end do
end function

! ------------------------------------------------------------------------------
subroutine ucldas_fields_update_halos(self)
  class(ucldas_fields), intent(inout) :: self
  integer :: i

  do i=1,size(self%fields)
    call self%fields(i)%update_halo(self%geom)
  end do
end subroutine ucldas_fields_update_halos

! ------------------------------------------------------------------------------
!> set all fields to one
subroutine ucldas_fields_ones(self)
  class(ucldas_fields), intent(inout) :: self
  integer :: i

  do i = 1, size(self%fields)
    self%fields(i)%val = 1.0_kind_real
  end do

end subroutine ucldas_fields_ones

! ------------------------------------------------------------------------------
!> reset all fields to zero
subroutine ucldas_fields_zeros(self)
  class(ucldas_fields), intent(inout) :: self
  integer :: i

  do i = 1, size(self%fields)
    self%fields(i)%val = 0.0_kind_real
  end do

end subroutine ucldas_fields_zeros


! ------------------------------------------------------------------------------
!> add two sets of fields together
subroutine ucldas_fields_add(self,rhs)
  class(ucldas_fields), intent(inout) :: self
  class(ucldas_fields),     intent(in) :: rhs
  integer :: i

  ! make sure fields are same shape
  call self%check_congruent(rhs)

  ! add
  do i=1,size(self%fields)
    self%fields(i)%val = self%fields(i)%val + rhs%fields(i)%val
  end do
end subroutine ucldas_fields_add


! ------------------------------------------------------------------------------
!> subtract two sets of fields
subroutine ucldas_fields_sub(self,rhs)
  class(ucldas_fields), intent(inout) :: self
  class(ucldas_fields),     intent(in) :: rhs
  integer :: i

  ! make sure fields are same shape
  call self%check_congruent(rhs)

  ! subtract
  do i=1,size(self%fields)
    self%fields(i)%val = self%fields(i)%val - rhs%fields(i)%val
  end do
end subroutine ucldas_fields_sub


! ------------------------------------------------------------------------------
!> multiply a set of fields by a constant
subroutine ucldas_fields_mul(self,zz)
  class(ucldas_fields), intent(inout) :: self
  real(kind=kind_real),  intent(in) :: zz
  integer :: i

  do i=1,size(self%fields)
    self%fields(i)%val = zz * self%fields(i)%val
  end do
end subroutine ucldas_fields_mul


! ------------------------------------------------------------------------------
!> Add two fields (multiplying the rhs first)
subroutine ucldas_fields_axpy(self,zz,rhs)
  class(ucldas_fields), intent(inout) :: self
  real(kind=kind_real),  intent(in) :: zz
  class(ucldas_fields),    intent(in) :: rhs

  type(ucldas_field), pointer :: f_rhs, f_lhs
  integer :: i

  ! make sure fields are correct shape
  call self%check_subset(rhs)

  do i=1,size(self%fields)
    f_lhs => self%fields(i)
    if (.not. rhs%has(f_lhs%name)) cycle
    call rhs%get(f_lhs%name, f_rhs)
    f_lhs%val = f_lhs%val + zz *f_rhs%val
  end do
end subroutine ucldas_fields_axpy


! ------------------------------------------------------------------------------
!> calculate the global dot product of two sets of fields
subroutine ucldas_fields_dotprod(fld1,fld2,zprod)
  class(ucldas_fields),     intent(in) :: fld1
  class(ucldas_fields),      intent(in) :: fld2
  real(kind=kind_real),  intent(out) :: zprod

  real(kind=kind_real) :: local_zprod
  integer :: ii, jj, kk, n
  type(ucldas_field), pointer :: field1, field2

  ! make sure fields are same shape
  call fld1%check_congruent(fld2)

  ! loop over (almost) all fields
  local_zprod = 0.0_kind_real
  do n=1,size(fld1%fields)
    field1 => fld1%fields(n)
    field2 => fld2%fields(n)

    ! add the given field to the dot product (only using the compute domain)
    do ii = fld1%geom%isc, fld1%geom%iec
      do jj = fld1%geom%jsc, fld1%geom%jec
        ! masking
        if (associated(field1%mask)) then
          if (field1%mask(ii,jj) < 1) cycle
        endif

        ! add to dot product
        do kk=1,field1%nz
          local_zprod = local_zprod + field1%val(ii,jj,kk) * field2%val(ii,jj,kk)
        end do
      end do
    end do
  end do

  ! Get global dot product
  call fld1%geom%f_comm%allreduce(local_zprod, zprod, fckit_mpi_sum())
end subroutine ucldas_fields_dotprod


! ------------------------------------------------------------------------------

subroutine ucldas_fields_read(fld, f_conf, vdate)
  class(ucldas_fields),         intent(inout) :: fld     !< Fields
  type(fckit_configuration), intent(in)    :: f_conf  !< Configuration
  type(datetime),            intent(inout) :: vdate   !< DateTime

  integer, parameter :: max_string_length=800
  character(len=max_string_length) :: ocn_filename, sfc_filename, ice_filename, wav_filename, filename
  character(len=:), allocatable :: basename, incr_filename
  integer :: iread = 0
  integer :: ii
  logical :: vert_remap=.false.
  character(len=max_string_length) :: remap_filename
  real(kind=kind_real), allocatable :: h_common(:,:,:)    !< layer thickness to remap to
  type(restart_file_type), target :: ocean_restart, sfc_restart, ice_restart, wav_restart
  type(restart_file_type) :: ocean_remap_restart
  type(restart_file_type), pointer :: restart
  integer :: idr
  integer :: isd, ied, jsd, jed
  integer :: isc, iec, jsc, jec
  integer :: i, j, nz, n
  type(remapping_CS)  :: remapCS
  character(len=:), allocatable :: str
  real(kind=kind_real), allocatable :: h_common_ij(:), hocn_ij(:), varocn_ij(:), varocn2_ij(:)
  logical :: read_sfc, read_ice, read_wav
  type(ucldas_field), pointer :: field, field2, hocn, mld, layer_depth

  if ( f_conf%has("read_from_file") ) &
      call f_conf%get_or_die("read_from_file", iread)

  call fld%get("hocn", hocn)

  ! Get Indices for data domain and allocate common layer depth array
  isd = fld%geom%isd ; ied = fld%geom%ied
  jsd = fld%geom%jsd ; jed = fld%geom%jed

  ! Check if vertical remapping needs to be applied
  nz = hocn%nz
  if ( f_conf%has("remap_filename") ) then
     vert_remap = .true.
     call f_conf%get_or_die("remap_filename", str)
     remap_filename = str
     allocate(h_common(isd:ied,jsd:jed,nz))
     h_common = 0.0_kind_real

     ! Read common vertical coordinate from file
     call fms_io_init()
     idr = register_restart_field(ocean_remap_restart, remap_filename, 'h', h_common, &
          domain=fld%geom%Domain%mpp_domain)
     call restore_state(ocean_remap_restart, directory='')
     call free_restart_type(ocean_remap_restart)
     call fms_io_exit()
  end if

  ! iread = 0: Invent state
  if (iread==0) then
     call fld%zeros()
     call f_conf%get_or_die("date", str)
     call datetime_set(str, vdate)
  end if

  ! TODO redo this to be generic

  ! iread = 1 (state) or 3 (increment): Read restart file
  if ((iread==1).or.(iread==3)) then

    ! filename for ocean
    call f_conf%get_or_die("basename", str)
    basename = str
    call f_conf%get_or_die("ocn_filename", str)
    ocn_filename = trim(basename) // trim(str)

    ! filename for ocn sfc
    read_sfc = .false.
    sfc_filename=""
    if ( f_conf%has("sfc_filename") ) then
      call f_conf%get_or_die("basename", str)
      basename = str
      call f_conf%get_or_die("sfc_filename", str)
      sfc_filename = trim(basename)//trim(str)
    end if

    ! filename for ice
    read_ice = .false.
    ice_filename=""
    if ( f_conf%has("ice_filename") ) then
      call f_conf%get_or_die("basename", str)
      basename = str
      call f_conf%get_or_die("ice_filename", str)
      ice_filename = trim(basename)//trim(str)
    end if

    ! filename for wav
    read_wav = .false.
    wav_filename=""
    if ( f_conf%has("wav_filename") ) then
      call f_conf%get_or_die("basename", str)
      basename = str
      call f_conf%get_or_die("wav_filename", str)
      wav_filename = trim(basename)//trim(str)
    end if

    call fms_io_init()

    ! built-in variables
    do i=1,size(fld%fields)
      if(fld%fields(i)%metadata%io_name /= "") then
        ! which file are we reading from?
        select case(fld%fields(i)%metadata%io_file)
        case ('ocn')
          filename = ocn_filename
          restart => ocean_restart
        case ('sfc')
          if (sfc_filename == "") cycle ! we have sfc fields, but no file to read from
          filename = sfc_filename
          restart => sfc_restart
          read_sfc = .true.
        case ('ice')
          filename = ice_filename
          restart => ice_restart
          read_ice = .true.
        case ('wav')
          filename = wav_filename
          restart => wav_restart
          read_wav = .true.
        case default
          call abor1_ftn('read_file(): illegal io_file: '//fld%fields(i)%metadata%io_file)
        end select

      ! setup to read
        if (fld%fields(i)%nz == 1) then
          idr = register_restart_field(restart, filename, fld%fields(i)%metadata%io_name, &
              fld%fields(i)%val(:,:,1), domain=fld%geom%Domain%mpp_domain)
        else
          idr = register_restart_field(restart, filename, fld%fields(i)%metadata%io_name, &
              fld%fields(i)%val(:,:,:), domain=fld%geom%Domain%mpp_domain)
        end if
      end if
    end do

    call restore_state(ocean_restart, directory='')
    call free_restart_type(ocean_restart)
    if (read_sfc) then
      call restore_state(sfc_restart, directory='')
      call free_restart_type(sfc_restart)
    end if
    if (read_ice) then
      call restore_state(ice_restart, directory='')
      call free_restart_type(ice_restart)
    end if
    if (read_wav) then
      call restore_state(wav_restart, directory='')
      call free_restart_type(wav_restart)
    end if

    call fms_io_exit()

    ! Indices for compute domain
    isc = fld%geom%isc ; iec = fld%geom%iec
    jsc = fld%geom%jsc ; jec = fld%geom%jec

    ! Initialize mid-layer depth from layer thickness
    if (fld%has("layer_depth")) then
      call fld%get("layer_depth", layer_depth)
      call fld%geom%thickness2depth(hocn%val, layer_depth%val)
    end if

    ! Compute mixed layer depth TODO: Move somewhere else ...
    if (fld%has("mld") .and. fld%has("layer_depth")) then
      call fld%get("tocn", field)
      call fld%get("socn", field2)
      call fld%get("mld", mld)
      do i = isc, iec
        do j = jsc, jec
            mld%val(i,j,1) = ucldas_mld(&
                &field2%val(i,j,:),&
                &field%val(i,j,:),&
                &layer_depth%val(i,j,:),&
                &fld%geom%lon(i,j),&
                &fld%geom%lat(i,j))
        end do
      end do
    end if

    ! Remap layers if needed
    if (vert_remap) then
      allocate(h_common_ij(nz), hocn_ij(nz), varocn_ij(nz), varocn2_ij(nz))
      call initialize_remapping(remapCS,'PCM')
      do i = isc, iec
        do j = jsc, jec
          h_common_ij = h_common(i,j,:)
          hocn_ij = hocn%val(i,j,:)

          do n=1,size(fld%fields)
            field => fld%fields(n)
            select case(field%name)
            ! TODO remove hardcoded variable names here
            ! TODO Add u and v. Remapping u and v will require interpolating h
            case ('tocn','socn')
              if (associated(field%mask) .and. field%mask(i,j).eq.1) then
                varocn_ij = field%val(i,j,:)
                call remapping_core_h(remapCS, nz, h_common_ij, varocn_ij,&
                      &nz, hocn_ij, varocn2_ij)
                field%val(i,j,:) = varocn2_ij
              else
                field%val(i,j,:) = 0.0_kind_real
              end if
            end select
          end do
        end do
      end do
      hocn%val = h_common
      deallocate(h_common_ij, hocn_ij, varocn_ij, varocn2_ij)
      call end_remapping(remapCS)
    end if

    ! Update halo
    do n=1,size(fld%fields)
      field => fld%fields(n)
      call mpp_update_domains(field%val, fld%geom%Domain%mpp_domain)
    end do

    ! Set vdate if reading state
    if (iread==1) then
      call f_conf%get_or_die("date", str)
      call datetime_set(str, vdate)
    end if

    return
  end if

end subroutine ucldas_fields_read

! ------------------------------------------------------------------------------
!> calculate global statistics for each field (min, max, average)
subroutine ucldas_fields_gpnorm(fld, nf, pstat)
  class(ucldas_fields),      intent(in) :: fld
  integer,                 intent(in) :: nf
  real(kind=kind_real), intent(inout) :: pstat(3, nf) !> [min, max, average]

  logical :: mask(fld%geom%isc:fld%geom%iec, fld%geom%jsc:fld%geom%jec)
  real(kind=kind_real) :: ocn_count, local_ocn_count, tmp(3)
  integer :: jj, isc, iec, jsc, jec
  type(ucldas_field), pointer :: field

  ! Indices for compute domain
  isc = fld%geom%isc ; iec = fld%geom%iec
  jsc = fld%geom%jsc ; jec = fld%geom%jec

  ! calculate global min, max, mean for each field
  do jj=1, size(fld%fields)
    call fld%get(fld%fields(jj)%name, field)

    ! get the mask and the total number of grid cells
    if (.not. associated(field%mask)) then
       mask = .true.
     else
       mask = field%mask(isc:iec, jsc:jec) > 0.0
     end if
    local_ocn_count = count(mask)
    call fld%geom%f_comm%allreduce(local_ocn_count, ocn_count, fckit_mpi_sum())

    ! calculate global min/max/mean
    call fldinfo(field%val(isc:iec,jsc:jec,:), mask, tmp)
    call fld%geom%f_comm%allreduce(tmp(1), pstat(1,jj), fckit_mpi_min())
    call fld%geom%f_comm%allreduce(tmp(2), pstat(2,jj), fckit_mpi_max())
    call fld%geom%f_comm%allreduce(tmp(3), pstat(3,jj), fckit_mpi_sum())
    pstat(3,jj) = pstat(3,jj)/ocn_count
  end do
end subroutine ucldas_fields_gpnorm


! ------------------------------------------------------------------------------
!> make sure two sets of fields are the same shape
!> (same variables, same resolution)
!> TODO: make this more robust (allow for different number of fields?)
subroutine ucldas_fields_check_congruent(f1, f2)
  class(ucldas_fields), intent(in) :: f1, f2

  integer :: i, j

  ! number of fields should be the same
  if (size(f1%fields) /= size(f2%fields)) &
    call abor1_ftn("ucldas_fields: contains different number of fields")

  ! each field should match (name, size, shape)
  do i=1,size(f1%fields)
    if (f1%fields(i)%name /= f2%fields(i)%name) &
      call abor1_ftn("ucldas_fields: field have different names")
    do j = 1, size(shape(f1%fields(i)%val))
      if (size(f1%fields(i)%val, dim=j) /= size(f2%fields(i)%val, dim=j) ) then
        call abor1_ftn("ucldas_fields: field '"//f1%fields(i)%name//"' has different dimensions")
      end if
    end do
  end do
end subroutine ucldas_fields_check_congruent


! ------------------------------------------------------------------------------
!> make sure two sets of fields are the same shape for fields they have in common
!> f1 must be a subset of f2
!> (same variables, same resolution)
!> TODO: make this more robust (allow for different number of fields?)
subroutine ucldas_fields_check_subset(f1, f2)
  class(ucldas_fields), intent(in) :: f1, f2

  type(ucldas_field), pointer :: fld
  integer :: i, j

  ! each field should match (name, size, shape)
  do i=1,size(f1%fields)
    if (.not. f2%has(f1%fields(i)%name)) &
      call abor1_ftn("ucldas_fields: f1 is not a subset of f2")
    call f2%get(f1%fields(i)%name, fld)
    do j = 1, size(shape(fld%val))
      if (size(f1%fields(i)%val, dim=j) /= size(fld%val, dim=j) ) then
        call abor1_ftn("ucldas_fields: field '"//f1%fields(i)%name//"' has different dimensions")
      end if
    end do
  end do
end subroutine ucldas_fields_check_subset


! ------------------------------------------------------------------------------
!> Save ucldas fields to file using fms write_data
subroutine ucldas_fields_write_file(fld, filename)
  class(ucldas_fields),  intent(in) :: fld    !< Fields
  character(len=*),   intent(in) :: filename

  integer :: ii

  call fms_io_init()
  call set_domain( fld%geom%Domain%mpp_domain )

  ! write out all fields
  do ii = 1, size(fld%fields)
    call write_data( filename, fld%fields(ii)%name, fld%fields(ii)%val(:,:,:), fld%geom%Domain%mpp_domain)
  end do

  ! some other derived fields that should be written out
  call write_data( filename, "rossby_radius", fld%geom%rossby_radius, fld%geom%Domain%mpp_domain)

  call fms_io_exit()
end subroutine ucldas_fields_write_file

! ------------------------------------------------------------------------------
!> Save ucldas fields in a restart format
!> TODO this can be generalized even more
subroutine ucldas_fields_write_rst(fld, f_conf, vdate)
  class(ucldas_fields),         intent(inout) :: fld      !< Fields
  type(fckit_configuration), intent(in)    :: f_conf   !< Configuration
  type(datetime),            intent(inout) :: vdate    !< DateTime

  integer, parameter :: max_string_length=800
  character(len=max_string_length) :: ocn_filename, sfc_filename, ice_filename, wav_filename, filename
  type(restart_file_type), target :: ocean_restart, sfc_restart, ice_restart, wav_restart
  type(restart_file_type), pointer :: restart
  integer :: idr, i
  type(ucldas_field), pointer :: field
  logical :: write_sfc, write_ice, write_wav

  write_ice = .false.
  write_sfc = .false.
  write_wav = .false.
  call fms_io_init()

  ! filenames
  ocn_filename = ucldas_genfilename(f_conf,max_string_length,vdate,"ocn")
  sfc_filename = ucldas_genfilename(f_conf,max_string_length,vdate,"sfc")
  ice_filename = ucldas_genfilename(f_conf, max_string_length,vdate,"ice")
  wav_filename = ucldas_genfilename(f_conf, max_string_length,vdate,"wav")

  ! built in variables
  do i=1,size(fld%fields)
    field => fld%fields(i)
    if (len_trim(field%metadata%io_file) /= 0) then
      ! which file are we writing to
      select case(field%metadata%io_file)
      case ('ocn')
        filename = ocn_filename
        restart => ocean_restart
      case ('sfc')
        filename = sfc_filename
        restart => sfc_restart
        write_sfc = .true.
      case ('ice')
        filename = ice_filename
        restart => ice_restart
        write_ice = .true.
      case ('wav')
        filename = wav_filename
        restart => wav_restart
        write_wav = .true.
      case default
        call abor1_ftn('ucldas_write_restart(): illegal io_file: '//field%metadata%io_file)
      end select

      ! write
      if (field%nz == 1) then
        idr = register_restart_field( restart, filename, field%metadata%io_name, &
          field%val(:,:,1), domain=fld%geom%Domain%mpp_domain)
      else
        idr = register_restart_field( restart, filename, field%metadata%io_name, &
        field%val(:,:,:), domain=fld%geom%Domain%mpp_domain)
      end if
    end if
  end do

  ! write out and cleanup
  call save_restart(ocean_restart, directory='')
  call free_restart_type(ocean_restart)
  if (write_sfc) then
    call save_restart(sfc_restart, directory='')
    call free_restart_type(sfc_restart)
  end if
  if (write_ice) then
    call save_restart(ice_restart, directory='')
    call free_restart_type(ice_restart)
  end if
  if (write_wav) then
    call save_restart(wav_restart, directory='')
    call free_restart_type(wav_restart)
  end if
  call fms_io_exit()

end subroutine ucldas_fields_write_rst

! ------------------------------------------------------------------------------
!
subroutine ucldas_fields_colocate(self, cgridlocout)
  class(ucldas_fields),    intent(inout) :: self
  character(len=1),         intent(in) :: cgridlocout !< colocate to cgridloc (u, v or h)

  integer :: i, k
  real(kind=kind_real), allocatable :: val(:,:,:)
  real(kind=kind_real), pointer :: lon_out(:,:) => null()
  real(kind=kind_real), pointer :: lat_out(:,:) => null()
  type(ucldas_geom),  pointer :: g => null()
  type(horiz_interp_type) :: interp2d

  ! Associate lon_out and lat_out according to cgridlocout
  select case(cgridlocout)
  ! TODO: Test colocation to u and v grid
  !case ('u')
  !  lon_out => self%geom%lonu
  !  lat_out => self%geom%latu
  !case ('v')
  !  lon_out => self%geom%lonv
  !  lat_out => self%geom%latv
  case ('h')
    lon_out => self%geom%lon
    lat_out => self%geom%lat
  case default
    call abor1_ftn('ucldas_fields::colocate(): unknown c-grid location '// cgridlocout)
  end select

  ! Apply interpolation to all fields, when necessary
  do i=1,size(self%fields)

    ! Check if already colocated
    if (self%fields(i)%metadata%grid == cgridlocout) cycle

    ! Initialize fms spherical idw interpolation
     g => self%geom
     call horiz_interp_spherical_new(interp2d, &
       & real(deg2rad*self%fields(i)%lon(g%isd:g%ied,g%jsd:g%jed), 8), &
       & real(deg2rad*self%fields(i)%lat(g%isd:g%ied,g%jsd:g%jed), 8), &
       & real(deg2rad*lon_out(g%isc:g%iec,g%jsc:g%jec), 8), &
       & real(deg2rad*lat_out(g%isc:g%iec,g%jsc:g%jec), 8))

    ! Make a temporary copy of field
    if (allocated(val)) deallocate(val)
    allocate(val, mold=self%fields(i)%val)
    val = self%fields(i)%val

    ! Interpolate all levels
    do k = 1, self%fields(i)%nz
      call self%fields(i)%stencil_interp(self%geom, interp2d)
    end do

    ! Update c-grid location
    self%fields(i)%metadata%grid = cgridlocout
    select case(cgridlocout)
    ! TODO: Test colocation to u and v grid
    !case ('u')
    !  self%fields(i)%lon => self%geom%lonu
    !  self%fields(i)%lat => self%geom%latu
    !case ('v')
    !  self%fields(i)%lon => self%geom%lonv
    !  self%fields(i)%lat => self%geom%latv
    case ('h')
      self%fields(i)%lon => self%geom%lon
      self%fields(i)%lat => self%geom%lat
    end select

 end do
 call horiz_interp_spherical_del(interp2d)

end subroutine ucldas_fields_colocate

! ------------------------------------------------------------------------------

subroutine ucldas_fields_serial_size(self, geom, vec_size)
  class(ucldas_fields),    intent(in)  :: self
  type(ucldas_geom),       intent(in)  :: geom
  integer,               intent(out) :: vec_size

  integer :: i

  ! Loop over fields
  vec_size = 0
  do i=1,size(self%fields)
    vec_size = vec_size + size(self%fields(i)%val)
  end do

end subroutine ucldas_fields_serial_size

! ------------------------------------------------------------------------------

subroutine ucldas_fields_serialize(self, geom, vec_size, vec)
  class(ucldas_fields),    intent(in)  :: self
  type(ucldas_geom),       intent(in)  :: geom
  integer,               intent(in)  :: vec_size
  real(kind=kind_real),  intent(out) :: vec(vec_size)

  integer :: index, i, nn

  ! Loop over fields, levels and horizontal points
  index = 1
  do i=1,size(self%fields)
    nn = size(self%fields(i)%val)
    vec(index:index+nn-1) = reshape(self%fields(i)%val, (/ nn /) )
    index = index + nn
  end do

end subroutine ucldas_fields_serialize

! ------------------------------------------------------------------------------

subroutine ucldas_fields_deserialize(self, geom, vec_size, vec, index)
  class(ucldas_fields), intent(inout) :: self
  type(ucldas_geom),       intent(in)    :: geom
  integer,               intent(in)    :: vec_size
  real(kind=kind_real),  intent(in)    :: vec(vec_size)
  integer,               intent(inout) :: index

  integer :: i, nn

  ! Loop over fields, levels and horizontal points
  do i=1,size(self%fields)
    nn = size(self%fields(i)%val)
    self%fields(i)%val = reshape(vec(index+1:index+1+nn), shape(self%fields(i)%val))
    index = index + nn
  end do

end subroutine ucldas_fields_deserialize

! ------------------------------------------------------------------------------

end module ucldas_fields_mod
