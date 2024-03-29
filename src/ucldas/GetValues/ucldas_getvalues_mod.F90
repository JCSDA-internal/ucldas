! (C) Copyright 2020-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_getvalues_mod

use ucldas_geom_mod, only: ucldas_geom
use ucldas_fields_mod, only: ucldas_fields, ucldas_field
use datetime_mod, only: datetime
use kinds, only: kind_real
use ufo_geovals_mod, only: ufo_geovals
use ufo_locations_mod
use unstructured_interpolation_mod, only: unstrc_interp
use fckit_log_module, only : fckit_log
use iso_c_binding

implicit none
private

!------------------------------------------------------------------------------
! ucldas_getvalues
!  forward and adjoint interpolation between the model and observation locations.
!  Several interpolators need to be created depending on which grid is used
!  (h, u, v) and if land masking is used. Since we do not know this information
!  until fill_geovals() or fill_geovals_ad() is called, creation of the interp
!  is postoned to then
type, public :: ucldas_getvalues
  ! the interpolator, and a flag for whether or not it has been initialized yet.
  type(unstrc_interp), allocatable :: horiz_interp(:)
  logical,             allocatable :: horiz_interp_init(:)

contains

  ! constructors / destructors
  procedure :: create => ucldas_getvalues_create
  procedure :: delete => ucldas_getvalues_delete

  ! apply interpolation
  procedure :: get_interp => ucldas_getvalues_getinterp
  procedure :: fill_geovals=> ucldas_getvalues_fillgeovals
  procedure :: fill_geovals_ad=> ucldas_getvalues_fillgeovals_ad

end type

!------------------------------------------------------------------------------
contains
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
subroutine ucldas_getvalues_create(self, geom, locs)
  class(ucldas_getvalues), intent(inout) :: self
  type(ucldas_geom),          intent(in) :: geom
  type(ufo_locations),      intent(in) :: locs

  ! why do things crash if I don't make these allocatable??
  allocate(self%horiz_interp(6))
  allocate(self%horiz_interp_init(6))
  self%horiz_interp_init = .false.

end subroutine ucldas_getvalues_create

!------------------------------------------------------------------------------
! Get the index of the interpolator for the given grid/masking.
! If the interpolator has not been initialized yet, it will initialize it.
! The index of horiz_interp and horiz_interp_init map to the following
!   1 = h, unmasked    2 = h, masked
!   3 = u, unmasked    4 = u, masked
!   5 = v, unmasked    6 = v, masked
function ucldas_getvalues_getinterp(self, geom, grid, masked, locs) result(idx)
  class(ucldas_getvalues), intent(inout) :: self
  type(ucldas_geom),  target, intent(in) :: geom
  character(len=1),         intent(in) :: grid   !< "h", "u", or "v"
  logical,                  intent(in) :: masked
  type(ufo_locations),      intent(in) :: locs
  integer :: idx

  integer :: isc, iec, jsc, jec
  integer :: ngrid_in, ngrid_out

  real(8), allocatable, dimension(:) :: locs_lons, locs_lats
  real(kind=kind_real), allocatable :: lats_in(:), lons_in(:)

  real(kind=kind_real),     pointer :: mask(:,:) => null() !< field mask
  real(kind=kind_real),     pointer :: lon(:,:) => null()  !< field lon
  real(kind=kind_real),     pointer :: lat(:,:) => null()  !< field lat

  ! interpolation parameters
  integer :: nn = 3
  character(len=8) :: wtype = 'barycent'

  ! get the model lat/lon/interpolator depending on grid type
  select case(grid)
  case ('h')
    idx = 1
    lon => geom%lon
    lat => geom%lat
    mask => geom%mask2d
  case ('u')
    idx = 3
    lon => geom%lonu
    lat => geom%latu
    mask => geom%mask2du
  case ('v')
    idx = 5
    lon => geom%lonv
    lat => geom%latv
    mask => geom%mask2dv
  case default
    call abor1_ftn('error in ucldas_getvalues_setupinterp. grid: '//grid)
  end select
  if (masked) idx = idx + 1

  ! has interpolation already been initialized? if so return.
  if (self%horiz_interp_init(idx)) return

  ! Indices for compute domain (no halo)
  isc = geom%isc ; iec = geom%iec
  jsc = geom%jsc ; jec = geom%jec

  ! get location lat/lons
  ngrid_out = locs%nlocs()
  allocate(locs_lons(ngrid_out), locs_lats(ngrid_out))
  call locs%get_lons(locs_lons)
  call locs%get_lats(locs_lats)

  if ( .not. masked ) then
    ! create interpolation weights for fields that do NOT use the land mask
    ngrid_in = (iec - isc + 1) * (jec - jsc + 1)
    allocate(lats_in(ngrid_in), lons_in(ngrid_in))
    lons_in = reshape(lon(isc:iec,jsc:jec), (/ngrid_in/))
    lats_in = reshape(lat(isc:iec,jsc:jec), (/ngrid_in/))
  else
    ! create interpolation weights for fields that DO use the land mask
    ngrid_in = count(mask(isc:iec,jsc:jec) > 0)
    allocate(lats_in(ngrid_in), lons_in(ngrid_in))
    lons_in = pack(lon(isc:iec,jsc:jec), mask=mask(isc:iec,jsc:jec) > 0)
    lats_in = pack(lat(isc:iec,jsc:jec), mask=mask(isc:iec,jsc:jec) > 0)
  end if

  call self%horiz_interp(idx)%create(geom%f_comm, nn, wtype, &
                     ngrid_in, lats_in, lons_in, &
                     ngrid_out, locs_lats, locs_lons)
  self%horiz_interp_init(idx) = .true.

end function

!------------------------------------------------------------------------------
subroutine ucldas_getvalues_delete(self)
  class(ucldas_getvalues), intent(inout) :: self

  deallocate(self%horiz_interp)
  deallocate(self%horiz_interp_init)
end subroutine ucldas_getvalues_delete

!------------------------------------------------------------------------------
subroutine ucldas_getvalues_fillgeovals(self, geom, fld, t1, t2, locs, geovals)
  class(ucldas_getvalues), intent(inout) :: self
  type(ucldas_geom),          intent(in) :: geom
  class(ucldas_fields),       intent(in) :: fld
  type(datetime),           intent(in) :: t1
  type(datetime),           intent(in) :: t2
  type(ufo_locations),      intent(in) :: locs
  type(ufo_geovals),     intent(inout) :: geovals

  logical(c_bool), allocatable :: time_mask(:)
  logical :: masked
  integer :: ivar, nlocs, n
  integer :: ival, nval, indx
  integer :: isc, iec, jsc, jec
  integer :: ns
  real(kind=kind_real), allocatable :: gom_window(:)
  real(kind=kind_real), allocatable :: fld3d(:,:,:), fld3d_un(:)
  type(ucldas_field), pointer :: fldptr
  integer :: interp_idx = -1

  ! Indices for compute domain (no halo)
  isc = geom%isc ; iec = geom%iec
  jsc = geom%jsc ; jec = geom%jec

  ! Get mask for locations in this time window
  allocate(time_mask(locs%nlocs()))
  call locs%get_timemask(t1,t2,time_mask)

  ! Allocate temporary geoval and 3d field for the current time window
  do ivar = 1, geovals%nvar

    call fld%get(geovals%variables(ivar), fldptr)
    if (fldptr%metadata%dummy_atm) cycle ! TODO remove this hack
    nval = fldptr%nz

    ! Return if no observations
    if ( geovals%geovals(ivar)%nlocs == 0 ) return

    allocate(gom_window(locs%nlocs()))
    allocate(fld3d(isc:iec,jsc:jec,1:nval))

    masked = fldptr%metadata%masked
    fld3d = fldptr%val(isc:iec,jsc:jec,1:nval)

    ! Apply forward interpolation: Model ---> Obs
    interp_idx = self%get_interp(geom, fldptr%metadata%grid, masked, locs)
    do ival = 1, nval

      if (masked) then
        ns = count(fldptr%mask(isc:iec,jsc:jec) > 0 )
        if (.not. allocated(fld3d_un)) allocate(fld3d_un(ns))
        fld3d_un = pack(fld3d(isc:iec,jsc:jec,ival), mask=fldptr%mask(isc:iec,jsc:jec) > 0)
      else
        ns = (iec - isc + 1) * (jec - jsc + 1)
        if (.not. allocated(fld3d_un)) allocate(fld3d_un(ns))
        fld3d_un = reshape(fld3d(isc:iec,jsc:jec,ival), (/ns/))
      end if
      call self%horiz_interp(interp_idx)%apply(fld3d_un(1:ns), gom_window)

      ! Fill proper geoval according to time window
      do indx = 1, locs%nlocs()
        if (time_mask(indx)) then
          geovals%geovals(ivar)%vals(ival, indx) = gom_window(indx)
        end if
      end do
    end do

    ! Deallocate temporary arrays
    deallocate(fld3d_un)
    deallocate(fld3d)
    deallocate(gom_window)
  end do

  ! If we reach this point, geovals has been initialized
  geovals%linit = .true.
end subroutine ucldas_getvalues_fillgeovals

!------------------------------------------------------------------------------
subroutine ucldas_getvalues_fillgeovals_ad(self, geom, incr, t1, t2, locs, geovals)
  class(ucldas_getvalues), intent(inout) :: self
  type(ucldas_geom),          intent(in) :: geom
  class(ucldas_fields),    intent(inout) :: incr
  type(datetime),           intent(in) :: t1
  type(datetime),           intent(in) :: t2
  type(ufo_locations),      intent(in) :: locs
  type(ufo_geovals),     intent(in) :: geovals

  logical(c_bool), allocatable :: time_mask(:)
  logical :: masked
  integer :: ivar, nlocs, n
  integer :: ival, nval, indx
    integer :: ni, nj
  integer :: isc, iec, jsc, jec
  integer :: ns
  real(kind=kind_real), allocatable :: gom_window(:,:)
    real(kind=kind_real), allocatable :: gom_window_ival(:)
  real(kind=kind_real), allocatable :: incr3d(:,:,:), incr3d_un(:)
  type(ucldas_field), pointer :: field
    logical :: found
  integer :: interp_idx = -1

  ! Indices for compute domain (no halo)
  isc = geom%isc ; iec = geom%iec
  jsc = geom%jsc ; jec = geom%jec

  ! Get mask for locations in this time window
  allocate(time_mask(locs%nlocs()))
  call locs%get_timemask(t1,t2,time_mask)
  allocate(gom_window_ival(locs%nlocs()))

  do ivar = 1, geovals%nvar
    call incr%get(geovals%variables(ivar), field)
    nval = field%nz

    ! Allocate temporary geoval and 3d field for the current time window
    allocate(gom_window(nval,locs%nlocs()))
    allocate(incr3d(isc:iec,jsc:jec,1:nval))
    incr3d = 0.0_kind_real
    gom_window = 0.0_kind_real

    ! determine if this variable should use the masked grid
    masked = field%metadata%masked

    ! Apply backward interpolation: Obs ---> Model
    if (masked) then
      ns = count(field%mask(isc:iec,jsc:jec) > 0)
    else
      ni = iec - isc + 1
      nj = jec - jsc + 1
      ns = ni * nj
    end if
    if (.not.allocated(incr3d_un)) allocate(incr3d_un(ns))

    interp_idx = self%get_interp(geom, field%metadata%grid, masked, locs)
    do ival = 1, nval
      ! Fill proper geoval according to time window
      do indx = 1, locs%nlocs()
        if (time_mask(indx)) then
          gom_window(ival, indx) = geovals%geovals(ivar)%vals(ival, indx)
        end if
      end do
      gom_window_ival = gom_window(ival,1:locs%nlocs())

      if (masked) then
        incr3d_un = pack(incr3d(isc:iec,jsc:jec,ival), mask = field%mask(isc:iec,jsc:jec) >0)
        call self%horiz_interp(interp_idx)%apply_ad(incr3d_un, gom_window_ival)
        incr3d(isc:iec,jsc:jec,ival) = unpack(incr3d_un, &
              mask = field%mask(isc:iec,jsc:jec) >0, &
              field = incr3d(isc:iec,jsc:jec,ival))
      else
        incr3d_un = reshape(incr3d(isc:iec,jsc:jec,ival), (/ns/))
        call self%horiz_interp(interp_idx)%apply_ad(incr3d_un(1:ns), gom_window_ival)
        incr3d(isc:iec,jsc:jec,ival) = reshape(incr3d_un(1:ns),(/ni,nj/))
      end if
    end do

    field%val(isc:iec, jsc:jec, 1:nval) = field%val(isc:iec, jsc:jec, 1:nval) + &
      incr3d(isc:iec, jsc:jec, 1:nval)

    ! Deallocate temporary arrays
    deallocate(incr3d)
    deallocate(gom_window)

  end do

  deallocate(gom_window_ival)

end subroutine ucldas_getvalues_fillgeovals_ad

end module ucldas_getvalues_mod
