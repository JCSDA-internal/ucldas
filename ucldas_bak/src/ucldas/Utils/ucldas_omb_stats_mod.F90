! (C) Copyright 2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_omb_stats_mod

use netcdf
use fckit_mpi_module, only: fckit_mpi_comm
use kinds, only: kind_real
use ucldas_utils, only: nc_check, ucldas_remap_idw

implicit none

private
public :: ucldas_domain_indices, ucldas_omb_stats

type :: ucldas_domain_indices ! TODO: Move elsewhere!
   integer :: is, ie, js, je     ! Compute domain indices
   integer :: isl, iel, jsl, jel ! Local compute domain indices
end type ucldas_domain_indices

type :: ucldas_omb_stats
   integer                            :: nlocs
   real(kind=kind_real),  allocatable :: lon(:)
   real(kind=kind_real),  allocatable :: lat(:)
   real(kind=kind_real),  allocatable :: bgerr(:)
   real(kind=kind_real),  allocatable :: bgerr_model(:,:)
   type(ucldas_domain_indices)          :: domain
 contains
   procedure :: init => ucldas_omb_stats_init
   procedure :: bin => ucldas_omb_stats_bin
   procedure :: exit => ucldas_omb_stats_exit
end type ucldas_omb_stats

contains

! ------------------------------------------------------------------------------
subroutine ucldas_omb_stats_init(self, domain)
  class(ucldas_omb_stats),           intent(inout) :: self
  type(ucldas_domain_indices),       intent(in) :: domain

  integer(kind=4) :: ncid
  integer(kind=4) :: dimid
  integer(kind=4) :: varid
  type(fckit_mpi_comm) :: f_comm
  integer :: myrank, root=0

  ! Setup Communicator
  f_comm = fckit_mpi_comm()
  myrank = f_comm%rank()

  if (myrank.eq.root) then

     call nc_check(nf90_open('godas_sst_bgerr.nc', nf90_nowrite,ncid))

     ! Get the size of the horizontal grid
     call nc_check(nf90_inq_dimid(ncid, 'nlocs', dimid))
     call nc_check(nf90_inquire_dimension(ncid, dimid, len = self%nlocs))

     allocate(self%lon(self%nlocs), self%lat(self%nlocs), self%bgerr(self%nlocs))

     ! Get longitude
     call nc_check(nf90_inq_varid(ncid,'longitude',varid))
     call nc_check(nf90_get_var(ncid,varid,self%lon))

     ! Get latitude
     call nc_check(nf90_inq_varid(ncid,'latitude',varid))
     call nc_check(nf90_get_var(ncid,varid,self%lat))

     ! Get omb stats
     call nc_check(nf90_inq_varid(ncid,'sst_bgerr',varid))
     call nc_check(nf90_get_var(ncid,varid,self%bgerr))

     ! Close netcdf file
     call nc_check(nf90_close(ncid))
  end if

  ! Broadcast to all workers
  call f_comm%broadcast(self%nlocs, root)
  if (myrank.ne.root) then
     allocate(self%lon(self%nlocs), self%lat(self%nlocs), self%bgerr(self%nlocs))
  end if
  call f_comm%broadcast(self%lon, root)
  call f_comm%broadcast(self%lat, root)
  call f_comm%broadcast(self%bgerr, root)
  call f_comm%barrier()

  ! Rotate longitude
  where (self%lon>180.0_kind_real)
     self%lon=self%lon-360.0_kind_real
  end where

  ! Compute domain info
  self%domain = domain

end subroutine ucldas_omb_stats_init

! ------------------------------------------------------------------------------
subroutine ucldas_omb_stats_bin(self, lon, lat)
  class(ucldas_omb_stats), intent(inout) :: self
  real(kind=kind_real),     intent(in) :: lon(:,:)
  real(kind=kind_real),     intent(in) :: lat(:,:)

  integer :: is, ie, js, je
  integer :: isl, iel, jsl, jel

  ! Short cuts to global indices
  is = self%domain%is
  ie = self%domain%ie
  js = self%domain%js
  je = self%domain%je

  ! Short cuts to local indices
  isl = self%domain%isl
  iel = self%domain%iel
  jsl = self%domain%jsl
  jel = self%domain%jel

  allocate(self%bgerr_model(is:ie,js:je))
  self%bgerr_model = 0.0_kind_real
  call ucldas_remap_idw(self%lon, self%lat, self%bgerr,&
                      lon(isl:iel,jsl:jel), lat(isl:iel,jsl:jel), &
                      self%bgerr_model(is:ie,js:je))

end subroutine ucldas_omb_stats_bin

! ------------------------------------------------------------------------------
subroutine ucldas_omb_stats_exit(self)
  class(ucldas_omb_stats), intent(inout) :: self

  deallocate(self%lon, self%lat, self%bgerr, self%bgerr_model)

end subroutine ucldas_omb_stats_exit

end module ucldas_omb_stats_mod
