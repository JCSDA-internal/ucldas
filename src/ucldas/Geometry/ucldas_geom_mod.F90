!
! (C) Copyright    2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!

module ucldas_geom_mod

use atlas_module, only: atlas_functionspace_pointcloud, atlas_fieldset, &
    atlas_field, atlas_real, atlas_integer, atlas_geometry, atlas_indexkdtree
use LND_domains, only : LND_domain_type, LND_infra_init
use LND_io,      only : io_infra_init
use ucldas_fields_metadata_mod
use ucldas_ucland, only: ucldas_ucland_config, ucldas_ucland_init, ucldas_geomdomain_init
use ucldas_utils, only: write2pe, ucldas_remap_idw
use kinds, only: kind_real
use fckit_configuration_module, only: fckit_configuration
use fckit_mpi_module, only: fckit_mpi_comm, fckit_mpi_sum
use fms_io_mod, only : fms_io_init, fms_io_exit, &
                       register_restart_field, restart_file_type, &
                       restore_state, query_initialized, &
                       free_restart_type, save_restart
use mpp_domains_mod, only : mpp_get_compute_domain, mpp_get_data_domain, &
                            mpp_get_global_domain, mpp_update_domains
use fms_mod,         only : write_data
use fms_io_mod,      only : fms_io_init, fms_io_exit
use LND_diag_remap,  only : diag_remap_ctrl, diag_remap_init, diag_remap_configure_axes, &
                            diag_remap_end, diag_remap_update
use LND_EOS,         only : EOS_type

implicit none

private
public :: ucldas_geom, &
          geom_write, geom_get_domain_indices

!> Geometry data structure
type :: ucldas_geom
    type(LND_domain_type), pointer :: Domain !< Ocean model domain
    integer :: nzo, nzo_zstar
    integer :: isc, iec, jsc, jec  !< indices of compute domain
    integer :: isd, ied, jsd, jed  !< indices of data domain
    integer :: isg, ieg, jsg, jeg  !< indices of global domain
    integer :: iscl, iecl, jscl, jecl  !< indices of local compute domain
    integer :: isdl, iedl, jsdl, jedl  !< indices of local data domain
    real(kind=kind_real), allocatable, dimension(:)   :: lonh, lath
    real(kind=kind_real), allocatable, dimension(:)   :: lonq, latq
    real(kind=kind_real), allocatable, dimension(:,:) :: lon, lat !< Tracer point grid
    real(kind=kind_real), allocatable, dimension(:,:) :: lonu, latu !< Zonal velocity grid
    real(kind=kind_real), allocatable, dimension(:,:) :: lonv, latv !< Meridional velocity grid
    real(kind=kind_real), allocatable, dimension(:,:) :: sin_rot, cos_rot !< Rotation between logical grid
                                                                          !< and North
    real(kind=kind_real), allocatable, dimension(:,:) :: mask2d    !< Tracer points. 0 = land 1 = ocean
    real(kind=kind_real), allocatable, dimension(:,:) :: mask2du   !< u        "   . 0 = land 1 = ocean
    real(kind=kind_real), allocatable, dimension(:,:) :: mask2dv   !< v        "   . 0 = land 1 = ocean
    real(kind=kind_real), allocatable, dimension(:,:) :: cell_area
    real(kind=kind_real), allocatable, dimension(:,:) :: rossby_radius
    real(kind=kind_real), allocatable, dimension(:,:) :: distance_from_coast
    real(kind=kind_real), allocatable, dimension(:,:,:) :: h
    real(kind=kind_real), allocatable, dimension(:,:,:) :: h_zstar
    logical :: save_local_domain = .false. ! If true, save the local geometry for each pe.
    character(len=:), allocatable :: geom_grid_file
    type(fckit_mpi_comm) :: f_comm
    type(atlas_functionspace_pointcloud) :: afunctionspace
    type(ucldas_fields_metadata) :: fields_metadata

    contains
    procedure :: init => geom_init
    procedure :: end => geom_end
    procedure :: set_atlas_lonlat => geom_set_atlas_lonlat
    procedure :: fill_atlas_fieldset => geom_fill_atlas_fieldset
    procedure :: clone => geom_clone
    procedure :: get_rossby_radius => geom_rossby_radius
    procedure :: gridgen => geom_gridgen
    procedure :: thickness2depth => geom_thickness2depth
    procedure :: struct2atlas => geom_struct2atlas
    procedure :: atlas2struct => geom_atlas2struct
    procedure :: write => geom_write
end type ucldas_geom

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
!> Setup geometry object
subroutine geom_init(self, f_conf, f_comm)
  class(ucldas_geom),         intent(out) :: self
  type(fckit_configuration), intent(in) :: f_conf
  type(fckit_mpi_comm),   intent(in)    :: f_comm

  character(len=:), allocatable :: str
  logical :: full_init = .false.

  ! MPI communicator
  self%f_comm = f_comm

  ! Domain decomposition
  call ucldas_geomdomain_init(self%Domain, self%nzo, f_comm)

  ! User-defined grid filename
  if ( .not. f_conf%get("geom_grid_file", self%geom_grid_file) ) &
     self%geom_grid_file = "ucldas_gridspec.nc" ! default if not found

  ! Allocate geometry arrays
  call geom_allocate(self)

  ! Check if a full initialization is required, default to false
  if ( .not. f_conf%get("full_init", full_init) ) full_init = .false.

  ! Read the geometry from file by default,
  ! skip this step if a full init is required
  if ( .not. full_init) call geom_read(self)

  ! Fill halo
  call mpp_update_domains(self%lon, self%Domain%mpp_domain)
  call mpp_update_domains(self%lat, self%Domain%mpp_domain)
  call mpp_update_domains(self%lonu, self%Domain%mpp_domain)
  call mpp_update_domains(self%latu, self%Domain%mpp_domain)
  call mpp_update_domains(self%lonv, self%Domain%mpp_domain)
  call mpp_update_domains(self%latv, self%Domain%mpp_domain)
  call mpp_update_domains(self%sin_rot, self%Domain%mpp_domain)
  call mpp_update_domains(self%cos_rot, self%Domain%mpp_domain)
  call mpp_update_domains(self%mask2d, self%Domain%mpp_domain)
  call mpp_update_domains(self%mask2du, self%Domain%mpp_domain)
  call mpp_update_domains(self%mask2dv, self%Domain%mpp_domain)
  call mpp_update_domains(self%cell_area, self%Domain%mpp_domain)
  call mpp_update_domains(self%rossby_radius, self%Domain%mpp_domain)
  call mpp_update_domains(self%distance_from_coast, self%Domain%mpp_domain)

  ! Set output option for local geometry
  if ( .not. f_conf%get("save_local_domain", self%save_local_domain) ) &
     self%save_local_domain = .false.

  ! process the fields metadata file
  call f_conf%get_or_die("fields metadata", str)
  call self%fields_metadata%create(str)

end subroutine geom_init

! ------------------------------------------------------------------------------
!> Geometry destructor
subroutine geom_end(self)
  class(ucldas_geom), intent(out)  :: self

  if (allocated(self%lonh))          deallocate(self%lonh)
  if (allocated(self%lath))          deallocate(self%lath)
  if (allocated(self%lonq))          deallocate(self%lonq)
  if (allocated(self%latq))          deallocate(self%latq)
  if (allocated(self%lon))           deallocate(self%lon)
  if (allocated(self%lat))           deallocate(self%lat)
  if (allocated(self%lonu))          deallocate(self%lonu)
  if (allocated(self%latu))          deallocate(self%latu)
  if (allocated(self%lonv))          deallocate(self%lonv)
  if (allocated(self%latv))          deallocate(self%latv)
  if (allocated(self%sin_rot))       deallocate(self%sin_rot)
  if (allocated(self%cos_rot))       deallocate(self%cos_rot)
  if (allocated(self%mask2d))        deallocate(self%mask2d)
  if (allocated(self%mask2du))       deallocate(self%mask2du)
  if (allocated(self%mask2dv))       deallocate(self%mask2dv)
  if (allocated(self%cell_area))     deallocate(self%cell_area)
  if (allocated(self%rossby_radius)) deallocate(self%rossby_radius)
  if (allocated(self%distance_from_coast)) deallocate(self%distance_from_coast)
  if (allocated(self%h))             deallocate(self%h)
  if (allocated(self%h_zstar))       deallocate(self%h_zstar)
  nullify(self%Domain)
  call self%afunctionspace%final()

end subroutine geom_end

! --------------------------------------------------------------------------------------------------
!> Set ATLAS lonlat fieldset
subroutine geom_set_atlas_lonlat(self, afieldset)
  class(ucldas_geom),  intent(inout) :: self
  type(atlas_fieldset), intent(inout) :: afieldset

  real(kind_real), pointer :: real_ptr(:,:)
  type(atlas_field) :: afield

  ! Create lon/lat field
  afield = atlas_field(name="lonlat", kind=atlas_real(kind_real), shape=(/2,(self%iec-self%isc+1)*(self%jec-self%jsc+1)/))
  call afield%data(real_ptr)
  real_ptr(1,:) = reshape(self%lon(self%isc:self%iec,self%jsc:self%jec),(/(self%iec-self%isc+1)*(self%jec-self%jsc+1)/))
  real_ptr(2,:) = reshape(self%lat(self%isc:self%iec,self%jsc:self%jec),(/(self%iec-self%isc+1)*(self%jec-self%jsc+1)/))
  call afieldset%add(afield)

end subroutine geom_set_atlas_lonlat

! --------------------------------------------------------------------------------------------------
!> Fill ATLAS fieldset
subroutine geom_fill_atlas_fieldset(self, afieldset)
  class(ucldas_geom),  intent(inout) :: self
  type(atlas_fieldset), intent(inout) :: afieldset

  integer :: i, jz, n
  integer, pointer :: int_ptr_2(:,:)
  real(kind=kind_real), pointer :: real_ptr_1(:), real_ptr_2(:,:)
  type(atlas_field) :: afield

  ! Add area
  afield = self%afunctionspace%create_field(name='area', kind=atlas_real(kind_real), levels=0)
  call afield%data(real_ptr_1)
  real_ptr_1 = reshape(self%cell_area(self%isc:self%iec,self%jsc:self%jec),(/(self%iec-self%isc+1)*(self%jec-self%jsc+1)/))
  call afieldset%add(afield)
  call afield%final()

  ! Add vertical unit
  afield = self%afunctionspace%create_field(name='vunit', kind=atlas_real(kind_real), levels=self%nzo)
  call afield%data(real_ptr_2)
  do jz=1,self%nzo
    real_ptr_2(jz,:) = real(jz, kind_real)
  end do
  call afieldset%add(afield)
  call afield%final()

  ! Add geographical mask
  afield = self%afunctionspace%create_field(name='gmask', kind=atlas_integer(kind(0)), levels=self%nzo)
  call afield%data(int_ptr_2)
  do jz=1,self%nzo
    int_ptr_2(jz,:) = int(reshape(self%mask2d(self%isc:self%iec,self%jsc:self%jec), &
  & (/(self%iec-self%isc+1)*(self%jec-self%jsc+1)/)))
  end do
  call afieldset%add(afield)
  call afield%final()

end subroutine geom_fill_atlas_fieldset

! ------------------------------------------------------------------------------
!> Clone, self = other
subroutine geom_clone(self, other)
  class(ucldas_geom), intent(inout) :: self
  class(ucldas_geom), intent(in) :: other

  ! Clone communicator
  self%f_comm = other%f_comm

  ! Clone fms domain and vertical levels
  self%Domain => other%Domain
  self%nzo = other%nzo

  !
  self%geom_grid_file = other%geom_grid_file

  ! Allocate and clone geometry
  call geom_allocate(self)
  self%lonh = other%lonh
  self%lath = other%lath
  self%lonq = other%lonq
  self%latq = other%latq
  self%lon = other%lon
  self%lat = other%lat
  self%lonu = other%lonu
  self%latu = other%latu
  self%lonv = other%lonv
  self%latv = other%latv
  self%sin_rot = other%sin_rot
  self%cos_rot = other%cos_rot
  self%mask2d = other%mask2d
  self%mask2du = other%mask2du
  self%mask2dv = other%mask2dv
  self%cell_area = other%cell_area
  self%rossby_radius = other%rossby_radius
  self%distance_from_coast = other%distance_from_coast
  self%h = other%h
  call other%fields_metadata%clone(self%fields_metadata)
end subroutine geom_clone

! ------------------------------------------------------------------------------
!>
subroutine geom_gridgen(self)
  class(ucldas_geom), intent(inout) :: self

  ! allocate variables for regridding to zstar coord
  type(ucldas_ucland_config) :: ucland_config
  type(diag_remap_ctrl) :: remap_ctrl
  type(EOS_type), pointer :: eqn_of_state
  integer :: k
  real(kind=kind_real), allocatable :: tracer(:,:,:)
  logical :: answers_2018 = .false.

  ! Generate grid
  call ucldas_ucland_init(ucland_config, partial_init=.true.)
  self%lonh = ucland_config%grid%gridlont
  self%lath = ucland_config%grid%gridlatt
  self%lonq = ucland_config%grid%gridlonb
  self%latq = ucland_config%grid%gridlatb
  self%lon = ucland_config%grid%GeoLonT
  self%lat = ucland_config%grid%GeoLatT
  self%lonu = ucland_config%grid%geoLonCu
  self%latu = ucland_config%grid%geoLatCu
  self%lonv = ucland_config%grid%geoLonCv
  self%latv = ucland_config%grid%geoLatCv

  self%sin_rot = ucland_config%grid%sin_rot
  self%cos_rot = ucland_config%grid%cos_rot

  self%mask2d = ucland_config%grid%mask2dT
  self%mask2du = ucland_config%grid%mask2dCu
  self%mask2dv = ucland_config%grid%mask2dCv
  self%cell_area  = ucland_config%grid%areaT
  self%h = ucland_config%LND_CSp%h

  ! Setup intermediate zstar coordinate
  allocate(tracer(self%isd:self%ied, self%jsd:self%jed, self%nzo))
  tracer = 0.d0 ! dummy tracer
  call diag_remap_init(remap_ctrl, coord_tuple='ZSTAR, ZSTAR, ZSTAR', answers_2018=answers_2018)
  call diag_remap_configure_axes(remap_ctrl, ucland_config%GV, ucland_config%scaling, ucland_config%param_file)
  self%nzo_zstar = remap_ctrl%nz
  if (allocated(self%h_zstar)) deallocate(self%h_zstar)
  allocate(self%h_zstar(self%isd:self%ied, self%jsd:self%jed, 1:remap_ctrl%nz))

  ! Compute intermediate vertical coordinate self%h_zstar
  call diag_remap_update(remap_ctrl, &
                         ucland_config%grid, &
                         ucland_config%GV, &
                         ucland_config%scaling, &
                         self%h, tracer, tracer, eqn_of_state, self%h_zstar)
  call diag_remap_end(remap_ctrl)

  ! Get Rossby Radius
  call geom_rossby_radius(self)

  call geom_distance_from_coast(self)

  ! Output to file
  call geom_write(self)

end subroutine geom_gridgen

! ------------------------------------------------------------------------------
!> Allocate memory and point to ucland data structure
subroutine geom_allocate(self)
  class(ucldas_geom), intent(inout) :: self

  integer :: nzo
  integer :: isd, ied, jsd, jed

  ! Get domain shape (number of levels, indices of data and compute domain)
  call geom_get_domain_indices(self, "compute", self%isc, self%iec, self%jsc, self%jec)
  call geom_get_domain_indices(self, "data", isd, ied, jsd, jed)
  self%isd = isd ;  self%ied = ied ; self%jsd = jsd; self%jed = jed
  call geom_get_domain_indices(self, "global", self%isg, self%ieg, self%jsg, self%jeg)
  call geom_get_domain_indices(self, "compute", self%iscl, self%iecl, self%jscl, self%jecl, local=.true.)
  call geom_get_domain_indices(self, "data", self%isdl, self%iedl, self%jsdl, self%jedl, local=.true.)
  nzo = self%nzo

  ! Allocate arrays on compute domain
  allocate(self%lonh(self%isg:self%ieg));        self%lonh = 0.0_kind_real
  allocate(self%lath(self%jsg:self%jeg));        self%lath = 0.0_kind_real
  allocate(self%lonq(self%isg:self%ieg));        self%lonq = 0.0_kind_real
  allocate(self%latq(self%jsg:self%jeg));        self%latq = 0.0_kind_real
  allocate(self%lon(isd:ied,jsd:jed));           self%lon = 0.0_kind_real
  allocate(self%lat(isd:ied,jsd:jed));           self%lat = 0.0_kind_real
  allocate(self%lonu(isd:ied,jsd:jed));          self%lonu = 0.0_kind_real
  allocate(self%latu(isd:ied,jsd:jed));          self%latu = 0.0_kind_real
  allocate(self%lonv(isd:ied,jsd:jed));          self%lonv = 0.0_kind_real
  allocate(self%latv(isd:ied,jsd:jed));          self%latv = 0.0_kind_real

  allocate(self%sin_rot(isd:ied,jsd:jed));       self%sin_rot = 0.0_kind_real
  allocate(self%cos_rot(isd:ied,jsd:jed));       self%cos_rot = 0.0_kind_real

  allocate(self%mask2d(isd:ied,jsd:jed));        self%mask2d = 0.0_kind_real
  allocate(self%mask2du(isd:ied,jsd:jed));       self%mask2du = 0.0_kind_real
  allocate(self%mask2dv(isd:ied,jsd:jed));       self%mask2dv = 0.0_kind_real

  allocate(self%cell_area(isd:ied,jsd:jed));     self%cell_area = 0.0_kind_real
  allocate(self%rossby_radius(isd:ied,jsd:jed)); self%rossby_radius = 0.0_kind_real
  allocate(self%distance_from_coast(isd:ied,jsd:jed)); self%distance_from_coast = 0.0_kind_real
  allocate(self%h(isd:ied,jsd:jed,1:nzo));       self%h = 0.0_kind_real

end subroutine geom_allocate

! ------------------------------------------------------------------------------
!> Calcuate distance from coast for the ocean points
subroutine geom_distance_from_coast(self)
  class(ucldas_geom), intent(inout) :: self
  type(atlas_indexkdtree) :: kd
  type(atlas_geometry) :: ageometry

  integer :: i, j, idx(1)
  integer :: num_land_l, num_land
  integer, allocatable :: rcvcnt(:), displs(:)
  real(kind=kind_real), allocatable :: land_lon(:), land_lat(:)
  real(kind=kind_real), allocatable :: land_lon_l(:), land_lat_l(:)
  real(kind=kind_real) :: closest_lon, closest_lat


  ! collect lat/lon of all land points on all procs
  ! (use the tracer grid and mask for this)
  ! --------------------------------------------------
  allocate(rcvcnt(self%f_comm%size()))
  allocate(displs(self%f_comm%size()))

  num_land_l = count(self%mask2d(self%isc:self%iec, self%jsc:self%jec)==0.0)
  call self%f_comm%allgather(num_land_l, rcvcnt)
  num_land = sum(rcvcnt)

  displs(1) = 0
  do j = 2, self%f_comm%size()
    displs(j) = displs(j-1) + rcvcnt(j-1)
  enddo

  allocate(land_lon_l(num_land_l))
  allocate(land_lat_l(num_land_l))
  land_lon_l = pack(self%lon(self%isc:self%iec, self%jsc:self%jec), &
                  mask=self%mask2d(self%isc:self%iec,self%jsc:self%jec)==0.0)
  land_lat_l = pack(self%lat(self%isc:self%iec, self%jsc:self%jec), &
                  mask=self%mask2d(self%isc:self%iec,self%jsc:self%jec)==0.0)
  allocate(land_lon(num_land))
  allocate(land_lat(num_land))

  call self%f_comm%allgather(land_lon_l, land_lon, num_land_l, rcvcnt, displs)
  call self%f_comm%allgather(land_lat_l, land_lat, num_land_l, rcvcnt, displs)


  ! pass land points to the kd tree
  !---------------------------------------
  ageometry = atlas_geometry("Earth") !< TODO: remove this hardcoded value so
                                      ! we can do DA on Europa at some point.
                                      ! (Next AOP??)
  kd = atlas_indexkdtree(ageometry)
  call kd%reserve(num_land)
  call kd%build(num_land, land_lon, land_lat)


  ! for each point in local domain, lookup distance to nearest land point
  ! ---------------------------------------
  do i = self%isc, self%iec
    do j = self%jsc, self%jec
      call kd%closestPoints( self%lon(i,j), self%lat(i,j), 1, idx )
      self%distance_from_coast(i,j) = ageometry%distance( &
            self%lon(i,j), self%lat(i,j), land_lon(idx(1)), land_lat(idx(1)))
    enddo
  enddo

  ! cleanup
  call kd%final()

end subroutine

! ------------------------------------------------------------------------------
!> Read and store Rossby Radius of deformation
subroutine geom_rossby_radius(self)
  class(ucldas_geom), intent(inout) :: self

  integer :: unit, i, n
  real(kind=kind_real) :: dum
  real(kind=kind_real), allocatable :: lon(:),lat(:),rr(:)
  integer :: isc, iec, jsc, jec
  integer :: io

  ! read in the file
  unit = 20
  open(unit=unit,file="rossrad.dat",status="old",action="read")
  n = 0
  do
     read(unit,*,iostat=io)
     if (io/=0) exit
     n = n+1
  end do
  rewind(unit)
  allocate(lon(n),lat(n),rr(n))
  do i = 1, n
     read(unit,*) lat(i),lon(i),dum,rr(i)
  end do
  close(unit)

  ! convert to meters
  rr = rr * 1e3

  ! remap
  isc = self%isc ;  iec = self%iec ; jsc = self%jsc ; jec = self%jec
  call ucldas_remap_idw(lon, lat, rr, self%lon(isc:iec,jsc:jec), &
                      self%lat(isc:iec,jsc:jec), self%rossby_radius(isc:iec,jsc:jec) )

end subroutine geom_rossby_radius


! ------------------------------------------------------------------------------
!> Write geometry to file
subroutine geom_write(self)
  class(ucldas_geom), intent(in) :: self

  character(len=256) :: geom_output_pe
  integer :: pe
  character(len=8) :: fmt = '(I5.5)'
  character(len=1024) :: strpe
  integer :: ns
  integer :: idr_geom
  type(restart_file_type) :: geom_restart

  ! Save global domain
  call fms_io_init()
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lonh', &
                                   &self%lonh(:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lath', &
                                   &self%lath(:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lonq', &
                                   &self%lonq(:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'latq', &
                                   &self%latq(:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lon', &
                                   &self%lon(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lat', &
                                   &self%lat(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lonu', &
                                   &self%lonu(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'latu', &
                                   &self%latu(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lonv', &
                                   &self%lonv(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'latv', &
                                   &self%latv(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'sin_rot', &
                                   &self%sin_rot(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'cos_rot', &
                                   &self%cos_rot(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'area', &
                                   &self%cell_area(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'rossby_radius', &
                                   &self%rossby_radius(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'mask2d', &
                                   &self%mask2d(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'mask2du', &
                                   &self%mask2du(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'mask2dv', &
                                   &self%mask2dv(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'h', &
                                   &self%h(:,:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'nzo_zstar', &
                                   &self%nzo_zstar, &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'h_zstar', &
                                   &self%h_zstar(:,:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'distance_from_coast', &
                                   &self%distance_from_coast(:,:), &
                                   domain=self%Domain%mpp_domain)

  call save_restart(geom_restart, directory='')
  call free_restart_type(geom_restart)
  call fms_io_exit()

  if (self%save_local_domain) then
     ! Save local compute grid
     pe = self%f_comm%rank()

     write (strpe,fmt) pe
     geom_output_pe='geom_output_'//trim(strpe)//'.nc'

     ns = (self%iec - self%isc + 1) * (self%jec - self%jsc + 1 )
     call write2pe(reshape(self%mask2d(self%isc:self%iec,self%jsc:self%jec),(/ns/)),'mask',geom_output_pe,.false.)
     call write2pe(reshape(self%lon(self%isc:self%iec,self%jsc:self%jec),(/ns/)),'lon',geom_output_pe,.true.)
     call write2pe(reshape(self%lat(self%isc:self%iec,self%jsc:self%jec),(/ns/)),'lat',geom_output_pe,.true.)
  end if

end subroutine geom_write

! ------------------------------------------------------------------------------
!> Read geometry from file
subroutine geom_read(self)
  class(ucldas_geom), intent(inout) :: self

  integer :: idr_geom
  type(restart_file_type) :: geom_restart

  call fms_io_init()
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lonh', &
                                   &self%lonh(:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lath', &
                                   &self%lath(:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lonq', &
                                   &self%lonq(:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'latq', &
                                   &self%latq(:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lon', &
                                   &self%lon(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lat', &
                                   &self%lat(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lonu', &
                                   &self%lonu(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'latu', &
                                   &self%latu(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'lonv', &
                                   &self%lonv(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'latv', &
                                   &self%latv(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'sin_rot', &
                                   &self%sin_rot(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'cos_rot', &
                                   &self%cos_rot(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'area', &
                                   &self%cell_area(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'rossby_radius', &
                                   &self%rossby_radius(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'mask2d', &
                                   &self%mask2d(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'mask2du', &
                                   &self%mask2du(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'mask2dv', &
                                   &self%mask2dv(:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'h', &
                                   &self%h(:,:,:), &
                                   domain=self%Domain%mpp_domain)
  idr_geom = register_restart_field(geom_restart, &
                                   &self%geom_grid_file, &
                                   &'distance_from_coast', &
                                   &self%distance_from_coast(:,:), &
                                   domain=self%Domain%mpp_domain)
  call restore_state(geom_restart, directory='')
  call free_restart_type(geom_restart)
  call fms_io_exit()

end subroutine geom_read

! ------------------------------------------------------------------------------
!> Get indices for compute or data domain
subroutine geom_get_domain_indices(self, domain_type, is, ie, js, je, local)
  class(ucldas_geom), intent(in) :: self
  character(len=*),       intent(in) :: domain_type
  integer,               intent(out) :: is, ie, js, je
  logical,      optional, intent(in) :: local

  integer :: isc, iec, jsc, jec
  integer :: isd, ied, jsd, jed
  integer :: isg, ieg, jsg, jeg

  call mpp_get_compute_domain(self%Domain%mpp_domain,isc,iec,jsc,jec)
  call mpp_get_data_domain(self%Domain%mpp_domain,isd,ied,jsd,jed)
  call mpp_get_global_domain(self%Domain%mpp_domain, isg, ieg, jsg, jeg)
  if (present(local)) then
     isc = isc - (isd-1) ; iec = iec - (isd-1) ; ied = ied - (isd-1) ; isd = 1
     jsc = jsc - (jsd-1) ; jec = jec - (jsd-1) ; jed = jed - (jsd-1) ; jsd = 1
  end if

  select case (trim(domain_type))
  case ("compute")
     is = isc; ie = iec; js = jsc; je = jec;
  case ("data")
     is = isd; ie = ied; js = jsd; je = jed;
  case ("global")
     is = isg; ie = ieg; js = jsg; je = jeg;
  end select

end subroutine geom_get_domain_indices

! ------------------------------------------------------------------------------
!> Get layer depth from layer thicknesses
subroutine geom_thickness2depth(self, h, z)
  class(ucldas_geom),     intent(in   ) :: self
  real(kind=kind_real), intent(in   ) :: h(:,:,:) ! Layer thickness
  real(kind=kind_real), intent(inout) :: z(:,:,:) ! Mid-layer depth

  integer :: is, ie, js, je, i, j, k

  ! Should check shape of z
  is = lbound(h,dim=1)
  ie = ubound(h,dim=1)
  js = lbound(h,dim=2)
  je = ubound(h,dim=2)

  !allocate(z(is:ie, js:je, self%nzo))

  do i = is, ie
     do j = js, je
        do k = 1, self%nzo
           if (k.eq.1) then
              z(i,j,k) = 0.5_kind_real*h(i,j,k)
           else
              z(i,j,k) = sum(h(i,j,1:k-1))+0.5_kind_real*h(i,j,k)
           end if
        end do
     end do
  end do
end subroutine geom_thickness2depth

! ------------------------------------------------------------------------------
!> Copy a structured field into an ATLAS fieldset
subroutine geom_struct2atlas(self, dx_struct, dx_atlas)
  class(ucldas_geom),     intent(in ) :: self
  real(kind=kind_real), intent(in ) :: dx_struct(:,:)
  type(atlas_fieldset), intent(out) :: dx_atlas

  real(kind_real), pointer :: real_ptr(:)
  type(atlas_field) :: afield

  dx_atlas = atlas_fieldset()
  afield = self%afunctionspace%create_field('var',kind=atlas_real(kind_real),levels=0)
  call dx_atlas%add(afield)
  call afield%data(real_ptr)
  real_ptr = reshape(dx_struct(self%iscl:self%iecl, self%jscl:self%jecl),(/(self%iecl-self%iscl+1)*(self%jecl-self%jscl+1)/))
  call afield%final()

end subroutine geom_struct2atlas

! ------------------------------------------------------------------------------
!> Copy a structured field from an ATLAS fieldset
subroutine geom_atlas2struct(self, dx_struct, dx_atlas)
  class(ucldas_geom),     intent(in   ) :: self
  real(kind=kind_real), intent(inout) :: dx_struct(:,:)
  type(atlas_fieldset), intent(inout) :: dx_atlas

  real(kind_real), pointer :: real_ptr(:)
  type(atlas_field) :: afield

  afield = dx_atlas%field('var')
  call afield%data(real_ptr)
  dx_struct(self%iscl:self%iecl, self%jscl:self%jecl) = reshape(real_ptr,(/(self%iecl-self%iscl+1),(self%jecl-self%jscl+1)/))
  call afield%final()

end subroutine geom_atlas2struct

! ------------------------------------------------------------------------------

end module ucldas_geom_mod
