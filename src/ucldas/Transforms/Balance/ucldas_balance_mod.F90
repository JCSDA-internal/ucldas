! (C) Copyright 2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_balance_mod

use fckit_configuration_module, only: fckit_configuration
use fms_mod, only: read_data
use fms_io_mod, only: fms_io_init, fms_io_exit
use kinds, only: kind_real
use ucldas_geom_mod
use ucldas_fields_mod
use ucldas_increment_mod
use ucldas_state_mod
use ucldas_kst_mod, only: ucldas_kst, ucldas_soft_jacobian
use ucldas_ksshts_mod, only: ucldas_ksshts, ucldas_steric_jacobian

implicit none

private
public :: ucldas_balance_config, &
          ucldas_balance_setup, ucldas_balance_delete, &
          ucldas_balance_mult, ucldas_balance_multad, &
          ucldas_balance_multinv, ucldas_balance_multinvad

!> Fortran derived type to hold configuration D
type :: ucldas_balance_config
   integer                    :: isc, iec, jsc, jec  !> Compute domain
   type(ucldas_kst)             :: kst                 !> T/S balance
   type(ucldas_ksshts)          :: ksshts              !> SSH/T/S balance
   real(kind=kind_real), allocatable :: kct(:,:)     !> C/T Jacobian
end type ucldas_balance_config

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
!> Initialization of the balance operator and its trajectory.
!> balances always used: T,S,SSH
!> optional balances depending on input fields: cicen
subroutine ucldas_balance_setup(f_conf, self, traj, geom)
  type(fckit_configuration),   intent(in)  :: f_conf
  type(ucldas_balance_config), intent(inout) :: self
  type(ucldas_state),    target, intent(in)  :: traj
  type(ucldas_geom),     target, intent(in)  :: geom

  integer :: isc, iec, jsc, jec
  integer :: isd, ied, jsd, jed
  integer :: i, j, k, nl
  real(kind=kind_real), allocatable :: jac(:)
  type(ucldas_field), pointer :: tocn, socn, hocn, cicen, mld, layer_depth

  ! declarations related to the dynamic height Jacobians
  character(len=:), allocatable :: filename, mask_name
  real(kind=kind_real), allocatable :: jac_mask(:,:) !> mask for Jacobian
  real(kind=kind_real) :: threshold
  integer :: nlayers               !> dynamic height Jac=0 in nlayers upper layers
  logical :: mask_detadt = .false. !> if true, set deta/dt to 0
  logical :: mask_detads = .false. !> if true, set deta/ds to 0

  ! declarations related to the sea-ice Jacobian
  character(len=:), allocatable :: kct_name
  real(kind=kind_real), allocatable :: kct(:,:) !> dc/dT

  ! Indices for compute domain
  isc=geom%isc; iec=geom%iec
  jsc=geom%jsc; jec=geom%jec
  isd=geom%isd; ied=geom%ied
  jsd=geom%jsd; jed=geom%jed
  self%isc=isc; self%iec=iec
  self%jsc=jsc; self%jec=jec

  ! Setup mask for Jacobians related to the dynamic height balance
  allocate(jac_mask(isd:ied,jsd:jed))
  jac_mask = 1.0_kind_real
  nlayers = 0.0_kind_real
  if ( f_conf%has("jac_mask") ) then
    jac_mask = 0.0_kind_real
    call f_conf%get_or_die("jac_mask.filename", filename)
    call f_conf%get_or_die("jac_mask.name", mask_name)
    call f_conf%get_or_die("jac_mask.threshold", threshold)
    call f_conf%get_or_die("jac_mask.nlayers", nlayers)
    call f_conf%get_or_die("jac_mask.detadt", mask_detadt)
    call f_conf%get_or_die("jac_mask.detads", mask_detads)
    call fms_io_init()
    call read_data(filename, mask_name, jac_mask, domain=geom%Domain%mpp_domain)
    call fms_io_exit()
    where(jac_mask<threshold)
      jac_mask = 0.0_kind_real
    end where
  end if

  ! Get configuration for Kst
  call f_conf%get_or_die("dsdtmax", self%kst%dsdtmax)
  call f_conf%get_or_die("dsdzmin", self%kst%dsdzmin)
  call f_conf%get_or_die("dtdzmin", self%kst%dtdzmin)
  call f_conf%get_or_die("nlayers", self%kst%nlayers) ! Set jac to 0 in the
                                                      ! nlayers top layers

  ! Get required fields
  call traj%get("tocn", tocn)
  call traj%get("socn", socn)
  call traj%get("hocn", hocn)
  call traj%get("mld", mld)
  call traj%get("layer_depth", layer_depth)
  if (traj%has("cicen"))  call traj%get("cicen", cicen)

  ! allocate space
  nl = hocn%nz
  allocate(self%kst%jacobian(isc:iec,jsc:jec,geom%nzo))
  allocate(jac(nl))
  self%kst%jacobian=0.0

  ! Compute and store Jacobian of Kst
  do i = isc, iec
     do j = jsc, jec
        jac=0.0
        call ucldas_soft_jacobian(jac,&
             &tocn%val(i,j,:),&
             &socn%val(i,j,:),&
             &hocn%val(i,j,:),&
             &self%kst%dsdtmax, self%kst%dsdzmin, self%kst%dtdzmin)
        ! Set Jacobian to 0 above mixed layer
        do k=1,nl
           if (layer_depth%val(i,j,k) < mld%val(i,j,1)) then
              jac(k) = 0.0_kind_Real
           end if
        end do
        self%kst%jacobian(i,j,:) = jac(:)
        self%kst%jacobian(i,j,1:self%kst%nlayers) =  0.0_kind_real
     end do
  end do
  deallocate(jac)

  ! Compute Jacobian of Ksshts
  allocate(self%ksshts%kssht, mold=self%kst%jacobian)
  allocate(self%ksshts%ksshs, mold=self%kst%jacobian)
  allocate(jac(2))
  self%ksshts%kssht=0.0_kind_real
  self%ksshts%ksshs=0.0_kind_real
  do i = isc, iec
    do j = jsc, jec
      do k = 1, nl
        call ucldas_steric_jacobian (jac, &
        tocn%val(i,j,k), &
        socn%val(i,j,k), &
        &layer_depth%val(i,j,k),&
        &hocn%val(i,j,k),&
        &geom%lon(i,j),&
        &geom%lat(i,j))
        self%ksshts%kssht(i,j,k) = jac(1)*jac_mask(i,j)
        self%ksshts%ksshs(i,j,k) = jac(2)*jac_mask(i,j)
      end do
      if (nlayers>0) then
        self%ksshts%kssht(i,j,1:nlayers) =  0.0_kind_real
        self%ksshts%ksshs(i,j,1:nlayers) =  0.0_kind_real
      end if
    end do
  end do
  deallocate(jac)

  ! Zero-out Jacobians if required by configuration
  if (mask_detadt) self%ksshts%kssht = 0.0_kind_real
  if (mask_detads) self%ksshts%ksshs = 0.0_kind_real

  ! Compute Kct
  if (traj%has("cicen")) then
    ! Setup dc/dT
    allocate(kct(isd:ied,jsd:jed))
    kct = 0.0_kind_real
    if ( f_conf%has("dcdt") ) then
      call f_conf%get_or_die("dcdt.filename", filename)
      call f_conf%get_or_die("dcdt.name", kct_name)
      call fms_io_init()
      call read_data(filename, kct_name, kct, domain=geom%Domain%mpp_domain)
      call fms_io_exit()
    end if
    allocate(self%kct(isc:iec,jsc:jec))
    self%kct = 0.0_kind_real
    do i = isc, iec
      do j = jsc, jec
          if (sum(cicen%val(i,j,:)) > 1.0e-3_kind_real) then
            self%kct = kct(i,j)
          end if
      end do
    end do
  end if

end subroutine ucldas_balance_setup

! ------------------------------------------------------------------------------
!> Destructor for the balance oprator
subroutine ucldas_balance_delete(self)
  type(ucldas_balance_config), intent(inout) :: self

  ! the following always exist
  deallocate(self%kst%jacobian)
  deallocate(self%ksshts%kssht)
  deallocate(self%ksshts%ksshs)

  ! only exists if cicen was given
  if (allocated(self%kct)) deallocate(self%kct)

end subroutine ucldas_balance_delete

! ------------------------------------------------------------------------------
! Apply forward balance operator
subroutine ucldas_balance_mult(self, dxa, dxm)
  type(ucldas_balance_config), intent(in) :: self
  type(ucldas_increment),      intent(in) :: dxa
  type(ucldas_increment),   intent(inout) :: dxm

  type(ucldas_field), pointer :: fld_m, fld_a
  type(ucldas_field), pointer :: tocn_a, socn_a

  integer :: i, j, k, n

  !>    [ I       0   0  0 ]
  !>    [ Kst     I   0  0 ]
  !> K= [ Ketat Ketas I  0 ]
  !>    [ Kct     0   0  I ]

  call dxa%get("tocn",tocn_a)
  call dxa%get("socn",socn_a)

  do n=1, size(dxm%fields)
    fld_m => dxm%fields(n)
    fld_a => dxa%fields(n)

    do i = self%isc, self%iec
      do j = self%jsc, self%jec
        select case(fld_m%name)
        case default
          fld_m%val(i,j,:) = fld_a%val(i,j,:)

        case("socn") ! Salinity
          fld_m%val(i,j,:) = fld_a%val(i,j,:) + &
            & self%kst%jacobian(i,j,:) * tocn_a%val(i,j,:)

        case ("ssh") ! SSH
          fld_m%val(i,j,:) = fld_a%val(i,j,:)
          do k = 1, tocn_a%nz
            fld_m%val(i,j,:) = fld_m%val(i,j,:) + &
              & self%ksshts%kssht(i,j,k) * tocn_a%val(i,j,k) + &
              & self%ksshts%ksshs(i,j,k) * socn_a%val(i,j,k)
          end do

        case ("cicen") ! Ice fraction
          do k = 1, fld_m%nz
            fld_m%val(i,j,k) = fld_a%val(i,j,k) + &
              & self%kct(i,j) * tocn_a%val(i,j,1)
          end do

        end select
      end do
    end do
  end do
end subroutine ucldas_balance_mult

! ------------------------------------------------------------------------------
! Apply backward balance operator
subroutine ucldas_balance_multad(self, dxa, dxm)
  type(ucldas_balance_config), intent(in) :: self
  type(ucldas_increment),      intent(in) :: dxm
  type(ucldas_increment),   intent(inout) :: dxa

  type(ucldas_field), pointer :: fld_a, fld_m
  type(ucldas_field), pointer :: socn_m, ssh_m, cicen_m
  integer :: i, j, n

  cicen_m => null()

  call dxm%get("socn", socn_m)
  call dxm%get("ssh",  ssh_m)
  if (dxm%has("cicen")) call dxm%get("cicen",cicen_m)

  do n = 1, size(dxa%fields)
    fld_a => dxa%fields(n)
    fld_m => dxm%fields(n)

    do i = self%isc, self%iec
      do j = self%jsc, self%jec
        select case(fld_a%name)
        case default
          fld_a%val(i,j,:) = fld_m%val(i,j,:)

        case ("tocn") ! Temperature
          fld_a%val(i,j,:) = fld_m%val(i,j,:) + &
            & self%kst%jacobian(i,j,:) * socn_m%val(i,j,:) + &
            & self%ksshts%kssht(i,j,:) * ssh_m%val(i,j,1)

          if (associated(cicen_m)) then ! use cicen only if present
            fld_a%val(i,j,1) = fld_a%val(i,j,1) + &
              & self%kct(i,j) * sum(cicen_m%val(i,j,:))
          end if

        case ("socn") ! Salinity
          fld_a%val(i,j,:) = fld_m%val(i,j,:) + &
            & self%ksshts%ksshs(i,j,:) * ssh_m%val(i,j, 1)

        end select
      end do
    end do
  end do
end subroutine ucldas_balance_multad

! ------------------------------------------------------------------------------
! Apply inverse of the forward balance operator
subroutine ucldas_balance_multinv(self, dxa, dxm)
  type(ucldas_balance_config), intent(in) :: self
  type(ucldas_increment),      intent(in) :: dxm
  type(ucldas_increment),   intent(inout) :: dxa

  integer :: i, j, k, n
  type(ucldas_field), pointer :: fld_m, fld_a
  type(ucldas_field), pointer :: tocn_m, socn_m

  call dxm%get("tocn", tocn_m)
  call dxm%get("socn", socn_m)

  do n = 1, size(dxa%fields)
    fld_a => dxa%fields(n)
    fld_m => dxm%fields(n)

    do i = self%isc, self%iec
      do j = self%jsc, self%jec
        select case(fld_a%name)
        case default
          fld_a%val(i,j,:) = fld_m%val(i,j,:)

        case ('socn') ! Salinity
          fld_a%val(i,j,:) = fld_m%val(i,j,:) - &
            & self%kst%jacobian(i,j,:) * tocn_m%val(i,j,:)

        case ('ssh') ! SSH
          fld_a%val(i,j, :) = fld_m%val(i,j, :)
          do k = 1, tocn_m%nz
            fld_a%val(i,j,:) = fld_a%val(i,j,:) + &
              & ( self%ksshts%ksshs(i,j,k) * self%kst%jacobian(i,j,k) - &
              & self%ksshts%kssht(i,j,k) ) *  tocn_m%val(i,j,k) - &
              & self%ksshts%ksshs(i,j,k) * socn_m%val(i,j,k)
          end do

        case ('cicen') ! Ice fraction
          fld_a%val(i,j,:) =  fld_m%val(i,j,:)
          do k = 1, fld_m%nz
            fld_a%val(i,j,k) = fld_a%val(i,j,k) - &
              & self%kct(i,j) * tocn_m%val(i,j,1)
          end do

        end select
      end do
    end do
  end do
end subroutine ucldas_balance_multinv

! ------------------------------------------------------------------------------
! Apply inverse of the backward balance operator
subroutine ucldas_balance_multinvad(self, dxa, dxm)
  type(ucldas_balance_config), intent(in) :: self
  type(ucldas_increment),   intent(inout) :: dxm
  type(ucldas_increment),      intent(in) :: dxa

  integer :: i, j, n
  type(ucldas_field), pointer :: fld_a, fld_m
  type(ucldas_field), pointer :: socn_a, ssh_a, cicen_a

  cicen_a => null()

  call dxa%get("socn", socn_a)
  call dxa%get("ssh",  ssh_a)
  if (dxa%has("cicen")) call dxa%get("cicen",cicen_a)

  do n = 1, size(dxm%fields)
    fld_m => dxm%fields(n)
    fld_a => dxa%fields(n)

    do i = self%isc, self%iec
      do j = self%jsc, self%jec
        select case (fld_m%name)
        case default
          fld_m%val(i,j,:) = fld_a%val(i,j,:)

        case ('tocn') ! Temperature
          fld_m%val(i,j,:) = fld_a%val(i,j,:) &
            & - self%kst%jacobian(i,j,:) * socn_a%val(i,j,:) &
            & + ( self%ksshts%ksshs(i,j,:) * self%kst%jacobian(i,j,:) &
            &     - self%ksshts%kssht(i,j,:) ) * ssh_a%val(i,j,1)

          if (associated(cicen_a)) then ! use cicen only if present
            fld_m%val(i,j,1) = fld_m%val(i,j,1) &
              & - self%kct(i,j) * sum(cicen_a%val(i,j,:))
          end if

        case ('socn') ! Salinity
          fld_m%val(i,j,:) = fld_a%val(i,j,:) - &
            & self%ksshts%ksshs(i,j,:) * ssh_a%val(i,j,1)

        end select
      end do
    end do
  end do
end subroutine ucldas_balance_multinvad

end module ucldas_balance_mod
