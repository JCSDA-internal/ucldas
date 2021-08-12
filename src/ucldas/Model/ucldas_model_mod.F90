! (C) Copyright 2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Structure holding configuration variables for the model

module ucldas_model_mod

use fckit_mpi_module, only: fckit_mpi_comm
use fms_io_mod, only : fms_io_init, fms_io_exit
use kinds, only: kind_real
use ucldas_geom_mod, only: ucldas_geom
use ucldas_ucland, only: ucldas_ucland_config, ucldas_ucland_init, ucldas_ucland_end
use ucldas_utils, only: ucldas_str2int
use ucldas_state_mod
use ucldas_fields_mod
use datetime_mod, only: datetime, datetime_to_string
use mpp_domains_mod, only : mpp_update_domains
use time_manager_mod, only : time_type, print_time, print_date, set_date
use LND, only : step_LND
use LND_restart, only : save_restart
use LND_surface_forcing, only : set_forcing
use LND_time_manager, only : real_to_time, time_type_to_real
use LND_time_manager, only : operator(+)

implicit none

private
public :: ucldas_model
public :: ucldas_setup
public :: ucldas_initialize_integration
public :: ucldas_finalize_integration
public :: ucldas_propagate
public :: ucldas_delete

!> Fortran derived type to hold configuration data for the model
type :: ucldas_model
   integer :: advance_ucland      !< call ucland step if true
   real(kind=kind_real) :: dt0  !< dimensional time (seconds)
   type(ucldas_ucland_config) :: ucland_config  !< UCLAND data structure
   real(kind_real), dimension(2) :: tocn_minmax, socn_minmax  !< min, max values
end type ucldas_model

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
!> Initialize model's data structure
subroutine ucldas_setup(self, geom)
  type(ucldas_model), intent(inout) :: self
  type(ucldas_geom),     intent(in) :: geom

  self%ucland_config%f_comm = geom%f_comm
  call ucldas_ucland_init(self%ucland_config)

end subroutine ucldas_setup

! ------------------------------------------------------------------------------
!> Prepare UCLAND integration
subroutine ucldas_initialize_integration(self, flds)
  type(ucldas_model), intent(inout) :: self
  type(ucldas_state), intent(inout) :: flds
  type(ucldas_field), pointer :: field

  integer :: i

  ! for each field
  do i=1,size(flds%fields)
    call flds%get(flds%fields(i)%name, field)

    ! Update halos
    call mpp_update_domains(field%val, flds%geom%Domain%mpp_domain)

    ! impose bounds, and set UCLAND state
    select case (field%name)
    case ("tocn")
      if ( self%tocn_minmax(1) /= real(-999., kind=8) ) &
        where( field%val < self%tocn_minmax(1) ) field%val = self%tocn_minmax(1)
      if ( self%tocn_minmax(2) /= real(-999., kind=8) ) &
        where( field%val > self%tocn_minmax(2) ) field%val = self%tocn_minmax(2)
      self%ucland_config%LND_CSp%T = real(field%val, kind=8)
    case ("socn")
      if ( self%socn_minmax(1) /= real(-999., kind=8) ) &
        where( field%val < self%socn_minmax(1) ) field%val = self%socn_minmax(1)
      if ( self%socn_minmax(2) /= real(-999., kind=8) ) &
        where( field%val > self%socn_minmax(2) ) field%val = self%socn_minmax(2)
      self%ucland_config%LND_CSp%S = real(field%val, kind=8)
    case ("uocn")
      self%ucland_config%LND_CSp%u = real(field%val, kind=8)
    case ("vocn")
      self%ucland_config%LND_CSp%v = real(field%val, kind=8)
    end select

    ! update forcing
    select case(field%name)
    case ("sw")
      field%val(:,:,1) = - real(self%ucland_config%fluxes%sw, kind=kind_real)
    case ("lw")
      field%val(:,:,1) = - real(self%ucland_config%fluxes%lw, kind=kind_real)
    case ("lhf")
      field%val(:,:,1) = - real(self%ucland_config%fluxes%latent, kind=kind_real)
    case ("shf")
      field%val(:,:,1) = - real(self%ucland_config%fluxes%sens, kind=kind_real)
    case ("us")
      field%val(:,:,1) =   real(self%ucland_config%fluxes%ustar, kind=kind_real)
    end select
  end do
end subroutine ucldas_initialize_integration

! ------------------------------------------------------------------------------
!> Advance UCLAND one baroclinic time step
subroutine ucldas_propagate(self, flds, fldsdate)
  type(ucldas_model), intent(inout) :: self
  type(ucldas_state), intent(inout) :: flds
  type(datetime),      intent(in) :: fldsdate

  type(ucldas_field), pointer :: field

  type(time_type) :: ocean_time  ! The ocean model's clock.
  integer :: year, month, day, hour, minute, second, i
  character(len=20) :: strdate

  ! Set ocean clock
  call datetime_to_string(fldsdate, strdate)
  call ucldas_str2int(strdate(1:4), year)
  call ucldas_str2int(strdate(6:7), month)
  call ucldas_str2int(strdate(9:10), day)
  call ucldas_str2int(strdate(12:13), hour)
  call ucldas_str2int(strdate(15:16), minute)
  call ucldas_str2int(strdate(18:19), second)
  self%ucland_config%Time = set_date(year, month, day, hour, minute, second)
  ocean_time = self%ucland_config%Time

  if (self%advance_ucland==1) then
     ! Set the forcing for the next steps.
     call fms_io_init()
     call set_forcing(self%ucland_config%sfc_state,&
                      self%ucland_config%forces,&
                      self%ucland_config%fluxes,&
                      self%ucland_config%Time,&
                      self%ucland_config%Time_step_ocean,&
                      self%ucland_config%grid, &
                      self%ucland_config%scaling, &
                      self%ucland_config%surface_forcing_CSp)
     call fms_io_exit()

     ! Advance LND in a single step call (advance dyna and thermo)
     call step_LND(self%ucland_config%forces, &
                   self%ucland_config%fluxes, &
                   self%ucland_config%sfc_state, &
                   self%ucland_config%Time, &
                   real(self%ucland_config%dt_forcing, kind=8), &
                   self%ucland_config%LND_CSp,&
                   start_cycle=.false.,&
                   cycle_length=self%ucland_config%LND_CSp%dt)
  end if

  ! Update ocean clock
  ocean_time = ocean_time + real_to_time(self%ucland_config%LND_CSp%dt)
  self%ucland_config%Time = ocean_time

  ! Update ucldas fields
  do i=1,size(flds%fields)
    field => flds%fields(i)
    select case(field%name)
    case ("tocn")
      field%val = real(self%ucland_config%LND_CSp%T, kind=kind_real)
    case ("socn")
      field%val = real(self%ucland_config%LND_CSp%S, kind=kind_real)
    case ("hocn")
      field%val = real(self%ucland_config%LND_CSp%h, kind=kind_real)
    case ("ssh")
      field%val(:,:,1) = real(self%ucland_config%LND_CSp%ave_ssh_ibc, kind=kind_real)
    case ("uocn")
      field%val = real(self%ucland_config%LND_CSp%u, kind=kind_real)
    case ("vocn")
      field%val = real(self%ucland_config%LND_CSp%v, kind=kind_real)
    case ("sw")
      field%val(:,:,1) = - real(self%ucland_config%fluxes%sw, kind=kind_real)
    case ("lw")
      field%val(:,:,1) = - real(self%ucland_config%fluxes%lw, kind=kind_real)
    case ("lhf")
      field%val(:,:,1) = - real(self%ucland_config%fluxes%latent, kind=kind_real)
    case ("shf")
      field%val(:,:,1) = - real(self%ucland_config%fluxes%sens, kind=kind_real)
    case ("us")
      field%val(:,:,1) = real(self%ucland_config%fluxes%ustar, kind=kind_real)
    end select
  end do
end subroutine ucldas_propagate

! ------------------------------------------------------------------------------
!> Finalize UCLAND integration: Update ucland's state and checkpoint
subroutine ucldas_finalize_integration(self, flds)
  type(ucldas_model), intent(inout) :: self
  type(ucldas_state), intent(inout) :: flds

  type(ucldas_field), pointer :: field
  integer :: i

  ! for each field
  do i=1,size(flds%fields)
    field => flds%fields(i)

    ! update halos
    call mpp_update_domains(field%val, flds%geom%Domain%mpp_domain)

    ! impose bounds and update UCLAND
    select case(field%name)
    case ("tocn")
      if ( self%tocn_minmax(1) /= real(-999., kind=8) ) &
        where( field%val < self%tocn_minmax(1) ) field%val = self%tocn_minmax(1)
      if ( self%tocn_minmax(2) /= real(-999., kind=8) ) &
        where( field%val > self%tocn_minmax(2) ) field%val = self%tocn_minmax(2)
      self%ucland_config%LND_CSp%T = real(field%val, kind=8)
    case ("socn")
      if ( self%socn_minmax(1) /= real(-999., kind=8) ) &
        where( field%val < self%socn_minmax(1) ) field%val = self%socn_minmax(1)
      if ( self%socn_minmax(2) /= real(-999., kind=8) ) &
        where( field%val > self%socn_minmax(2) ) field%val = self%socn_minmax(2)
      self%ucland_config%LND_CSp%S = real(field%val, kind=8)
    case ("uocn")
      self%ucland_config%LND_CSp%u = real(field%val, kind=8)
    case ("vocn")
      self%ucland_config%LND_CSp%v = real(field%val, kind=8)
    end select
  end do

  ! Save LND restarts with updated UCLDAS fields
  call save_restart(self%ucland_config%dirs%restart_output_dir, &
                   self%ucland_config%Time, &
                   self%ucland_config%grid, &
                   self%ucland_config%restart_CSp, &
                   GV=self%ucland_config%GV)

end subroutine ucldas_finalize_integration

! ------------------------------------------------------------------------------
!> Release memory
subroutine ucldas_delete(self)
  type(ucldas_model), intent(inout) :: self

  call ucldas_ucland_end(self%ucland_config)

end subroutine ucldas_delete

end module ucldas_model_mod
