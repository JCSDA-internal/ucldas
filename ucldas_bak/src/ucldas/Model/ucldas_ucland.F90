! (C) Copyright 2017-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module ucldas_ucland

use fckit_mpi_module, only: fckit_mpi_comm
use mpp_mod,    only : mpp_init
use fms_io_mod, only : fms_io_init, fms_io_exit
use fms_mod,    only : read_data, write_data, fms_init, fms_end
use time_interp_external_mod, only : time_interp_external_init
use time_manager_mod,         only: time_type

use kinds, only: kind_real

use LND,                 only : initialize_LND, step_LND, LND_control_struct, LND_end, &
                                extract_surface_state, finish_LND_initialization, &
                                get_LND_state_elements
use LND_diag_mediator,   only : diag_ctrl
use LND_domains,         only : LND_infra_init, LND_infra_end, &
                                LND_domains_init, clone_LND_domain, LND_domain_type
use LND_error_handler,   only : LND_error, LND_mesg, WARNING, FATAL, is_root_pe
use LND_file_parser,     only : get_param, param_file_type, close_param_file
use LND_forcing_type,    only : forcing, mech_forcing, forcing_diagnostics, &
                                mech_forcing_diags, LND_forcing_chksum, &
                                LND_mech_forcing_chksum
use LND_get_input,       only : directories, Get_LND_Input, directories
use LND_grid,            only : ocean_grid_type, LND_grid_init
use LND_io,              only : open_file, close_file, &
                                check_nml_error, io_infra_init, io_infra_end, &
                                ASCII_FILE, READONLY_FILE
use LND_restart,         only : LND_restart_CS
use LND_string_functions,only : uppercase
use LND_surface_forcing, only : set_forcing, forcing_save_restart, &
                                surface_forcing_init, surface_forcing_CS
use LND_time_manager,    only : time_type, set_date, get_date, &
                                real_to_time, time_type_to_real, &
                                operator(+), operator(-), operator(*), operator(/), &
                                operator(>), operator(<), operator(>=), &
                                increment_date, set_calendar_type, month_name, &
                                JULIAN, GREGORIAN, NOLEAP, THIRTY_DAY_MONTHS, &
                                NO_CALENDAR
use LND_tracer_flow_control, only : tracer_flow_control_CS
use LND_unit_scaling,        only : unit_scale_type
use LND_variables,       only : surface
use LND_verticalGrid,    only : verticalGrid_type, &
                                verticalGridInit, verticalGridEnd

implicit none

private
public :: ucldas_geomdomain_init, &
          ucldas_ucland_init, ucldas_ucland_config, ucldas_ucland_end

!> Data structure neccessary to initialize/run ucland
type ucldas_ucland_config
  type(mech_forcing) :: forces     !< Driving mechanical surface forces
  type(forcing)      :: fluxes     !< Pointers to the thermodynamic forcing fields
                                   !< at the ocean surface.
  type(surface)      :: sfc_state  !< Pointers to the ocean surface state fields.
  real               :: dt_forcing !< Coupling time step in seconds.
  type(time_type)    :: Time_step_ocean !< time_type version of dt_forcing
  type(directories)  :: dirs       !< Relevant dirs/path
  type(time_type)    :: Time       !< Model's time before call to step_LND.
  type(unit_scale_type), pointer :: scaling !< Unit conversion factors
  type(ocean_grid_type),    pointer :: grid !< Grid metrics
  type(verticalGrid_type),  pointer :: GV   !< Vertical grid
  type(LND_control_struct), pointer :: LND_CSp  !< Tracer flow control structure.
  type(LND_restart_CS),     pointer :: restart_CSp !< A pointer to the restart control structure
  type(surface_forcing_CS), pointer :: surface_forcing_CSp => NULL()
  type(fckit_mpi_comm) :: f_comm
  type(param_file_type) :: param_file
end type ucldas_ucland_config

contains

! ------------------------------------------------------------------------------
!> Initialize ucland's domain
subroutine ucldas_geomdomain_init(Domain, nk, f_comm)
  type(LND_domain_type), pointer, intent(in) :: Domain !< Ocean model domain
  integer, intent(out)                       :: nk
  type(fckit_mpi_comm),           intent(in) :: f_comm

  type(param_file_type) :: param_file                !< Structure to parse for run-time parameters
  type(directories)     :: dirs                      !< Structure containing several relevant directory paths
  character(len=40)  :: mod_name = "ucldas_ucland" ! This module's name.

  call mpp_init(localcomm=f_comm%communicator())

  ! Initialize fms
  call fms_init()

  ! Initialize fms io
  call fms_io_init()

  ! Parse grid inputs
  call Get_LND_Input(param_file, dirs)

  ! Domain decomposition/Inintialize mpp domains
  call LND_domains_init(Domain, param_file)

  ! Get number of levels
  call get_param(param_file, mod_name, "NK", nk, fail_if_missing=.true.)

  call close_param_file(param_file)
  call fms_io_exit()

end subroutine ucldas_geomdomain_init

! ------------------------------------------------------------------------------
!> Setup/initialize/prepare ucland for time integration
subroutine ucldas_ucland_init(ucland_config, partial_init)
  type(ucldas_ucland_config), intent(out) :: ucland_config
  logical,       optional, intent(in) :: partial_init

  type(time_type) :: Start_time   ! The start time of the simulation.
  type(time_type) :: Time_in      !
  real :: dt                      ! The baroclinic dynamics time step, in seconds.
  integer :: date_init(6)=0                ! The start date of the whole simulation.
  integer :: years=0, months=0, days=0     ! These may determine the segment run
  integer :: hours=0, minutes=0, seconds=0 ! length, if read from a namelist.
  type(param_file_type) :: param_file      ! The structure indicating the file(s)
  ! containing all run-time parameters.
  character(len=16) :: calendar = 'julian'
  integer :: calendar_type=-1
  integer :: unit, io_status, ierr
  logical :: offline_tracer_mode = .false.

  type(tracer_flow_control_CS), pointer :: tracer_flow_CSp => NULL()
  type(diag_ctrl), pointer :: diag => NULL() !< Diagnostic structure
  character(len=4), parameter :: vers_num = 'v2.0'
  character(len=40)  :: mod_name = "ucldas_ucland" ! This module's name.
  integer :: ocean_nthreads = 1
  integer :: ncores_per_node = 1
  logical :: use_hyper_thread = .false.
  !integer :: omp_get_num_threads,omp_get_thread_num,get_cpu_affinity,adder,base_cpu
  namelist /ocean_solo_nml/ date_init, calendar, months, days, hours, minutes, seconds,&
       ocean_nthreads, ncores_per_node, use_hyper_thread
  integer :: param_int
  logical :: a_partial_init = .false.

  ! Check if partial ucland init is requiered
  if (present(partial_init)) a_partial_init = partial_init

  call LND_infra_init(localcomm=ucland_config%f_comm%communicator())
  call io_infra_init()

  ! Provide for namelist specification of the run length and calendar data.
  call open_file(unit, 'input.nml', form=ASCII_FILE, action=READONLY_FILE)
  read(unit, ocean_solo_nml, iostat=io_status)
  call close_file(unit)
  ierr = check_nml_error(io_status,'ocean_solo_nml')
  if (years+months+days+hours+minutes+seconds > 0) then
     if (is_root_pe()) write(*,ocean_solo_nml)
  endif
  calendar = uppercase(calendar)
  if (calendar(1:6) == 'JULIAN') then ;         calendar_type = JULIAN
  elseif (calendar(1:9) == 'GREGORIAN') then ; calendar_type = GREGORIAN
  elseif (calendar(1:6) == 'NOLEAP') then ;    calendar_type = NOLEAP
  elseif (calendar(1:10)=='THIRTY_DAY') then ; calendar_type = THIRTY_DAY_MONTHS
  elseif (calendar(1:11)=='NO_CALENDAR') then; calendar_type = NO_CALENDAR
  elseif (calendar(1:1) /= ' ') then
     call LND_error(FATAL,'LND_driver: Invalid namelist value '//trim(calendar)//' for calendar')
  else
     call LND_error(FATAL,'LND_driver: No namelist value for calendar')
  endif
  call set_calendar_type(calendar_type)

  Start_time = set_date(date_init(1),date_init(2), date_init(3), &
       date_init(4),date_init(5),date_init(6))

  call time_interp_external_init

  ! Nullify ucland_config pointers
  ucland_config%LND_CSp => NULL()
  ucland_config%restart_CSp => NULL()
  ucland_config%grid => NULL()
  ucland_config%GV => NULL()

  ! Set ucland_config%Time to time parsed from ucland config
  ucland_config%Time = Start_time

  ! Initialize ucland
  Time_in = ucland_config%Time

  call initialize_LND(ucland_config%Time, &
      Start_time, &
      param_file, &
      ucland_config%dirs, &
      ucland_config%LND_CSp, &
      ucland_config%restart_CSp, &
      offline_tracer_mode=offline_tracer_mode, diag_ptr=diag, &
      tracer_flow_CSp=tracer_flow_CSp, Time_in=Time_in)

  !US => ucland_config%scaling
  ! Continue initialization
  call get_LND_state_elements(ucland_config%LND_CSp,&
                              G=ucland_config%grid,&
                              GV=ucland_config%GV,&
                              US=ucland_config%scaling,&
                              C_p=ucland_config%fluxes%C_p)

  ucland_config%param_file = param_file
  ! Exit here for partial initialization
  if (a_partial_init) return

  ! Setup surface forcing
  call extract_surface_state(ucland_config%LND_CSp, ucland_config%sfc_state)
  call surface_forcing_init(ucland_config%Time,&
                            ucland_config%grid,&
                            ucland_config%scaling,&
                            param_file,&
                            diag,&
                            ucland_config%surface_forcing_CSp,&
                            tracer_flow_CSp)

  ! Get time step from LND config. TODO: Get DT from DA config
  call get_param(param_file, mod_name, "DT", param_int, fail_if_missing=.true.)
  dt = real(param_int)
  ucland_config%dt_forcing = dt
  ucland_config%Time_step_ocean = real_to_time(real(ucland_config%dt_forcing, kind=8))

  ! Finalize file parsing
  call close_param_file(param_file)

  ! Set the forcing for the first steps.
  call set_forcing(ucland_config%sfc_state,&
                   ucland_config%forces,&
                   ucland_config%fluxes,&
                   ucland_config%Time,&
                   ucland_config%Time_step_ocean,&
                   ucland_config%grid, &
                   ucland_config%scaling, &
                   ucland_config%surface_forcing_CSp)

  ! Do more stuff for mom init ...
  call finish_LND_initialization(ucland_config%Time,&
                                 ucland_config%dirs,&
                                 ucland_config%LND_CSp,&
                                 ucland_config%restart_CSp)

end subroutine ucldas_ucland_init

! ------------------------------------------------------------------------------
!> Release memory and possibly dump ucland's restart
subroutine ucldas_ucland_end(ucland_config)
  type(ucldas_ucland_config), intent(inout) :: ucland_config

  ! Finalize fms
  call io_infra_end

  !! as a temporary workaround to MPI_Finalize() issues, LND_infra_end is NOT called
  ! call LND_infra_end

  ! Finalize ucland
  call LND_end(ucland_config%LND_CSp)

end subroutine ucldas_ucland_end

end module ucldas_ucland
