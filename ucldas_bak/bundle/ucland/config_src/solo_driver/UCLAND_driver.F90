program UCLAND_main

  use UCLAND_cpu_clock,       only : cpu_clock_id, cpu_clock_begin, cpu_clock_end
  use UCLAND_domains,         only : UCLAND_infra_init, UCLAND_infra_end
  use UCLAND_error_handler,   only : UCLAND_error, UCLAND_mesg, WARNING, FATAL, is_root_pe
  use UCLAND_error_handler,   only : callTree_enter, callTree_leave, callTree_waypoint
! use UCLAND_get_input,       only : Get_UCLAND_Input, directories
  use UCLAND_get_input,       only : directories
! use UCLAND_io,              only : UCLAND_io_init, file_exists, open_file, close_file
  use UCLAND_io,              only : file_exists, open_file, close_file
  use UCLAND_io,              only : check_nml_error, io_infra_init, io_infra_end
  use UCLAND_io,              only : APPEND_FILE, ASCII_FILE, READONLY_FILE, SINGLE_FILE
  use UCLAND_string_functions,only : uppercase
  use UCLAND_time_manager,    only : time_type, set_date, get_date
  use UCLAND_time_manager,    only : real_to_time, time_type_to_real
  use UCLAND_time_manager,    only : increment_date, set_calendar_type, month_name
  use UCLAND_time_manager,    only : JULIAN, GREGORIAN, NOLEAP, THIRTY_DAY_MONTHS, NO_CALENDAR

  use UCLAND_write_cputime,   only : write_cputime_start_clock, write_cputime_CS

  implicit none

  ! A structure containing several relevant directory paths.
  type(directories) :: dirs

  type(time_type) :: Start_time         ! The start time of the simulation.

  integer :: date_init(6)=0                ! The start date of the whole simulation.
  integer :: date(6)=-1                    ! Possibly the start date of this run segment.

  character(len=9)  :: month
  character(len=16) :: calendar = 'julian'
  integer :: calendar_type=-1

  integer :: unit, io_status, ierr

  integer :: initClock, mainClock, termClock

  type(write_cputime_CS),    pointer :: write_CPU_CSp => NULL()

  !=====================================================================

  call write_cputime_start_clock(write_CPU_CSp)

  call UCLAND_infra_init() ; call io_infra_init()

  ! These clocks are on the global pelist.
  initClock = cpu_clock_id( 'Initialization' )
  mainClock = cpu_clock_id( 'Main loop' )
  termClock = cpu_clock_id( 'Termination' )
  call cpu_clock_begin(initClock)
  call UCLAND_mesg('======== Model being driven by UCLAND_driver ========', 2)
  call callTree_waypoint("Program UCLAND_driver.F90")

  if (file_exists('input.nml')) then
    ! Provide for namelist specification of the run length and calendar data.
    call open_file(unit, 'input.nml', form=ASCII_FILE, action=READONLY_FILE)
    !read(unit, ice_solo_nml, iostat=io_status)
    call close_file(unit)
    !ierr = check_nml_error(io_status,'ice_solo_nml')
    !if (years+months+days+hours+minutes+seconds > 0) then
    !  if (is_root_pe()) write(*,ice_solo_nml)
    !endif
  endif

  ! Read land_solo restart, which can override settings from the namelist.
  if (file_exists(trim(dirs%restart_input_dir)//'land_solo.res')) then
    call open_file(unit,trim(dirs%restart_input_dir)//'land_solo.res', &
                   form=ASCII_FILE,action=READONLY_FILE)
    read(unit,*) calendar_type
    read(unit,*) date_init
    read(unit,*) date
    call close_file(unit)
  else
    calendar = uppercase(calendar)
    if (calendar(1:6) == 'JULIAN') then ;        calendar_type = JULIAN
    elseif (calendar(1:9) == 'GREGORIAN') then ; calendar_type = GREGORIAN
    elseif (calendar(1:6) == 'NOLEAP') then ;    calendar_type = NOLEAP
    elseif (calendar(1:10)=='THIRTY_DAY') then ; calendar_type = THIRTY_DAY_MONTHS
    elseif (calendar(1:11)=='NO_CALENDAR') then; calendar_type = NO_CALENDAR
    elseif (calendar(1:1) /= ' ') then
      call UCLAND_error(FATAL,'UCLAND_driver: Invalid namelist value '//trim(calendar)//' for calendar')
    else
      call UCLAND_error(FATAL,'UCLAND_driver: No namelist value for calendar')
    endif
  endif
  call set_calendar_type(calendar_type)

  if (sum(date_init) > 0) then
    Start_time = set_date(date_init(1),date_init(2), date_init(3), &
         date_init(4),date_init(5),date_init(6))
  else
    Start_time = real_to_time(0.0)
  endif

  call io_infra_end ; call UCLAND_infra_end

end program UCLAND_main
