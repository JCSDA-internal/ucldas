!> A module to monitor the overall CPU time used by UCLAND and project when to stop the model
module UCLAND_write_cputime

use UCLAND_time_manager, only : time_type, get_time!, operator(>)

implicit none !; private

!public write_cputime, UCLAND_write_cputime_init, UCLAND_write_cputime_end, write_cputime_start_clock
public write_cputime_start_clock

!-----------------------------------------------------------------------

integer :: CLOCKS_PER_SEC = 1000 !< The number of clock cycles per second, used by the system clock
integer :: MAX_TICKS      = 1000 !< The number of ticks per second, used by the system clock

!> A control structure that regulates the writing of CPU time
type, public :: write_cputime_CS ; private
  real :: maxcpu                !<   The maximum amount of cpu time per processor
                                !! for which LND should run before saving a restart
                                !! file and quiting with a return value that
                                !! indicates that further execution is required to
                                !! complete the simulation, in wall-clock seconds.
  type(time_type) :: Start_time !< The start time of the simulation.
                                !! Start_time is set in LND_initialization.F90
  real :: startup_cputime       !< The CPU time used in the startup phase of the model.
  real :: prev_cputime = 0.0    !< The last measured CPU time.
  real :: dn_dcpu_min = -1.0    !< The minimum derivative of timestep with CPU time.
  real :: cputime2 = 0.0        !< The accumulated cpu time.
  integer :: previous_calls = 0 !< The number of times write_CPUtime has been called.
  integer :: prev_n = 0         !< The value of n from the last call.
  integer :: fileCPU_ascii= -1  !< The unit number of the CPU time file.
  character(len=200) :: CPUfile !< The name of the CPU time file.
end type write_cputime_CS

contains

!> Evaluate the CPU time returned by SYSTEM_CLOCK at the start of a run
subroutine write_cputime_start_clock(CS)
  type(write_cputime_CS), pointer :: CS !< The control structure set up by a previous
                                        !! call to LND_write_cputime_init.
  integer :: new_cputime   ! The CPU time returned by SYSTEM_CLOCK
  if (.not.associated(CS)) allocate(CS)

  call SYSTEM_CLOCK(new_cputime, CLOCKS_PER_SEC, MAX_TICKS)
  CS%prev_cputime = new_cputime
end subroutine write_cputime_start_clock

end module UCLAND_write_cputime
