!> Interfaces to non-domain-oriented communication subroutines, including the
!! UCLAND reproducing sums facility
module UCLAND_coms

use fms_mod, only : fms_end, UCLAND_infra_init => fms_init
use memutils_mod, only : print_memuse_stats
use mpp_mod, only : PE_here => mpp_pe, root_PE => mpp_root_pe, num_PEs => mpp_npes

implicit none ; private

public :: PE_here, root_PE, num_PEs, UCLAND_infra_init, UCLAND_infra_end

contains

!> This subroutine carries out all of the calls required to close out the infrastructure cleanly.
!! This should only be called in ocean-only runs, as the coupler takes care of this in coupled runs.
subroutine UCLAND_infra_end
  call print_memuse_stats( 'Memory HiWaterMark', always=.TRUE. )
  call fms_end
end subroutine UCLAND_infra_end

end module UCLAND_coms
