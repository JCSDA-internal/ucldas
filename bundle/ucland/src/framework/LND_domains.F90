!> Describes the decomposed LND domain and has routines for communications across PEs
module LND_domains

! This file is part of UCLAND. See LICENSE.md for the license.

use LND_array_transform, only : rotate_array
use LND_coms, only : PE_here, root_PE, num_PEs, LND_infra_init, LND_infra_end
use LND_coms, only : broadcast, sum_across_PEs, min_across_PEs, max_across_PEs
use LND_cpu_clock, only : cpu_clock_begin, cpu_clock_end
use LND_error_handler, only : LND_error, LND_mesg, NOTE, WARNING, FATAL, is_root_pe
use LND_file_parser, only : get_param, log_param, log_version
use LND_file_parser, only : param_file_type
use LND_string_functions, only : slasher

use mpp_domains_mod, only : mpp_define_layout, mpp_get_boundary
use mpp_domains_mod, only : LND_define_io_domain => mpp_define_io_domain
use mpp_domains_mod, only : LND_define_domain => mpp_define_domains
use mpp_domains_mod, only : domain2D, domain1D, mpp_get_data_domain
use mpp_domains_mod, only : mpp_get_compute_domain, mpp_get_global_domain
use mpp_domains_mod, only : global_field_sum => mpp_global_sum
use mpp_domains_mod, only : mpp_update_domains, CYCLIC_GLOBAL_DOMAIN, FOLD_NORTH_EDGE
use mpp_domains_mod, only : mpp_start_update_domains, mpp_complete_update_domains
use mpp_domains_mod, only : mpp_create_group_update, mpp_do_group_update
use mpp_domains_mod, only : group_pass_type => mpp_group_update_type
use mpp_domains_mod, only : mpp_reset_group_update_field
use mpp_domains_mod, only : mpp_group_update_initialized
use mpp_domains_mod, only : mpp_start_group_update, mpp_complete_group_update
use mpp_domains_mod, only : compute_block_extent => mpp_compute_block_extent
use mpp_domains_mod, only : AGRID, BGRID_NE, CGRID_NE, SCALAR_PAIR, BITWISE_EXACT_SUM
use mpp_domains_mod, only : To_East => WUPDATE, To_West => EUPDATE, Omit_Corners => EDGEUPDATE
use mpp_domains_mod, only : To_North => SUPDATE, To_South => NUPDATE
use mpp_domains_mod, only : CENTER, CORNER, NORTH_FACE => NORTH, EAST_FACE => EAST
use fms_io_mod,      only : file_exist, parse_mask_table
use fms_affinity_mod, only : fms_affinity_init, fms_affinity_set, fms_affinity_get

implicit none ; private

public :: LND_domains_init, LND_infra_init, LND_infra_end, get_domain_extent, get_domain_extent_dsamp2
public :: LND_define_domain, LND_define_io_domain, clone_LND_domain
public :: pass_var, pass_vector, PE_here, root_PE, num_PEs
public :: pass_var_start, pass_var_complete, fill_symmetric_edges, broadcast
public :: pass_vector_start, pass_vector_complete
public :: global_field_sum, sum_across_PEs, min_across_PEs, max_across_PEs
public :: AGRID, BGRID_NE, CGRID_NE, SCALAR_PAIR, BITWISE_EXACT_SUM
public :: CORNER, CENTER, NORTH_FACE, EAST_FACE
public :: To_East, To_West, To_North, To_South, To_All, Omit_Corners
public :: create_group_pass, do_group_pass, group_pass_type
public :: start_group_pass, complete_group_pass
public :: compute_block_extent, get_global_shape
public :: get_simple_array_i_ind, get_simple_array_j_ind
public :: domain2D

!> Do a halo update on an array
interface pass_var
  module procedure pass_var_3d, pass_var_2d
end interface pass_var

!> Do a halo update on a pair of arrays representing the two components of a vector
interface pass_vector
  module procedure pass_vector_3d, pass_vector_2d
end interface pass_vector

!> Initiate a non-blocking halo update on an array
interface pass_var_start
  module procedure pass_var_start_3d, pass_var_start_2d
end interface pass_var_start

!> Complete a non-blocking halo update on an array
interface pass_var_complete
  module procedure pass_var_complete_3d, pass_var_complete_2d
end interface pass_var_complete

!> Initiate a halo update on a pair of arrays representing the two components of a vector
interface pass_vector_start
  module procedure pass_vector_start_3d, pass_vector_start_2d
end interface pass_vector_start

!> Complete a halo update on a pair of arrays representing the two components of a vector
interface pass_vector_complete
  module procedure pass_vector_complete_3d, pass_vector_complete_2d
end interface pass_vector_complete

!> Set up a group of halo updates
interface create_group_pass
  module procedure create_var_group_pass_2d
  module procedure create_var_group_pass_3d
  module procedure create_vector_group_pass_2d
  module procedure create_vector_group_pass_3d
end interface create_group_pass

!> Do a set of halo updates that fill in the values at the duplicated edges
!! of a staggered symmetric memory domain
interface fill_symmetric_edges
  module procedure fill_vector_symmetric_edges_2d !, fill_vector_symmetric_edges_3d
!   module procedure fill_scalar_symmetric_edges_2d, fill_scalar_symmetric_edges_3d
end interface fill_symmetric_edges

!> Copy one LND_domain_type into another
interface clone_LND_domain
  module procedure clone_MD_to_MD, clone_MD_to_d2D
end interface clone_LND_domain

!> The LND_domain_type contains information about the domain decompositoin.
type, public :: LND_domain_type
  type(domain2D), pointer :: mpp_domain => NULL() !< The FMS domain with halos
                                !! on this processor, centered at h points.
  type(domain2D), pointer :: mpp_domain_d2 => NULL() !< A coarse FMS domain with halos
                                !! on this processor, centered at h points.
  integer :: niglobal           !< The total horizontal i-domain size.
  integer :: njglobal           !< The total horizontal j-domain size.
  integer :: nihalo             !< The i-halo size in memory.
  integer :: njhalo             !< The j-halo size in memory.
  logical :: symmetric          !< True if symmetric memory is used with
                                !! this domain.
  logical :: nonblocking_updates  !< If true, non-blocking halo updates are
                                !! allowed.  The default is .false. (for now).
  logical :: thin_halo_updates  !< If true, optional arguments may be used to
                                !! specify the width of the halos that are
                                !! updated with each call.
  integer :: layout(2)          !< This domain's processor layout.  This is
                                !! saved to enable the construction of related
                                !! new domains with different resolutions or
                                !! other properties.
  integer :: io_layout(2)       !< The IO-layout used with this domain.
  integer :: X_FLAGS            !< Flag that specifies the properties of the
                                !! domain in the i-direction in a define_domain call.
  integer :: Y_FLAGS            !< Flag that specifies the properties of the
                                !! domain in the j-direction in a define_domain call.
  logical, pointer :: maskmap(:,:) => NULL() !< A pointer to an array indicating
                                !! which logical processors are actually used for
                                !! the ocean code. The other logical processors
                                !! would be contain only land points and are not
                                !! assigned to actual processors. This need not be
                                !! assigned if all logical processors are used.
end type LND_domain_type

integer, parameter :: To_All = To_East + To_West + To_North + To_South !< A flag for passing in all directions

contains

!> pass_var_3d does a halo update for a three-dimensional array.
subroutine pass_var_3d(array, LND_dom, sideflag, complete, position, halo, &
                       clock)
  real, dimension(:,:,:), intent(inout) :: array    !< The array which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: sideflag !< An optional integer indicating which
      !! directions the data should be sent.  It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH.  For example, TO_EAST sends the data to the processor to the east,
      !! sothe halos on the western side are filled.  TO_ALL is the default if sideflag is omitted.
  logical,      optional, intent(in)    :: complete !< An optional argument indicating whether the
                                                    !! halo updates should be completed before
                                                    !! progress resumes. Omitting complete is the
                                                    !! same as setting complete to .true.
  integer,      optional, intent(in)    :: position !< An optional argument indicating the position.
                                                    !! This is CENTER by default and is often CORNER,
                                                    !! but could also be EAST_FACE or NORTH_FACE.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.

  integer :: dirflag
  logical :: block_til_complete

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  dirflag = To_All ! 60
  if (present(sideflag)) then ; if (sideflag > 0) dirflag = sideflag ; endif
  block_til_complete = .true.
  if (present(complete)) block_til_complete = complete

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_update_domains(array, LND_dom%mpp_domain, flags=dirflag, &
                        complete=block_til_complete, position=position, &
                        whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    call mpp_update_domains(array, LND_dom%mpp_domain, flags=dirflag, &
                          complete=block_til_complete, position=position)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine pass_var_3d

!> pass_var_2d does a halo update for a two-dimensional array.
subroutine pass_var_2d(array, LND_dom, sideflag, complete, position, halo, inner_halo, clock)
  real, dimension(:,:),  intent(inout) :: array    !< The array which is having its halos points
                                                   !! exchanged.
  type(LND_domain_type), intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                   !! needed to determine where data should be sent.
  integer,     optional, intent(in)    :: sideflag !< An optional integer indicating which
      !! directions the data should be sent. It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH.  For example, TO_EAST sends the data to the processor to the east,
      !! so the halos on the western side are filled.  TO_ALL is the default if sideflag is omitted.
  logical,     optional, intent(in)    :: complete !< An optional argument indicating whether the
                                                   !! halo updates should be completed before
                                                   !! progress resumes.  Omitting complete is the
                                                   !! same as setting complete to .true.
  integer,     optional, intent(in)    :: position !< An optional argument indicating the position.
                                                   !! This is CENTER by default and is often CORNER,
                                                   !! but could also be EAST_FACE or NORTH_FACE.
  integer,     optional, intent(in)    :: halo     !< The size of the halo to update - the full halo
                                                   !! by default.
  integer,     optional, intent(in)    :: inner_halo !< The size of an inner halo to avoid updating,
                                                   !! or 0 to avoid updating symmetric memory
                                                   !! computational domain points.  Setting this >=0
                                                   !! also enforces that complete=.true.
  integer,     optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                   !! started then stopped to time this routine.

  ! Local variables
  real, allocatable, dimension(:,:) :: tmp
  integer :: pos, i_halo, j_halo
  integer :: isc, iec, jsc, jec, isd, ied, jsd, jed, IscB, IecB, JscB, JecB
  integer :: inner, i, j, isfw, iefw, isfe, iefe, jsfs, jefs, jsfn, jefn
  integer :: dirflag
  logical :: block_til_complete

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  dirflag = To_All ! 60
  if (present(sideflag)) then ; if (sideflag > 0) dirflag = sideflag ; endif
  block_til_complete = .true. ; if (present(complete)) block_til_complete = complete
  pos = CENTER ; if (present(position)) pos = position

  if (present(inner_halo)) then ; if (inner_halo >= 0) then
    ! Store the original values.
    allocate(tmp(size(array,1), size(array,2)))
    tmp(:,:) = array(:,:)
    block_til_complete = .true.
  endif ; endif

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_update_domains(array, LND_dom%mpp_domain, flags=dirflag, &
                        complete=block_til_complete, position=position, &
                        whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    call mpp_update_domains(array, LND_dom%mpp_domain, flags=dirflag, &
                        complete=block_til_complete, position=position)
  endif

  if (present(inner_halo)) then ; if (inner_halo >= 0) then
    call mpp_get_compute_domain(LND_dom%mpp_domain, isc, iec, jsc, jec)
    call mpp_get_data_domain(LND_dom%mpp_domain, isd, ied, jsd, jed)
    ! Convert to local indices for arrays starting at 1.
    isc = isc - (isd-1) ; iec = iec - (isd-1) ; ied = ied - (isd-1) ; isd = 1
    jsc = jsc - (jsd-1) ; jec = jec - (jsd-1) ; jed = jed - (jsd-1) ; jsd = 1
    i_halo = min(inner_halo, isc-1) ; j_halo = min(inner_halo, jsc-1)

    ! Figure out the array index extents of the eastern, western, northern and southern regions to copy.
    if (pos == CENTER) then
      if (size(array,1) == ied) then
        isfw = isc - i_halo ; iefw = isc ; isfe = iec ; iefe = iec + i_halo
      else ; call LND_error(FATAL, "pass_var_2d: wrong i-size for CENTER array.") ; endif
      if (size(array,2) == jed) then
        isfw = isc - i_halo ; iefw = isc ; isfe = iec ; iefe = iec + i_halo
      else ; call LND_error(FATAL, "pass_var_2d: wrong j-size for CENTER array.") ; endif
    elseif (pos == CORNER) then
      if (size(array,1) == ied) then
        isfw = max(isc - (i_halo+1), 1) ; iefw = isc ; isfe = iec ; iefe = iec + i_halo
      elseif (size(array,1) == ied+1) then
        isfw = isc - i_halo ; iefw = isc+1 ; isfe = iec+1 ; iefe = min(iec + 1 + i_halo, ied+1)
      else ; call LND_error(FATAL, "pass_var_2d: wrong i-size for CORNER array.") ; endif
      if (size(array,2) == jed) then
        jsfs = max(jsc - (j_halo+1), 1) ; jefs = jsc ; jsfn = jec ; jefn = jec + j_halo
      elseif (size(array,2) == jed+1) then
        jsfs = jsc - j_halo ; jefs = jsc+1 ; jsfn = jec+1 ; jefn = min(jec + 1 + j_halo, jed+1)
      else ; call LND_error(FATAL, "pass_var_2d: wrong j-size for CORNER array.") ; endif
    elseif (pos == NORTH_FACE) then
      if (size(array,1) == ied) then
        isfw = isc - i_halo ; iefw = isc ; isfe = iec ; iefe = iec + i_halo
      else ; call LND_error(FATAL, "pass_var_2d: wrong i-size for NORTH_FACE array.") ; endif
      if (size(array,2) == jed) then
        jsfs = max(jsc - (j_halo+1), 1) ; jefs = jsc ; jsfn = jec ; jefn = jec + j_halo
      elseif (size(array,2) == jed+1) then
        jsfs = jsc - j_halo ; jefs = jsc+1 ; jsfn = jec+1 ; jefn = min(jec + 1 + j_halo, jed+1)
      else ; call LND_error(FATAL, "pass_var_2d: wrong j-size for NORTH_FACE array.") ; endif
    elseif (pos == EAST_FACE) then
      if (size(array,1) == ied) then
        isfw = max(isc - (i_halo+1), 1) ; iefw = isc ; isfe = iec ; iefe = iec + i_halo
      elseif (size(array,1) == ied+1) then
        isfw = isc - i_halo ; iefw = isc+1 ; isfe = iec+1 ; iefe = min(iec + 1 + i_halo, ied+1)
      else ; call LND_error(FATAL, "pass_var_2d: wrong i-size for EAST_FACE array.") ; endif
      if (size(array,2) == jed) then
        isfw = isc - i_halo ; iefw = isc ; isfe = iec ; iefe = iec + i_halo
      else ; call LND_error(FATAL, "pass_var_2d: wrong j-size for EAST_FACE array.") ; endif
    else
      call LND_error(FATAL, "pass_var_2d: Unrecognized position")
    endif

    ! Copy back the stored inner halo points
    do j=jsfs,jefn ; do i=isfw,iefw ; array(i,j) = tmp(i,j) ; enddo ; enddo
    do j=jsfs,jefn ; do i=isfe,iefe ; array(i,j) = tmp(i,j) ; enddo ; enddo
    do j=jsfs,jefs ; do i=isfw,iefe ; array(i,j) = tmp(i,j) ; enddo ; enddo
    do j=jsfn,jefn ; do i=isfw,iefe ; array(i,j) = tmp(i,j) ; enddo ; enddo

    deallocate(tmp)
  endif ; endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine pass_var_2d

!> pass_var_start_2d starts a halo update for a two-dimensional array.
function pass_var_start_2d(array, LND_dom, sideflag, position, complete, halo, &
                           clock)
  real, dimension(:,:),   intent(inout) :: array    !< The array which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: sideflag !< An optional integer indicating which
      !! directions the data should be sent. It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH.  For example, TO_EAST sends the data to the processor to the east,
      !! so the halos on the western side are filled.  TO_ALL is the default if sideflag is omitted.
  integer,      optional, intent(in)    :: position !< An optional argument indicating the position.
                                                    !! This is CENTER by default and is often CORNER,
                                                    !! but could also be EAST_FACE or NORTH_FACE.
  logical,      optional, intent(in)    :: complete !< An optional argument indicating whether the
                                                    !! halo updates should be completed before
                                                    !! progress resumes.  Omitting complete is the
                                                    !! same as setting complete to .true.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  integer                               :: pass_var_start_2d  !<The integer index for this update.

  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  dirflag = To_All ! 60
  if (present(sideflag)) then ; if (sideflag > 0) dirflag = sideflag ; endif

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    pass_var_start_2d = mpp_start_update_domains(array, LND_dom%mpp_domain, &
                            flags=dirflag, position=position, &
                            whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    pass_var_start_2d = mpp_start_update_domains(array, LND_dom%mpp_domain, &
                            flags=dirflag, position=position)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end function pass_var_start_2d

!> pass_var_start_3d starts a halo update for a three-dimensional array.
function pass_var_start_3d(array, LND_dom, sideflag, position, complete, halo, &
                           clock)
  real, dimension(:,:,:), intent(inout) :: array    !< The array which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: sideflag !< An optional integer indicating which
      !! directions the data should be sent. It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH.  For example, TO_EAST sends the data to the processor to the east,
      !! so the halos on the western side are filled.  TO_ALL is the default if sideflag is omitted.
  integer,      optional, intent(in)    :: position !< An optional argument indicating the position.
                                                    !! This is CENTER by default and is often CORNER,
                                                    !! but could also be EAST_FACE or NORTH_FACE.
  logical,      optional, intent(in)    :: complete !< An optional argument indicating whether the
                                                    !! halo updates should be completed before
                                                    !! progress resumes.  Omitting complete is the
                                                    !! same as setting complete to .true.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  integer                               :: pass_var_start_3d  !< The integer index for this update.

  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  dirflag = To_All ! 60
  if (present(sideflag)) then ; if (sideflag > 0) dirflag = sideflag ; endif

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    pass_var_start_3d = mpp_start_update_domains(array, LND_dom%mpp_domain, &
                            flags=dirflag, position=position, &
                            whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    pass_var_start_3d = mpp_start_update_domains(array, LND_dom%mpp_domain, &
                            flags=dirflag, position=position)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end function pass_var_start_3d

!> pass_var_complete_2d completes a halo update for a two-dimensional array.
subroutine pass_var_complete_2d(id_update, array, LND_dom, sideflag, position, halo, &
                                clock)
  integer,                intent(in)    :: id_update !< The integer id of this update which has
                                                    !! been returned from a previous call to
                                                    !! pass_var_start.
  real, dimension(:,:),   intent(inout) :: array    !< The array which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: sideflag !< An optional integer indicating which
      !! directions the data should be sent. It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH.  For example, TO_EAST sends the data to the processor to the east,
      !! so the halos on the western side are filled.  TO_ALL is the default if sideflag is omitted.
  integer,      optional, intent(in)    :: position !< An optional argument indicating the position.
                                                    !! This is CENTER by default and is often CORNER,
                                                    !! but could also be EAST_FACE or NORTH_FACE.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.

  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  dirflag = To_All ! 60
  if (present(sideflag)) then ; if (sideflag > 0) dirflag = sideflag ; endif

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_complete_update_domains(id_update, array, LND_dom%mpp_domain, &
                            flags=dirflag, position=position, &
                            whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    call mpp_complete_update_domains(id_update, array, LND_dom%mpp_domain, &
                                     flags=dirflag, position=position)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine pass_var_complete_2d

!> pass_var_complete_3d completes a halo update for a three-dimensional array.
subroutine pass_var_complete_3d(id_update, array, LND_dom, sideflag, position, halo, &
                                clock)
  integer,                intent(in)    :: id_update !< The integer id of this update which has
                                                    !! been returned from a previous call to
                                                    !! pass_var_start.
  real, dimension(:,:,:), intent(inout) :: array    !< The array which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: sideflag !< An optional integer indicating which
      !! directions the data should be sent. It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH.  For example, TO_EAST sends the data to the processor to the east,
      !! so the halos on the western side are filled.  TO_ALL is the default if sideflag is omitted.
  integer,      optional, intent(in)    :: position !< An optional argument indicating the position.
                                                    !! This is CENTER by default and is often CORNER,
                                                    !! but could also be EAST_FACE or NORTH_FACE.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.

  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  dirflag = To_All ! 60
  if (present(sideflag)) then ; if (sideflag > 0) dirflag = sideflag ; endif

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_complete_update_domains(id_update, array, LND_dom%mpp_domain, &
                            flags=dirflag, position=position, &
                            whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    call mpp_complete_update_domains(id_update, array, LND_dom%mpp_domain, &
                                     flags=dirflag, position=position)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine pass_var_complete_3d

!> pass_vector_2d does a halo update for a pair of two-dimensional arrays
!! representing the compontents of a two-dimensional horizontal vector.
subroutine pass_vector_2d(u_cmpt, v_cmpt, LND_dom, direction, stagger, complete, halo, &
                          clock)
  real, dimension(:,:),  intent(inout) :: u_cmpt    !< The nominal zonal (u) component of the vector
                                                    !! pair which is having its halos points
                                                    !! exchanged.
  real, dimension(:,:),  intent(inout) :: v_cmpt    !< The nominal meridional (v) component of the
                                                    !! vector pair which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type), intent(inout) :: LND_dom   !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,     optional, intent(in)    :: direction !< An optional integer indicating which
      !! directions the data should be sent.  It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH, possibly plus SCALAR_PAIR if these are paired non-directional
      !! scalars discretized at the typical vector component locations.  For example, TO_EAST sends
      !! the data to the processor to the east, so the halos on the western side are filled. TO_ALL
      !! is the default if omitted.
  integer,     optional, intent(in)    :: stagger   !< An optional flag, which may be one of A_GRID,
                     !! BGRID_NE, or CGRID_NE, indicating where the two components of the vector are
                     !! discretized. Omitting stagger is the same as setting it to CGRID_NE.
  logical,     optional, intent(in)    :: complete  !< An optional argument indicating whether the
                                     !! halo updates should be completed before progress resumes.
                                     !! Omitting complete is the same as setting complete to .true.
  integer,     optional, intent(in)    :: halo      !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,     optional, intent(in)    :: clock     !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.

  ! Local variables
  integer :: stagger_local
  integer :: dirflag
  logical :: block_til_complete

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  stagger_local = CGRID_NE ! Default value for type of grid
  if (present(stagger)) stagger_local = stagger

  dirflag = To_All ! 60
  if (present(direction)) then ; if (direction > 0) dirflag = direction ; endif
  block_til_complete = .true.
  if (present(complete)) block_til_complete = complete

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_update_domains(u_cmpt, v_cmpt, LND_dom%mpp_domain, flags=dirflag, &
                   gridtype=stagger_local, complete = block_til_complete, &
                   whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    call mpp_update_domains(u_cmpt, v_cmpt, LND_dom%mpp_domain, flags=dirflag, &
                   gridtype=stagger_local, complete = block_til_complete)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine pass_vector_2d

!> fill_vector_symmetric_edges_2d does an usual set of halo updates that only
!! fill in the values at the edge of a pair of symmetric memory two-dimensional
!! arrays representing the compontents of a two-dimensional horizontal vector.
!! If symmetric memory is not being used, this subroutine does nothing except to
!! possibly turn optional cpu clocks on or off.
subroutine fill_vector_symmetric_edges_2d(u_cmpt, v_cmpt, LND_dom, stagger, scalar, &
                                          clock)
  real, dimension(:,:),  intent(inout) :: u_cmpt  !< The nominal zonal (u) component of the vector
                                                  !! pair which is having its halos points
                                                  !! exchanged.
  real, dimension(:,:),  intent(inout) :: v_cmpt  !< The nominal meridional (v) component of the
                                                  !! vector pair which is having its halos points
                                                  !! exchanged.
  type(LND_domain_type), intent(inout) :: LND_dom !< The LND_domain_type containing the mpp_domain
                                                  !! needed to determine where data should be
                                                  !! sent.
  integer,     optional, intent(in)    :: stagger !< An optional flag, which may be one of A_GRID,
                     !! BGRID_NE, or CGRID_NE, indicating where the two components of the vector are
                     !! discretized. Omitting stagger is the same as setting it to CGRID_NE.
  logical,     optional, intent(in)    :: scalar  !< An optional argument indicating whether.
  integer,     optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                   !! started then stopped to time this routine.

  ! Local variables
  integer :: stagger_local
  integer :: dirflag
  integer :: i, j, isc, iec, jsc, jec, isd, ied, jsd, jed, IscB, IecB, JscB, JecB
  real, allocatable, dimension(:) :: sbuff_x, sbuff_y, wbuff_x, wbuff_y
  logical :: block_til_complete

  if (.not. LND_dom%symmetric) then
      return
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  stagger_local = CGRID_NE ! Default value for type of grid
  if (present(stagger)) stagger_local = stagger

  if (.not.(stagger_local == CGRID_NE .or. stagger_local == BGRID_NE)) return

  call mpp_get_compute_domain(LND_dom%mpp_domain, isc, iec, jsc, jec)
  call mpp_get_data_domain(LND_dom%mpp_domain, isd, ied, jsd, jed)

  ! Adjust isc, etc., to account for the fact that the input arrays indices all
  ! start at 1 (and are effectively on a SW grid!).
  isc = isc - (isd-1) ; iec = iec - (isd-1)
  jsc = jsc - (jsd-1) ; jec = jec - (jsd-1)
  IscB = isc ; IecB = iec+1 ; JscB = jsc ; JecB = jec+1

  dirflag = To_All ! 60
  if (present(scalar)) then ; if (scalar) dirflag = To_All+SCALAR_PAIR ; endif

  if (stagger_local == CGRID_NE) then
    allocate(wbuff_x(jsc:jec)) ; allocate(sbuff_y(isc:iec))
    wbuff_x(:) = 0.0 ; sbuff_y(:) = 0.0
    call mpp_get_boundary(u_cmpt, v_cmpt, LND_dom%mpp_domain, flags=dirflag, &
                          wbufferx=wbuff_x, sbuffery=sbuff_y, &
                          gridtype=CGRID_NE)
    do i=isc,iec
      v_cmpt(i,JscB) = sbuff_y(i)
    enddo
    do j=jsc,jec
      u_cmpt(IscB,j) = wbuff_x(j)
    enddo
    deallocate(wbuff_x) ; deallocate(sbuff_y)
  elseif  (stagger_local == BGRID_NE) then
    allocate(wbuff_x(JscB:JecB)) ; allocate(sbuff_x(IscB:IecB))
    allocate(wbuff_y(JscB:JecB)) ; allocate(sbuff_y(IscB:IecB))
    wbuff_x(:) = 0.0 ; wbuff_y(:) = 0.0 ; sbuff_x(:) = 0.0 ; sbuff_y(:) = 0.0
    call mpp_get_boundary(u_cmpt, v_cmpt, LND_dom%mpp_domain, flags=dirflag, &
                          wbufferx=wbuff_x, sbufferx=sbuff_x, &
                          wbuffery=wbuff_y, sbuffery=sbuff_y, &
                          gridtype=BGRID_NE)
    do I=IscB,IecB
      u_cmpt(I,JscB) = sbuff_x(I) ; v_cmpt(I,JscB) = sbuff_y(I)
    enddo
    do J=JscB,JecB
      u_cmpt(IscB,J) = wbuff_x(J) ; v_cmpt(IscB,J) = wbuff_y(J)
    enddo
    deallocate(wbuff_x) ; deallocate(sbuff_x)
    deallocate(wbuff_y) ; deallocate(sbuff_y)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine fill_vector_symmetric_edges_2d

!> pass_vector_3d does a halo update for a pair of three-dimensional arrays
!! representing the compontents of a three-dimensional horizontal vector.
subroutine pass_vector_3d(u_cmpt, v_cmpt, LND_dom, direction, stagger, complete, halo, &
                          clock)
  real, dimension(:,:,:), intent(inout) :: u_cmpt   !< The nominal zonal (u) component of the vector
                                                    !! pair which is having its halos points
                                                    !! exchanged.
  real, dimension(:,:,:), intent(inout) :: v_cmpt   !< The nominal meridional (v) component of the
                                                    !! vector pair which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: direction !< An optional integer indicating which
      !! directions the data should be sent.  It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH, possibly plus SCALAR_PAIR if these are paired non-directional
      !! scalars discretized at the typical vector component locations.  For example, TO_EAST sends
      !! the data to the processor to the east, so the halos on the western side are filled. TO_ALL
      !! is the default if omitted.
  integer,      optional, intent(in)    :: stagger  !< An optional flag, which may be one of A_GRID,
                     !! BGRID_NE, or CGRID_NE, indicating where the two components of the vector are
                     !! discretized. Omitting stagger is the same as setting it to CGRID_NE.
  logical,      optional, intent(in)    :: complete !< An optional argument indicating whether the
                                     !! halo updates should be completed before progress resumes.
                                     !! Omitting complete is the same as setting complete to .true.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.

  ! Local variables
  integer :: stagger_local
  integer :: dirflag
  logical :: block_til_complete

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  stagger_local = CGRID_NE ! Default value for type of grid
  if (present(stagger)) stagger_local = stagger

  dirflag = To_All ! 60
  if (present(direction)) then ; if (direction > 0) dirflag = direction ; endif
  block_til_complete = .true.
  if (present(complete)) block_til_complete = complete

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_update_domains(u_cmpt, v_cmpt, LND_dom%mpp_domain, flags=dirflag, &
                   gridtype=stagger_local, complete = block_til_complete, &
                   whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    call mpp_update_domains(u_cmpt, v_cmpt, LND_dom%mpp_domain, flags=dirflag, &
                   gridtype=stagger_local, complete = block_til_complete)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine pass_vector_3d

!> pass_vector_start_2d starts a halo update for a pair of two-dimensional arrays
!! representing the compontents of a two-dimensional horizontal vector.
function pass_vector_start_2d(u_cmpt, v_cmpt, LND_dom, direction, stagger, complete, halo, &
                              clock)
  real, dimension(:,:),   intent(inout) :: u_cmpt   !< The nominal zonal (u) component of the vector
                                                    !! pair which is having its halos points
                                                    !! exchanged.
  real, dimension(:,:),   intent(inout) :: v_cmpt   !< The nominal meridional (v) component of the
                                                    !! vector pair which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: direction !< An optional integer indicating which
      !! directions the data should be sent.  It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH, possibly plus SCALAR_PAIR if these are paired non-directional
      !! scalars discretized at the typical vector component locations.  For example, TO_EAST sends
      !! the data to the processor to the east, so the halos on the western side are filled. TO_ALL
      !! is the default if omitted.
  integer,      optional, intent(in)    :: stagger  !< An optional flag, which may be one of A_GRID,
                     !! BGRID_NE, or CGRID_NE, indicating where the two components of the vector are
                     !! discretized. Omitting stagger is the same as setting it to CGRID_NE.
  logical,      optional, intent(in)    :: complete !< An optional argument indicating whether the
                                     !! halo updates should be completed before progress resumes.
                                     !! Omitting complete is the same as setting complete to .true.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  integer                               :: pass_vector_start_2d !< The integer index for this
                                                                !! update.

  ! Local variables
  integer :: stagger_local
  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  stagger_local = CGRID_NE ! Default value for type of grid
  if (present(stagger)) stagger_local = stagger

  dirflag = To_All ! 60
  if (present(direction)) then ; if (direction > 0) dirflag = direction ; endif

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    pass_vector_start_2d = mpp_start_update_domains(u_cmpt, v_cmpt, &
        LND_dom%mpp_domain, flags=dirflag, gridtype=stagger_local, &
        whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    pass_vector_start_2d = mpp_start_update_domains(u_cmpt, v_cmpt, &
        LND_dom%mpp_domain, flags=dirflag, gridtype=stagger_local)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end function pass_vector_start_2d

!> pass_vector_start_3d starts a halo update for a pair of three-dimensional arrays
!! representing the compontents of a three-dimensional horizontal vector.
function pass_vector_start_3d(u_cmpt, v_cmpt, LND_dom, direction, stagger, complete, halo, &
                              clock)
  real, dimension(:,:,:), intent(inout) :: u_cmpt   !< The nominal zonal (u) component of the vector
                                                    !! pair which is having its halos points
                                                    !! exchanged.
  real, dimension(:,:,:), intent(inout) :: v_cmpt   !< The nominal meridional (v) component of the
                                                    !! vector pair which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: direction !< An optional integer indicating which
      !! directions the data should be sent.  It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH, possibly plus SCALAR_PAIR if these are paired non-directional
      !! scalars discretized at the typical vector component locations.  For example, TO_EAST sends
      !! the data to the processor to the east, so the halos on the western side are filled. TO_ALL
      !! is the default if omitted.
  integer,      optional, intent(in)    :: stagger  !< An optional flag, which may be one of A_GRID,
                     !! BGRID_NE, or CGRID_NE, indicating where the two components of the vector are
                     !! discretized. Omitting stagger is the same as setting it to CGRID_NE.
  logical,      optional, intent(in)    :: complete !< An optional argument indicating whether the
                                     !! halo updates should be completed before progress resumes.
                                     !! Omitting complete is the same as setting complete to .true.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  integer                               :: pass_vector_start_3d !< The integer index for this
                                                                !! update.
  ! Local variables
  integer :: stagger_local
  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  stagger_local = CGRID_NE ! Default value for type of grid
  if (present(stagger)) stagger_local = stagger

  dirflag = To_All ! 60
  if (present(direction)) then ; if (direction > 0) dirflag = direction ; endif

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    pass_vector_start_3d = mpp_start_update_domains(u_cmpt, v_cmpt, &
        LND_dom%mpp_domain, flags=dirflag, gridtype=stagger_local, &
        whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    pass_vector_start_3d = mpp_start_update_domains(u_cmpt, v_cmpt, &
        LND_dom%mpp_domain, flags=dirflag, gridtype=stagger_local)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end function pass_vector_start_3d

!> pass_vector_complete_2d completes a halo update for a pair of two-dimensional arrays
!! representing the compontents of a two-dimensional horizontal vector.
subroutine pass_vector_complete_2d(id_update, u_cmpt, v_cmpt, LND_dom, direction, stagger, halo, &
                                   clock)
  integer,                intent(in)    :: id_update !< The integer id of this update which has been
                                                    !! returned from a previous call to
                                                    !! pass_var_start.
  real, dimension(:,:),   intent(inout) :: u_cmpt   !< The nominal zonal (u) component of the vector
                                                    !! pair which is having its halos points
                                                    !! exchanged.
  real, dimension(:,:),   intent(inout) :: v_cmpt   !< The nominal meridional (v) component of the
                                                    !! vector pair which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: direction !< An optional integer indicating which
      !! directions the data should be sent.  It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH, possibly plus SCALAR_PAIR if these are paired non-directional
      !! scalars discretized at the typical vector component locations.  For example, TO_EAST sends
      !! the data to the processor to the east, so the halos on the western side are filled. TO_ALL
      !! is the default if omitted.
  integer,      optional, intent(in)    :: stagger  !< An optional flag, which may be one of A_GRID,
                     !! BGRID_NE, or CGRID_NE, indicating where the two components of the vector are
                     !! discretized. Omitting stagger is the same as setting it to CGRID_NE.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  ! Local variables
  integer :: stagger_local
  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  stagger_local = CGRID_NE ! Default value for type of grid
  if (present(stagger)) stagger_local = stagger

  dirflag = To_All ! 60
  if (present(direction)) then ; if (direction > 0) dirflag = direction ; endif

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_complete_update_domains(id_update, u_cmpt, v_cmpt, &
             LND_dom%mpp_domain, flags=dirflag, gridtype=stagger_local, &
             whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    call mpp_complete_update_domains(id_update, u_cmpt, v_cmpt, &
             LND_dom%mpp_domain, flags=dirflag, gridtype=stagger_local)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine pass_vector_complete_2d

!> pass_vector_complete_3d completes a halo update for a pair of three-dimensional
!! arrays representing the compontents of a three-dimensional horizontal vector.
subroutine pass_vector_complete_3d(id_update, u_cmpt, v_cmpt, LND_dom, direction, stagger, halo, &
                                   clock)
  integer,                intent(in)    :: id_update !< The integer id of this update which has been
                                                    !! returned from a previous call to
                                                    !! pass_var_start.
  real, dimension(:,:,:), intent(inout) :: u_cmpt   !< The nominal zonal (u) component of the vector
                                                    !! pair which is having its halos points
                                                    !! exchanged.
  real, dimension(:,:,:), intent(inout) :: v_cmpt   !< The nominal meridional (v) component of the
                                                    !! vector pair which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: direction !< An optional integer indicating which
      !! directions the data should be sent.  It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH, possibly plus SCALAR_PAIR if these are paired non-directional
      !! scalars discretized at the typical vector component locations.  For example, TO_EAST sends
      !! the data to the processor to the east, so the halos on the western side are filled. TO_ALL
      !! is the default if omitted.
  integer,      optional, intent(in)    :: stagger  !< An optional flag, which may be one of A_GRID,
                     !! BGRID_NE, or CGRID_NE, indicating where the two components of the vector are
                     !! discretized. Omitting stagger is the same as setting it to CGRID_NE.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  ! Local variables
  integer :: stagger_local
  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  stagger_local = CGRID_NE ! Default value for type of grid
  if (present(stagger)) stagger_local = stagger

  dirflag = To_All ! 60
  if (present(direction)) then ; if (direction > 0) dirflag = direction ; endif

  if (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_complete_update_domains(id_update, u_cmpt, v_cmpt, &
             LND_dom%mpp_domain, flags=dirflag, gridtype=stagger_local, &
                   whalo=halo, ehalo=halo, shalo=halo, nhalo=halo)
  else
    call mpp_complete_update_domains(id_update, u_cmpt, v_cmpt, &
             LND_dom%mpp_domain, flags=dirflag, gridtype=stagger_local)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine pass_vector_complete_3d

!> create_var_group_pass_2d sets up a group of two-dimensional array halo updates.
subroutine create_var_group_pass_2d(group, array, LND_dom, sideflag, position, &
                                    halo, clock)
  type(group_pass_type),  intent(inout) :: group    !< The data type that store information for
                                                    !! group update. This data will be used in
                                                    !! do_group_pass.
  real, dimension(:,:),   intent(inout) :: array    !< The array which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: sideflag !< An optional integer indicating which
      !! directions the data should be sent. It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH.  For example, TO_EAST sends the data to the processor to the east,
      !! so the halos on the western side are filled.  TO_ALL is the default if sideflag is omitted.
  integer,      optional, intent(in)    :: position !< An optional argument indicating the position.
                                                    !! This is CENTER by default and is often CORNER,
                                                    !! but could also be EAST_FACE or NORTH_FACE.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  ! Local variables
  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  dirflag = To_All ! 60
  if (present(sideflag)) then ; if (sideflag > 0) dirflag = sideflag ; endif

  if (mpp_group_update_initialized(group)) then
    call mpp_reset_group_update_field(group,array)
  elseif (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_create_group_update(group, array, LND_dom%mpp_domain, flags=dirflag, &
                                 position=position, whalo=halo, ehalo=halo, &
                                 shalo=halo, nhalo=halo)
  else
    call mpp_create_group_update(group, array, LND_dom%mpp_domain, flags=dirflag, &
                                 position=position)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine create_var_group_pass_2d

!> create_var_group_pass_3d sets up a group of three-dimensional array halo updates.
subroutine create_var_group_pass_3d(group, array, LND_dom, sideflag, position, halo, &
                                    clock)
  type(group_pass_type),  intent(inout) :: group    !< The data type that store information for
                                                    !! group update. This data will be used in
                                                    !! do_group_pass.
  real, dimension(:,:,:), intent(inout) :: array    !< The array which is having its halos points
                                                    !! exchanged.
  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: sideflag !< An optional integer indicating which
      !! directions the data should be sent. It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH.  For example, TO_EAST sends the data to the processor to the east,
      !! so the halos on the western side are filled.  TO_ALL is the default if sideflag is omitted.
  integer,      optional, intent(in)    :: position !< An optional argument indicating the position.
                                                    !! This is CENTER by default and is often CORNER,
                                                    !! but could also be EAST_FACE or NORTH_FACE.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  ! Local variables
  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  dirflag = To_All ! 60
  if (present(sideflag)) then ; if (sideflag > 0) dirflag = sideflag ; endif

  if (mpp_group_update_initialized(group)) then
    call mpp_reset_group_update_field(group,array)
  elseif (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_create_group_update(group, array, LND_dom%mpp_domain, flags=dirflag, &
                                 position=position, whalo=halo, ehalo=halo, &
                                 shalo=halo, nhalo=halo)
  else
    call mpp_create_group_update(group, array, LND_dom%mpp_domain, flags=dirflag, &
                                 position=position)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine create_var_group_pass_3d

!> create_vector_group_pass_2d sets up a group of two-dimensional vector halo updates.
subroutine create_vector_group_pass_2d(group, u_cmpt, v_cmpt, LND_dom, direction, stagger, halo, &
                                       clock)
  type(group_pass_type),  intent(inout) :: group    !< The data type that store information for
                                                    !! group update. This data will be used in
                                                    !! do_group_pass.
  real, dimension(:,:),   intent(inout) :: u_cmpt   !< The nominal zonal (u) component of the vector
                                                    !! pair which is having its halos points
                                                    !! exchanged.
  real, dimension(:,:),   intent(inout) :: v_cmpt   !< The nominal meridional (v) component of the
                                                    !! vector pair which is having its halos points
                                                    !! exchanged.

  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent
  integer,      optional, intent(in)    :: direction !< An optional integer indicating which
      !! directions the data should be sent.  It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH, possibly plus SCALAR_PAIR if these are paired non-directional
      !! scalars discretized at the typical vector component locations.  For example, TO_EAST sends
      !! the data to the processor to the east, so the halos on the western side are filled. TO_ALL
      !! is the default if omitted.
  integer,      optional, intent(in)    :: stagger  !< An optional flag, which may be one of A_GRID,
                     !! BGRID_NE, or CGRID_NE, indicating where the two components of the vector are
                     !! discretized. Omitting stagger is the same as setting it to CGRID_NE.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  ! Local variables
  integer :: stagger_local
  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  stagger_local = CGRID_NE ! Default value for type of grid
  if (present(stagger)) stagger_local = stagger

  dirflag = To_All ! 60
  if (present(direction)) then ; if (direction > 0) dirflag = direction ; endif

  if (mpp_group_update_initialized(group)) then
    call mpp_reset_group_update_field(group,u_cmpt, v_cmpt)
  elseif (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_create_group_update(group, u_cmpt, v_cmpt, LND_dom%mpp_domain, &
            flags=dirflag, gridtype=stagger_local, whalo=halo, ehalo=halo, &
            shalo=halo, nhalo=halo)
  else
    call mpp_create_group_update(group, u_cmpt, v_cmpt, LND_dom%mpp_domain, &
            flags=dirflag, gridtype=stagger_local)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine create_vector_group_pass_2d

!> create_vector_group_pass_3d sets up a group of three-dimensional vector halo updates.
subroutine create_vector_group_pass_3d(group, u_cmpt, v_cmpt, LND_dom, direction, stagger, halo, &
                                       clock)
  type(group_pass_type),  intent(inout) :: group    !< The data type that store information for
                                                    !! group update. This data will be used in
                                                    !! do_group_pass.
  real, dimension(:,:,:), intent(inout) :: u_cmpt   !< The nominal zonal (u) component of the vector
                                                    !! pair which is having its halos points
                                                    !! exchanged.
  real, dimension(:,:,:), intent(inout) :: v_cmpt   !< The nominal meridional (v) component of the
                                                    !! vector pair which is having its halos points
                                                    !! exchanged.

  type(LND_domain_type),  intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: direction !< An optional integer indicating which
      !! directions the data should be sent.  It is TO_ALL or the sum of any of TO_EAST, TO_WEST,
      !! TO_NORTH, and TO_SOUTH, possibly plus SCALAR_PAIR if these are paired non-directional
      !! scalars discretized at the typical vector component locations.  For example, TO_EAST sends
      !! the data to the processor to the east, so the halos on the western side are filled. TO_ALL
      !! is the default if omitted.
  integer,      optional, intent(in)    :: stagger  !< An optional flag, which may be one of A_GRID,
                     !! BGRID_NE, or CGRID_NE, indicating where the two components of the vector are
                     !! discretized. Omitting stagger is the same as setting it to CGRID_NE.
  integer,      optional, intent(in)    :: halo     !< The size of the halo to update - the full
                                                    !! halo by default.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.

  ! Local variables
  integer :: stagger_local
  integer :: dirflag

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  stagger_local = CGRID_NE ! Default value for type of grid
  if (present(stagger)) stagger_local = stagger

  dirflag = To_All ! 60
  if (present(direction)) then ; if (direction > 0) dirflag = direction ; endif

  if (mpp_group_update_initialized(group)) then
    call mpp_reset_group_update_field(group,u_cmpt, v_cmpt)
  elseif (present(halo) .and. LND_dom%thin_halo_updates) then
    call mpp_create_group_update(group, u_cmpt, v_cmpt, LND_dom%mpp_domain, &
            flags=dirflag, gridtype=stagger_local, whalo=halo, ehalo=halo, &
            shalo=halo, nhalo=halo)
  else
    call mpp_create_group_update(group, u_cmpt, v_cmpt, LND_dom%mpp_domain, &
            flags=dirflag, gridtype=stagger_local)
  endif

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine create_vector_group_pass_3d

!> do_group_pass carries out a group halo update.
subroutine do_group_pass(group, LND_dom, clock)
  type(group_pass_type), intent(inout) :: group     !< The data type that store information for
                                                    !! group update. This data will be used in
                                                    !! do_group_pass.
  type(LND_domain_type), intent(inout) :: LND_dom   !< The LND_domain_type containing the mpp_domain
                                                    !! needed to determine where data should be
                                                    !! sent.
  integer,      optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                    !! started then stopped to time this routine.
  real :: d_type

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  call mpp_do_group_update(group, LND_dom%mpp_domain, d_type)

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine do_group_pass

!> start_group_pass starts out a group halo update.
subroutine start_group_pass(group, LND_dom, clock)
  type(group_pass_type), intent(inout) :: group    !< The data type that store information for
                                                   !! group update. This data will be used in
                                                   !! do_group_pass.
  type(LND_domain_type), intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                   !! needed to determine where data should be
                                                   !! sent.
  integer,     optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                   !! started then stopped to time this routine.

  real                                 :: d_type

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  call mpp_start_group_update(group, LND_dom%mpp_domain, d_type)

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine start_group_pass

!> complete_group_pass completes a group halo update.
subroutine complete_group_pass(group, LND_dom, clock)
  type(group_pass_type), intent(inout) :: group    !< The data type that store information for
                                                   !! group update. This data will be used in
                                                   !! do_group_pass.
  type(LND_domain_type), intent(inout) :: LND_dom  !< The LND_domain_type containing the mpp_domain
                                                   !! needed to determine where data should be
                                                   !! sent.
  integer,     optional, intent(in)    :: clock    !< The handle for a cpu time clock that should be
                                                   !! started then stopped to time this routine.
  real                                 :: d_type

  if (present(clock)) then ; if (clock>0) call cpu_clock_begin(clock) ; endif

  call mpp_complete_group_update(group, LND_dom%mpp_domain, d_type)

  if (present(clock)) then ; if (clock>0) call cpu_clock_end(clock) ; endif

end subroutine complete_group_pass

!> LND_domains_init initalizes a LND_domain_type variable, based on the information
!! read in from a param_file_type, and optionally returns data describing various'
!! properties of the domain type.
subroutine LND_domains_init(LND_dom, param_file, symmetric, static_memory, &
                            NIHALO, NJHALO, NIGLOBAL, NJGLOBAL, NIPROC, NJPROC, &
                            min_halo, domain_name, include_name, param_suffix)
  type(LND_domain_type),           pointer       :: LND_dom      !< A pointer to the LND_domain_type
                                                                 !! being defined here.
  type(param_file_type),           intent(in)    :: param_file   !< A structure to parse for
                                                                 !! run-time parameters
  logical, optional,               intent(in)    :: symmetric    !< If present, this specifies
                                            !! whether this domain is symmetric, regardless of
                                            !! whether the macro SYMMETRIC_MEMORY_ is defined.
  logical, optional,               intent(in)    :: static_memory !< If present and true, this
                         !! domain type is set up for static memory and error checking of
                         !! various input values is performed against those in the input file.
  integer, optional,               intent(in)    :: NIHALO       !< Default halo sizes, required
                                                                 !! with static memory.
  integer, optional,               intent(in)    :: NJHALO       !< Default halo sizes, required
                                                                 !! with static memory.
  integer, optional,               intent(in)    :: NIGLOBAL     !< Total domain sizes, required
                                                                 !! with static memory.
  integer, optional,               intent(in)    :: NJGLOBAL     !< Total domain sizes, required
                                                                 !! with static memory.
  integer, optional,               intent(in)    :: NIPROC       !< Processor counts, required with
                                                                 !! static memory.
  integer, optional,               intent(in)    :: NJPROC       !< Processor counts, required with
                                                                 !! static memory.
  integer, dimension(2), optional, intent(inout) :: min_halo     !< If present, this sets the
                                        !! minimum halo size for this domain in the i- and j-
                                        !! directions, and returns the actual halo size used.
  character(len=*),      optional, intent(in)    :: domain_name  !< A name for this domain, "LND"
                                                                 !! if missing.
  character(len=*),      optional, intent(in)    :: include_name !< A name for model's include file,
                                                                 !! "LND_memory.h" if missing.
  character(len=*),      optional, intent(in)    :: param_suffix !< A suffix to apply to
                                                                 !! layout-specific parameters.

  ! Local variables
  integer, dimension(2) :: layout = (/ 1, 1 /)
  integer, dimension(2) :: io_layout = (/ 0, 0 /)
  integer, dimension(4) :: global_indices
!$ integer :: ocean_nthreads       ! Number of Openmp threads
!$ integer :: get_cpu_affinity, omp_get_thread_num, omp_get_num_threads
!$ logical :: ocean_omp_hyper_thread
  integer :: nihalo_dflt, njhalo_dflt
  integer :: pe, proc_used
  integer :: X_FLAGS, Y_FLAGS
  logical :: reentrant_x, reentrant_y, tripolar_N, is_static
  logical            :: mask_table_exists
  character(len=128) :: mask_table, inputdir
  character(len=64)  :: dom_name, inc_nm
  character(len=200) :: mesg

  integer :: xsiz, ysiz, nip_parsed, njp_parsed
  integer :: isc,iec,jsc,jec ! The bounding indices of the computational domain.
  character(len=8) :: char_xsiz, char_ysiz, char_niglobal, char_njglobal
  character(len=40) :: nihalo_nm, njhalo_nm, layout_nm, io_layout_nm, masktable_nm
  character(len=40) :: niproc_nm, njproc_nm
  integer :: xhalo_d2,yhalo_d2
! This include declares and sets the variable "version".
#include "version_variable.h"
  character(len=40)  :: mdl ! This module's name.

  if (.not.associated(LND_dom)) then
    allocate(LND_dom)
    allocate(LND_dom%mpp_domain)
    allocate(LND_dom%mpp_domain_d2)
  endif

  pe = PE_here()
  proc_used = num_PEs()

  mdl = "LND_domains"

  LND_dom%symmetric = .true.
  if (present(symmetric)) then ; LND_dom%symmetric = symmetric ; endif
  if (present(min_halo)) mdl = trim(mdl)//" min_halo"

  dom_name = "LND" ; inc_nm = "LND_memory.h"
  if (present(domain_name)) dom_name = trim(domain_name)
  if (present(include_name)) inc_nm = trim(include_name)

  nihalo_nm = "NIHALO" ; njhalo_nm = "NJHALO"
  layout_nm = "LAYOUT" ; io_layout_nm = "IO_LAYOUT" ; masktable_nm = "MASKTABLE"
  niproc_nm = "NIPROC" ; njproc_nm = "NJPROC"
  if (present(param_suffix)) then ; if (len(trim(adjustl(param_suffix))) > 0) then
    nihalo_nm = "NIHALO"//(trim(adjustl(param_suffix)))
    njhalo_nm = "NJHALO"//(trim(adjustl(param_suffix)))
    layout_nm = "LAYOUT"//(trim(adjustl(param_suffix)))
    io_layout_nm = "IO_LAYOUT"//(trim(adjustl(param_suffix)))
    masktable_nm = "MASKTABLE"//(trim(adjustl(param_suffix)))
    niproc_nm = "NIPROC"//(trim(adjustl(param_suffix)))
    njproc_nm = "NJPROC"//(trim(adjustl(param_suffix)))
  endif ; endif

  is_static = .false. ; if (present(static_memory)) is_static = static_memory
  if (is_static) then
    if (.not.present(NIHALO)) call LND_error(FATAL, "NIHALO must be "// &
      "present in the call to LND_domains_init with static memory.")
    if (.not.present(NJHALO)) call LND_error(FATAL, "NJHALO must be "// &
      "present in the call to LND_domains_init with static memory.")
    if (.not.present(NIGLOBAL)) call LND_error(FATAL, "NIGLOBAL must be "// &
      "present in the call to LND_domains_init with static memory.")
    if (.not.present(NJGLOBAL)) call LND_error(FATAL, "NJGLOBAL must be "// &
      "present in the call to LND_domains_init with static memory.")
    if (.not.present(NIPROC)) call LND_error(FATAL, "NIPROC must be "// &
      "present in the call to LND_domains_init with static memory.")
    if (.not.present(NJPROC)) call LND_error(FATAL, "NJPROC must be "// &
      "present in the call to LND_domains_init with static memory.")
  endif

  ! Read all relevant parameters and write them to the model log.
  call log_version(param_file, mdl, version, "", log_to_all=.true., layout=.true.)
  call get_param(param_file, mdl, "REENTRANT_X", reentrant_x, &
                 "If true, the domain is zonally reentrant.", default=.true.)
  call get_param(param_file, mdl, "REENTRANT_Y", reentrant_y, &
                 "If true, the domain is meridionally reentrant.", &
                 default=.false.)
  call get_param(param_file, mdl, "TRIPOLAR_N", tripolar_N, &
                 "Use tripolar connectivity at the northern edge of the "//&
                 "domain.  With TRIPOLAR_N, NIGLOBAL must be even.", &
                 default=.false.)

#ifndef NOT_SET_AFFINITY
!$  call fms_affinity_init
!$OMP PARALLEL
!$OMP master
!$ ocean_nthreads = omp_get_num_threads()
!$OMP END MASTER
!$OMP END PARALLEL
!$ if(ocean_nthreads < 2 ) then
!$   call get_param(param_file, mdl, "OCEAN_OMP_THREADS", ocean_nthreads, &
!$              "The number of OpenMP threads that UCLAND will use.", &
!$              default = 1, layoutParam=.true.)
!$   call get_param(param_file, mdl, "OCEAN_OMP_HYPER_THREAD", ocean_omp_hyper_thread, &
!$              "If True, use hyper-threading.", default = .false., layoutParam=.true.)
!$   call fms_affinity_set('OCEAN', ocean_omp_hyper_thread, ocean_nthreads)
!$   call omp_set_num_threads(ocean_nthreads)
!$   write(6,*) "LND_domains_mod OMPthreading ", fms_affinity_get(), omp_get_thread_num(), omp_get_num_threads()
!$   call flush(6)
!$ endif
#endif
  call log_param(param_file, mdl, "!SYMMETRIC_MEMORY_", LND_dom%symmetric, &
                 "If defined, the velocity point data domain includes "//&
                 "every face of the thickness points. In other words, "//&
                 "some arrays are larger than others, depending on where "//&
                 "they are on the staggered grid.  Also, the starting "//&
                 "index of the velocity-point arrays is usually 0, not 1. "//&
                 "This can only be set at compile time.",&
                 layoutParam=.true.)
  call get_param(param_file, mdl, "NONBLOCKING_UPDATES", LND_dom%nonblocking_updates, &
                 "If true, non-blocking halo updates may be used.", &
                 default=.false., layoutParam=.true.)
  call get_param(param_file, mdl, "THIN_HALO_UPDATES", LND_dom%thin_halo_updates, &
                 "If true, optional arguments may be used to specify the "//&
                 "the width of the halos that are updated with each call.", &
                 default=.true., layoutParam=.true.)

  nihalo_dflt = 4 ; njhalo_dflt = 4
  if (present(NIHALO)) nihalo_dflt = NIHALO
  if (present(NJHALO)) njhalo_dflt = NJHALO

  call log_param(param_file, mdl, "!STATIC_MEMORY_", is_static, &
                 "If STATIC_MEMORY_ is defined, the principle variables "//&
                 "will have sizes that are statically determined at "//&
                 "compile time.  Otherwise the sizes are not determined "//&
                 "until run time. The STATIC option is substantially "//&
                 "faster, but does not allow the PE count to be changed "//&
                 "at run time.  This can only be set at compile time.",&
                 layoutParam=.true.)

  if (is_static) then
    call get_param(param_file, mdl, "NIGLOBAL", LND_dom%niglobal, &
                 "The total number of thickness grid points in the "//&
                 "x-direction in the physical domain. With STATIC_MEMORY_ "//&
                 "this is set in "//trim(inc_nm)//" at compile time.", &
                 static_value=NIGLOBAL)
    call get_param(param_file, mdl, "NJGLOBAL", LND_dom%njglobal, &
                 "The total number of thickness grid points in the "//&
                 "y-direction in the physical domain. With STATIC_MEMORY_ "//&
                 "this is set in "//trim(inc_nm)//" at compile time.", &
                 static_value=NJGLOBAL)
    if (LND_dom%niglobal /= NIGLOBAL) call LND_error(FATAL,"LND_domains_init: " // &
     "static mismatch for NIGLOBAL_ domain size. Header file does not match input namelist")
    if (LND_dom%njglobal /= NJGLOBAL) call LND_error(FATAL,"LND_domains_init: " // &
     "static mismatch for NJGLOBAL_ domain size. Header file does not match input namelist")

  else
    call get_param(param_file, mdl, "NIGLOBAL", LND_dom%niglobal, &
                 "The total number of thickness grid points in the "//&
                 "x-direction in the physical domain. With STATIC_MEMORY_ "//&
                 "this is set in "//trim(inc_nm)//" at compile time.", &
                 fail_if_missing=.true.)
    call get_param(param_file, mdl, "NJGLOBAL", LND_dom%njglobal, &
                 "The total number of thickness grid points in the "//&
                 "y-direction in the physical domain. With STATIC_MEMORY_ "//&
                 "this is set in "//trim(inc_nm)//" at compile time.", &
                 fail_if_missing=.true.)
  endif

  call get_param(param_file, mdl, trim(nihalo_nm), LND_dom%nihalo, &
                 "The number of halo points on each side in the x-direction.  How this is set "//&
                 "varies with the calling component and static or dynamic memory configuration.", &
                 default=nihalo_dflt, static_value=nihalo_dflt)
  call get_param(param_file, mdl, trim(njhalo_nm), LND_dom%njhalo, &
                 "The number of halo points on each side in the y-direction.  How this is set "//&
                 "varies with the calling component and static or dynamic memory configuration.", &
                 default=njhalo_dflt, static_value=njhalo_dflt)
  if (present(min_halo)) then
    LND_dom%nihalo = max(LND_dom%nihalo, min_halo(1))
    min_halo(1) = LND_dom%nihalo
    LND_dom%njhalo = max(LND_dom%njhalo, min_halo(2))
    min_halo(2) = LND_dom%njhalo
    ! These are generally used only with static memory, so they are considerd layout params.
    call log_param(param_file, mdl, "!NIHALO min_halo", LND_dom%nihalo, layoutParam=.true.)
    call log_param(param_file, mdl, "!NJHALO min_halo", LND_dom%nihalo, layoutParam=.true.)
  endif
  if (is_static .and. .not.present(min_halo)) then
    if (LND_dom%nihalo /= NIHALO) call LND_error(FATAL,"LND_domains_init: " // &
           "static mismatch for "//trim(nihalo_nm)//" domain size")
    if (LND_dom%njhalo /= NJHALO) call LND_error(FATAL,"LND_domains_init: " // &
           "static mismatch for "//trim(njhalo_nm)//" domain size")
  endif

  global_indices(1) = 1 ; global_indices(2) = LND_dom%niglobal
  global_indices(3) = 1 ; global_indices(4) = LND_dom%njglobal

  call get_param(param_file, mdl, "INPUTDIR", inputdir, do_not_log=.true., default=".")
  inputdir = slasher(inputdir)

  call get_param(param_file, mdl, trim(masktable_nm), mask_table, &
                 "A text file to specify n_mask, layout and mask_list. "//&
                 "This feature masks out processors that contain only land points. "//&
                 "The first line of mask_table is the number of regions to be masked out. "//&
                 "The second line is the layout of the model and must be "//&
                 "consistent with the actual model layout. "//&
                 "The following (n_mask) lines give the logical positions "//&
                 "of the processors that are masked out. The mask_table "//&
                 "can be created by tools like check_mask. The "//&
                 "following example of mask_table masks out 2 processors, "//&
                 "(1,2) and (3,6), out of the 24 in a 4x6 layout: \n"//&
                 " 2\n 4,6\n 1,2\n 3,6\n", default="LND_mask_table", &
                 layoutParam=.true.)
  mask_table = trim(inputdir)//trim(mask_table)
  mask_table_exists = file_exist(mask_table)

  if (is_static) then
    layout(1) = NIPROC ; layout(2) = NJPROC
  else
    call get_param(param_file, mdl, trim(layout_nm), layout, &
                 "The processor layout to be used, or 0, 0 to automatically "//&
                 "set the layout based on the number of processors.", default=0, &
                 do_not_log=.true.)
    call get_param(param_file, mdl, trim(niproc_nm), nip_parsed, &
                 "The number of processors in the x-direction.", default=-1, &
                 do_not_log=.true.)
    call get_param(param_file, mdl, trim(njproc_nm), njp_parsed, &
                 "The number of processors in the y-direction.", default=-1, &
                 do_not_log=.true.)
    if (nip_parsed > -1) then
      if ((layout(1) > 0) .and. (layout(1) /= nip_parsed)) &
        call LND_error(FATAL, trim(layout_nm)//" and "//trim(niproc_nm)//" set inconsistently. "//&
                              "Only LAYOUT should be used.")
      layout(1) = nip_parsed
      call LND_mesg(trim(niproc_nm)//" used to set "//trim(layout_nm)//" in dynamic mode.  "//&
                    "Shift to using "//trim(layout_nm)//" instead.")
    endif
    if (njp_parsed > -1) then
      if ((layout(2) > 0) .and. (layout(2) /= njp_parsed)) &
        call LND_error(FATAL, trim(layout_nm)//" and "//trim(njproc_nm)//" set inconsistently. "//&
                              "Only "//trim(layout_nm)//" should be used.")
      layout(2) = njp_parsed
      call LND_mesg(trim(njproc_nm)//" used to set "//trim(layout_nm)//" in dynamic mode.  "//&
                    "Shift to using "//trim(layout_nm)//" instead.")
    endif

    if ( layout(1)==0 .and. layout(2)==0 ) &
      call mpp_define_layout(global_indices, proc_used, layout)
    if ( layout(1)/=0 .and. layout(2)==0 ) layout(2) = proc_used/layout(1)
    if ( layout(1)==0 .and. layout(2)/=0 ) layout(1) = proc_used/layout(2)

    if (layout(1)*layout(2) /= proc_used .and. (.not. mask_table_exists) ) then
      write(mesg,'("LND_domains_init: The product of the two components of layout, ", &
            &      2i4,", is not the number of PEs used, ",i5,".")') &
            layout(1),layout(2),proc_used
      call LND_error(FATAL, mesg)
    endif
  endif
  call log_param(param_file, mdl, trim(niproc_nm), layout(1), &
                 "The number of processors in the x-direction. With "//&
                 "STATIC_MEMORY_ this is set in "//trim(inc_nm)//" at compile time.",&
                 layoutParam=.true.)
  call log_param(param_file, mdl, trim(njproc_nm), layout(2), &
                 "The number of processors in the y-direction. With "//&
                 "STATIC_MEMORY_ this is set in "//trim(inc_nm)//" at compile time.",&
                 layoutParam=.true.)
  call log_param(param_file, mdl, trim(layout_nm), layout, &
                 "The processor layout that was actually used.",&
                 layoutParam=.true.)

  ! Idiot check that fewer PEs than columns have been requested
  if (layout(1)*layout(2)>LND_dom%niglobal*LND_dom%njglobal)  then
    write(mesg,'(a,2(i5,x,a))') 'You requested to use',layout(1)*layout(2), &
      'PEs but there are only',LND_dom%niglobal*LND_dom%njglobal,'columns in the model'
    call LND_error(FATAL, mesg)
  endif

  if (mask_table_exists) then
    call LND_error(NOTE, 'LND_domains_init: reading maskmap information from '//&
                         trim(mask_table))
    allocate(LND_dom%maskmap(layout(1), layout(2)))
    call parse_mask_table(mask_table, LND_dom%maskmap, dom_name)
  endif

  !   Set up the I/O layout, and check that it uses an even multiple of the
  ! number of PEs in each direction.
  io_layout(:) = (/ 1, 1 /)
  call get_param(param_file, mdl, trim(io_layout_nm), io_layout, &
                 "The processor layout to be used, or 0,0 to automatically "//&
                 "set the io_layout to be the same as the layout.", default=1, &
                 layoutParam=.true.)

  if (io_layout(1) < 0) then
    write(mesg,'("LND_domains_init: IO_LAYOUT(1) = ",i4,".  Negative values "//&
         &"are not allowed in ")') io_layout(1)
    call LND_error(FATAL, mesg//trim(IO_layout_nm))
  elseif (io_layout(1) > 0) then ; if (modulo(layout(1), io_layout(1)) /= 0) then
    write(mesg,'("LND_domains_init: The i-direction I/O-layout, IO_LAYOUT(1)=",i4, &
         &", does not evenly divide the i-direction layout, NIPROC=,",i4,".")') &
          io_layout(1),layout(1)
    call LND_error(FATAL, mesg)
  endif ; endif

  if (io_layout(2) < 0) then
    write(mesg,'("LND_domains_init: IO_LAYOUT(2) = ",i4,".  Negative values "//&
         &"are not allowed in ")') io_layout(2)
    call LND_error(FATAL, mesg//trim(IO_layout_nm))
  elseif (io_layout(2) /= 0) then ; if (modulo(layout(2), io_layout(2)) /= 0) then
    write(mesg,'("LND_domains_init: The j-direction I/O-layout, IO_LAYOUT(2)=",i4, &
         &", does not evenly divide the j-direction layout, NJPROC=,",i4,".")') &
          io_layout(2),layout(2)
    call LND_error(FATAL, mesg)
  endif ; endif

  if (io_layout(2) == 0) io_layout(2) = layout(2)
  if (io_layout(1) == 0) io_layout(1) = layout(1)

  X_FLAGS = 0 ; Y_FLAGS = 0
  if (reentrant_x) X_FLAGS = CYCLIC_GLOBAL_DOMAIN
  if (reentrant_y) Y_FLAGS = CYCLIC_GLOBAL_DOMAIN
  if (tripolar_N) then
    Y_FLAGS = FOLD_NORTH_EDGE
    if (reentrant_y) call LND_error(FATAL,"LND_domains: "// &
      "TRIPOLAR_N and REENTRANT_Y may not be defined together.")
  endif

  global_indices(1) = 1 ; global_indices(2) = LND_dom%niglobal
  global_indices(3) = 1 ; global_indices(4) = LND_dom%njglobal

  if (mask_table_exists) then
    call LND_define_domain( global_indices, layout, LND_dom%mpp_domain, &
                xflags=X_FLAGS, yflags=Y_FLAGS, &
                xhalo=LND_dom%nihalo, yhalo=LND_dom%njhalo, &
                symmetry = LND_dom%symmetric, name=dom_name, &
                maskmap=LND_dom%maskmap )
  else
    call LND_define_domain( global_indices, layout, LND_dom%mpp_domain, &
                xflags=X_FLAGS, yflags=Y_FLAGS, &
                xhalo=LND_dom%nihalo, yhalo=LND_dom%njhalo, &
                symmetry = LND_dom%symmetric, name=dom_name)
  endif

  if ((io_layout(1) > 0) .and. (io_layout(2) > 0) .and. &
      (layout(1)*layout(2) > 1)) then
    call LND_define_io_domain(LND_dom%mpp_domain, io_layout)
  endif

! Save the extra data for creating other domains of different resolution that overlay this domain
  LND_dom%X_FLAGS = X_FLAGS
  LND_dom%Y_FLAGS = Y_FLAGS
  LND_dom%layout = layout
  LND_dom%io_layout = io_layout

  if (is_static) then
  !   A requirement of equal sized compute domains is necessary when STATIC_MEMORY_
  ! is used.
    call mpp_get_compute_domain(LND_dom%mpp_domain,isc,iec,jsc,jec)
    xsiz = iec - isc + 1
    ysiz = jec - jsc + 1
    if (xsiz*NIPROC /= LND_dom%niglobal .OR. ysiz*NJPROC /= LND_dom%njglobal) then
       write( char_xsiz,'(i4)' ) NIPROC
       write( char_ysiz,'(i4)' ) NJPROC
       write( char_niglobal,'(i4)' ) LND_dom%niglobal
       write( char_njglobal,'(i4)' ) LND_dom%njglobal
       call LND_error(WARNING,'LND_domains: Processor decomposition (NIPROC_,NJPROC_) = (' &
           //trim(char_xsiz)//','//trim(char_ysiz)// &
           ') does not evenly divide size set by preprocessor macro ('&
           //trim(char_niglobal)//','//trim(char_njglobal)// '). ')
       call LND_error(FATAL,'LND_domains:  #undef STATIC_MEMORY_ in "//trim(inc_nm)//" to use &
           &dynamic allocation, or change processor decomposition to evenly divide the domain.')
    endif
  endif

  global_indices(1) = 1 ; global_indices(2) = int(LND_dom%niglobal/2)
  global_indices(3) = 1 ; global_indices(4) = int(LND_dom%njglobal/2)
  !For downsampled domain, recommend a halo of 1 (or 0?) since we're not doing wide-stencil computations.
  !But that does not work because the downsampled field would not have the correct size to pass the checks, e.g., we get
  !error: downsample_diag_indices_get: peculiar size 28 in i-direction\ndoes not match one of 24 25 26 27
  xhalo_d2 = int(LND_dom%nihalo/2)
  yhalo_d2 = int(LND_dom%njhalo/2)
  if (mask_table_exists) then
    call LND_define_domain( global_indices, layout, LND_dom%mpp_domain_d2, &
                xflags=X_FLAGS, yflags=Y_FLAGS, &
                xhalo=xhalo_d2, yhalo=yhalo_d2, &
                symmetry = LND_dom%symmetric, name=trim("LNDc"), &
                maskmap=LND_dom%maskmap )
  else
    call LND_define_domain( global_indices, layout, LND_dom%mpp_domain_d2, &
                xflags=X_FLAGS, yflags=Y_FLAGS, &
                xhalo=xhalo_d2, yhalo=yhalo_d2, &
                symmetry = LND_dom%symmetric, name=trim("LNDc"))
  endif

  if ((io_layout(1) > 0) .and. (io_layout(2) > 0) .and. &
      (layout(1)*layout(2) > 1)) then
    call LND_define_io_domain(LND_dom%mpp_domain_d2, io_layout)
  endif

end subroutine LND_domains_init

!> clone_MD_to_MD copies one LND_domain_type into another, while allowing
!! some properties of the new type to differ from the original one.
subroutine clone_MD_to_MD(MD_in, LND_dom, min_halo, halo_size, symmetric, &
                          domain_name, turns)
  type(LND_domain_type), intent(in)    :: MD_in  !< An existing LND_domain
  type(LND_domain_type), pointer       :: LND_dom !< A pointer to a LND_domain that will be
                                  !! allocated if it is unassociated, and will have data
                                  !! copied from MD_in
  integer, dimension(2), &
               optional, intent(inout) :: min_halo !< If present, this sets the
                                  !! minimum halo size for this domain in the i- and j-
                                  !! directions, and returns the actual halo size used.
  integer,     optional, intent(in)    :: halo_size !< If present, this sets the halo
                                  !! size for the domian in the i- and j-directions.
                                  !! min_halo and halo_size can not both be present.
  logical,     optional, intent(in)    :: symmetric !< If present, this specifies
                                  !! whether the new domain is symmetric, regardless of
                                  !! whether the macro SYMMETRIC_MEMORY_ is defined.
  character(len=*), &
               optional, intent(in)    :: domain_name !< A name for the new domain, "LND"
                                  !! if missing.
  integer, optional, intent(in) :: turns   !< Number of quarter turns

  integer :: global_indices(4)
  logical :: mask_table_exists
  character(len=64) :: dom_name
  integer :: qturns

  qturns = 0
  if (present(turns)) qturns = turns

  if (.not.associated(LND_dom)) then
    allocate(LND_dom)
    allocate(LND_dom%mpp_domain)
    allocate(LND_dom%mpp_domain_d2)
  endif

! Save the extra data for creating other domains of different resolution that overlay this domain
  LND_dom%symmetric = MD_in%symmetric
  LND_dom%nonblocking_updates = MD_in%nonblocking_updates
  LND_dom%thin_halo_updates = MD_in%thin_halo_updates

  if (modulo(qturns, 2) /= 0) then
    LND_dom%niglobal = MD_in%njglobal ; LND_dom%njglobal = MD_in%niglobal
    LND_dom%nihalo = MD_in%njhalo ; LND_dom%njhalo = MD_in%nihalo

    LND_dom%X_FLAGS = MD_in%Y_FLAGS ; LND_dom%Y_FLAGS = MD_in%X_FLAGS
    LND_dom%layout(:) = MD_in%layout(2:1:-1)
    LND_dom%io_layout(:) = MD_in%io_layout(2:1:-1)
  else
    LND_dom%niglobal = MD_in%niglobal ; LND_dom%njglobal = MD_in%njglobal
    LND_dom%nihalo = MD_in%nihalo ; LND_dom%njhalo = MD_in%njhalo

    LND_dom%X_FLAGS = MD_in%X_FLAGS ; LND_dom%Y_FLAGS = MD_in%Y_FLAGS
    LND_dom%layout(:) = MD_in%layout(:)
    LND_dom%io_layout(:) = MD_in%io_layout(:)
  endif

  global_indices(1) = 1 ; global_indices(2) = LND_dom%niglobal
  global_indices(3) = 1 ; global_indices(4) = LND_dom%njglobal

  if (associated(MD_in%maskmap)) then
    mask_table_exists = .true.
    allocate(LND_dom%maskmap(LND_dom%layout(1), LND_dom%layout(2)))
    if (qturns /= 0) then
      call rotate_array(MD_in%maskmap(:,:), qturns, LND_dom%maskmap(:,:))
    else
      LND_dom%maskmap(:,:) = MD_in%maskmap(:,:)
    endif
  else
    mask_table_exists = .false.
  endif

  if (present(halo_size) .and. present(min_halo)) call LND_error(FATAL, &
      "clone_LND_domain can not have both halo_size and min_halo present.")

  if (present(min_halo)) then
    LND_dom%nihalo = max(LND_dom%nihalo, min_halo(1))
    min_halo(1) = LND_dom%nihalo
    LND_dom%njhalo = max(LND_dom%njhalo, min_halo(2))
    min_halo(2) = LND_dom%njhalo
  endif

  if (present(halo_size)) then
    LND_dom%nihalo = halo_size ; LND_dom%njhalo = halo_size
  endif

  if (present(symmetric)) then ; LND_dom%symmetric = symmetric ; endif

  dom_name = "LND"
  if (present(domain_name)) dom_name = trim(domain_name)

  if (mask_table_exists) then
    call LND_define_domain(global_indices, LND_dom%layout, LND_dom%mpp_domain, &
                xflags=LND_dom%X_FLAGS, yflags=LND_dom%Y_FLAGS, &
                xhalo=LND_dom%nihalo, yhalo=LND_dom%njhalo, &
                symmetry=LND_dom%symmetric, name=dom_name, &
                maskmap=LND_dom%maskmap)

    global_indices(2) = global_indices(2) / 2
    global_indices(4) = global_indices(4) / 2
    call LND_define_domain(global_indices, LND_dom%layout, &
                LND_dom%mpp_domain_d2, &
                xflags=LND_dom%X_FLAGS, yflags=LND_dom%Y_FLAGS, &
                xhalo=(LND_dom%nihalo/2), yhalo=(LND_dom%njhalo/2), &
                symmetry=LND_dom%symmetric, name=dom_name, &
                maskmap=LND_dom%maskmap)
  else
    call LND_define_domain(global_indices, LND_dom%layout, LND_dom%mpp_domain, &
                xflags=LND_dom%X_FLAGS, yflags=LND_dom%Y_FLAGS, &
                xhalo=LND_dom%nihalo, yhalo=LND_dom%njhalo, &
                symmetry=LND_dom%symmetric, name=dom_name)

    global_indices(2) = global_indices(2) / 2
    global_indices(4) = global_indices(4) / 2
    call LND_define_domain(global_indices, LND_dom%layout, &
                LND_dom%mpp_domain_d2, &
                xflags=LND_dom%X_FLAGS, yflags=LND_dom%Y_FLAGS, &
                xhalo=(LND_dom%nihalo/2), yhalo=(LND_dom%njhalo/2), &
                symmetry=LND_dom%symmetric, name=dom_name)
  endif

  if ((LND_dom%io_layout(1) + LND_dom%io_layout(2) > 0) .and. &
      (LND_dom%layout(1)*LND_dom%layout(2) > 1)) then
    call LND_define_io_domain(LND_dom%mpp_domain, LND_dom%io_layout)
  endif

end subroutine clone_MD_to_MD

!> clone_MD_to_d2D uses information from a LND_domain_type to create a new
!! domain2d type, while allowing some properties of the new type to differ from
!! the original one.
subroutine clone_MD_to_d2D(MD_in, mpp_domain, min_halo, halo_size, symmetric, &
                           domain_name, turns)
  type(LND_domain_type), intent(in)    :: MD_in !< An existing LND_domain to be cloned
  type(domain2d),        intent(inout) :: mpp_domain !< The new mpp_domain to be set up
  integer, dimension(2), &
               optional, intent(inout) :: min_halo !< If present, this sets the
                                  !! minimum halo size for this domain in the i- and j-
                                  !! directions, and returns the actual halo size used.
  integer,     optional, intent(in)    :: halo_size !< If present, this sets the halo
                                  !! size for the domian in the i- and j-directions.
                                  !! min_halo and halo_size can not both be present.
  logical,     optional, intent(in)    :: symmetric !< If present, this specifies
                                  !! whether the new domain is symmetric, regardless of
                                  !! whether the macro SYMMETRIC_MEMORY_ is defined.
  character(len=*), &
               optional, intent(in)    :: domain_name !< A name for the new domain, "LND"
                                  !! if missing.
  integer, optional, intent(in) :: turns   !< If true, swap X and Y axes

  integer :: global_indices(4), layout(2), io_layout(2)
  integer :: X_FLAGS, Y_FLAGS, niglobal, njglobal, nihalo, njhalo
  logical :: symmetric_dom
  character(len=64) :: dom_name

  if (present(turns)) &
    call LND_error(FATAL, "Rotation not supported for LND_domain to domain2d")

! Save the extra data for creating other domains of different resolution that overlay this domain
  niglobal = MD_in%niglobal ; njglobal = MD_in%njglobal
  nihalo = MD_in%nihalo ; njhalo = MD_in%njhalo

  symmetric_dom = MD_in%symmetric

  X_FLAGS = MD_in%X_FLAGS ; Y_FLAGS = MD_in%Y_FLAGS
  layout(:) = MD_in%layout(:) ; io_layout(:) = MD_in%io_layout(:)

  if (present(halo_size) .and. present(min_halo)) call LND_error(FATAL, &
      "clone_LND_domain can not have both halo_size and min_halo present.")

  if (present(min_halo)) then
    nihalo = max(nihalo, min_halo(1))
    njhalo = max(njhalo, min_halo(2))
    min_halo(1) = nihalo ; min_halo(2) = njhalo
  endif

  if (present(halo_size)) then
    nihalo = halo_size ; njhalo = halo_size
  endif

  if (present(symmetric)) then ; symmetric_dom = symmetric ; endif

  dom_name = "LND"
  if (present(domain_name)) dom_name = trim(domain_name)

  global_indices(1) = 1 ; global_indices(2) = niglobal
  global_indices(3) = 1 ; global_indices(4) = njglobal
  if (associated(MD_in%maskmap)) then
    call LND_define_domain( global_indices, layout, mpp_domain, &
                xflags=X_FLAGS, yflags=Y_FLAGS, &
                xhalo=nihalo, yhalo=njhalo, &
                symmetry = symmetric, name=dom_name, &
                maskmap=MD_in%maskmap )
  else
    call LND_define_domain( global_indices, layout, mpp_domain, &
                xflags=X_FLAGS, yflags=Y_FLAGS, &
                xhalo=nihalo, yhalo=njhalo, &
                symmetry = symmetric, name=dom_name)
  endif

  if ((io_layout(1) + io_layout(2) > 0) .and. &
      (layout(1)*layout(2) > 1)) then
    call LND_define_io_domain(mpp_domain, io_layout)
  endif

end subroutine clone_MD_to_d2D

!> Returns various data that has been stored in a LND_domain_type
subroutine get_domain_extent(Domain, isc, iec, jsc, jec, isd, ied, jsd, jed, &
                             isg, ieg, jsg, jeg, idg_offset, jdg_offset, &
                             symmetric, local_indexing, index_offset)
  type(LND_domain_type), &
           intent(in)  :: Domain !< The LND domain from which to extract information
  integer, intent(out) :: isc    !< The start i-index of the computational domain
  integer, intent(out) :: iec    !< The end i-index of the computational domain
  integer, intent(out) :: jsc    !< The start j-index of the computational domain
  integer, intent(out) :: jec    !< The end j-index of the computational domain
  integer, intent(out) :: isd    !< The start i-index of the data domain
  integer, intent(out) :: ied    !< The end i-index of the data domain
  integer, intent(out) :: jsd    !< The start j-index of the data domain
  integer, intent(out) :: jed    !< The end j-index of the data domain
  integer, intent(out) :: isg    !< The start i-index of the global domain
  integer, intent(out) :: ieg    !< The end i-index of the global domain
  integer, intent(out) :: jsg    !< The start j-index of the global domain
  integer, intent(out) :: jeg    !< The end j-index of the global domain
  integer, intent(out) :: idg_offset !< The offset between the corresponding global and
                                 !! data i-index spaces.
  integer, intent(out) :: jdg_offset !< The offset between the corresponding global and
                                 !! data j-index spaces.
  logical, intent(out) :: symmetric  !< True if symmetric memory is used.
  logical, optional, intent(in)  :: local_indexing !< If true, local tracer array indices start at 1,
                                           !! as in most UCLAND code.
  integer, optional, intent(in)  :: index_offset   !< A fixed additional offset to all indices. This
                                           !! can be useful for some types of debugging with
                                           !! dynamic memory allocation.
  ! Local variables
  integer :: ind_off
  logical :: local

  local = .true. ; if (present(local_indexing)) local = local_indexing
  ind_off = 0 ; if (present(index_offset)) ind_off = index_offset

  call mpp_get_compute_domain(Domain%mpp_domain, isc, iec, jsc, jec)
  call mpp_get_data_domain(Domain%mpp_domain, isd, ied, jsd, jed)
  call mpp_get_global_domain(Domain%mpp_domain, isg, ieg, jsg, jeg)

  ! This code institutes the LND convention that local array indices start at 1.
  if (local) then
    idg_offset = isd-1 ; jdg_offset = jsd-1
    isc = isc-isd+1 ; iec = iec-isd+1 ; jsc = jsc-jsd+1 ; jec = jec-jsd+1
    ied = ied-isd+1 ; jed = jed-jsd+1
    isd = 1 ; jsd = 1
  else
    idg_offset = 0 ; jdg_offset = 0
  endif
  if (ind_off /= 0) then
    idg_offset = idg_offset + ind_off ; jdg_offset = jdg_offset + ind_off
    isc = isc + ind_off ; iec = iec + ind_off
    jsc = jsc + ind_off ; jec = jec + ind_off
    isd = isd + ind_off ; ied = ied + ind_off
    jsd = jsd + ind_off ; jed = jed + ind_off
  endif
  symmetric = Domain%symmetric

end subroutine get_domain_extent

subroutine get_domain_extent_dsamp2(Domain, isc_d2, iec_d2, jsc_d2, jec_d2,&
                                            isd_d2, ied_d2, jsd_d2, jed_d2,&
                                            isg_d2, ieg_d2, jsg_d2, jeg_d2)
  type(LND_domain_type), &
           intent(in)  :: Domain !< The LND domain from which to extract information
  integer, intent(out) :: isc_d2 !< The start i-index of the computational domain
  integer, intent(out) :: iec_d2 !< The end i-index of the computational domain
  integer, intent(out) :: jsc_d2 !< The start j-index of the computational domain
  integer, intent(out) :: jec_d2 !< The end j-index of the computational domain
  integer, intent(out) :: isd_d2 !< The start i-index of the data domain
  integer, intent(out) :: ied_d2 !< The end i-index of the data domain
  integer, intent(out) :: jsd_d2 !< The start j-index of the data domain
  integer, intent(out) :: jed_d2 !< The end j-index of the data domain
  integer, intent(out) :: isg_d2 !< The start i-index of the global domain
  integer, intent(out) :: ieg_d2 !< The end i-index of the global domain
  integer, intent(out) :: jsg_d2 !< The start j-index of the global domain
  integer, intent(out) :: jeg_d2 !< The end j-index of the global domain

  call mpp_get_compute_domain(Domain%mpp_domain_d2, isc_d2, iec_d2, jsc_d2, jec_d2)
  call mpp_get_data_domain(Domain%mpp_domain_d2, isd_d2, ied_d2, jsd_d2, jed_d2)
  call mpp_get_global_domain (Domain%mpp_domain_d2, isg_d2, ieg_d2, jsg_d2, jeg_d2)
  ! This code institutes the LND convention that local array indices start at 1.
  isc_d2 = isc_d2-isd_d2+1 ; iec_d2 = iec_d2-isd_d2+1
  jsc_d2 = jsc_d2-jsd_d2+1 ; jec_d2 = jec_d2-jsd_d2+1
  ied_d2 = ied_d2-isd_d2+1 ; jed_d2 = jed_d2-jsd_d2+1
  isd_d2 = 1 ; jsd_d2 = 1
end subroutine get_domain_extent_dsamp2

!> Return the (potentially symmetric) computational domain i-bounds for an array
!! passed without index specifications (i.e. indices start at 1) based on an array size.
subroutine get_simple_array_i_ind(domain, size, is, ie, symmetric)
  type(LND_domain_type), intent(in)  :: domain !< LND domain from which to extract information
  integer,               intent(in)  :: size   !< The i-array size
  integer,               intent(out) :: is     !< The computational domain starting i-index.
  integer,               intent(out) :: ie     !< The computational domain ending i-index.
  logical,     optional, intent(in)  :: symmetric !< If present, indicates whether symmetric sizes
                                               !! can be considered.
  ! Local variables
  logical :: sym
  character(len=120) :: mesg, mesg2
  integer :: isc, iec, jsc, jec, isd, ied, jsd, jed

  call mpp_get_compute_domain(Domain%mpp_domain, isc, iec, jsc, jec)
  call mpp_get_data_domain(Domain%mpp_domain, isd, ied, jsd, jed)

  isc = isc-isd+1 ; iec = iec-isd+1 ; ied = ied-isd+1 ; isd = 1
  sym = Domain%symmetric ; if (present(symmetric)) sym = symmetric

  if (size == ied) then ; is = isc ; ie = iec
  elseif (size == 1+iec-isc) then ; is = 1 ; ie = size
  elseif (sym .and. (size == 1+ied)) then ; is = isc ; ie = iec+1
  elseif (sym .and. (size == 2+iec-isc)) then ; is = 1 ; ie = size+1
  else
    write(mesg,'("Unrecognized size ", i6, "in call to get_simple_array_i_ind.  \")') size
    if (sym) then
      write(mesg2,'("Valid sizes are : ", 2i7)') ied, 1+iec-isc
    else
      write(mesg2,'("Valid sizes are : ", 4i7)') ied, 1+iec-isc, 1+ied, 2+iec-isc
    endif
    call LND_error(FATAL, trim(mesg)//trim(mesg2))
  endif

end subroutine get_simple_array_i_ind


!> Return the (potentially symmetric) computational domain j-bounds for an array
!! passed without index specifications (i.e. indices start at 1) based on an array size.
subroutine get_simple_array_j_ind(domain, size, js, je, symmetric)
  type(LND_domain_type), intent(in)  :: domain !< LND domain from which to extract information
  integer,               intent(in)  :: size   !< The j-array size
  integer,               intent(out) :: js     !< The computational domain starting j-index.
  integer,               intent(out) :: je     !< The computational domain ending j-index.
  logical,     optional, intent(in)  :: symmetric !< If present, indicates whether symmetric sizes
                                               !! can be considered.
  ! Local variables
  logical :: sym
  character(len=120) :: mesg, mesg2
  integer :: isc, iec, jsc, jec, isd, ied, jsd, jed

  call mpp_get_compute_domain(Domain%mpp_domain, isc, iec, jsc, jec)
  call mpp_get_data_domain(Domain%mpp_domain, isd, ied, jsd, jed)

  jsc = jsc-jsd+1 ; jec = jec-jsd+1 ; jed = jed-jsd+1 ; jsd = 1
  sym = Domain%symmetric ; if (present(symmetric)) sym = symmetric

  if (size == jed) then ; js = jsc ; je = jec
  elseif (size == 1+jec-jsc) then ; js = 1 ; je = size
  elseif (sym .and. (size == 1+jed)) then ; js = jsc ; je = jec+1
  elseif (sym .and. (size == 2+jec-jsc)) then ; js = 1 ; je = size+1
  else
    write(mesg,'("Unrecognized size ", i6, "in call to get_simple_array_j_ind.  \")') size
    if (sym) then
      write(mesg2,'("Valid sizes are : ", 2i7)') jed, 1+jec-jsc
    else
      write(mesg2,'("Valid sizes are : ", 4i7)') jed, 1+jec-jsc, 1+jed, 2+jec-jsc
    endif
    call LND_error(FATAL, trim(mesg)//trim(mesg2))
  endif

end subroutine get_simple_array_j_ind

!> Returns the global shape of h-point arrays
subroutine get_global_shape(domain, niglobal, njglobal)
  type(LND_domain_type), intent(in)  :: domain   !< LND domain
  integer,               intent(out) :: niglobal !< i-index global size of h-point arrays
  integer,               intent(out) :: njglobal !< j-index global size of h-point arrays

  niglobal = domain%niglobal
  njglobal = domain%njglobal

end subroutine get_global_shape

end module LND_domains
