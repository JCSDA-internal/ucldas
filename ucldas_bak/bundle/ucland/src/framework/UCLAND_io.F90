!> This module contains I/O framework code
module UCLAND_io

use UCLAND_error_handler,    only : UCLAND_error, NOTE, FATAL, WARNING
use UCLAND_domains,          only : UCLAND_domain_type, AGRID, BGRID_NE, CGRID_NE
use UCLAND_string_functions, only : lowercase, slasher

use ensemble_manager_mod, only : get_ensemble_id
use fms_mod,              only : write_version_number, open_namelist_file, check_nml_error
use fms_io_mod,           only : file_exist, field_size, read_data
use fms_io_mod,           only : field_exists => field_exist, io_infra_end=>fms_io_exit
use fms_io_mod,           only : get_filename_appendix => get_filename_appendix
use mpp_domains_mod,      only : domain1d, domain2d, mpp_get_domain_components
use mpp_io_mod,           only : open_file => mpp_open, close_file => mpp_close
use mpp_io_mod,           only : mpp_get_fields, fieldtype, axistype, flush_file => mpp_flush
use mpp_io_mod,           only : APPEND_FILE=>MPP_APPEND, ASCII_FILE=>MPP_ASCII
use mpp_io_mod,           only : MULTIPLE=>MPP_MULTI, NETCDF_FILE=>MPP_NETCDF
use mpp_io_mod,           only : OVERWRITE_FILE=>MPP_OVERWR, READONLY_FILE=>MPP_RDONLY
use mpp_io_mod,           only : SINGLE_FILE=>MPP_SINGLE, WRITEONLY_FILE=>MPP_WRONLY
use mpp_io_mod,           only : get_file_fields=>mpp_get_fields, get_file_times=>mpp_get_times
use mpp_io_mod,           only : io_infra_init=>mpp_io_init

implicit none ; private

!public :: close_file, create_file, field_exists, field_size, fieldtype, get_filename_appendix
public :: close_file, field_exists, field_size, fieldtype, get_filename_appendix
!public :: file_exists, flush_file, get_file_info, get_file_atts, get_file_fields
public :: file_exists
!public :: get_file_times, open_file, read_axis_data, read_data
public :: get_file_times, open_file, read_data
!public :: num_timelevels, UCLAND_read_data, UCLAND_read_vector, ensembler
public :: ensembler
!public :: reopen_file, slasher, write_field, write_version_number, UCLAND_io_init
public :: slasher
public :: open_namelist_file, check_nml_error, io_infra_init, io_infra_end
public :: APPEND_FILE, ASCII_FILE, MULTIPLE, NETCDF_FILE, OVERWRITE_FILE
public :: READONLY_FILE, SINGLE_FILE, WRITEONLY_FILE

!> Indicate whether a file exists, perhaps with domain decomposition
interface file_exists
  module procedure FMS_file_exists
  module procedure UCLAND_file_exists
end interface

contains
!> Returns a name with "%#E" or "%E" replaced with the ensemble member number.
function ensembler(name, ens_no_in) result(en_nm)
  character(len=*),  intent(in) :: name       !< The name to be modified
  integer, optional, intent(in) :: ens_no_in  !< The number of the current ensemble member
  character(len=len(name)) :: en_nm  !< The name encoded with the ensemble number

  ! This function replaces "%#E" or "%E" with the ensemble number anywhere it
  ! occurs in name, with %E using 4 or 6 digits (depending on the ensemble size)
  ! and %#E using # digits, where # is a number from 1 to 9.

  character(len=len(name)) :: tmp
  character(10) :: ens_num_char
  character(3)  :: code_str
  integer :: ens_no
  integer :: n, is, ie

  en_nm = trim(name)
  if (index(name,"%") == 0) return

  if (present(ens_no_in)) then
    ens_no = ens_no_in
  else
    ens_no = get_ensemble_id()
  endif

  write(ens_num_char, '(I10)') ens_no ; ens_num_char = adjustl(ens_num_char)
  do
    is = index(en_nm,"%E")
    if (is == 0) exit
    if (len(en_nm) < len(trim(en_nm)) + len(trim(ens_num_char)) - 2) &
      call UCLAND_error(FATAL, "UCLAND_io ensembler: name "//trim(name)// &
      " is not long enough for %E expansion for ens_no "//trim(ens_num_char))
    tmp = en_nm(1:is-1)//trim(ens_num_char)//trim(en_nm(is+2:))
    en_nm = tmp
  enddo

  if (index(name,"%") == 0) return

  write(ens_num_char, '(I10.10)') ens_no
  do n=1,9 ; do
    write(code_str, '("%",I1,"E")') n

    is = index(en_nm,code_str)
    if (is == 0) exit
    if (ens_no < 10**n) then
      if (len(en_nm) < len(trim(en_nm)) + n-3) call UCLAND_error(FATAL, &
        "UCLAND_io ensembler: name "//trim(name)//" is not long enough for %E expansion.")
      tmp = en_nm(1:is-1)//trim(ens_num_char(11-n:10))//trim(en_nm(is+3:))
    else
      call UCLAND_error(FATAL, "UCLAND_io ensembler: Ensemble number is too large "//&
          "to be encoded with "//code_str//" in "//trim(name))
    endif
    en_nm = tmp
  enddo ; enddo

end function ensembler

!> Returns true if the named file or its domain-decomposed variant exists.
function UCLAND_file_exists(filename, UCLAND_Domain)
  character(len=*),        intent(in) :: filename   !< The name of the file being inquired about
  type(UCLAND_domain_type), intent(in) :: UCLAND_Domain !< The UCLAND_Domain that describes the decomposition

! This function uses the fms_io function file_exist to determine whether
! a named file (or its decomposed variant) exists.

  logical :: UCLAND_file_exists

  UCLAND_file_exists = file_exist(filename, UCLAND_Domain%mpp_domain)

end function UCLAND_file_exists

!> Returns true if the named file or its domain-decomposed variant exists.
function FMS_file_exists(filename, domain, no_domain)
  character(len=*), intent(in)         :: filename  !< The name of the file being inquired about
  type(domain2d), optional, intent(in) :: domain    !< The mpp domain2d that describes the decomposition
  logical,        optional, intent(in) :: no_domain !< This file does not use domain decomposition
! This function uses the fms_io function file_exist to determine whether
! a named file (or its decomposed variant) exists.

  logical :: FMS_file_exists

  FMS_file_exists = file_exist(filename, domain, no_domain)

end function FMS_file_exists

end module UCLAND_io
