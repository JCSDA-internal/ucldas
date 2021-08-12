!> \brief Reads the only Fortran name list needed to boot-strap the model.
!!
!! The name list parameters indicate which directories to use for
!! certain types of input and output, and which files to look in for
!! the full parsable input parameter file(s).
module UCLAND_get_input

! This file is part of UCLAND. See LICENSE.md for the license.

use UCLAND_error_handler, only : UCLAND_mesg, UCLAND_error, FATAL, WARNING, is_root_pe
!use UCLAND_file_parser, only : open_param_file, param_file_type
use UCLAND_io, only : file_exists, close_file, slasher, ensembler
use UCLAND_io, only : open_namelist_file, check_nml_error

implicit none ; private

!public get_UCLAND_input

!> Container for paths and parameter file names.
type, public :: directories
  character(len=240) :: &
    restart_input_dir = ' ',& !< The directory to read restart and input files.
    restart_output_dir = ' ',&!< The directory into which to write restart files.
    output_directory = ' '    !< The directory to use to write the model output.
  character(len=2048) :: &
    input_filename  = ' '     !< A string that indicates the input files or how
                              !! the run segment should be started.
end type directories

end module UCLAND_get_input
