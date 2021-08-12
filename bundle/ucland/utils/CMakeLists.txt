list ( APPEND _utils_files
utils/src/module_interpolation_utilities.f90
utils/src/module_cosine_zenith.f90
utils/src/module_time_utilities.f90
utils/src/module_error_handling.f90
utils/src/module_nf90_utilities.F90
)

set ( utils_src_files
      ${_utils_files}
      PARENT_SCOPE
)
