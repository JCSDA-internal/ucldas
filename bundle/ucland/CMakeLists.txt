# (C) Copyright 2017-2020 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# UCLAND
################################################################################

cmake_minimum_required( VERSION 3.3.2 FATAL_ERROR )

project( ucland C CXX Fortran)

set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH})

#set( ECBUILD_DEFAULT_BUILD_TYPE Release )
set( ENABLE_OS_TESTS           OFF CACHE BOOL "Disable OS tests" FORCE )
set( ENABLE_LARGE_FILE_SUPPORT OFF CACHE BOOL "Disable testing of large file support" FORCE )
set( ENABLE_MPI ON CACHE BOOL "Compile with MPI" )

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.5 )

################################################################################
# Project
################################################################################

ecbuild_declare_project()

ecbuild_enable_fortran( REQUIRED )
ecbuild_add_cxx11_flags()

set( UCLAND_LINKER_LANGUAGE Fortran )

################################################################################
# Dependencies
################################################################################
# Boost
set( Boost_MINIMUM_VERSION "1.47" )
include_directories( ${Boost_INCLUDE_DIR} )

# NetCDF
find_package( NetCDF REQUIRED COMPONENTS Fortran )
include_directories( ${NETCDF_INCLUDE_DIRS} )

# MPI
ecbuild_find_mpi( COMPONENTS CXX Fortran REQUIRED )
ecbuild_include_mpi()
link_libraries(${MPI_CXX_LIBRARIES} ${MPI_Fortran_LIBRARIES})

################################################################################
# Definitions
################################################################################

################################################################################
# Export package info
################################################################################
list( APPEND UCLAND_TPLS MPI NetCDF )

set( UCLAND_INCLUDE_DIRS
                       ${CMAKE_CURRENT_SOURCE_DIR}/utils
                       ${CMAKE_Fortran_MODULE_DIRECTORY} )
set( UCLAND_LIBRARIES ucland )

get_directory_property( UCLAND_DEFINITIONS COMPILE_DEFINITIONS )

foreach( _tpl ${UCLAND_TPLS} )
string( TOUPPER ${_tpl} TPL )
list( APPEND UCLAND_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  ${${TPL}_TPL_DEFINITIONS}  )
list( APPEND UCLAND_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} ${${TPL}_TPL_INCLUDE_DIRS} )
list( APPEND UCLAND_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES}    ${${TPL}_TPL_LIBRARIES}    )
endforeach()

################################################################################
# Sources
################################################################################
include( ucland_compiler_flags )
include_directories( ${UCLAND_INCLUDE_DIRS} ${UCLAND_EXTRA_INCLUDE_DIRS} )

add_subdirectory( utils )

list( APPEND ucland_src_files
    ${src_files}
)

# Build Doxygen documentation
#add_subdirectory( Documents )

ecbuild_add_executable( TARGET  convert_restart_vec2tiles.x
                        SOURCES ./mains/convert_restart_vec2tiles.F90
                        LIBS    utils
                      )

if(ECBUILD_INSTALL_FORTRAN_MODULES)
  install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR} DESTINATION ${INSTALL_INCLUDE_DIR} )
endif()

################################################################################
# Finalise configuration
################################################################################

# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME ucland )

# print the summary of the configuration
ecbuild_print_summary()