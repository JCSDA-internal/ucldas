# (C) Copyright 2018 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# UCLAND
################################################################################

cmake_minimum_required( VERSION 3.12)
project( ucland VERSION 2020.4.0 LANGUAGES Fortran )

# ecbuild integration
find_package(ecbuild 3.3.2 REQUIRED)
include( ecbuild_system NO_POLICY_SCOPE )
ecbuild_declare_project()

list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
include( ucland_compiler_flags )

# ocean_bgc option
option( ENABLE_OCEAN_BGC "Compile and use land bgc modules in UCLAND")


################################################################################
# Dependencies
################################################################################

find_package(OpenMP COMPONENTS C Fortran)
find_package(MPI REQUIRED COMPONENTS C Fortran)
find_package(NetCDF REQUIRED COMPONENTS Fortran )
find_package(fms 2020.4.0 REQUIRED )
find_package(gsw 3.0.5 REQUIRED )


################################################################################
# Sources
################################################################################

set( UCLAND_LINKER_LANGUAGE Fortran )

# ucland library
#-------------------------------------------------------------------------------
add_subdirectory( config_src )
add_subdirectory( src )
add_subdirectory( noahmp )
add_subdirectory( utils )
add_subdirectory( driver )

list( APPEND ucland_src_files
    ${config_src_files}
    ${src_files}
    ${noahmp_src_files}
    ${utils_src_files}
    ${driver_src_files}
)

if (${ENABLE_OCEAN_BGC})
    add_definitions( -D_USE_GENERIC_TRACER )
    add_subdirectory( pkg/ocean_BGC )
    list( APPEND ucland_src_files ${ocean_bgc_src_files} )
endif()

ecbuild_add_library( TARGET   ucland
                     SOURCES  ${ucland_src_files}
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${UCLAND_LINKER_LANGUAGE}
                    )

# target_include_directories( ucland PRIVATE
#    ${CMAKE_Fortran_MODULE_DIRECTORY} )

target_link_libraries(ucland PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(ucland PUBLIC MPI::MPI_Fortran)
target_link_libraries(ucland PUBLIC OpenMP::OpenMP_C OpenMP::OpenMP_Fortran)
target_link_libraries(ucland PUBLIC fms)
target_link_libraries(ucland PUBLIC gsw)

# Fortran module output directory for build and install interfaces
set(MODULE_DIR module/${PROJECT_NAME}/${CMAKE_Fortran_COMPILER_ID}/${CMAKE_Fortran_COMPILER_VERSION})
set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR})
install(DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR}/ DESTINATION ${MODULE_DIR})
target_include_directories(${PROJECT_NAME} INTERFACE
                            $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${MODULE_DIR}>
                            $<INSTALL_INTERFACE:${MODULE_DIR}>)

# Installed .h include locations
target_include_directories(ucland PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/config_src/dynamic>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/framework>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/utils/src>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)


# ucland.x executable
#-------------------------------------------------------------------------------
set ( ucland_exe_files
    config_src/solo_driver/LND_driver.F90
)

ecbuild_add_executable( TARGET  ufsLandDriver.x
                        SOURCES ./driver/ufsLandDriver.f90
                        LIBS    ucland
                      )

ecbuild_add_executable( TARGET   convert_restart_vec2tiles.x
                        SOURCES  ./config_src/solo_driver/convert_restart_vec2tiles.F90
                        LIBS     ucland
                       )

ecbuild_add_executable( TARGET   convert_restart_tiles2vec.x
                        SOURCES  ./config_src/solo_driver/convert_restart_tiles2vec.F90
                        LIBS     ucland
                       )

ecbuild_add_executable( TARGET  transform_static_c96.x
                        SOURCES ./config_src/transform_static_c96.F90
                        LIBS    ucland
                      )

ecbuild_add_executable( TARGET   ucland.x
                        SOURCES  ${ucland_exe_files}
                        LIBS     ucland
                        LINKER_LANGUAGE ${UCLAND_LINKER_LANGUAGE}
                       )


################################################################################
# Finalise configuration
################################################################################

ecbuild_install_project( NAME ucland )
ecbuild_print_summary()
