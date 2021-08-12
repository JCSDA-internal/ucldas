# (C) Copyright 2017-2020 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# only the source code in this directory, all other files
# are handled through the subdirectory's respective CMakeLists.txt
list( APPEND ucldas_src_files
  Traits.h
  Fortran.h
)

# a convenience function to make adding source files from subdirectories easier
# (to keep from having to use the full path every time)
function(ucldas_target_sources)
  foreach( src IN LISTS ARGN)
    target_sources(ucldas PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/${src})
  endforeach()
endfunction()

# The main ucldas library... behold its glory
ecbuild_add_library( TARGET   ucldas
                     SOURCES ${ucldas_src_files}
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${UCLDAS_LINKER_LANGUAGE}
                    )

target_include_directories(ucldas PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/..>
  $<INSTALL_INTERFACE:include/ucldas>)

target_compile_features( ucldas PUBLIC cxx_std_11 )

target_link_libraries( ucldas PUBLIC NetCDF::NetCDF_Fortran )
target_link_libraries( ucldas PUBLIC fckit )
target_link_libraries( ucldas PUBLIC atlas )
target_link_libraries( ucldas PUBLIC atlas_f )
target_link_libraries( ucldas PUBLIC fms )
target_link_libraries( ucldas PUBLIC gsw )
target_link_libraries( ucldas PUBLIC ucland )
target_link_libraries( ucldas PUBLIC oops )
target_link_libraries( ucldas PUBLIC saber )
target_link_libraries( ucldas PUBLIC ioda )
target_link_libraries( ucldas PUBLIC ufo )

# Add source code in the subdirectories
add_subdirectory(Covariance)
add_subdirectory(Fields)
add_subdirectory(Geometry)
add_subdirectory(GeometryIterator)
add_subdirectory(GetValues)
add_subdirectory(Increment)
add_subdirectory(LinearModel)
add_subdirectory(Localization)
add_subdirectory(Model)
add_subdirectory(ModelBias)
add_subdirectory(State)
add_subdirectory(Transforms)
add_subdirectory(Utils)

## Fortran modules
set(MODULE_DIR ${PROJECT_NAME}/module)
set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR})
install(DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR}/ DESTINATION ${CMAKE_INSTALL_LIBDIR}/${MODULE_DIR})
target_include_directories(${PROJECT_NAME} INTERFACE
                            $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${MODULE_DIR}>
                            $<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}/${MODULE_DIR}>)
