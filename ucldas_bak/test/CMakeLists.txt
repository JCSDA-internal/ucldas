set( ucldas_test_input
  testinput/3dhyb.yml
  testinput/3dhybfgat.yml
  testinput/3dvarbump.yml
  testinput/3dvar_godas.yml
  testinput/3dvar_ucldas.yml
  testinput/3dvarlowres_ucldas.yml
  testinput/3dvarfgat.yml
  testinput/3dvarfgat_pseudo.yml
  testinput/addincrement.yml
  testinput/balance_mask.yml
  testinput/checkpointmodel.yml
  testinput/convertstate.yml
  testinput/convertstate_changevar.yml
  testinput/diffstates.yml
  testinput/dirac_bump_cov.yml
  testinput/dirac_horizfilt.yml
  testinput/dirac_ucldas_cov.yml
  testinput/dirac_ucldashyb_cov.yml
  testinput/enshofx_1.yml
  testinput/enshofx_2.yml
  testinput/enshofx_3.yml
  testinput/enshofx.yml
  testinput/enspert.yml
  testinput/ensrecenter.yml
  testinput/ensvariance.yml
  testinput/forecast_identity.yml
  testinput/forecast_ucland.yml
  testinput/forecast_ucland_bgc.yml
  testinput/forecast_pseudo.yml
  testinput/geometry.yml
  testinput/geometry_iterator.yml
  testinput/geometryatm.yml
  testinput/getvalues.yml
  testinput/gridgen.yml
  testinput/gridgen_small.yml
  testinput/hofx_3d.yml
  testinput/hofx_3dcrtm.yml
  testinput/hofx_4d.yml
  testinput/hofx_4d_pseudo.yml
  testinput/hybridgain.yml
  testinput/increment.yml
  testinput/letkf_observer.yml
  testinput/letkf_solver.yml
  testinput/lineargetvalues.yml
  testinput/linearmodel.yml
  testinput/makeobs.yml
  testinput/marine2ioda.yml
  testinput/model.yml
  testinput/obslocalization.yml
  testinput/parameters_bump_cor_nicas.yml
  testinput/parameters_bump_cov_lct.yml
  testinput/parameters_bump_cov_nicas.yml
  testinput/parameters_bump_loc.yml
  testinput/state.yml
  testinput/static_ucldaserror_init.yml
  testinput/static_ucldaserrorlowres_init.yml
  testinput/static_ucldaserror_test.yml
  testinput/varchange_ana2model.yml
  testinput/varchange_balance.yml
  testinput/varchange_balance_TSSSH.yml
  testinput/varchange_bkgerrfilt.yml
  testinput/varchange_bkgerrgodas.yml
  testinput/varchange_bkgerrucldas.yml
  testinput/varchange_horizfilt.yml
  testinput/varchange_vertconv.yml
)

set( ucldas_test_ref
  testref/3dhyb.test
  testref/3dhybfgat.test
  testref/3dvarbump.test
  testref/3dvar_godas.test
  testref/3dvar_ucldas.test
  testref/3dvarlowres_ucldas.test
  testref/3dvarfgat.test
  testref/3dvarfgat_pseudo.test
  testref/addincrement.test
  testref/balance_mask.test
  testref/checkpointmodel.test
  testref/convertstate.test
  testref/convertstate_changevar.test
  testref/diffstates.test
  testref/dirac_bump_cov.test
  testref/dirac_horizfilt.test
  testref/dirac_ucldas_cov.test
  testref/dirac_ucldashyb_cov.test
  testref/enshofx.test
  testref/enspert.test
  testref/ensrecenter.test
  testref/ensvariance.test
  testref/forecast_identity.test
  testref/forecast_ucland.test
  testref/forecast_ucland_bgc.test
  testref/forecast_pseudo.test
  testref/hofx_3d.test
  testref/hofx_3dcrtm.test
  testref/hofx_4d.test
  testref/hofx_4d_pseudo.test
  testref/hybridgain.test
  testref/letkf_observer.test
  testref/letkf_solver.test
  testref/makeobs.test
  testref/parameters_bump_cor_nicas.test
  testref/parameters_bump_cov_lct.test
)

set( ucldas_data_files
  Data/godas_sst_bgerr.nc
  Data/rossrad.dat
  Data/fields_metadata.yml
)

set( ucldas_model_param
  Data/72x35x25/diag_table
  Data/72x35x25/field_table
  Data/72x35x25/ice.bkgerror.nc
  Data/72x35x25/ocn.bkgerror.nc
  Data/72x35x25/wav.bkgerror.nc
  Data/72x35x25/LND_input
  Data/72x35x25/LND_input_small
  Data/72x35x25/LND_override_bgc
)

set( ucldas_model_namelist
  Data/72x35x25/input.nml
  Data/72x35x25/input_bgc.nml
  Data/72x35x25/input_small.nml
)

set( ucldas_model_restarts
  Data/72x35x25/INPUT/forcing_daily.nc
  Data/72x35x25/INPUT/forcing_monthly.nc
  Data/72x35x25/INPUT/hycom1_25.nc
  Data/72x35x25/INPUT/cice.res.nc
  Data/72x35x25/INPUT/layer_coord25.nc
  Data/72x35x25/INPUT/LND.res.nc
  Data/72x35x25/INPUT/sfc.res.nc
  Data/72x35x25/INPUT/wav.res.nc
  Data/72x35x25/INPUT/ocean_hgrid.nc
  Data/72x35x25/INPUT/ocean_topog.nc
)

set( ucldas_obs
  Data/Obs/adt.nc
  Data/Obs/biomass_p.nc
  Data/Obs/chl.nc
  Data/Obs/gmi_gpm_geoval.nc
  Data/Obs/gmi_gpm_obs.nc
  Data/Obs/icec.nc
  Data/Obs/icefb.nc
  Data/Obs/prof.nc
  Data/Obs/sss.nc
  Data/Obs/sst.nc
  Data/Obs/swh.nc
  Data/Obs/uocn_surface.nc
  Data/Obs/vocn_surface.nc
)

# Create Data directory for test input/output and symlink all files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testoutput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testref)
foreach(FILENAME ${ucldas_test_input} ${ucldas_test_ref})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

# Create Data directory for model input and symlink all files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/INPUT)
foreach(FILENAME ${ucldas_model_restarts})
     get_filename_component(filename ${FILENAME} NAME )
     execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/INPUT/${filename} )
endforeach(FILENAME)
install(FILES ${ucldas_model_restarts}
        DESTINATION ${INSTALL_DATA_DIR}/testdata/72x35x25/INPUT/ )

# LND's resource files
foreach(FILENAME ${ucldas_model_param} ${ucldas_data_files})
     get_filename_component(filename ${FILENAME} NAME )
     execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${filename} )
endforeach(FILENAME)
install(FILES ${ucldas_data_files}
        DESTINATION ${INSTALL_DATA_DIR}/testdata )


# FMS input.nml that may be overwritten during the testing
foreach(FILENAME ${ucldas_model_namelist})
     get_filename_component(filename ${FILENAME} NAME )
     execute_process( COMMAND ${CMAKE_COMMAND} -E copy
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/inputnml/${filename} )
endforeach(FILENAME)

# LND's output directory
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/RESTART)

ecbuild_add_resources( TARGET   ucldas_test_scripts
                       SOURCES_PACK
                       ${ucldas_test_input}
                       )
# Link to UCLDAS obs
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Data)
foreach(FILENAME ${ucldas_obs})
     get_filename_component(filename ${FILENAME} NAME )
     execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/Data/${filename} )
endforeach(FILENAME)


# Link CRTM coeff
if( ${crtm_FOUND} )

  # the coefficient files needed
  # TODO(someone) for now these are our own copy in git-lfs,
  #  because why download 627MB if I only need 7MB of files??
  #  Download these directly from aws instead when that is an option.
  list( APPEND crtm_coef
    Data/crtm/gmi_gpm.SpcCoeff.bin
    Data/crtm/gmi_gpm.TauCoeff.bin
    Data/crtm/CloudCoeff.bin
    Data/crtm/AerosolCoeff.bin
    Data/crtm/FASTEM6.MWwater.EmisCoeff.bin
  )

  # link the files
  foreach(FILENAME ${crtm_coef})
       get_filename_component(filename ${FILENAME} NAME )
       execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
             ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
             ${CMAKE_CURRENT_BINARY_DIR}/Data/${filename} )
  endforeach(FILENAME)
endif()


# Create directory for BUMP output
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bump)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bump_lowres)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bump-kmask)

# find compare.py provided by oops
find_file(COMPARE_BIN oops_compare.py HINTS ${oops_BINDIR})

#####################################################################

# Configurable parameters for the subsequent tests

set( UCLDAS_TESTS_MAX_MPI 2 CACHE STRING
  "Maximum number of PEs to use for UCLDAS's MPI based tests.\
  NOTE: should leave this at 2 to ensure all tests pass.")

set( UCLDAS_TESTS_DEFAULT_TOL "1.0e-12;0" CACHE STRING
  "Default test tolerances for compare.py based tests. (float_tol;int_tol) ")

set( UCLDAS_TESTS_FORC_DEFAULT_TOL OFF CACHE BOOL
  "If true, the specified test tolerances are ignored and\
  UCLDAS_TESTS_DEFAULT_TOL is used" )

set( UCLDAS_TESTS_FORC_TRAPFPE OFF CACHE BOOL
  "If true, the specified per-test trapping on/off flag is ignored\
  and is forced to ON for all tests" )

set( UCLDAS_TESTS_VALGRIND OFF CACHE BOOL
  "If true, some tests are run under valgrind")


#-------------------------------------------------------------------------------
# The following is a wrapper to simplify the generation of tests.
# There are two types of tests:
#  1) ucldas executables (EXE must be given)
#  2) interface tests  (SRC must be given)
# Ucldas executables by default will also run a second step where the output test
# log stream results are compared with reference values. The interface tests
# do not do this.
#
# Arguments:
#  NOCOMPARE - if flag is present, no compare test is performed
#  NAME      - the name of the test (after test_ucldas is prepended), as well
#              as the base name of the input confg file
#  EXE       - name of executable to be used.
#  SRC       - The name of the unit test source file to compile
#  CFG       - The name of the yaml file, if the default testinput/${NAME}.yaml
#              is not to be used
#  TOL       - The tolerances to use for the test "float_tolerance int_tolerance"
#              If none are given, ${UCLDAS_TESTS_DEFAULT_TOL} is used. Only used
#              if an EXE with no NOCOMPARE flag set.
#  MPI       - The number of MPI PEs to use. If not specified, ${UCLDAS_TESTS_MAX_MPI}
#              will be used
# TEST_DEPENDS - list of tests this test depends on
#-------------------------------------------------------------------------------
function(ucldas_add_test)
  # parse the passed arguments
  set(prefix     ARG)
  set(novals     NOCOMPARE NOTRAPFPE NOVALGRIND)
  set(singlevals NAME EXE SRC CFG MPI)
  set(multivals  TEST_DEPENDS TOL)
  cmake_parse_arguments(${prefix}
                        "${novals}" "${singlevals}" "${multivals}"
                        ${ARGN})

  # set the tolerances to defaults if none given, or if we want to override
  IF(NOT ARG_TOL OR UCLDAS_TESTS_FORC_DEFAULT_TOL)
    SET(ARG_TOL ${UCLDAS_TESTS_DEFAULT_TOL})
  ENDIF()

  # determine if floating point error trapping should be set
  if ( ARG_NOTRAPFPE AND NOT UCLDAS_TESTS_FORC_TRAPFPE)
    set ( TRAPFPE_ENV "OOPS_TRAPFPE=0")
  else()
    set ( TRAPFPE_ENV "OOPS_TRAPFPE=1")
  endif()

  if ( NOT UCLDAS_TESTS_VALGRIND )
    set ( ARG_NOVALGRIND TRUE)
  endif()

  # determine the default config file name
  if ( ARG_CFG )
    set ( CONFIG_FILE testinput/${ARG_CFG} )
  else()
    set ( CONFIG_FILE testinput/${ARG_NAME}.yml )
  endif()

  # MPI PEs
  set( MPI ${UCLDAS_TESTS_MAX_MPI} )
  if ( ARG_MPI )
    set( MPI ${ARG_MPI})
  endif()

  # Are we building a unit test / or running a ucldas executable?
  if ( ARG_SRC )
    # building a unit test, therfore also assume no compare step
    ecbuild_add_test( TARGET  test_ucldas_${ARG_NAME}
                      SOURCES executables/${ARG_SRC}
                      ARGS    ${CONFIG_FILE}
                      LIBS    ucldas
                      MPI     ${MPI}
                      ENVIRONMENT ${TRAPFPE_ENV}
                      TEST_DEPENDS ${ARG_TEST_DEPENDS})
    set( EXE ./test_ucldas_${ARG_NAME})

  else()
    LIST(GET ARG_TOL 0 TOL_F)
    LIST(GET ARG_TOL 1 TOL_I)

    # find the MPI command
    set(MPI_CMD "${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPI}")

    # running a ucldas executable.
    # This is run with the run wrapper, which will also optionally
    # run the comapre.py script afterwards
    ecbuild_add_test( TARGET  test_ucldas_${ARG_NAME}
                      TYPE    SCRIPT
                      COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/test_wrapper.sh"
                      ARGS    "${CMAKE_BINARY_DIR}/bin/${ARG_EXE}"
                              ${CONFIG_FILE}
                      ENVIRONMENT
                              ${TRAPFPE_ENV}
                              COMPARE_SCRIPT=${COMPARE_BIN}
                              COMPARE_TESTNAME=${ARG_NAME}
                              COMPARE_TOL_F=${TOL_F}
                              COMPARE_TOL_I=${TOL_I}
                              MPI_CMD=${MPI_CMD}
                              SKIP_COMPARE=${ARG_NOCOMPARE}
                      DEPENDS ${ARG_EXE}
                      TEST_DEPENDS ${ARG_TEST_DEPENDS})
    set( EXE ${CMAKE_BINARY_DIR}/bin/${ARG_EXE})

  endif()

  # optional valgrind tests
  if ( NOT ARG_NOVALGRIND )
    ecbuild_add_test( TARGET test_ucldas_${ARG_NAME}_valgrind
                      MPI ${MPI}
                      COMMAND valgrind
                      ARGS -q --error-exitcode=42
                           --suppressions=${CMAKE_CURRENT_SOURCE_DIR}/valgrind.sup
                           --gen-suppressions=all
                           ${EXE} ${CONFIG_FILE}
                      TEST_DEPENDS test_ucldas_${ARG_NAME}
                    )
  endif()
endfunction()



#-------------------------------------------------------------------------------
# Tests that create data other tests will use
#-------------------------------------------------------------------------------

# Create subsampled netcdf files
add_test ( test_ucldas_subsample_netcdf
           bash
           ${CMAKE_CURRENT_SOURCE_DIR}/Data/subsample_uclandfiles.sh )

# Test of grid generation and create subsampled grid
ucldas_add_test( NAME gridgen_small
               EXE  ucldas_gridgen.x
               NOCOMPARE
               TEST_DEPENDS test_ucldas_subsample_netcdf )

# Test of grid generation and create regular grid
ucldas_add_test( NAME gridgen
               EXE  ucldas_gridgen.x
               NOCOMPARE )

# Remapping UCLAND (horiz+vertical intterpolation)
ucldas_add_test( NAME convertstate
               EXE  ucldas_convertstate.x
               TOL  2e-5 0
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )

# Apply a nonlinear change of variable to an ensemble of states
ucldas_add_test( NAME convertstate_changevar
               EXE  ucldas_convertstate.x
               TOL  2e-5 0
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )

# Test of read/write restart for UCLAND&ICE, model advance = Id
ucldas_add_test( NAME forecast_identity
               EXE  ucldas_forecast.x
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen)

# Test ucland forecast
ucldas_add_test( NAME forecast_ucland
               EXE  ucldas_forecast.x
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )

# Test ucland_bgc forecast (optional)
if ( ENABLE_OCEAN_BGC )
ucldas_add_test( NAME forecast_ucland_bgc
               EXE  ucldas_forecast.x
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )
endif()

# Test PseudoModel forecast
ucldas_add_test( NAME forecast_pseudo
               EXE  ucldas_forecast.x
               TEST_DEPENDS test_ucldas_forecast_ucland )

# Initialize static error covariance model defined in ucldas, used for 3DVAR
ucldas_add_test( NAME static_ucldaserror_init
               EXE  ucldas_staticbinit.x
               NOCOMPARE
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME static_ucldaserrorlowres_init
               EXE  ucldas_staticbinit.x
               NOCOMPARE
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )

# Use the ucldas_dirac.x application to create a mask for the dynamic height Jacobians
ucldas_add_test( NAME balance_mask
               EXE  ucldas_dirac.x
               TEST_DEPENDS test_ucldas_gridgen )

# Create dc/dt file for ocean/ice balance and convert dirac file into a mask
add_test ( test_ucldas_create_kmask
           python3
           ${CMAKE_CURRENT_SOURCE_DIR}/Data/create_kmask.py
           TEST_DEPENDS test_static_ucldaserror_init
                        test_balance_mask )

# ensemble generation
ucldas_add_test( NAME enspert
               EXE  ucldas_enspert.x
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen
                            test_ucldas_static_ucldaserror_init)

# TODO Test below can take a few hours to complete.
# Revive once we have tiered testing?
#ucldas_add_test( NAME covar_lengthscale
#               EXE  ucldas_staticbinit.x
#               TEST_DEPENDS test_ucldas_gridgen)

# Generate synthetic observations
ucldas_add_test( NAME makeobs
               EXE  ucldas_hofx.x
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )

# Tests of class interfaces
#-------------------------------------------------------------------------------
ucldas_add_test( NAME geometry
               SRC  TestGeometry.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME geometry_iterator
               SRC  TestGeometryIterator.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME geometryatm
               SRC  TestGeometry.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME state
               SRC  TestState.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME getvalues
               SRC  TestGetValues.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME model
               SRC  TestModel.cc
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME modelaux
               SRC  TestModelAuxControl.cc
               CFG  model.yml
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME increment
               SRC  TestIncrement.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME lineargetvalues
               SRC  TestLinearGetValues.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME errorcovariance
               SRC  TestErrorCovariance.cc
               CFG  static_ucldaserror_test.yml
               TEST_DEPENDS test_ucldas_gridgen
                            test_ucldas_static_ucldaserror_init )

ucldas_add_test( NAME linearmodel
               SRC  TestLinearModel.cc
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen
                            test_ucldas_static_ucldaserror_init )

ucldas_add_test( NAME varchange_ana2model
               SRC  TestNLVariableChange.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME varchange_balance
               SRC  TestVariableChange.cc
               TEST_DEPENDS test_ucldas_gridgen
                            test_ucldas_create_dcdt )

ucldas_add_test( NAME varchange_balance_TSSSH
               SRC  TestVariableChange.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME varchange_bkgerrfilt
               SRC TestVariableChange.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME varchange_horizfilt
               SRC TestVariableChange.cc
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME varchange_bkgerrucldas
               SRC  TestVariableChange.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME varchange_bkgerrgodas
               SRC  TestVariableChange.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME varchange_vertconv
               SRC  TestVariableChange.cc
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME obslocalization
               SRC  TestObsLocalization.cc
               TEST_DEPENDS test_ucldas_gridgen
               NOCOMPARE )


#-------------------------------------------------------------------------------
# Tests using the main UCLDAS applications
#  TEST output stream is compared against existing testoutput/ files
#-------------------------------------------------------------------------------
ucldas_add_test( NAME ensvariance
               EXE  ucldas_ensvariance.x
               TEST_DEPENDS test_ucldas_enspert )

ucldas_add_test( NAME parameters_bump_loc
               EXE  ucldas_parameters.x
               NOCOMPARE
               TEST_DEPENDS test_ucldas_enspert )

ucldas_add_test( NAME ensrecenter
               EXE  ucldas_ensrecenter.x
               TEST_DEPENDS test_ucldas_enspert )

ucldas_add_test( NAME hybridgain
               EXE  ucldas_hybridgain.x
               TEST_DEPENDS test_ucldas_enspert )

# TODO the next couple of tests are a little slow.
# Revive them once we have tiered testing?
# ucldas_add_test( NAME parameters_bump_cov_lct
#                EXE  ucldas_parameters.x
#                TEST_DEPENDS test_ucldas_enspert )
#
# ucldas_add_test( NAME parameters_bump_cov_nicas
#                EXE  ucldas_parameters.x
#                TEST_DEPENDS test_ucldas_parameters_bump_cov_lct )

# ucldas_add_test( NAME dirac_bump_cov
#                EXE  ucldas_dirac.x )

ucldas_add_test( NAME parameters_bump_cor_nicas
               NOTRAPFPE
               EXE  ucldas_parameters.x
               TEST_DEPENDS test_ucldas_gridgen_small
                            test_ucldas_convertstate)


# Dirac tests

ucldas_add_test( NAME dirac_ucldas_cov
               EXE  ucldas_dirac.x
               TEST_DEPENDS test_ucldas_static_ucldaserror_init)

ucldas_add_test( NAME dirac_ucldashyb_cov
               EXE  ucldas_dirac.x
               TEST_DEPENDS test_ucldas_parameters_bump_loc)

ucldas_add_test( NAME dirac_horizfilt
               EXE  ucldas_dirac.x
               TEST_DEPENDS test_ucldas_static_ucldaserror_init)

#ecbuild_add_test( TARGET  test_ucldas_localization
#                  SOURCES executables/TestLocalization.cc
#                  ARGS    "testinput/localization_test.yml"
#                  MPI     1
#                  LIBS    ucldas )

#ecbuild_add_test( TARGET test_ucldas_obserrorcov
#                  SOURCES executables/TestObsErrorCovariance.cc
#                  ARGS "testinput/obserrorcovariance_test.yml"
#                  LIBS    ucldas )
#

# h(x) executables
ucldas_add_test( NAME hofx_3d
               EXE  ucldas_hofx3d.x
               TEST_DEPENDS test_ucldas_gridgen )

if( ${crtm_FOUND} )
 ucldas_add_test( NAME hofx_3dcrtm
                EXE  ucldas_hofx3d.x
                TEST_DEPENDS test_ucldas_gridgen )
endif()

ucldas_add_test( NAME hofx_4d
               EXE  ucldas_hofx.x
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME hofx_4d_pseudo
               EXE  ucldas_hofx.x
               TEST_DEPENDS test_ucldas_forecast_ucland )

# TODO enshofx currently breaks compare.py
# Re-enable the comparison once that's fixed
ucldas_add_test( NAME enshofx
               EXE  ucldas_enshofx.x
               MPI 3
               NOCOMPARE
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_enspert )


# variational methods
ucldas_add_test( NAME 3dvar_ucldas
               EXE  ucldas_var.x
               TOL  4e-9 0
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_static_ucldaserror_init )

ucldas_add_test( NAME 3dvarbump
               EXE  ucldas_var.x
               TOL  4e-9 0
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_parameters_bump_cor_nicas )

ucldas_add_test( NAME 3dvar_godas
               EXE  ucldas_var.x
               TOL  2e-6 0
               TEST_DEPENDS test_ucldas_static_ucldaserror_init )

# TODO: Remove NOCOMPARE when oops PR#585 is merged
ucldas_add_test( NAME addincrement
               EXE  ucldas_addincrement.x
               NOCOMPARE
               TEST_DEPENDS 3dvar_godas )

ucldas_add_test( NAME 3dvarlowres_ucldas
               EXE  ucldas_var.x
               TOL  0.1 0
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_static_ucldaserrorsmall_init )

ucldas_add_test( NAME diffstates
               EXE  ucldas_diffstates.x
               TEST_DEPENDS test_ucldas_gridgen )

ucldas_add_test( NAME 3dvarfgat
               EXE  ucldas_var.x
               TOL  1e-6 0
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_static_ucldaserror_init )

ucldas_add_test( NAME 3dvarfgat_pseudo
               EXE  ucldas_var.x
               TOL  1e-6 0
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_static_ucldaserror_init )

ucldas_add_test( NAME 3dhyb
               EXE  ucldas_var.x
               TOL  2e-7 0
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_static_ucldaserror_init
                            test_ucldas_parameters_bump_loc )

ucldas_add_test( NAME 3dhybfgat
               EXE  ucldas_var.x
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_static_ucldaserror_init
                            test_ucldas_parameters_bump_loc )

# ucldas_add_test( NAME 4dhyb
#                EXE  ucldas_var.x
#                TEST_DEPENDS test_ucldas_static_ucldaserror_init
#                             test_ucldas_parameters_bump_loc )

ucldas_add_test( NAME letkf_observer
               EXE  ucldas_letkf.x
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_enspert )

ecbuild_add_test( TARGET letkf_observer_post
                  TYPE SCRIPT
                  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/letkf_obscat.sh
                  TEST_DEPENDS letkf_observer)

ucldas_add_test( NAME letkf_solver
               EXE  ucldas_letkf.x
               NOTRAPFPE
               TEST_DEPENDS letkf_observer_post )

# restart checkpointing
ucldas_add_test( NAME checkpointmodel
               EXE  ucldas_checkpoint_model.x
               NOTRAPFPE
               TEST_DEPENDS test_ucldas_3dvar_godas)
