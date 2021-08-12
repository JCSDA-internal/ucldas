ecbuild_add_executable( TARGET  ucldas_dirac.x
                        SOURCES Dirac.cc
                        LIBS    ucldas
                                saber
                      )

ecbuild_add_executable( TARGET  ucldas_forecast.x
                        SOURCES Forecast.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_hofx3d.x
                        SOURCES HofX3D.cc
                        LIBS    ucldas
)

ecbuild_add_executable( TARGET  ucldas_hofx.x
                        SOURCES HofX.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_enshofx.x
                        SOURCES EnsHofX.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_var.x
                        SOURCES Var.cc
                        LIBS    ucldas
                                saber
                      )

ecbuild_add_executable( TARGET  ucldas_parameters.x
                        SOURCES EstimateParams.cc
                        LIBS    ucldas
                                saber
                      )

ecbuild_add_executable( TARGET  ucldas_staticbinit.x
                        SOURCES StaticBInit.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_enspert.x
                        SOURCES GenEnsPertB.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_ensvariance.x
                        SOURCES EnsVariance.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_ensrecenter.x
                        SOURCES EnsRecenter.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_hybridgain.x
                        SOURCES HybridGain.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_checkpoint_model.x
                        SOURCES CheckpointModel.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_gridgen.x
                        SOURCES GridGen.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_letkf.x
                        SOURCES LETKF.cc
                        LIBS    ucldas
                      )
ecbuild_add_executable( TARGET  ucldas_convertstate.x
                        SOURCES ConvertState.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_addincrement.x
                        SOURCES AddIncrement.cc
                        LIBS    ucldas
                      )

ecbuild_add_executable( TARGET  ucldas_diffstates.x
                        SOURCES DiffStates.cc
                        LIBS    ucldas
                      )
