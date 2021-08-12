list ( APPEND _driver_files
driver/src/ufsLandNoahMPRestartModule.f90
driver/src/ufsLandNoahRestartModule.f90 
driver/src/ufsLandStaticModule.f90
driver/src/ufsLandNoahType.f90
driver/src/ufsLandNoahMPType.f90
driver/src/ufsLandForcingModule.f90
driver/src/ufsLandNoahDriverModule.f90
driver/src/ufsLandNoahMPDriverModule.f90
driver/src/ufsLandIOModule.f90
driver/src/ufsLandInitialModule.f90
driver/src/ufsLandNamelistRead.f90
)

set ( driver_src_files
      ${_driver_files}
      PARENT_SCOPE
)
