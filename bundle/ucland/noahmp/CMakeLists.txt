list ( APPEND _noahmp_files
  noahmp/physics/machine.F
  noahmp/physics/set_soilveg.f
  noahmp/physics/sflx.f
  noahmp/physics/funcphys.f90
  noahmp/physics/namelist_soilveg.f
  noahmp/physics/sfc_noahmp_drv.f
  noahmp/physics/sfc_drv.f
  noahmp/physics/sfc_diff.f
  noahmp/physics/date_def.f
  noahmp/physics/machine.F
  noahmp/physics/noahmp_tables.f90
  noahmp/physics/module_sf_noahmp_glacier.f90
  noahmp/physics/surface_perturbation.F90
  noahmp/physics/physcons.F90
  noahmp/physics/module_sf_noahmplsm.f90
)

set ( noahmp_src_files
      ${_noahmp_files}
      PARENT_SCOPE
)
