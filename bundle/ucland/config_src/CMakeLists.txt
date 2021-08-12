list ( APPEND _files
config_src/dynamic/LND_memory.h
config_src/external/ODA_hooks/kdtree.f90
config_src/external/ODA_hooks/ocean_da_core.F90
config_src/external/ODA_hooks/ocean_da_types.F90
config_src/external/ODA_hooks/write_ocean_obs.F90
config_src/solo_driver/atmos_ocean_fluxes.F90
config_src/solo_driver/MESO_surface_forcing.F90
config_src/solo_driver/LND_surface_forcing.F90
config_src/solo_driver/user_surface_forcing.F90
)

if ( NOT ENABLE_OCEAN_BGC )
    list ( APPEND _files
    config_src/external/GFDL_ocean_BGC/generic_tracer.F90
    config_src/external/GFDL_ocean_BGC/generic_tracer_utils.F90
    )
endif ()

set ( config_src_files
      ${_files}
      PARENT_SCOPE
)
