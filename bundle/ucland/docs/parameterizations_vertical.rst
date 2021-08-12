Vertical Parameterizations
==========================

The following sub-grid scale parameterizations generally yield fluxes that act in the vertical direction, with no lateral components resolved by the model.

Upper boundary
--------------

K-profile parameterization (KPP)
  Provided by module LND_KPP, uses the CVmix implementation of KPP.

Energetic Planetary Boundary Layer (ePBL)
  A energetically constrained boundary layer scheme following Reichl and Hallberg, 2017. Implemented in LND_energetic_PBL.

Bulk mixed layer (BML)
  A 2-layer bulk mixed layer used in pure-isopycnal model. Implemented in LND_bulk_mixed_layer.

Interior and bottom-driven mixing
---------------------------------

Kappa-shear
  LND_kappa_shear implement the shear-driven mixing of Jackson et al., XXXX.

Internal-tide driven mixing
  The schemes of St Laurent et al., XXXX, and Polzin et al., XXXX, and Melet et al., XXXX, are all implemented through LND_set_diffusivity and LND_diabatic_driver.

Vertical friction
-----------------

Vertical viscosity is implemented in LND_vert_frict and coefficient computed in LND_set_viscosity, although contributions to viscosity from other parameterizations are calculated in those respective modules (e.g. LND_kappa_shear, LND_KPP, LND_energetic_PBL).

Vertical diffusion
------------------

Vertical diffusion of scalars is implemented in LND_diabatic_driver although contributions to diffusion from other parameterizations are calculated in those respective modules (e.g. LND_kappa_shear, LND_KPP, LND_energetic_PBL).

Radiation
---------

Opacity
  Ocean color is prescribed or dynamically calculated in converted into optical properties in LND_opacity.

Short-wave absorption
  Optical properties from LND_opacity are used to calculate the convergence of shortwave radiation penetrating from the upper surface in LND_shortwave_abs.

Geothermal heating
------------------

Geothermal heat fluxes are implemented in LND_geothermal.

Isopycnal-mode entrainment and diapycnal diffusion
--------------------------------------------------

Diapycnal diffusion in a layered isopycnal mode following Hallberg, 2000, is implemented in LND_entrain_diffuse.
