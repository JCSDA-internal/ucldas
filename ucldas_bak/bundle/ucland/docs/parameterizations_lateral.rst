Lateral Parameterizations
=========================

The following sub-grid scale parameterizations generally yield fluxes that act in the lateral direction.

Lateral viscosity
-----------------

Laplacian and bi-harmonic viscosities with linear and Smagorinsky options are implemented in LND_hor_visc.

Gent-McWilliams/TEM/isopycnal height diffusion
----------------------------------------------

Lagrangian mean eddy mass transport is parameterized following Gent and McWilliams, XXXX, in LND_thickness_diffuse.

The diffusivity coefficients are calculated in LND_lateral_mixing_coeffs and LND_thickness_diffuse and includes constants and the Visbeck et al., XXXX, scaling.

A model of sub-grid scale Mesoscale Eddy Kinetic Energy (MEKE) is implement in LND_MEKE and the associated diffusivity added in LND_thickness_diffuse.

Backscatter
-----------

A parameterization of the upscale unresolved cascade utilizes LND_MEKE and negative Laplacian viscosity in LND_hor_visc.

Mixed layer restratification by sub-mesoscale eddies
----------------------------------------------------

Fox-Kemper et al., 2008, is implemented in LND_mixed_layer_restrat.

Tidal forcing
-------------

Astronomical tidal forcings and self-attraction and loading are implement in LND_tidal_forcing.


