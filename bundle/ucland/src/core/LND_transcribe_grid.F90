!> Module with routines for copying information from a shared dynamic horizontal
!! grid to an ocean-specific horizontal grid and the reverse.
module LND_transcribe_grid

! This file is part of UCLAND. See LICENSE.md for the license.

use LND_array_transform, only: rotate_array, rotate_array_pair
use LND_domains, only : pass_var, pass_vector
use LND_domains, only : To_All, SCALAR_PAIR, CGRID_NE, AGRID, BGRID_NE, CORNER
use LND_dyn_horgrid, only : dyn_horgrid_type, set_derived_dyn_horgrid
use LND_error_handler, only : LND_error, LND_mesg, FATAL, WARNING
use LND_grid, only : ocean_grid_type, set_derived_metrics
use LND_unit_scaling, only : unit_scale_type


implicit none ; private

public copy_dyngrid_to_LND_grid, copy_LND_grid_to_dyngrid, rotate_dyngrid

contains

!> Copies information from a dynamic (shared) horizontal grid type into an
!! ocean_grid_type.
subroutine copy_dyngrid_to_LND_grid(dG, oG, US)
  type(dyn_horgrid_type), intent(in)    :: dG  !< Common horizontal grid type
  type(ocean_grid_type),  intent(inout) :: oG  !< Ocean grid type
  type(unit_scale_type),  intent(in)    :: US  !< A dimensional unit scaling type

  integer :: isd, ied, jsd, jed      ! Common data domains.
  integer :: IsdB, IedB, JsdB, JedB  ! Common data domains.
  integer :: ido, jdo, Ido2, Jdo2    ! Indexing offsets between the grids.
  integer :: Igst, Jgst              ! Global starting indices.
  integer :: i, j

  ! LND_grid_init and create_dyn_horgrid are called outside of this routine.
  ! This routine copies over the fields that were set by LND_initialized_fixed.

  ! Determine the indexing offsets between the grids.
  ido = dG%idg_offset - oG%idg_offset
  jdo = dG%jdg_offset - oG%jdg_offset

  isd = max(oG%isd, dG%isd+ido) ; jsd = max(oG%jsd, dG%jsd+jdo)
  ied = min(oG%ied, dG%ied+ido) ; jed = min(oG%jed, dG%jed+jdo)
  IsdB = max(oG%IsdB, dG%IsdB+ido) ; JsdB = max(oG%JsdB, dG%JsdB+jdo)
  IedB = min(oG%IedB, dG%IedB+ido) ; JedB = min(oG%JedB, dG%JedB+jdo)

  ! Check that the grids conform.
  if ((isd > oG%isc) .or. (ied < oG%ied) .or. (jsd > oG%jsc) .or. (jed > oG%jed)) &
    call LND_error(FATAL, "copy_dyngrid_to_LND_grid called with incompatible grids.")

  do i=isd,ied ; do j=jsd,jed
    oG%geoLonT(i,j) = dG%geoLonT(i+ido,j+jdo)
    oG%geoLatT(i,j) = dG%geoLatT(i+ido,j+jdo)
    oG%dxT(i,j) = dG%dxT(i+ido,j+jdo)
    oG%dyT(i,j) = dG%dyT(i+ido,j+jdo)
    oG%areaT(i,j) = dG%areaT(i+ido,j+jdo)
    oG%bathyT(i,j) = dG%bathyT(i+ido,j+jdo)

    oG%dF_dx(i,j) = dG%dF_dx(i+ido,j+jdo)
    oG%dF_dy(i,j) = dG%dF_dy(i+ido,j+jdo)
    oG%sin_rot(i,j) = dG%sin_rot(i+ido,j+jdo)
    oG%cos_rot(i,j) = dG%cos_rot(i+ido,j+jdo)
    oG%mask2dT(i,j) = dG%mask2dT(i+ido,j+jdo)
  enddo ; enddo

  do I=IsdB,IedB ; do j=jsd,jed
    oG%geoLonCu(I,j) = dG%geoLonCu(I+ido,j+jdo)
    oG%geoLatCu(I,j) = dG%geoLatCu(I+ido,j+jdo)
    oG%dxCu(I,j) = dG%dxCu(I+ido,j+jdo)
    oG%dyCu(I,j) = dG%dyCu(I+ido,j+jdo)
    oG%dy_Cu(I,j) = dG%dy_Cu(I+ido,j+jdo)

    oG%mask2dCu(I,j) = dG%mask2dCu(I+ido,j+jdo)
    oG%areaCu(I,j) = dG%areaCu(I+ido,j+jdo)
    oG%IareaCu(I,j) = dG%IareaCu(I+ido,j+jdo)
  enddo ; enddo

  do i=isd,ied ; do J=JsdB,JedB
    oG%geoLonCv(i,J) = dG%geoLonCv(i+ido,J+jdo)
    oG%geoLatCv(i,J) = dG%geoLatCv(i+ido,J+jdo)
    oG%dxCv(i,J) = dG%dxCv(i+ido,J+jdo)
    oG%dyCv(i,J) = dG%dyCv(i+ido,J+jdo)
    oG%dx_Cv(i,J) = dG%dx_Cv(i+ido,J+jdo)

    oG%mask2dCv(i,J) = dG%mask2dCv(i+ido,J+jdo)
    oG%areaCv(i,J) = dG%areaCv(i+ido,J+jdo)
    oG%IareaCv(i,J) = dG%IareaCv(i+ido,J+jdo)
  enddo ; enddo

  do I=IsdB,IedB ; do J=JsdB,JedB
    oG%geoLonBu(I,J) = dG%geoLonBu(I+ido,J+jdo)
    oG%geoLatBu(I,J) = dG%geoLatBu(I+ido,J+jdo)
    oG%dxBu(I,J) = dG%dxBu(I+ido,J+jdo)
    oG%dyBu(I,J) = dG%dyBu(I+ido,J+jdo)
    oG%areaBu(I,J) = dG%areaBu(I+ido,J+jdo)
    oG%CoriolisBu(I,J) = dG%CoriolisBu(I+ido,J+jdo)
    oG%mask2dBu(I,J) = dG%mask2dBu(I+ido,J+jdo)
  enddo ; enddo

  oG%bathymetry_at_vel = dG%bathymetry_at_vel
  if (oG%bathymetry_at_vel) then
    do I=IsdB,IedB ; do j=jsd,jed
      oG%Dblock_u(I,j) = dG%Dblock_u(I+ido,j+jdo)
      oG%Dopen_u(I,j) = dG%Dopen_u(I+ido,j+jdo)
    enddo ; enddo
    do i=isd,ied ; do J=JsdB,JedB
      oG%Dblock_v(i,J) = dG%Dblock_v(i+ido,J+jdo)
      oG%Dopen_v(i,J) = dG%Dopen_v(i+ido,J+jdo)
    enddo ; enddo
  endif

  oG%gridLonT(oG%isg:oG%ieg) = dG%gridLonT(dG%isg:dG%ieg)
  oG%gridLatT(oG%jsg:oG%jeg) = dG%gridLatT(dG%jsg:dG%jeg)
  ! The more complicated logic here avoids segmentation faults if one grid uses
  ! global symmetric memory while the other does not.  Because a northeast grid
  ! convention is being used, the upper bounds for each array correspond.
  !   Note that the dynamic grid always uses symmetric memory.
  Ido2 = dG%IegB-oG%IegB ; Igst = max(oG%IsgB, (dG%isg-1)-Ido2)
  Jdo2 = dG%JegB-oG%JegB ; Jgst = max(oG%JsgB, (dG%jsg-1)-Jdo2)
  do I=Igst,oG%IegB ; oG%gridLonB(I) = dG%gridLonB(I+Ido2) ; enddo
  do J=Jgst,oG%JegB ; oG%gridLatB(J) = dG%gridLatB(J+Jdo2) ; enddo

  ! Copy various scalar variables and strings.
  oG%x_axis_units = dG%x_axis_units ; oG%y_axis_units = dG%y_axis_units
  oG%areaT_global = dG%areaT_global ; oG%IareaT_global = dG%IareaT_global
  oG%south_lat = dG%south_lat ; oG%west_lon  = dG%west_lon
  oG%len_lat = dG%len_lat ; oG%len_lon = dG%len_lon
  oG%Rad_Earth = dG%Rad_Earth ; oG%max_depth = dG%max_depth

! Update the halos in case the dynamic grid has smaller halos than the ocean grid.
  call pass_var(oG%areaT, oG%Domain)
  call pass_var(oG%bathyT, oG%Domain)
  call pass_var(oG%geoLonT, oG%Domain)
  call pass_var(oG%geoLatT, oG%Domain)
  call pass_vector(oG%dxT, oG%dyT, oG%Domain, To_All+Scalar_Pair, AGRID)
  call pass_vector(oG%dF_dx, oG%dF_dy, oG%Domain, To_All, AGRID)
  call pass_vector(oG%cos_rot, oG%sin_rot, oG%Domain, To_All, AGRID)
  call pass_var(oG%mask2dT, oG%Domain)

  call pass_vector(oG%areaCu, oG%areaCv, oG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(oG%dyCu, oG%dxCv, oG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(oG%dxCu, oG%dyCv, oG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(oG%dy_Cu, oG%dx_Cv, oG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(oG%mask2dCu, oG%mask2dCv, oG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(oG%IareaCu, oG%IareaCv, oG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(oG%IareaCu, oG%IareaCv, oG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(oG%geoLatCu, oG%geoLatCv, oG%Domain, To_All+Scalar_Pair, CGRID_NE)

  call pass_var(oG%areaBu, oG%Domain, position=CORNER)
  call pass_var(oG%geoLonBu, oG%Domain, position=CORNER, inner_halo=oG%isc-isd)
  call pass_var(oG%geoLatBu, oG%Domain, position=CORNER)
  call pass_vector(oG%dxBu, oG%dyBu, oG%Domain, To_All+Scalar_Pair, BGRID_NE)
  call pass_var(oG%CoriolisBu, oG%Domain, position=CORNER)
  call pass_var(oG%mask2dBu, oG%Domain, position=CORNER)

  if (oG%bathymetry_at_vel) then
    call pass_vector(oG%Dblock_u, oG%Dblock_v, oG%Domain, To_All+Scalar_Pair, CGRID_NE)
    call pass_vector(oG%Dopen_u, oG%Dopen_v, oG%Domain, To_All+Scalar_Pair, CGRID_NE)
  endif

  call set_derived_metrics(oG, US)

end subroutine copy_dyngrid_to_LND_grid


!> Copies information from an ocean_grid_type into a dynamic (shared)
!! horizontal grid type.
subroutine copy_LND_grid_to_dyngrid(oG, dG, US)
  type(ocean_grid_type),  intent(in)    :: oG  !< Ocean grid type
  type(dyn_horgrid_type), intent(inout) :: dG  !< Common horizontal grid type
  type(unit_scale_type), optional, intent(in) :: US !< A dimensional unit scaling type

  integer :: isd, ied, jsd, jed      ! Common data domains.
  integer :: IsdB, IedB, JsdB, JedB  ! Common data domains.
  integer :: ido, jdo, Ido2, Jdo2    ! Indexing offsets between the grids.
  integer :: Igst, Jgst              ! Global starting indices.
  integer :: i, j

  ! LND_grid_init and create_dyn_horgrid are called outside of this routine.
  ! This routine copies over the fields that were set by LND_initialized_fixed.

  ! Determine the indexing offsets between the grids.
  ido = oG%idG_offset - dG%idG_offset
  jdo = oG%jdG_offset - dG%jdG_offset

  isd = max(dG%isd, oG%isd+ido) ; jsd = max(dG%jsd, oG%jsd+jdo)
  ied = min(dG%ied, oG%ied+ido) ; jed = min(dG%jed, oG%jed+jdo)
  IsdB = max(dG%IsdB, oG%IsdB+ido) ; JsdB = max(dG%JsdB, oG%JsdB+jdo)
  IedB = min(dG%IedB, oG%IedB+ido) ; JedB = min(dG%JedB, oG%JedB+jdo)

  ! Check that the grids conform.
  if ((isd > dG%isc) .or. (ied < dG%ied) .or. (jsd > dG%jsc) .or. (jed > dG%jed)) &
    call LND_error(FATAL, "copy_dyngrid_to_LND_grid called with incompatible grids.")

  do i=isd,ied ; do j=jsd,jed
    dG%geoLonT(i,j) = oG%geoLonT(i+ido,j+jdo)
    dG%geoLatT(i,j) = oG%geoLatT(i+ido,j+jdo)
    dG%dxT(i,j) = oG%dxT(i+ido,j+jdo)
    dG%dyT(i,j) = oG%dyT(i+ido,j+jdo)
    dG%areaT(i,j) = oG%areaT(i+ido,j+jdo)
    dG%bathyT(i,j) = oG%bathyT(i+ido,j+jdo)

    dG%dF_dx(i,j) = oG%dF_dx(i+ido,j+jdo)
    dG%dF_dy(i,j) = oG%dF_dy(i+ido,j+jdo)
    dG%sin_rot(i,j) = oG%sin_rot(i+ido,j+jdo)
    dG%cos_rot(i,j) = oG%cos_rot(i+ido,j+jdo)
    dG%mask2dT(i,j) = oG%mask2dT(i+ido,j+jdo)
  enddo ; enddo

  do I=IsdB,IedB ; do j=jsd,jed
    dG%geoLonCu(I,j) = oG%geoLonCu(I+ido,j+jdo)
    dG%geoLatCu(I,j) = oG%geoLatCu(I+ido,j+jdo)
    dG%dxCu(I,j) = oG%dxCu(I+ido,j+jdo)
    dG%dyCu(I,j) = oG%dyCu(I+ido,j+jdo)
    dG%dy_Cu(I,j) = oG%dy_Cu(I+ido,j+jdo)

    dG%mask2dCu(I,j) = oG%mask2dCu(I+ido,j+jdo)
    dG%areaCu(I,j) = oG%areaCu(I+ido,j+jdo)
    dG%IareaCu(I,j) = oG%IareaCu(I+ido,j+jdo)
  enddo ; enddo

  do i=isd,ied ; do J=JsdB,JedB
    dG%geoLonCv(i,J) = oG%geoLonCv(i+ido,J+jdo)
    dG%geoLatCv(i,J) = oG%geoLatCv(i+ido,J+jdo)
    dG%dxCv(i,J) = oG%dxCv(i+ido,J+jdo)
    dG%dyCv(i,J) = oG%dyCv(i+ido,J+jdo)
    dG%dx_Cv(i,J) = oG%dx_Cv(i+ido,J+jdo)

    dG%mask2dCv(i,J) = oG%mask2dCv(i+ido,J+jdo)
    dG%areaCv(i,J) = oG%areaCv(i+ido,J+jdo)
    dG%IareaCv(i,J) = oG%IareaCv(i+ido,J+jdo)
  enddo ; enddo

  do I=IsdB,IedB ; do J=JsdB,JedB
    dG%geoLonBu(I,J) = oG%geoLonBu(I+ido,J+jdo)
    dG%geoLatBu(I,J) = oG%geoLatBu(I+ido,J+jdo)
    dG%dxBu(I,J) = oG%dxBu(I+ido,J+jdo)
    dG%dyBu(I,J) = oG%dyBu(I+ido,J+jdo)
    dG%areaBu(I,J) = oG%areaBu(I+ido,J+jdo)
    dG%CoriolisBu(I,J) = oG%CoriolisBu(I+ido,J+jdo)
    dG%mask2dBu(I,J) = oG%mask2dBu(I+ido,J+jdo)
  enddo ; enddo

  dG%bathymetry_at_vel = oG%bathymetry_at_vel
  if (dG%bathymetry_at_vel) then
    do I=IsdB,IedB ; do j=jsd,jed
      dG%Dblock_u(I,j) = oG%Dblock_u(I+ido,j+jdo)
      dG%Dopen_u(I,j) = oG%Dopen_u(I+ido,j+jdo)
    enddo ; enddo
    do i=isd,ied ; do J=JsdB,JedB
      dG%Dblock_v(i,J) = oG%Dblock_v(i+ido,J+jdo)
      dG%Dopen_v(i,J) = oG%Dopen_v(i+ido,J+jdo)
    enddo ; enddo
  endif

  dG%gridLonT(dG%isg:dG%ieg) = oG%gridLonT(oG%isg:oG%ieg)
  dG%gridLatT(dG%jsg:dG%jeg) = oG%gridLatT(oG%jsg:oG%jeg)

  ! The more complicated logic here avoids segmentation faults if one grid uses
  ! global symmetric memory while the other does not.  Because a northeast grid
  ! convention is being used, the upper bounds for each array correspond.
  !   Note that the dynamic grid always uses symmetric memory.
  Ido2 = oG%IegB-dG%IegB ; Igst = max(dG%isg-1, oG%IsgB-Ido2)
  Jdo2 = oG%JegB-dG%JegB ; Jgst = max(dG%jsg-1, oG%JsgB-Jdo2)
  do I=Igst,dG%IegB ; dG%gridLonB(I) = oG%gridLonB(I+Ido2) ; enddo
  do J=Jgst,dG%JegB ; dG%gridLatB(J) = oG%gridLatB(J+Jdo2) ; enddo

  ! Copy various scalar variables and strings.
  dG%x_axis_units = oG%x_axis_units ; dG%y_axis_units = oG%y_axis_units
  dG%areaT_global = oG%areaT_global ; dG%IareaT_global = oG%IareaT_global
  dG%south_lat = oG%south_lat ; dG%west_lon  = oG%west_lon
  dG%len_lat = oG%len_lat ; dG%len_lon = oG%len_lon
  dG%Rad_Earth = oG%Rad_Earth ; dG%max_depth = oG%max_depth

! Update the halos in case the dynamic grid has smaller halos than the ocean grid.
  call pass_var(dG%areaT, dG%Domain)
  call pass_var(dG%bathyT, dG%Domain)
  call pass_var(dG%geoLonT, dG%Domain)
  call pass_var(dG%geoLatT, dG%Domain)
  call pass_vector(dG%dxT, dG%dyT, dG%Domain, To_All+Scalar_Pair, AGRID)
  call pass_vector(dG%dF_dx, dG%dF_dy, dG%Domain, To_All, AGRID)
  call pass_vector(dG%cos_rot, dG%sin_rot, dG%Domain, To_All, AGRID)
  call pass_var(dG%mask2dT, dG%Domain)

  call pass_vector(dG%areaCu, dG%areaCv, dG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(dG%dyCu, dG%dxCv, dG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(dG%dxCu, dG%dyCv, dG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(dG%dy_Cu, dG%dx_Cv, dG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(dG%mask2dCu, dG%mask2dCv, dG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(dG%IareaCu, dG%IareaCv, dG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(dG%IareaCu, dG%IareaCv, dG%Domain, To_All+Scalar_Pair, CGRID_NE)
  call pass_vector(dG%geoLatCu, dG%geoLatCv, dG%Domain, To_All+Scalar_Pair, CGRID_NE)

  call pass_var(dG%areaBu, dG%Domain, position=CORNER)
  call pass_var(dG%geoLonBu, dG%Domain, position=CORNER, inner_halo=dG%isc-isd)
  call pass_var(dG%geoLatBu, dG%Domain, position=CORNER)
  call pass_vector(dG%dxBu, dG%dyBu, dG%Domain, To_All+Scalar_Pair, BGRID_NE)
  call pass_var(dG%CoriolisBu, dG%Domain, position=CORNER)
  call pass_var(dG%mask2dBu, dG%Domain, position=CORNER)

  if (dG%bathymetry_at_vel) then
    call pass_vector(dG%Dblock_u, dG%Dblock_v, dG%Domain, To_All+Scalar_Pair, CGRID_NE)
    call pass_vector(dG%Dopen_u, dG%Dopen_v, dG%Domain, To_All+Scalar_Pair, CGRID_NE)
  endif

  call  set_derived_dyn_horgrid(dG, US)

end subroutine copy_LND_grid_to_dyngrid

subroutine rotate_dyngrid(G_in, G, US, turns)
  type(dyn_horgrid_type), intent(in)    :: G_in   !< Common horizontal grid type
  type(dyn_horgrid_type), intent(inout) :: G      !< Ocean grid type
  type(unit_scale_type),  intent(in)    :: US     !< A dimensional unit scaling type
  integer, intent(in) :: turns                    !< Number of quarter turns

  integer :: jsc, jec, jscB, jecB
  integer :: qturn

  ! Center point
  call rotate_array(G_in%geoLonT, turns, G%geoLonT)
  call rotate_array(G_in%geoLatT, turns, G%geoLatT)
  call rotate_array_pair(G_in%dxT, G_in%dyT, turns, G%dxT, G%dyT)
  call rotate_array(G_in%areaT, turns, G%areaT)
  call rotate_array(G_in%bathyT, turns, G%bathyT)

  call rotate_array_pair(G_in%df_dx, G_in%df_dy, turns, G%df_dx, G%df_dy)
  call rotate_array(G_in%sin_rot, turns, G%sin_rot)
  call rotate_array(G_in%cos_rot, turns, G%cos_rot)
  call rotate_array(G_in%mask2dT, turns, G%mask2dT)

  ! Face point
  call rotate_array_pair(G_in%geoLonCu, G_in%geoLonCv, turns, &
      G%geoLonCu, G%geoLonCv)
  call rotate_array_pair(G_in%geoLatCu, G_in%geoLatCv, turns, &
      G%geoLatCu, G%geoLatCv)
  call rotate_array_pair(G_in%dxCu, G_in%dyCv, turns, G%dxCu, G%dyCv)
  call rotate_array_pair(G_in%dxCv, G_in%dyCu, turns, G%dxCv, G%dyCu)
  call rotate_array_pair(G_in%dx_Cv, G_in%dy_Cu, turns, G%dx_Cv, G%dy_Cu)

  call rotate_array_pair(G_in%mask2dCu, G_in%mask2dCv, turns, &
      G%mask2dCu, G%mask2dCv)
  call rotate_array_pair(G_in%areaCu, G_in%areaCv, turns, &
      G%areaCu, G%areaCv)
  call rotate_array_pair(G_in%IareaCu, G_in%IareaCv, turns, &
      G%IareaCu, G%IareaCv)

  ! Vertex point
  call rotate_array(G_in%geoLonBu, turns, G%geoLonBu)
  call rotate_array(G_in%geoLatBu, turns, G%geoLatBu)
  call rotate_array_pair(G_in%dxBu, G_in%dyBu, turns, G%dxBu, G%dyBu)
  call rotate_array(G_in%areaBu, turns, G%areaBu)
  call rotate_array(G_in%CoriolisBu, turns, G%CoriolisBu)
  call rotate_array(G_in%mask2dBu, turns, G%mask2dBu)

  ! Topographic
  G%bathymetry_at_vel = G_in%bathymetry_at_vel
  if (G%bathymetry_at_vel) then
    call rotate_array_pair(G_in%Dblock_u, G_in%Dblock_v, turns, &
        G%Dblock_u, G%Dblock_v)
    call rotate_array_pair(G_in%Dopen_u, G_in%Dopen_v, turns, &
        G%Dopen_u, G%Dopen_v)
  endif

  ! Nominal grid axes
  ! TODO: We should not assign lat values to the lon axis, and vice versa.
  !   We temporarily copy lat <-> lon since several components still expect
  !   lat and lon sizes to match the first and second dimension sizes.
  !   But we ought to instead leave them unchanged and adjust the references to
  !   these axes.
  if (modulo(turns, 2) /= 0) then
    G%gridLonT(:) = G_in%gridLatT(G_in%jeg:G_in%jsg:-1)
    G%gridLatT(:) = G_in%gridLonT(:)
    G%gridLonB(:) = G_in%gridLatB(G_in%jeg:(G_in%jsg-1):-1)
    G%gridLatB(:) = G_in%gridLonB(:)
  else
    G%gridLonT(:) = G_in%gridLonT(:)
    G%gridLatT(:) = G_in%gridLatT(:)
    G%gridLonB(:) = G_in%gridLonB(:)
    G%gridLatB(:) = G_in%gridLatB(:)
  endif

  G%x_axis_units = G_in%y_axis_units
  G%y_axis_units = G_in%x_axis_units
  G%south_lat = G_in%south_lat
  G%west_lon = G_in%west_lon
  G%len_lat = G_in%len_lat
  G%len_lon = G_in%len_lon

  ! Rotation-invariant fields
  G%areaT_global = G_in%areaT_global
  G%IareaT_global = G_in%IareaT_global
  G%Rad_Earth = G_in%Rad_Earth
  G%max_depth = G_in%max_depth

  call set_derived_dyn_horgrid(G, US)
end subroutine rotate_dyngrid

end module LND_transcribe_grid
