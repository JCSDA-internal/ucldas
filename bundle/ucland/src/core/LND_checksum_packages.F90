!> Provides routines that do checksums of groups of LND variables
module LND_checksum_packages

! This file is part of UCLAND. See LICENSE.md for the license.

!   This module provides several routines that do check-sums of groups
! of variables in the various dynamic solver routines.

use LND_coms, only : min_across_PEs, max_across_PEs, reproducing_sum
use LND_debugging, only : hchksum, uvchksum
use LND_error_handler, only : LND_mesg, is_root_pe
use LND_grid, only : ocean_grid_type
use LND_unit_scaling, only : unit_scale_type
use LND_variables, only : thermo_var_ptrs, surface
use LND_verticalGrid, only : verticalGrid_type

implicit none ; private

public LND_state_chksum, LND_thermo_chksum, LND_accel_chksum
public LND_state_stats, LND_surface_chksum

!> Write out checksums of the UCLAND state variables
interface LND_state_chksum
  module procedure LND_state_chksum_5arg
  module procedure LND_state_chksum_3arg
end interface

#include <LND_memory.h>

!> A type for storing statistica about a variable
type :: stats ; private
  real :: minimum = 1.E34  !< The minimum value
  real :: maximum = -1.E34 !< The maximum value
  real :: average = 0.     !< The average value
end type stats

contains

! =============================================================================

!> Write out chksums for the model's basic state variables, including transports.
subroutine LND_state_chksum_5arg(mesg, u, v, h, uh, vh, G, GV, US, haloshift, symmetric, vel_scale)
  character(len=*),                          &
                           intent(in) :: mesg !< A message that appears on the chksum lines.
  type(ocean_grid_type),   intent(in) :: G    !< The ocean's grid structure.
  type(verticalGrid_type), intent(in) :: GV   !< The ocean's vertical grid structure.
  real, dimension(SZIB_(G),SZJ_(G),SZK_(G)), &
                           intent(in) :: u    !< The zonal velocity [L T-1 ~> m s-1] or other units.
  real, dimension(SZI_(G),SZJB_(G),SZK_(G)), &
                           intent(in) :: v    !< The meridional velocity [L T-1 ~> m s-1] or other units.
  real, dimension(SZI_(G),SZJ_(G),SZK_(G)),  &
                           intent(in) :: h    !< Layer thicknesses [H ~> m or kg m-2].
  real, dimension(SZIB_(G),SZJ_(G),SZK_(G)), &
                           intent(in) :: uh   !< Volume flux through zonal faces = u*h*dy
                                              !! [H L2 T-1 ~> m3 s-1 or kg s-1].
  real, dimension(SZI_(G),SZJB_(G),SZK_(G)), &
                           intent(in) :: vh   !< Volume flux through meridional faces = v*h*dx
                                              !! [H L2 T-1 ~> m3 s-1 or kg s-1].
  type(unit_scale_type),   intent(in) :: US   !< A dimensional unit scaling type
  integer,       optional, intent(in) :: haloshift !< The width of halos to check (default 0).
  logical,       optional, intent(in) :: symmetric !< If true, do checksums on the fully symmetric
                                                   !! computational domain.
  real,          optional, intent(in) :: vel_scale !< The scaling factor to convert velocities to [m s-1]

  real :: scale_vel ! The scaling factor to convert velocities to [m s-1]
  logical :: sym
  integer :: is, ie, js, je, nz, hs
  is = G%isc ; ie = G%iec ; js = G%jsc ; je = G%jec ; nz = G%ke

  ! Note that for the chksum calls to be useful for reproducing across PE
  ! counts, there must be no redundant points, so all variables use is..ie
  ! and js...je as their extent.
  hs = 1 ; if (present(haloshift)) hs=haloshift
  sym = .false. ; if (present(symmetric)) sym=symmetric
  scale_vel = US%L_T_to_m_s ; if (present(vel_scale)) scale_vel = vel_scale

  call uvchksum(mesg//" [uv]", u, v, G%HI, haloshift=hs, symmetric=sym, scale=scale_vel)
  call hchksum(h, mesg//" h", G%HI, haloshift=hs, scale=GV%H_to_m)
  call uvchksum(mesg//" [uv]h", uh, vh, G%HI, haloshift=hs, &
                symmetric=sym, scale=GV%H_to_m*US%L_to_m**2*US%s_to_T)
end subroutine LND_state_chksum_5arg

! =============================================================================

!> Write out chksums for the model's basic state variables.
subroutine LND_state_chksum_3arg(mesg, u, v, h, G, GV, US, haloshift, symmetric)
  character(len=*),                intent(in) :: mesg !< A message that appears on the chksum lines.
  type(ocean_grid_type),           intent(in) :: G  !< The ocean's grid structure.
  type(verticalGrid_type),         intent(in) :: GV !< The ocean's vertical grid structure.
  real, dimension(SZIB_(G),SZJ_(G),SZK_(G)), &
                                   intent(in) :: u  !< Zonal velocity [L T-1 ~> m s-1] or [m s-1].
  real, dimension(SZI_(G),SZJB_(G),SZK_(G)), &
                                   intent(in) :: v  !< Meridional velocity [L T-1 ~> m s-1] or [m s-1]..
  real, dimension(SZI_(G),SZJ_(G),SZK_(G)),  &
                                   intent(in) :: h  !< Layer thicknesses [H ~> m or kg m-2].
  type(unit_scale_type), optional, intent(in) :: US !< A dimensional unit scaling type, which is
                                                    !! used to rescale u and v if present.
  integer,               optional, intent(in) :: haloshift !< The width of halos to check (default 0).
  logical,               optional, intent(in) :: symmetric !< If true, do checksums on the fully
                                                    !! symmetric computational domain.
  real :: L_T_to_m_s ! A rescaling factor for velocities [m T s-1 L-1 ~> nondim] or [nondim]
  integer :: is, ie, js, je, nz, hs
  logical :: sym

  is = G%isc ; ie = G%iec ; js = G%jsc ; je = G%jec ; nz = G%ke
  L_T_to_m_s = 1.0 ; if (present(US)) L_T_to_m_s = US%L_T_to_m_s

  ! Note that for the chksum calls to be useful for reproducing across PE
  ! counts, there must be no redundant points, so all variables use is..ie
  ! and js...je as their extent.
  hs=1; if (present(haloshift)) hs=haloshift
  sym=.false.; if (present(symmetric)) sym=symmetric
  call uvchksum(mesg//" u", u, v, G%HI, haloshift=hs, symmetric=sym, scale=L_T_to_m_s)
  call hchksum(h, mesg//" h",G%HI, haloshift=hs, scale=GV%H_to_m)
end subroutine LND_state_chksum_3arg

! =============================================================================

!> Write out chksums for the model's thermodynamic state variables.
subroutine LND_thermo_chksum(mesg, tv, G, US, haloshift)
  character(len=*),         intent(in) :: mesg !< A message that appears on the chksum lines.
  type(thermo_var_ptrs),    intent(in) :: tv   !< A structure pointing to various
                                               !! thermodynamic variables.
  type(ocean_grid_type),    intent(in) :: G    !< The ocean's grid structure.
  type(unit_scale_type),    intent(in) :: US   !< A dimensional unit scaling type
  integer,        optional, intent(in) :: haloshift !< The width of halos to check (default 0).

  integer :: is, ie, js, je, nz, hs
  is = G%isc ; ie = G%iec ; js = G%jsc ; je = G%jec ; nz = G%ke
  hs=1; if (present(haloshift)) hs=haloshift

  if (associated(tv%T)) call hchksum(tv%T, mesg//" T", G%HI, haloshift=hs)
  if (associated(tv%S)) call hchksum(tv%S, mesg//" S", G%HI, haloshift=hs)
  if (associated(tv%frazil)) call hchksum(tv%frazil, mesg//" frazil", G%HI, haloshift=hs, &
                                          scale=US%Q_to_J_kg*US%R_to_kg_m3*US%Z_to_m)
  if (associated(tv%salt_deficit)) &
    call hchksum(tv%salt_deficit, mesg//" salt deficit", G%HI, haloshift=hs, scale=US%RZ_to_kg_m2)

end subroutine LND_thermo_chksum

! =============================================================================

!> Write out chksums for the ocean surface variables.
subroutine LND_surface_chksum(mesg, sfc_state, G, US, haloshift, symmetric)
  character(len=*),      intent(in)    :: mesg !< A message that appears on the chksum lines.
  type(surface),         intent(inout) :: sfc_state !< transparent ocean surface state structure
                                               !! shared with the calling routine data in this
                                               !! structure is intent out.
  type(ocean_grid_type), intent(in)    :: G    !< The ocean's grid structure.
  type(unit_scale_type), intent(in)    :: US    !< A dimensional unit scaling type
  integer,     optional, intent(in)    :: haloshift !< The width of halos to check (default 0).
  logical,     optional, intent(in)    :: symmetric !< If true, do checksums on the fully symmetric
                                               !! computational domain.

  integer :: hs
  logical :: sym

  sym = .false. ; if (present(symmetric)) sym = symmetric
  hs = 1 ; if (present(haloshift)) hs = haloshift

  if (allocated(sfc_state%SST)) call hchksum(sfc_state%SST, mesg//" SST", G%HI, haloshift=hs)
  if (allocated(sfc_state%SSS)) call hchksum(sfc_state%SSS, mesg//" SSS", G%HI, haloshift=hs)
  if (allocated(sfc_state%sea_lev)) call hchksum(sfc_state%sea_lev, mesg//" sea_lev", G%HI, &
                                                 haloshift=hs, scale=US%Z_to_m)
  if (allocated(sfc_state%Hml)) call hchksum(sfc_state%Hml, mesg//" Hml", G%HI, haloshift=hs, &
                                             scale=US%Z_to_m)
  if (allocated(sfc_state%u) .and. allocated(sfc_state%v)) &
    call uvchksum(mesg//" SSU", sfc_state%u, sfc_state%v, G%HI, haloshift=hs, symmetric=sym, &
                  scale=US%L_T_to_m_s)
!  if (allocated(sfc_state%salt_deficit)) &
!    call hchksum(sfc_state%salt_deficit, mesg//" salt deficit", G%HI, haloshift=hs, scale=US%RZ_to_kg_m2)
  if (allocated(sfc_state%frazil)) call hchksum(sfc_state%frazil, mesg//" frazil", G%HI, &
                                                haloshift=hs, scale=US%Q_to_J_kg*US%RZ_to_kg_m2)

end subroutine LND_surface_chksum

! =============================================================================

!> Write out chksums for the model's accelerations
subroutine LND_accel_chksum(mesg, CAu, CAv, PFu, PFv, diffu, diffv, G, GV, US, pbce, &
                            u_accel_bt, v_accel_bt, symmetric)
  character(len=*),         intent(in) :: mesg !< A message that appears on the chksum lines.
  type(ocean_grid_type),    intent(in) :: G    !< The ocean's grid structure.
  type(verticalGrid_type),  intent(in) :: GV   !< The ocean's vertical grid structure.
  real, dimension(SZIB_(G),SZJ_(G),SZK_(G)), &
                            intent(in) :: CAu  !< Zonal acceleration due to Coriolis
                                               !! and lndentum advection terms [L T-2 ~> m s-2].
  real, dimension(SZI_(G),SZJB_(G),SZK_(G)), &
                            intent(in) :: CAv  !< Meridional acceleration due to Coriolis
                                               !! and lndentum advection terms [L T-2 ~> m s-2].
  real, dimension(SZIB_(G),SZJ_(G),SZK_(G)), &
                            intent(in) :: PFu  !< Zonal acceleration due to pressure gradients
                                               !! (equal to -dM/dx) [L T-2 ~> m s-2].
  real, dimension(SZI_(G),SZJB_(G),SZK_(G)), &
                            intent(in) :: PFv  !< Meridional acceleration due to pressure gradients
                                               !! (equal to -dM/dy) [L T-2 ~> m s-2].
  real, dimension(SZIB_(G),SZJ_(G),SZK_(G)), &
                            intent(in) :: diffu !< Zonal acceleration due to convergence of the
                                                !! along-isopycnal stress tensor [L T-2 ~> m s-2].
  real, dimension(SZI_(G),SZJB_(G),SZK_(G)), &
                            intent(in) :: diffv !< Meridional acceleration due to convergence of
                                                !! the along-isopycnal stress tensor [L T-2 ~> m s-2].
  type(unit_scale_type),    intent(in) :: US    !< A dimensional unit scaling type
  real, dimension(SZI_(G),SZJ_(G),SZK_(G)),  &
                  optional, intent(in) :: pbce !< The baroclinic pressure anomaly in each layer
                                               !! due to free surface height anomalies
                                               !! [L2 T-2 H-1 ~> m s-2 or m4 s-2 kg-1].
  real, dimension(SZIB_(G),SZJ_(G),SZK_(G)), &
                  optional, intent(in) :: u_accel_bt !< The zonal acceleration from terms in the
                                                     !! barotropic solver [L T-2 ~> m s-2].
  real, dimension(SZI_(G),SZJB_(G),SZK_(G)), &
                  optional, intent(in) :: v_accel_bt !< The meridional acceleration from terms in
                                                     !! the barotropic solver [L T-2 ~> m s-2].
  logical,        optional, intent(in) :: symmetric !< If true, do checksums on the fully symmetric
                                                    !! computational domain.

  integer :: is, ie, js, je, nz
  logical :: sym

  is = G%isc ; ie = G%iec ; js = G%jsc ; je = G%jec ; nz = G%ke
  sym=.false.; if (present(symmetric)) sym=symmetric

  ! Note that for the chksum calls to be useful for reproducing across PE
  ! counts, there must be no redundant points, so all variables use is..ie
  ! and js...je as their extent.
  call uvchksum(mesg//" CA[uv]", CAu, CAv, G%HI, haloshift=0, symmetric=sym, scale=US%L_T2_to_m_s2)
  call uvchksum(mesg//" PF[uv]", PFu, PFv, G%HI, haloshift=0, symmetric=sym, scale=US%L_T2_to_m_s2)
  call uvchksum(mesg//" diffu", diffu, diffv, G%HI,haloshift=0, symmetric=sym, scale=US%L_T2_to_m_s2)
  if (present(pbce)) &
    call hchksum(pbce, mesg//" pbce",G%HI,haloshift=0, scale=GV%m_to_H*US%L_T_to_m_s**2)
  if (present(u_accel_bt) .and. present(v_accel_bt)) &
    call uvchksum(mesg//" [uv]_accel_bt", u_accel_bt, v_accel_bt, G%HI,haloshift=0, symmetric=sym, &
                  scale=US%L_T2_to_m_s2)
end subroutine LND_accel_chksum

! =============================================================================

!> Monitor and write out statistics for the model's state variables.
subroutine LND_state_stats(mesg, u, v, h, Temp, Salt, G, GV, US, allowChange, permitDiminishing)
  type(ocean_grid_type),   intent(in) :: G    !< The ocean's grid structure.
  type(verticalGrid_type), intent(in) :: GV   !< The ocean's vertical grid structure.
  character(len=*),        intent(in) :: mesg !< A message that appears on the chksum lines.
  real, dimension(SZIB_(G),SZJ_(G),SZK_(G)), &
                           intent(in) :: u    !< The zonal velocity [L T-1 ~> m s-1].
  real, dimension(SZI_(G),SZJB_(G),SZK_(G)), &
                           intent(in) :: v    !< The meridional velocity [L T-1 ~> m s-1].
  real, dimension(SZI_(G),SZJ_(G),SZK_(G)),  &
                           intent(in) :: h    !< Layer thicknesses [H ~> m or kg m-2].
  real, pointer, dimension(:,:,:),           &
                           intent(in) :: Temp !< Temperature [degC].
  real, pointer, dimension(:,:,:),           &
                           intent(in) :: Salt !< Salinity [ppt].
  type(unit_scale_type),   intent(in) :: US    !< A dimensional unit scaling type
  logical,       optional, intent(in) :: allowChange !< do not flag an error
                                                     !! if the statistics change.
  logical,       optional, intent(in) :: permitDiminishing !< do not flag error if the
                                                           !! extrema are diminishing.

  ! Local variables
  real, dimension(G%isc:G%iec, G%jsc:G%jec) :: &
    tmp_A, &  ! The area per cell [m2] (unscaled to permit reproducing sum).
    tmp_V, &  ! The column-integrated volume [m3] (unscaled to permit reproducing sum)
    tmp_T, &  ! The column-integrated temperature [degC m3]
    tmp_S     ! The column-integrated salinity [ppt m3]
  real :: Vol, dV    ! The total ocean volume and its change [m3] (unscaled to permit reproducing sum).
  real :: Area       ! The total ocean surface area [m2] (unscaled to permit reproducing sum).
  real :: h_minimum  ! The minimum layer thicknesses [H ~> m or kg m-2]
  logical :: do_TS   ! If true, evaluate statistics for temperature and salinity
  type(stats) :: T, S, delT, delS

  ! NOTE: save data is not normally allowed but we use it for debugging purposes here on the
  !       assumption we will not turn this on with threads
  type(stats), save :: oldT, oldS
  logical, save :: firstCall = .true.
  real, save :: oldVol ! The previous total ocean volume [m3]

  character(len=80) :: lMsg
  integer :: is, ie, js, je, nz, i, j, k

  is = G%isc ; ie = G%iec ; js = G%jsc ; je = G%jec ; nz = G%ke
  do_TS = associated(Temp) .and. associated(Salt)

  tmp_A(:,:) = 0.0
  tmp_V(:,:) = 0.0
  tmp_T(:,:) = 0.0
  tmp_S(:,:) = 0.0

  ! First collect local stats
  do j=js,je ; do i=is,ie
    tmp_A(i,j) = tmp_A(i,j) + US%L_to_m**2*G%areaT(i,j)
  enddo ; enddo
  T%minimum = 1.E34 ; T%maximum = -1.E34 ; T%average = 0.
  S%minimum = 1.E34 ; S%maximum = -1.E34 ; S%average = 0.
  h_minimum = 1.E34*GV%m_to_H
  do k=1,nz ; do j=js,je ; do i=is,ie
    if (G%mask2dT(i,j)>0.) then
      dV = US%L_to_m**2*G%areaT(i,j)*GV%H_to_m*h(i,j,k)
      tmp_V(i,j) = tmp_V(i,j) + dV
      if (do_TS .and. h(i,j,k)>0.) then
        T%minimum = min( T%minimum, Temp(i,j,k) ) ; T%maximum = max( T%maximum, Temp(i,j,k) )
        T%average = T%average + dV*Temp(i,j,k)
        S%minimum = min( S%minimum, Salt(i,j,k) ) ; S%maximum = max( S%maximum, Salt(i,j,k) )
        S%average = S%average + dV*Salt(i,j,k)
      endif
      if (h_minimum > h(i,j,k)) h_minimum = h(i,j,k)
    endif
  enddo ; enddo ; enddo
  Area = reproducing_sum( tmp_A ) ; Vol = reproducing_sum( tmp_V )
  if (do_TS) then
    call min_across_PEs( T%minimum ) ; call max_across_PEs( T%maximum )
    call min_across_PEs( S%minimum ) ; call max_across_PEs( S%maximum )
    T%average = reproducing_sum( tmp_T ) ; S%average = reproducing_sum( tmp_S )
    T%average = T%average / Vol ; S%average = S%average / Vol
  endif
  if (is_root_pe()) then
    if (.not.firstCall) then
      dV = Vol - oldVol
      delT%minimum = T%minimum - oldT%minimum ; delT%maximum = T%maximum - oldT%maximum
      delT%average = T%average - oldT%average
      delS%minimum = S%minimum - oldS%minimum ; delS%maximum = S%maximum - oldS%maximum
      delS%average = S%average - oldS%average
      write(lMsg(1:80),'(2(a,es12.4))') 'Mean thickness =', Vol/Area,' frac. delta=',dV/Vol
      call LND_mesg(lMsg//trim(mesg))
      if (do_TS) then
        write(lMsg(1:80),'(a,3es12.4)') 'Temp min/mean/max =',T%minimum,T%average,T%maximum
        call LND_mesg(lMsg//trim(mesg))
        write(lMsg(1:80),'(a,3es12.4)') 'delT min/mean/max =',delT%minimum,delT%average,delT%maximum
        call LND_mesg(lMsg//trim(mesg))
        write(lMsg(1:80),'(a,3es12.4)') 'Salt min/mean/max =',S%minimum,S%average,S%maximum
        call LND_mesg(lMsg//trim(mesg))
        write(lMsg(1:80),'(a,3es12.4)') 'delS min/mean/max =',delS%minimum,delS%average,delS%maximum
        call LND_mesg(lMsg//trim(mesg))
      endif
    else
      write(lMsg(1:80),'(a,es12.4)') 'Mean thickness =', Vol/Area
      call LND_mesg(lMsg//trim(mesg))
      if (do_TS) then
        write(lMsg(1:80),'(a,3es12.4)') 'Temp min/mean/max =', T%minimum, T%average, T%maximum
        call LND_mesg(lMsg//trim(mesg))
        write(lMsg(1:80),'(a,3es12.4)') 'Salt min/mean/max =', S%minimum, S%average, S%maximum
        call LND_mesg(lMsg//trim(mesg))
      endif
    endif
  endif
  firstCall = .false. ; oldVol = Vol
  oldT%minimum = T%minimum ; oldT%maximum = T%maximum ; oldT%average = T%average
  oldS%minimum = S%minimum ; oldS%maximum = S%maximum ; oldS%average = S%average

  if (do_TS .and. T%minimum<-5.0) then
    do j=js,je ; do i=is,ie
      if (minval(Temp(i,j,:)) == T%minimum) then
        write(0,'(a,2f12.5)') 'x,y=', G%geoLonT(i,j), G%geoLatT(i,j)
        write(0,'(a3,3a12)') 'k','h','Temp','Salt'
        do k = 1, nz
          write(0,'(i3,3es12.4)') k, h(i,j,k), Temp(i,j,k), Salt(i,j,k)
        enddo
        stop 'Extremum detected'
      endif
    enddo ; enddo
  endif

  if (h_minimum<0.0) then
    do j=js,je ; do i=is,ie
      if (minval(h(i,j,:)) == h_minimum) then
        write(0,'(a,2f12.5)') 'x,y=',G%geoLonT(i,j),G%geoLatT(i,j)
        write(0,'(a3,3a12)') 'k','h','Temp','Salt'
        do k = 1, nz
          write(0,'(i3,3es12.4)') k, h(i,j,k), Temp(i,j,k), Salt(i,j,k)
        enddo
        stop 'Negative thickness detected'
      endif
    enddo ; enddo
  endif

end subroutine LND_state_stats

end module LND_checksum_packages
