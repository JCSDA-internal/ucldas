! (C) Copyright 2020-2020 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Interfaces to be called from C++ for Fortran handling of model fields

! ------------------------------------------------------------------------------
module ucldas_getvalue_mod_c

use iso_c_binding

use duration_mod
use oops_variables_mod

use datetime_mod, only: datetime, c_f_datetime
use fckit_configuration_module, only: fckit_configuration

use ucldas_geom_mod, only: ucldas_geom
use ucldas_geom_mod_c, only: ucldas_geom_registry
use ucldas_getvalues_mod
use ucldas_getvalues_reg
use ucldas_state_mod
use ucldas_state_reg
use ucldas_increment_mod
use ucldas_increment_reg

use ufo_geovals_mod_c, only: ufo_geovals_registry
use ufo_geovals_mod, only: ufo_geovals
use ufo_locations_mod


implicit none
private

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

subroutine ucldas_getvalues_create_c(c_key_self, c_key_geom, c_locs) &
           bind (c, name='ucldas_getvalues_create_f90')
integer(c_int),     intent(inout) :: c_key_self      !< Key to self
integer(c_int),     intent(in)    :: c_key_geom      !< Key to geometry
type(c_ptr), value, intent(in)    :: c_locs

type(ucldas_getvalues), pointer :: self
type(ucldas_geom),      pointer :: geom
type(ufo_locations)           :: locs

! Create object
call ucldas_getvalues_registry%init()
call ucldas_getvalues_registry%add(c_key_self)
call ucldas_getvalues_registry%get(c_key_self, self)

! Others
call ucldas_geom_registry%get(c_key_geom, geom)
locs = ufo_locations(c_locs)

! Call method
call self%create(geom, locs)

end subroutine ucldas_getvalues_create_c

! --------------------------------------------------------------------------------------------------

subroutine ucldas_getvalues_delete_c(c_key_self) bind (c, name='ucldas_getvalues_delete_f90')
integer(c_int), intent(inout) :: c_key_self !< Key to self

type(ucldas_getvalues), pointer :: self

! Get object
call ucldas_getvalues_registry%get(c_key_self, self)

! Call method
call self%delete()

! Remove object
call ucldas_getvalues_registry%remove(c_key_self)

end subroutine ucldas_getvalues_delete_c

! --------------------------------------------------------------------------------------------------

subroutine ucldas_getvalues_fill_geovals_c(c_key_self, c_key_geom, c_key_state, c_t1, c_t2, &
                                         c_locs, c_key_geovals) &
           bind (c, name='ucldas_getvalues_fill_geovals_f90')

integer(c_int),     intent(in) :: c_key_self
integer(c_int),     intent(in) :: c_key_geom
integer(c_int),     intent(in) :: c_key_state
type(c_ptr), value, intent(in) :: c_t1
type(c_ptr), value, intent(in) :: c_t2
type(c_ptr), value, intent(in) :: c_locs
integer(c_int),     intent(in) :: c_key_geovals

type(ucldas_getvalues), pointer :: self
type(ucldas_geom),      pointer :: geom
type(ucldas_state),     pointer :: state
type(datetime)                :: t1
type(datetime)                :: t2
type(ufo_locations)           :: locs
type(ufo_geovals),    pointer :: geovals

! Get objects
call ucldas_getvalues_registry%get(c_key_self, self)
call ucldas_geom_registry%get(c_key_geom, geom)
call ucldas_state_registry%get(c_key_state, state)
call c_f_datetime(c_t1, t1)
call c_f_datetime(c_t2, t2)
locs = ufo_locations(c_locs)
call ufo_geovals_registry%get(c_key_geovals, geovals)

! Call method
call self%fill_geovals(geom, state, t1, t2, locs, geovals)

end subroutine ucldas_getvalues_fill_geovals_c

! --------------------------------------------------------------------------------------------------

subroutine ucldas_getvalues_fill_geovals_tl_c(c_key_self, c_key_geom, c_key_incr, c_t1, c_t2, &
                                         c_locs, c_key_geovals) &
           bind (c, name='ucldas_getvalues_fill_geovals_tl_f90')

integer(c_int),     intent(in) :: c_key_self
integer(c_int),     intent(in) :: c_key_geom
integer(c_int),     intent(in) :: c_key_incr
type(c_ptr), value, intent(in) :: c_t1
type(c_ptr), value, intent(in) :: c_t2
type(c_ptr), value, intent(in) :: c_locs
integer(c_int),     intent(in) :: c_key_geovals

type(ucldas_getvalues), pointer :: self
type(ucldas_geom),      pointer :: geom
type(ucldas_increment), pointer :: incr
type(datetime)                :: t1
type(datetime)                :: t2
type(ufo_locations)           :: locs
type(ufo_geovals),    pointer :: geovals

! Get objects
call ucldas_getvalues_registry%get(c_key_self, self)
call ucldas_geom_registry%get(c_key_geom, geom)
call ucldas_increment_registry%get(c_key_incr, incr)
call c_f_datetime(c_t1, t1)
call c_f_datetime(c_t2, t2)
locs = ufo_locations(c_locs)
call ufo_geovals_registry%get(c_key_geovals, geovals)

! Call method
call self%fill_geovals(geom, incr, t1, t2, locs, geovals)

end subroutine ucldas_getvalues_fill_geovals_tl_c

! --------------------------------------------------------------------------------------------------

subroutine ucldas_getvalues_fill_geovals_ad_c(c_key_self, c_key_geom, c_key_incr, c_t1, c_t2, &
                                            c_locs, c_key_geovals) &
           bind (c, name='ucldas_getvalues_fill_geovals_ad_f90')

integer(c_int),     intent(in) :: c_key_self
integer(c_int),     intent(in) :: c_key_geom
integer(c_int),     intent(in) :: c_key_incr
type(c_ptr), value, intent(in) :: c_t1
type(c_ptr), value, intent(in) :: c_t2
type(c_ptr), value, intent(in) :: c_locs
integer(c_int),     intent(in) :: c_key_geovals

type(ucldas_getvalues), pointer :: self
type(ucldas_geom),      pointer :: geom
type(ucldas_increment), pointer :: incr
type(datetime)                :: t1
type(datetime)                :: t2
type(ufo_locations)           :: locs
type(ufo_geovals),    pointer :: geovals

! Get objects
call ucldas_getvalues_registry%get(c_key_self, self)
call ucldas_geom_registry%get(c_key_geom, geom)
call ucldas_increment_registry%get(c_key_incr, incr)
call c_f_datetime(c_t1, t1)
call c_f_datetime(c_t2, t2)
locs = ufo_locations(c_locs)
call ufo_geovals_registry%get(c_key_geovals, geovals)

! Call method
call self%fill_geovals_ad(geom, incr, t1, t2, locs, geovals)

end subroutine ucldas_getvalues_fill_geovals_ad_c

end module ucldas_getvalue_mod_c
