/*
 * (C) Copyright 2020-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "oops/util/DateTime.h"

#include "ucldas/Geometry/Geometry.h"
#include "ucldas/GetValues/GetValuesFortran.h"
#include "ucldas/GetValues/LinearGetValues.h"
#include "ucldas/Increment/Increment.h"
#include "ucldas/State/State.h"

#include "ufo/GeoVaLs.h"
#include "ufo/Locations.h"

namespace ucldas {

// -----------------------------------------------------------------------------
/// Constructor, destructor
// -----------------------------------------------------------------------------
LinearGetValues::LinearGetValues(const Geometry & geom,
                                 const ufo::Locations & locs)
  : locs_(locs), geom_(new Geometry(geom))
{
  ucldas_getvalues_create_f90(keyLinearGetValues_,
                            geom.toFortran(),
                            locs);
}

LinearGetValues::~LinearGetValues()
{
  ucldas_getvalues_delete_f90(keyLinearGetValues_);
}
// -----------------------------------------------------------------------------
/// Interpolate to obs locations
// -----------------------------------------------------------------------------
void LinearGetValues::setTrajectory(const State & state,
                                    const util::DateTime & t1,
                                    const util::DateTime & t2,
                                    ufo::GeoVaLs & geovals) {
  ucldas_getvalues_fill_geovals_f90(keyLinearGetValues_,
                                  geom_->toFortran(),
                                  state.toFortran(),
                                  t1, t2, locs_,
                                  geovals.toFortran());
}
// -------------------------------------------------------------------------------------------------
void LinearGetValues::fillGeoVaLsTL(const Increment & incr,
                                    const util::DateTime & t1,
                                    const util::DateTime & t2,
                                    ufo::GeoVaLs & geovals) const {
  ucldas_getvalues_fill_geovals_tl_f90(keyLinearGetValues_,
                                     geom_->toFortran(),
                                     incr.toFortran(),
                                     t1, t2, locs_,
                                     geovals.toFortran());
}
// -------------------------------------------------------------------------------------------------
void LinearGetValues::fillGeoVaLsAD(Increment & incr,
                                    const util::DateTime & t1,
                                    const util::DateTime & t2,
                                    const ufo::GeoVaLs & geovals) const {
  ucldas_getvalues_fill_geovals_ad_f90(keyLinearGetValues_,
                                     geom_->toFortran(),
                                     incr.toFortran(),
                                     t1, t2, locs_,
                                     geovals.toFortran());
}

// -----------------------------------------------------------------------------
void LinearGetValues::print(std::ostream & os) const {
  os << "LinearGetValues" << std::endl;
}
// -----------------------------------------------------------------------------

}  // namespace ucldas
