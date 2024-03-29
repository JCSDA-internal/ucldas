/*
 * (C) Copyright 2019-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "eckit/config/LocalConfiguration.h"

#include "ioda/ObsSpace.h"

#include "oops/mpi/mpi.h"
#include "oops/util/DateTime.h"

#include "ucldas/Geometry/Geometry.h"
#include "ucldas/GetValues/GetValues.h"
#include "ucldas/GetValues/GetValuesFortran.h"
#include "ucldas/State/State.h"
#include "ucldas/Transforms/Model2GeoVaLs/Model2GeoVaLs.h"

#include "ufo/GeoVaLs.h"
#include "ufo/Locations.h"

namespace ucldas {

// -----------------------------------------------------------------------------
/// Constructor, destructor
// -----------------------------------------------------------------------------
GetValues::GetValues(const Geometry & geom,
                     const ufo::Locations & locs,
                     const eckit::Configuration & config)
  : locs_(locs), geom_(new Geometry(geom)),
    model2geovals_(new Model2GeoVaLs(geom, config)) {
  ucldas_getvalues_create_f90(keyGetValues_, geom.toFortran(), locs);
}
// -----------------------------------------------------------------------------
GetValues::~GetValues()
{
  ucldas_getvalues_delete_f90(keyGetValues_);
}
// -----------------------------------------------------------------------------
/// Get state values at observation locations
// -----------------------------------------------------------------------------
void GetValues::fillGeoVaLs(const State & state,
                            const util::DateTime & t1,
                            const util::DateTime & t2,
                            ufo::GeoVaLs & geovals) const {
  // overwrite with atm geovals
  // NOTE this is a horrible hack. Remove soon?
  if (geom_->getAtmInit())
  {
    // Get atm geovals
    // The variables in vars that are also defined in ucldas will be
    // over-written in the interpolation call bellow
    getValuesFromFile(locs_, geovals.getVars(), geovals);
  }

  // Do variable change if it has not already been done.
  // TODO(travis): remove this once Yannick is done rearranging things in oops.
  std::unique_ptr<State> varChangeState;
  const State * state_ptr;
  if (geovals.getVars() <= state.variables()) {
    state_ptr = &state;
  } else {
    varChangeState.reset(new State(*geom_, geovals.getVars(),
                                   state.validTime()));
    model2geovals_->changeVar(state, *varChangeState);
    state_ptr = varChangeState.get();
  }
  // Get ocean geovals
  ucldas_getvalues_fill_geovals_f90(keyGetValues_,
                                  geom_->toFortran(),
                                  state_ptr->toFortran(),
                                  t1, t2, locs_,
                                  geovals.toFortran());
}
// -----------------------------------------------------------------------------
/// Read Interpolated GeoVaLs from file
// TODO(Guillaume) 3D only, make it 4D
// -----------------------------------------------------------------------------
void GetValues::getValuesFromFile(const ufo::Locations & locs,
                              const oops::Variables & vars,
                              ufo::GeoVaLs & atmgom) const {
    // Get Atm configuration
    eckit::LocalConfiguration conf(geom_->getAtmConf());

    // Get Time Bounds
    util::DateTime bgn = util::DateTime(conf.getString("notocean.date_begin"));
    util::DateTime end = util::DateTime(conf.getString("notocean.date_end"));

    // Create the Atmospheric Geometry in Observation Space
    eckit::LocalConfiguration confatmobs(conf, "notocean.obs space");
    ioda::ObsTopLevelParameters paramsatmobs;
    paramsatmobs.validateAndDeserialize(confatmobs);
    ioda::ObsSpace atmobs(paramsatmobs, geom_->getComm(), bgn, end,
                          oops::mpi::myself());

    // Get GeoVaLs from file
    eckit::LocalConfiguration confatm(conf, "notocean");
    atmgom.read(confatm, atmobs);
  }
// -----------------------------------------------------------------------------
void GetValues::print(std::ostream & os) const {
  os << "GetValues" << std::endl;
}
// -----------------------------------------------------------------------------

}  // namespace ucldas
