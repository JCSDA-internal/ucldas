/*
 * (C) Copyright 2021-2021  UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */


#include "eckit/config/Configuration.h"

#include "oops/interface/LinearVariableChange.h"
#include "oops/util/abor1_cpp.h"

#include "ucldas/Geometry/Geometry.h"
#include "ucldas/Increment/Increment.h"
#include "ucldas/State/State.h"
#include "ucldas/Traits.h"
#include "ucldas/Transforms/Model2GeoVaLs/LinearModel2GeoVaLs.h"
#include "ucldas/Transforms/Model2GeoVaLs/Model2GeoVaLsFortran.h"

namespace ucldas {

static oops::LinearVariableChangeMaker<Traits,
       oops::LinearVariableChange<Traits, LinearModel2GeoVaLs> >
       makerLinearVariableChangeModel2GeoVaLs_("Model2GeoVaLs");

// -----------------------------------------------------------------------------

LinearModel2GeoVaLs::LinearModel2GeoVaLs(const State & bg, const State &fg,
                                        const Geometry &geom,
                                        const eckit::Configuration &conf)
  : geom_(new Geometry(geom)) {
}

// -----------------------------------------------------------------------------

LinearModel2GeoVaLs::~LinearModel2GeoVaLs() {
}

// -----------------------------------------------------------------------------

void LinearModel2GeoVaLs::multiply(const Increment &dxin,
                                         Increment &dxout) const {
  ucldas_model2geovals_linear_changevar_f90(geom_->toFortran(),
                                          dxin.toFortran(), dxout.toFortran());
}

// -----------------------------------------------------------------------------

void LinearModel2GeoVaLs::multiplyInverse(const Increment &,
                                                Increment &) const {
  util::abor1_cpp("LinearModel2GeoVaLs::multiplyInverse not implemented");
}

// -----------------------------------------------------------------------------

void LinearModel2GeoVaLs::multiplyAD(const Increment &dxin,
                                           Increment &dxout) const {
  ucldas_model2geovals_linear_changevarAD_f90(geom_->toFortran(),
                                            dxin.toFortran(),
                                            dxout.toFortran());
}

// -----------------------------------------------------------------------------

void LinearModel2GeoVaLs::multiplyInverseAD(const Increment &,
                                                  Increment &) const {
  util::abor1_cpp("LinearModel2GeoVaLs::multiplyInverseAD not implemented");
}

// -----------------------------------------------------------------------------

}  // namespace ucldas
