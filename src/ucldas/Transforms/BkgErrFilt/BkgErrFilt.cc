/*
 * (C) Copyright 2017-2021  UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <ostream>
#include <string>

#include "eckit/config/Configuration.h"

#include "oops/interface/LinearVariableChange.h"
#include "oops/util/Logger.h"

#include "ucldas/Geometry/Geometry.h"
#include "ucldas/Increment/Increment.h"
#include "ucldas/State/State.h"
#include "ucldas/Traits.h"
#include "ucldas/Transforms/BkgErrFilt/BkgErrFilt.h"
#include "ucldas/Transforms/BkgErrFilt/BkgErrFiltFortran.h"

using oops::Log;

namespace ucldas {

  // -----------------------------------------------------------------------------
  static oops::LinearVariableChangeMaker<Traits,
            oops::LinearVariableChange<Traits, BkgErrFilt> >
            makerLinearVariableChangeBkgErrFilt_("BkgErrFILT");

  // -----------------------------------------------------------------------------
  BkgErrFilt::BkgErrFilt(const State & bkg,
                 const State & traj,
                 const Geometry & geom,
                 const eckit::Configuration & conf) {
    const eckit::Configuration * configc = &conf;

    // Interpolate trajectory to the geom resolution
    State traj_at_geomres(geom, traj);

    // Setup background error filter
    ucldas_bkgerrfilt_setup_f90(keyFtnConfig_,
                              &configc,
                              traj_at_geomres.toFortran(),
                              geom.toFortran());
  }
  // -----------------------------------------------------------------------------
  BkgErrFilt::~BkgErrFilt() {
    ucldas_bkgerrfilt_delete_f90(keyFtnConfig_);
  }
  // -----------------------------------------------------------------------------
  void BkgErrFilt::multiply(const Increment & dxa, Increment & dxm) const {
    // dxm = K dxa
    ucldas_bkgerrfilt_mult_f90(keyFtnConfig_, dxa.toFortran(), dxm.toFortran());
  }
  // -----------------------------------------------------------------------------
  void BkgErrFilt::multiplyInverse(const Increment & dxm,
                                   Increment & dxa) const {
    dxa = dxm;
  }
  // -----------------------------------------------------------------------------
  void BkgErrFilt::multiplyAD(const Increment & dxm, Increment & dxa) const {
    // dxa = K^T dxm
    ucldas_bkgerrfilt_mult_f90(keyFtnConfig_, dxm.toFortran(), dxa.toFortran());
  }
  // -----------------------------------------------------------------------------
  void BkgErrFilt::multiplyInverseAD(const Increment & dxa,
                                     Increment & dxm) const {
    dxm = dxa;
  }
  // -----------------------------------------------------------------------------
  void BkgErrFilt::print(std::ostream & os) const {
    os << "UCLDAS change variable";
  }
  // -----------------------------------------------------------------------------
}  // namespace ucldas
