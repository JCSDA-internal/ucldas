/*
 * (C) Copyright 2017-2020  UCAR.
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
#include "ucldas/Transforms/Balance/Balance.h"
#include "ucldas/Transforms/Balance/BalanceFortran.h"

using oops::Log;

namespace ucldas {

  // -----------------------------------------------------------------------------

  static oops::LinearVariableChangeMaker<Traits,
            oops::LinearVariableChange<Traits, Balance> >
            makerLinearVariableChangeBalance_("BalanceUCLDAS");

  // -----------------------------------------------------------------------------
  Balance::Balance(const State & bkg,
                   const State & traj,
                   const Geometry & geom,
                   const eckit::Configuration & conf) {
    oops::Log::trace() << "ucldas::Balance::setup " << std::endl;
    const eckit::Configuration * configc = &conf;

    // Interpolate trajectory to the geom resolution
    State traj_at_geomres(geom, traj);

    // Compute Jacobians of the balance wrt traj
    ucldas_balance_setup_f90(keyFtnConfig_,
                           &configc,
                           traj_at_geomres.toFortran(),
                           geom.toFortran());
  }
  // -----------------------------------------------------------------------------
  Balance::~Balance() {
    oops::Log::trace() << "ucldas::Balance::delete " << std::endl;
    ucldas_balance_delete_f90(keyFtnConfig_);
  }
  // -----------------------------------------------------------------------------
  void Balance::multiply(const Increment & dxa, Increment & dxm) const {
    // dxm = K dxa
    oops::Log::trace() << "ucldas::Balance::multiply " << std::endl;
    ucldas_balance_mult_f90(keyFtnConfig_, dxa.toFortran(), dxm.toFortran());
  }
  // -----------------------------------------------------------------------------
  void Balance::multiplyInverse(const Increment & dxm, Increment & dxa) const {
    // dxa = K^-1 dxm
    oops::Log::trace() << "ucldas::Balance::multiplyInverse " << std::endl;
    ucldas_balance_multinv_f90(keyFtnConfig_, dxm.toFortran(), dxa.toFortran());
  }
  // -----------------------------------------------------------------------------
  void Balance::multiplyAD(const Increment & dxm, Increment & dxa) const {
    // dxa = K^T dxm
    oops::Log::trace() << "ucldas::Balance::multiplyAD " << std::endl;
    ucldas_balance_multad_f90(keyFtnConfig_, dxm.toFortran(), dxa.toFortran());
  }
  // -----------------------------------------------------------------------------
  void Balance::multiplyInverseAD(const Increment & dxa,
                                  Increment & dxm) const {
    // dxm = (K^-1)^T dxa
    oops::Log::trace() << "ucldas::Balance::multiplyInverseAD " << std::endl;
    ucldas_balance_multinvad_f90(keyFtnConfig_, dxa.toFortran(), dxm.toFortran());
  }
  // -----------------------------------------------------------------------------
  void Balance::print(std::ostream & os) const {
    os << "UCLDAS change variable: Balance";
  }
  // -----------------------------------------------------------------------------
}  // namespace ucldas
