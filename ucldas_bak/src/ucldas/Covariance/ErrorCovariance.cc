/*
 * (C) Copyright 2017-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <cmath>

#include "ucldas/Covariance/ErrorCovariance.h"
#include "ucldas/Covariance/ErrorCovarianceFortran.h"
#include "ucldas/Geometry/Geometry.h"
#include "ucldas/Increment/Increment.h"
#include "ucldas/State/State.h"

#include "eckit/config/Configuration.h"

#include "oops/assimilation/GMRESR.h"
#include "oops/base/IdentityMatrix.h"
#include "oops/base/Variables.h"
#include "oops/util/Logger.h"

using oops::Log;

// -----------------------------------------------------------------------------
namespace ucldas {
  // -----------------------------------------------------------------------------

  ErrorCovariance::ErrorCovariance(const Geometry & resol,
                                   const oops::Variables &,
                                   const eckit::Configuration & conf,
                                   const State & bkg,
                                   const State & traj) {
    // bkg: Background state, invariant wrt outer-loop.
    const eckit::Configuration * configc = &conf;
    vars_ = oops::Variables(conf, "analysis variables");
    ucldas_b_setup_f90(keyFtnConfig_, &configc, resol.toFortran(),
                     bkg.toFortran(), vars_);
    Log::trace() << "ErrorCovariance created" << std::endl;
  }

  // -----------------------------------------------------------------------------

  ErrorCovariance::~ErrorCovariance() {
    ucldas_b_delete_f90(keyFtnConfig_);
    Log::trace() << "ErrorCovariance destructed" << std::endl;
  }

  // -----------------------------------------------------------------------------

  void ErrorCovariance::linearize(const State & traj, const Geometry & resol) {
    geom_.reset(new Geometry(resol));
    Log::trace() << "Trajectory for ErrorCovariance" << std::endl;
  }

  // -----------------------------------------------------------------------------

  void ErrorCovariance::multiply(const Increment & dxin, Increment & dxout)
    const {
    dxout = dxin;
    ucldas_b_mult_f90(keyFtnConfig_, dxin.toFortran(), dxout.toFortran());
    Log::trace() << "ErrorCovariance multiply" << std::endl;
  }

  // -----------------------------------------------------------------------------

  void ErrorCovariance::inverseMultiply(const Increment & dxin,
                                        Increment & dxout) const {
    // oops::IdentityMatrix<Increment> Id;
    // dxout.zero();
    // GMRESR(dxout, dxin, *this, Id, 10, 1.0e-6);
    dxout = dxin;
    Log::trace() << "ErrorCovariance inversemultiply" << std::endl;
  }

  // -----------------------------------------------------------------------------

  //  void ErrorCovariance::doRandomize(Increment & dx) const {
  void ErrorCovariance::randomize(Increment & dx) const {
    ucldas_b_randomize_f90(keyFtnConfig_, dx.toFortran());
  }

  // -----------------------------------------------------------------------------

  void ErrorCovariance::print(std::ostream & os) const {
    os << "ErrorCovariance::print not implemented";
  }

  // -----------------------------------------------------------------------------

}  // namespace ucldas
