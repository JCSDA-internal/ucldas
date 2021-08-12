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
#include "ucldas/Transforms/VertConv/VertConv.h"
#include "ucldas/Transforms/VertConv/VertConvFortran.h"


using oops::Log;

namespace ucldas {

  // -----------------------------------------------------------------------------
  static oops::LinearVariableChangeMaker<Traits,
            oops::LinearVariableChange<Traits, VertConv> >
            makerLinearVariableCHangeVertConv_("VertConvUCLDAS");

  // -----------------------------------------------------------------------------
  VertConv::VertConv(const State & bkg,
                     const State & traj,
                     const Geometry & geom,
                     const eckit::Configuration & conf) :
          bkg_lr_(geom, bkg), geom_(geom) {
    oops::Log::trace() << "ucldas::VertConv::setup " << std::endl;
    const eckit::Configuration * configc = &conf;

    // Compute convolution weights
    ucldas_vertconv_setup_f90(keyFtnConfig_,
                            &configc,
                            bkg_lr_.toFortran(),
                            geom.toFortran());
  }
  // -----------------------------------------------------------------------------
  VertConv::~VertConv() {
    oops::Log::trace() << "ucldas::VertConv::delete " << std::endl;
    ucldas_vertconv_delete_f90(keyFtnConfig_);
  }
  // -----------------------------------------------------------------------------
  void VertConv::multiply(const Increment & dxa, Increment & dxm) const {
    // dxm = K dxa
    oops::Log::trace() << "ucldas::VertConv::multiply " << std::endl;
    ucldas_vertconv_mult_f90(dxa.toFortran(), dxm.toFortran(), keyFtnConfig_);
  }
  // -----------------------------------------------------------------------------
  void VertConv::multiplyInverse(const Increment & dxm, Increment & dxa) const {
    oops::Log::trace() << "ucldas::VertConv::multiplyInverse " << std::endl;
    dxa = dxm;
  }
  // -----------------------------------------------------------------------------
  void VertConv::multiplyAD(const Increment & dxm, Increment & dxa) const {
    // dxa = K^T dxm
    oops::Log::trace() << "ucldas::VertConv::multiplyAD " << std::endl;
    ucldas_vertconv_multad_f90(dxm.toFortran(), dxa.toFortran(), keyFtnConfig_);
  }
  // -----------------------------------------------------------------------------
  void VertConv::multiplyInverseAD(const Increment & dxa,
                                   Increment & dxm) const {
    oops::Log::trace() << "ucldas::VertConv::multiplyInverseAD " << std::endl;
    dxm = dxa;
  }
  // -----------------------------------------------------------------------------
  void VertConv::print(std::ostream & os) const {
    os << "UCLDAS change variable: VertConv";
  }
  // -----------------------------------------------------------------------------
}  // namespace ucldas
