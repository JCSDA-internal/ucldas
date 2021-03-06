/*
 * (C) Copyright 2009-2016 ECMWF.
 * (C) Copyright 2017-2020 UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef UCLDAS_LINEARMODEL_TLMID_H_
#define UCLDAS_LINEARMODEL_TLMID_H_

#include <string>

#include <boost/noncopyable.hpp>

#include "oops/base/LinearModelBase.h"
#include "oops/util/Duration.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

// Forward declarations
namespace eckit {
  class Configuration;
}
namespace ucldas {
  class Geometry;
  class Increment;
  class ModelBias;
  class State;
  struct Traits;
}

// -----------------------------------------------------------------------------

namespace ucldas {

///  linear identity model definition.
/*!
 *   linear identity model definition and configuration parameters.
 */

class TlmId: public oops::LinearModelBase<Traits>,
              private util::ObjectCounter<TlmId> {
 public:
  static const std::string classname() {return "ucldas::TlmId";}

  TlmId(const Geometry &, const eckit::Configuration &);
  ~TlmId();

/// Model trajectory computation
  void setTrajectory(const State &, State &, const ModelBias &) override;

/// Run TLM and its adjoint
  void initializeTL(Increment &) const override;
  void stepTL(Increment &, const ModelBiasIncrement &) const override;
  void finalizeTL(Increment &) const override;

  void initializeAD(Increment &) const override;
  void stepAD(Increment &, ModelBiasIncrement &) const override;
  void finalizeAD(Increment &) const override;

/// Other utilities
  const util::Duration & timeResolution() const override {return tstep_;}
  const Geometry & resolution() const {return resol_;}
  const oops::Variables & variables() const override {return linvars_;}

 private:
  void print(std::ostream &) const override;

// Data
  int keyConfig_;
  util::Duration tstep_;
  const Geometry resol_;
  const oops::Variables linvars_;
};
// -----------------------------------------------------------------------------

}  // namespace ucldas
#endif  // UCLDAS_LINEARMODEL_TLMID_H_
