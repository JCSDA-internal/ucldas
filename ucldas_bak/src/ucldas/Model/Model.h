/*
 * (C) Copyright 2017-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_MODEL_MODEL_H_
#define UCLDAS_MODEL_MODEL_H_

#include <memory>
#include <ostream>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>

#include "oops/base/Variables.h"
#include "oops/interface/ModelBase.h"
#include "oops/util/Duration.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

#include "ucldas/Fortran.h"

// Forward declarations
namespace eckit {
  class Configuration;
}
namespace ucldas {
  class Geometry;
  class ModelBias;
  class State;
  struct Traits;
}

// -----------------------------------------------------------------------------

namespace ucldas {

  /// UCLDAS model definition.
  /*!
   *  UCLDAS nonlinear model definition and configuration parameters.
   */

  class Model:public oops::interface::ModelBase<Traits>,
              private util::ObjectCounter<Model>
  {
   public:
    static const std::string classname() {return "ucldas::Model";}

    Model(const Geometry &, const eckit::Configuration &);
    ~Model();

    /// Prepare model integration
    void initialize(State &) const;

    /// Model integration
    void step(State &, const ModelBias &) const;
    int saveTrajectory(State &, const ModelBias &) const;

    /// Finish model integration
    void finalize(State &) const;

    /// Utilities
    const util::Duration & timeResolution() const {return tstep_;}
    const oops::Variables & variables() const {return vars_;}

   private:
    void print(std::ostream &) const;
    int keyConfig_;
    util::Duration tstep_;
    bool setup_ucland_;
    std::unique_ptr<const Geometry> geom_;
    const oops::Variables vars_;
  };
  // -----------------------------------------------------------------------------

}  // namespace ucldas
#endif  // UCLDAS_MODEL_MODEL_H_
