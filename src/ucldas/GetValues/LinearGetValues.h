/*
 * (C) Copyright 2019-2021 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_GETVALUES_LINEARGETVALUES_H_
#define UCLDAS_GETVALUES_LINEARGETVALUES_H_

#include <memory>
#include <ostream>
#include <string>

#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

#include "ucldas/Fortran.h"

#include "ufo/Locations.h"

// Forward declarations
namespace ucldas {
  class Increment;
  class State;
  class Model2GeoVaLs;
  class LinearModel2GeoVaLs;
}
namespace ufo {
  class GeoVaLs;
}

//-----------------------------------------------------------------------------

namespace ucldas {

  /// UCLDAS LinearGetValues
  /*!
   * LinearGetValues interpolates Increment and State trajectory to observation locations
   */
class LinearGetValues : public util::Printable,
                        private util::ObjectCounter<LinearGetValues> {
 public:
  static const std::string classname() {return "ucldas::LinearGetValues";}

  ///  Constructor, destructor
  LinearGetValues(const Geometry &, const ufo::Locations &,
                  const eckit::Configuration &);
  virtual ~LinearGetValues();

  /// Trajectory for the linearized interpolation
  void setTrajectory(const State & state,
                     const util::DateTime & t1,
                     const util::DateTime & t2,
                     ufo::GeoVaLs & geovals);  // NOLINT

  /// Forward and backward interpolation
  void fillGeoVaLsTL(const Increment & inc,
                     const util::DateTime & t1,
                     const util::DateTime & t2,
                     ufo::GeoVaLs & geovals) const;  // NOLINT
  void fillGeoVaLsAD(Increment & inc,   // NOLINT
                     const util::DateTime & t1,
                     const util::DateTime & t2,
                     const ufo::GeoVaLs & geovals) const;

 private:
  void print(std::ostream &) const;
  F90getval keyLinearGetValues_;
  ufo::Locations locs_;
  std::shared_ptr<const Geometry> geom_;
  std::unique_ptr<Model2GeoVaLs> model2geovals_;
  std::unique_ptr<LinearModel2GeoVaLs> linearmodel2geovals_;
};
// -----------------------------------------------------------------------------

}  // namespace ucldas

#endif  // UCLDAS_GETVALUES_LINEARGETVALUES_H_
