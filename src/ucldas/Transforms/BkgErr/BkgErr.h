/*
 * (C) Copyright 2017-2020  UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_TRANSFORMS_BKGERR_BKGERR_H_
#define UCLDAS_TRANSFORMS_BKGERR_BKGERR_H_

#include <ostream>
#include <string>

#include "oops/util/DateTime.h"
#include "oops/util/Printable.h"

// Forward declarations
namespace eckit {
  class Configuration;
}
namespace ucldas {
  class State;
  class Increment;
  class Geometry;
}

// -----------------------------------------------------------------------------

namespace ucldas {

/// UCLDAS linear change of variable
class BkgErr: public util::Printable {
 public:
  static const std::string classname() {return "ucldas::BkgErr";}

  explicit BkgErr(const State &, const State &, const Geometry &,
                  const eckit::Configuration &);
  ~BkgErr();

/// Perform linear transforms
  void multiply(const Increment &, Increment &) const;
  void multiplyInverse(const Increment &, Increment &) const;
  void multiplyAD(const Increment &, Increment &) const;
  void multiplyInverseAD(const Increment &, Increment &) const;

 private:
  void print(std::ostream &) const override;
  int keyFtnConfig_;
};
// -----------------------------------------------------------------------------

}  // namespace ucldas
#endif  // UCLDAS_TRANSFORMS_BKGERR_BKGERR_H_
