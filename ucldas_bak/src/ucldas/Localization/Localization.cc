/*
 * (C) Copyright 2017-2020 UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "ucldas/Geometry/Geometry.h"
#include "ucldas/Increment/Increment.h"
#include "ucldas/Localization/Localization.h"
#include "ucldas/Localization/LocalizationFortran.h"

#include "eckit/config/Configuration.h"

// -----------------------------------------------------------------------------
namespace ucldas {
// -----------------------------------------------------------------------------
Localization::Localization(const Geometry & resol,
                                       const eckit::Configuration & config) {
  const eckit::Configuration * configc = &config;
  ucldas_localization_setup_f90(keyFtnConfig_, &configc, resol.toFortran());
}
// -----------------------------------------------------------------------------
Localization::~Localization() {
  ucldas_localization_delete_f90(keyFtnConfig_);
}
// -----------------------------------------------------------------------------
void Localization::randomize(Increment & dx) const {
  ucldas_localization_randomize_f90(keyFtnConfig_, dx.fields().toFortran());
}
// -----------------------------------------------------------------------------
void Localization::multiply(Increment & dx) const {
  ucldas_localization_mult_f90(keyFtnConfig_, dx.fields().toFortran());
}
// -----------------------------------------------------------------------------
void Localization::print(std::ostream & os) const {
  os << "Localization::print not implemented";
}
// -----------------------------------------------------------------------------

}  // namespace ucldas
