/*
 * (C) Copyright 2019-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_LOCALIZATION_LOCALIZATIONFORTRAN_H_
#define UCLDAS_LOCALIZATION_LOCALIZATIONFORTRAN_H_

#include "ucldas/Fortran.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace ucldas {

  extern "C" {
    void ucldas_localization_setup_f90(F90lclz &,
                                     const eckit::Configuration * const *,
                                     const F90geom &);
    void ucldas_localization_delete_f90(F90lclz &);
    void ucldas_localization_randomize_f90(const F90lclz &, const F90flds &);
    void ucldas_localization_mult_f90(const F90lclz &, const F90flds &);
  }
}  // namespace ucldas
#endif  // UCLDAS_LOCALIZATION_LOCALIZATIONFORTRAN_H_
