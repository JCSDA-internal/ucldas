/*
 * (C) Copyright 2017-2020  UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_MODEL_MODELFORTRAN_H_
#define UCLDAS_MODEL_MODELFORTRAN_H_

#include "ucldas/Fortran.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace ucldas {

  extern "C" {
    void ucldas_setup_f90(const eckit::Configuration * const *,
                        const F90geom &, F90model &);
    void ucldas_delete_f90(F90model &);
    void ucldas_initialize_integration_f90(const F90model &, const F90flds &);
    void ucldas_finalize_integration_f90(const F90model &, const F90flds &);
    void ucldas_propagate_f90(const F90model &, const F90flds &,
                            util::DateTime * const *);
  }
}  // namespace ucldas

#endif  // UCLDAS_MODEL_MODELFORTRAN_H_
