/*
 * (C) Copyright 2019-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_COVARIANCE_ERRORCOVARIANCEFORTRAN_H_
#define UCLDAS_COVARIANCE_ERRORCOVARIANCEFORTRAN_H_

#include "ucldas/Fortran.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace ucldas {

  extern "C" {
    void ucldas_b_setup_f90(F90bmat &, const eckit::Configuration * const *,
                          const F90geom &, const F90flds &,
                          const oops::Variables &);
    void ucldas_b_delete_f90(F90bmat &);
    void ucldas_b_mult_f90(const F90bmat &, const F90flds &,
                         const F90flds &);
    void ucldas_b_invmult_f90(const F90bmat &, const F90flds &, const F90flds &);
    void ucldas_b_randomize_f90(const F90bmat &, const F90flds &);
  }
}  // namespace ucldas
#endif  // UCLDAS_COVARIANCE_ERRORCOVARIANCEFORTRAN_H_
