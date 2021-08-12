/*
 * (C) Copyright 2017-2020  UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_TRANSFORMS_BKGERRFILT_BKGERRFILTFORTRAN_H_
#define UCLDAS_TRANSFORMS_BKGERRFILT_BKGERRFILTFORTRAN_H_

#include "ucldas/Fortran.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace ucldas {

  extern "C" {
    void ucldas_bkgerrfilt_setup_f90(F90balopmat &,
                                   const eckit::Configuration * const *,
                                   const F90flds &,
                                   const F90geom &);
    void ucldas_bkgerrfilt_delete_f90(F90balopmat &);
    void ucldas_bkgerrfilt_mult_f90(const F90balopmat &,
                                  const F90flds &,
                                  F90flds &);
    void ucldas_bkgerrfilt_multad_f90(const F90balopmat,
                                    F90flds &,
                                    const F90flds &);
  }
}  // namespace ucldas

#endif  // UCLDAS_TRANSFORMS_BKGERRFILT_BKGERRFILTFORTRAN_H_
