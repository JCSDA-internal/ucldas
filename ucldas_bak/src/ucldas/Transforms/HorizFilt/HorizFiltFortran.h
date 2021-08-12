/*
 * (C) Copyright 2017-2020  UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_TRANSFORMS_HORIZFILT_HORIZFILTFORTRAN_H_
#define UCLDAS_TRANSFORMS_HORIZFILT_HORIZFILTFORTRAN_H_

#include "ucldas/Fortran.h"

#include "oops/base/Variables.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace ucldas {

  extern "C" {
    void ucldas_horizfilt_setup_f90(F90balopmat &,
                                  const eckit::Configuration * const *,
                                  const F90geom &,
                                  const F90flds &,
                                  const oops::Variables &);
    void ucldas_horizfilt_delete_f90(F90balopmat &);
    void ucldas_horizfilt_mult_f90(const F90balopmat &,
                                 const F90flds &,
                                 F90flds &,
                                 const F90geom &);
    void ucldas_horizfilt_multad_f90(const F90balopmat &,
                                   const F90flds &,
                                   F90flds &,
                                   const F90geom &);
  }
}  // namespace ucldas

#endif  // UCLDAS_TRANSFORMS_HORIZFILT_HORIZFILTFORTRAN_H_
