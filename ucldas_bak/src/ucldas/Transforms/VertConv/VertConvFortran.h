/*
 * (C) Copyright 2019-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_TRANSFORMS_VERTCONV_VERTCONVFORTRAN_H_
#define UCLDAS_TRANSFORMS_VERTCONV_VERTCONVFORTRAN_H_

#include "ucldas/Fortran.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace ucldas {

  extern "C" {
    void ucldas_vertconv_setup_f90(F90balopmat &,
                                 const eckit::Configuration * const *,
                                 const F90flds &,
                                 const F90geom &);
    void ucldas_vertconv_delete_f90(F90balopmat &);
    void ucldas_vertconv_mult_f90(const F90balopmat &, F90balopmat &,
                                const F90balopmat &);
    void ucldas_vertconv_multad_f90(const F90balopmat &, F90balopmat &,
                                  const F90balopmat &);
  }
}  // namespace ucldas
#endif  // UCLDAS_TRANSFORMS_VERTCONV_VERTCONVFORTRAN_H_
