/*
 * (C) Copyright 2020-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_STATE_STATEFORTRAN_H_
#define UCLDAS_STATE_STATEFORTRAN_H_

#include "ucldas/Fortran.h"

#include "oops/base/Variables.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace util {
  class DateTime;
}

namespace ucldas {

  extern "C" {
    void ucldas_state_create_f90(F90flds &, const F90geom &,
                               const oops::Variables &);
    void ucldas_state_delete_f90(F90flds &);
    void ucldas_state_copy_f90(const F90flds &, const F90flds &);
    void ucldas_state_zero_f90(const F90flds &);
    void ucldas_state_axpy_f90(const F90flds &, const double &, const F90flds &);
    void ucldas_state_add_incr_f90(const F90flds &, const F90flds &);
    void ucldas_state_read_file_f90(const F90flds &,
                                  const eckit::Configuration * const &,
                                  util::DateTime * const *);
    void ucldas_state_write_file_f90(const F90flds &,
                                   const eckit::Configuration * const &,
                                   const util::DateTime * const *);
    void ucldas_state_rotate2grid_f90(const F90flds &,
                                    const oops::Variables &,
                                    const oops::Variables &);
    void ucldas_state_rotate2north_f90(const F90flds &,
                                     const oops::Variables &,
                                     const oops::Variables &);
    void ucldas_state_logtrans_f90(const F90flds &, const oops::Variables &);
    void ucldas_state_expontrans_f90(const F90flds &, const oops::Variables &);
    void ucldas_state_gpnorm_f90(const F90flds &, const int &, double &);
    void ucldas_state_sizes_f90(const F90flds &, int &,
                              int &, int &, int &);
    void ucldas_state_rms_f90(const F90flds &, double &);
    void ucldas_state_change_resol_f90(const F90flds &, const F90flds &);
    void ucldas_state_serial_size_f90(const F90flds &,
                                    const F90geom &,
                                    size_t &);
    void ucldas_state_serialize_f90(const F90flds &,
                                  const F90geom &,
                                  const size_t &,
                                  double[]);
    void ucldas_state_deserialize_f90(const F90flds &,
                                    const F90geom &,
                                    const size_t &,
                                    const double[],
                                    size_t &);
  }
}  // namespace ucldas
#endif  // UCLDAS_STATE_STATEFORTRAN_H_
