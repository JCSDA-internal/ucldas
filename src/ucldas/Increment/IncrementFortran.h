/*
 * (C) Copyright 2020-2021 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_INCREMENT_INCREMENTFORTRAN_H_
#define UCLDAS_INCREMENT_INCREMENTFORTRAN_H_

#include "oops/base/Variables.h"
#include "ucldas/Fortran.h"

// Forward declarations
namespace atlas {
  namespace field {
    class FieldSetImpl;
  }
}
namespace eckit {
  class Configuration;
}

namespace util {
  class DateTime;
  class Duration;
}

namespace ucldas {

  extern "C" {
    void ucldas_increment_create_f90(F90flds &, const F90geom &,
                               const oops::Variables &);
    void ucldas_increment_delete_f90(F90flds &);
    void ucldas_increment_copy_f90(const F90flds &, const F90flds &);
    void ucldas_increment_ones_f90(const F90flds &);
    void ucldas_increment_zero_f90(const F90flds &);
    void ucldas_increment_self_add_f90(const F90flds &, const F90flds &);
    void ucldas_increment_self_sub_f90(const F90flds &, const F90flds &);
    void ucldas_increment_self_mul_f90(const F90flds &, const double &);
    void ucldas_increment_accumul_f90(const F90flds &, const double &,
                                    const F90flds &);
    void ucldas_increment_axpy_f90(const F90flds &, const double &,
                                 const F90flds &);
    void ucldas_increment_dot_prod_f90(const F90flds &, const F90flds &,
                                     double &);
    void ucldas_increment_self_schur_f90(const F90flds &, const F90flds &);
    void ucldas_increment_random_f90(const F90flds &);
    void ucldas_increment_dirac_f90(const F90flds &,
                              const eckit::Configuration * const &);
    void ucldas_increment_diff_incr_f90(const F90flds &, const F90flds &,
                                  const F90flds &);
    void ucldas_increment_change_resol_f90(const F90flds &, const F90flds &);
    void ucldas_increment_read_file_f90(const F90flds &,
                                  const eckit::Configuration * const &,
                                  util::DateTime * const *);
    void ucldas_increment_write_file_f90(const F90flds &,
                                   const eckit::Configuration * const &,
                                   const util::DateTime * const *);
    void ucldas_increment_set_atlas_f90(const F90flds &,
                                  const F90geom &,
                                  const oops::Variables &,
                                  atlas::field::FieldSetImpl *);
    void ucldas_increment_to_atlas_f90(const F90flds &,
                                 const F90geom &,
                                 const oops::Variables &,
                                 atlas::field::FieldSetImpl *);
    void ucldas_increment_from_atlas_f90(const F90flds &,
                                   const F90geom &,
                                   const oops::Variables &,
                                   atlas::field::FieldSetImpl *);
    void ucldas_increment_gpnorm_f90(const F90flds &, const int &, double &);
    void ucldas_increment_getpoint_f90(const F90flds &, const F90iter &, double &,
                           const int &);
    void ucldas_increment_setpoint_f90(F90flds &, const F90iter &, const double &,
                           const int &);
    void ucldas_increment_sizes_f90(const F90flds &, int &,
                              int &, int &, int &);
    void ucldas_increment_rms_f90(const F90flds &, double &);
    void ucldas_increment_serial_size_f90(const F90flds &,
                                        const F90geom &,
                                        size_t &);
    void ucldas_increment_serialize_f90(const F90flds &,
                                      const F90geom &,
                                      const size_t &,
                                      double[]);
    void ucldas_increment_deserialize_f90(const F90flds &,
                                        const F90geom &,
                                        const size_t &,
                                        const double[],
                                        size_t &);
  }
}  // namespace ucldas
#endif  // UCLDAS_INCREMENT_INCREMENTFORTRAN_H_
