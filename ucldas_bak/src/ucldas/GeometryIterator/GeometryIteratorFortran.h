/*
 * (C) Copyright 2019-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_GEOMETRYITERATOR_GEOMETRYITERATORFORTRAN_H_
#define UCLDAS_GEOMETRYITERATOR_GEOMETRYITERATORFORTRAN_H_

#include "ucldas/Fortran.h"

namespace ucldas {

  extern "C" {
    void ucldas_geom_iter_setup_f90(F90iter &, const F90geom &,
                                  const int &, const int &);
    void ucldas_geom_iter_clone_f90(F90iter &, const F90iter &);
    void ucldas_geom_iter_delete_f90(F90iter &);
    void ucldas_geom_iter_equals_f90(const F90iter &, const F90iter&, int &);
    void ucldas_geom_iter_current_f90(const F90iter &, double &, double &);
    void ucldas_geom_iter_next_f90(const F90iter &);
  }
}  // namespace ucldas
#endif  // UCLDAS_GEOMETRYITERATOR_GEOMETRYITERATORFORTRAN_H_
