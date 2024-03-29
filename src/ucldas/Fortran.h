/*
 * (C) Copyright 2017-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_FORTRAN_H_
#define UCLDAS_FORTRAN_H_

namespace ucldas {

  // Geometry key type
  typedef int F90geom;
  // Geometry iterator key type
  typedef int F90iter;
  // Model key type
  typedef int F90model;
  // Goms key type
  typedef int F90goms;
  // Fields key type
  typedef int F90flds;
  // Trajectory key type
  typedef int F90traj;
  // GetValues key type
  typedef int F90getval;
  // LinearGetValues key type
  typedef int F90lingetval;
  // Background error covariance key type
  typedef int F90bmat;
  // Background error covariance key type
  typedef int F90balopmat;
  // Observation vector key type
  typedef int F90ovec;
  // Obs operator key type
  typedef int F90hop;
  // Observation data base type
  typedef int F90odb;

}  // namespace ucldas
#endif  // UCLDAS_FORTRAN_H_
