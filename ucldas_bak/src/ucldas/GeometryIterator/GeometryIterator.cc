/*
 * (C) Copyright 2019-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "ucldas/Geometry/Geometry.h"
#include "ucldas/GeometryIterator/GeometryIterator.h"
#include "ucldas/GeometryIterator/GeometryIteratorFortran.h"

#include "eckit/config/Configuration.h"
#include "eckit/geometry/Point2.h"
#include "oops/util/Logger.h"

// -----------------------------------------------------------------------------

namespace ucldas {


// -----------------------------------------------------------------------------

GeometryIterator::GeometryIterator(const GeometryIterator& iter) {
  ucldas_geom_iter_clone_f90(keyIter_, iter.toFortran());
}

// -----------------------------------------------------------------------------

GeometryIterator::GeometryIterator(const Geometry& geom,
                                       const int & iindex, const int & jindex) {
  ucldas_geom_iter_setup_f90(keyIter_, geom.toFortran(), iindex, jindex);
}


// -----------------------------------------------------------------------------

GeometryIterator::~GeometryIterator() {
  ucldas_geom_iter_delete_f90(keyIter_);
}

// -----------------------------------------------------------------------------

bool GeometryIterator::operator==(const GeometryIterator & other) const {
  int equals = 0;
  ucldas_geom_iter_equals_f90(keyIter_, other.toFortran(), equals);
  return (equals == 1);
}

// -----------------------------------------------------------------------------

bool GeometryIterator::operator!=(const GeometryIterator & other) const {
  int equals = 0;
  ucldas_geom_iter_equals_f90(keyIter_, other.toFortran(), equals);
  return (equals == 0);
}

// -----------------------------------------------------------------------------

eckit::geometry::Point2 GeometryIterator::operator*() const {
  double lat, lon;
  ucldas_geom_iter_current_f90(keyIter_, lon, lat);
  return eckit::geometry::Point2(lon, lat);
}

// -----------------------------------------------------------------------------

GeometryIterator& GeometryIterator::operator++() {
  ucldas_geom_iter_next_f90(keyIter_);
  return *this;
}

// -----------------------------------------------------------------------------

void GeometryIterator::print(std::ostream & os) const {
  double lat, lon;
  ucldas_geom_iter_current_f90(keyIter_, lon, lat);
  os << "GeometryIterator, lat/lon: " << lat << " / " << lon << std::endl;
}

// -----------------------------------------------------------------------------

}  // namespace ucldas
