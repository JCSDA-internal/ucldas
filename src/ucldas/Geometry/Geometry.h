/*
 * (C) Copyright 2017-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_GEOMETRY_GEOMETRY_H_
#define UCLDAS_GEOMETRY_GEOMETRY_H_

#include <fstream>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/mpi/Comm.h"

#include "ucldas/Fortran.h"
#include "ucldas/Geometry/FmsInput.h"
#include "ucldas/Geometry/GeometryFortran.h"
#include "ucldas/GeometryIterator/GeometryIterator.h"
#include "ucldas/GeometryIterator/GeometryIteratorFortran.h"

#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

// Forward declarations
namespace atlas {
  class FieldSet;
  class FunctionSpace;
  namespace functionspace {
    class PointCloud;
  }
}
namespace oops {
  class Variables;
}

// -----------------------------------------------------------------------------

namespace ucldas {

  /// Geometry handles geometry for UCLDAS model.
  class Geometry : public util::Printable,
    private util::ObjectCounter<Geometry> {
   public:
      static const std::string classname() {return "ucldas::Geometry";}

      explicit Geometry(const eckit::Configuration &, const eckit::mpi::Comm &);
      Geometry(const Geometry &);
      ~Geometry();

      GeometryIterator begin() const;
      GeometryIterator end() const;
      std::vector<size_t> variableSizes(const oops::Variables & vars) const;
      std::vector<double> verticalCoord(std::string &) const {return {};}

      int& toFortran() {return keyGeom_;}
      const int& toFortran() const {return keyGeom_;}
      void gridgen() const;
      const eckit::mpi::Comm & getComm() const {return comm_;}
      eckit::LocalConfiguration  getAtmConf() const {return atmconf_;}
      bool  getAtmInit() const {return initatm_;}
      bool initAtm(const eckit::Configuration & conf) const
      {
        return conf.getBool("notocean.init", false);
      }

      atlas::FunctionSpace * atlasFunctionSpace() const;
      atlas::FieldSet * atlasFieldSet() const;

   private:
      Geometry & operator=(const Geometry &);
      void print(std::ostream &) const;
      int keyGeom_;
      const eckit::mpi::Comm & comm_;
      eckit::LocalConfiguration atmconf_;
      bool initatm_;
      FmsInput fmsinput_;
      std::unique_ptr<atlas::functionspace::PointCloud> atlasFunctionSpace_;
      std::unique_ptr<atlas::FieldSet> atlasFieldSet_;
  };
  // -----------------------------------------------------------------------------

}  // namespace ucldas

#endif  // UCLDAS_GEOMETRY_GEOMETRY_H_
