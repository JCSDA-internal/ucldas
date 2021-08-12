/*
 * (C) Copyright 2017-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef MAINS_GRIDGEN_H_
#define MAINS_GRIDGEN_H_

#include <string>

#include "ucldas/Traits.h"

#include "ucldas/Geometry/Geometry.h"
#include "ucldas/Model/Model.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/mpi/Comm.h"
#include "oops/base/PostProcessor.h"
#include "oops/mpi/mpi.h"
#include "oops/runs/Application.h"

namespace ucldas {

  class GridGen : public oops::Application {
   public:
    explicit GridGen(const eckit::mpi::Comm & comm = oops::mpi::world())
      : Application(comm) {}
    static const std::string classname() {return "ucldas::GridGen";}

    int execute(const eckit::Configuration & fullConfig) const {
      //  Setup resolution
      const eckit::LocalConfiguration geomconfig(fullConfig, "geometry");
      const Geometry geom(geomconfig, this->getComm());

      //  Generate model grid
      geom.gridgen();

      return 0;
    }
    // -----------------------------------------------------------------------------
   private:
    std::string appname() const {
      return "ucldas::GridGen<";
    }
    // -----------------------------------------------------------------------------
  };

}  // namespace ucldas
#endif  // MAINS_GRIDGEN_H_
