/*
 * (C) Copyright 2020-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_GEOMETRY_FMSINPUT_H_
#define UCLDAS_GEOMETRY_FMSINPUT_H_

#include <sys/stat.h>

#include <unistd.h>
#include <cstring>
#include <fstream>
#include <ostream>
#include <string>

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"

// -----------------------------------------------------------------------------

namespace ucldas {

  /// FmsInput handles the hard-coded fms input.nml file.
  struct FmsInput {
    FmsInput(const eckit::mpi::Comm &, const eckit::Configuration & conf);
    FmsInput(const FmsInput &);
    ~FmsInput();

    void updateNameList();
    int getFileSN(const std::string &);

    const eckit::mpi::Comm & comm_;
    const eckit::Configuration & conf_;
    int inputnml_sn_;
    std::string inputnml_orig_;
  };
}  // namespace ucldas

#endif  // UCLDAS_GEOMETRY_FMSINPUT_H_
