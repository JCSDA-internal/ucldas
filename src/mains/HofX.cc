/*
 * (C) Copyright 2009-2016 ECMWF.
 * (C) Copyright 2017-2020 UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "ucldas/Traits.h"

#include "oops/generic/instantiateModelFactory.h"
#include "oops/runs/HofX4D.h"
#include "oops/runs/Run.h"
#include "ufo/instantiateObsErrorFactory.h"
#include "ufo/instantiateObsFilterFactory.h"
#include "ufo/ObsTraits.h"

int main(int argc,  char ** argv) {
  oops::Run run(argc, argv);
  oops::instantiateModelFactory<ucldas::Traits>();
  ufo::instantiateObsErrorFactory();
  ufo::instantiateObsFilterFactory();
  oops::HofX4D<ucldas::Traits, ufo::ObsTraits> hofx;
  return run.execute(hofx);
}
