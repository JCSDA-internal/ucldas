/*
 * (C) Copyright 2017-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_TRAITS_H_
#define UCLDAS_TRAITS_H_

#include <string>

#include "ucldas/Covariance/ErrorCovariance.h"
#include "ucldas/Geometry/Geometry.h"
#include "ucldas/GeometryIterator/GeometryIterator.h"
#include "ucldas/GetValues/GetValues.h"
#include "ucldas/GetValues/LinearGetValues.h"
#include "ucldas/Increment/Increment.h"
#include "ucldas/Localization/Localization.h"
#include "ucldas/ModelBias/ModelBias.h"
#include "ucldas/ModelBias/ModelBiasCovariance.h"
#include "ucldas/ModelBias/ModelBiasIncrement.h"
#include "ucldas/State/State.h"

namespace ucldas {

struct Traits {
  static std::string name() {return "UCLDAS";}
  static std::string nameCovar() {return "UcldasError";}

  typedef ucldas::Geometry            Geometry;
  typedef ucldas::GeometryIterator    GeometryIterator;
  typedef ucldas::State               State;
  typedef ucldas::Increment           Increment;
  typedef ucldas::ErrorCovariance     Covariance;
  typedef ucldas::GetValues           GetValues;
  typedef ucldas::LinearGetValues     LinearGetValues;

  typedef ucldas::ModelBias           ModelAuxControl;
  typedef ucldas::ModelBiasIncrement  ModelAuxIncrement;
  typedef ucldas::ModelBiasCovariance ModelAuxCovariance;
  typedef ucldas::Localization        Localization;
};

}  // namespace ucldas

#endif  // UCLDAS_TRAITS_H_
