/*
 * (C) Copyright 2017-2020 UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_LOCALIZATION_INSTANTIATELOCALIZATIONFACTORY_H_
#define UCLDAS_LOCALIZATION_INSTANTIATELOCALIZATIONFACTORY_H_

#include "ucldas/Traits.h"

#include "ucldas/Localization/Localization.h"

#include "oops/interface/LocalizationBase.h"

namespace ucldas {

void instantiateLocalizationFactory() {
}
// static oops::LocalizationMaker<ucldas::Traits,
//                                Localization> maker_("UCLDAS");
}  // namespace ucldas

#endif  // UCLDAS_LOCALIZATION_INSTANTIATELOCALIZATIONFACTORY_H_
