# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of ComparativeCharacterization
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' @param type    Either 'cohort' or 'covariate'. The default is covariate
#' @export
createStratifyingSetting <- function(type = 'cohort',
                                     stratyfingCohortIds = NULL,
                                     analysisId = NULL,
                                     covariateIds = NULL
){
  if( (type == "cohort") & is.null(stratyfingCohortIds)) stop ("Please define stratyfingCohortIds when set the type as cohort")
  if( (type == "covariate") & is.null(analysisId)) stop ("Please define analysisId when set the type as covariate")
  if( (type == "covariate") & (length(analysisId)>1) ) stop ("Currently multiple analysisId is not supported")

  stratifyingSetting <- list(type = type,
                             stratyfingCohortIds = stratyfingCohortIds,
                             analysisId = analysisId,
                             covariateIds = covariateIds)
  class (stratifyingSetting) <- "characterizationStratifyingSetting"
}
