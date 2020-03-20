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


#' @param useDemographicsGender = FALSE
#' @param useDemographicsAge
#' @param useDemographicsAgeGroup
#' @param useDemographicsRace
#' @param useDemographicsEthnicity
#' @param useDemographicsIndexYear
#' @param useDemographicsIndexMonth
#' @param useDemographicsPriorObservationTime
#' @param useDemographicsPostObservationTime
#' @param useDemographicsTimeInCohort
#' @param useDemographicsIndexYearMonth
#' @param conceptIdsConditionOccurrenceAnyTimePrior
#' @param conceptIdsConditionOccurrenceLongTerm
#' @param conceptIdsConditionOccurrenceMediumTerm
#' @param conceptIdsConditionOccurrenceShortTerm
#' @param conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior
#' @param conceptIdsConditionOccurrencePrimaryInpatientLongTerm
#' @param conceptIdsConditionOccurrencePrimaryInpatientMediumTerm
#' @param conceptIdsConditionOccurrencePrimaryInpatientShortTerm
#' @param conceptIdsConditionEraAnyTimePrior
#' @param conceptIdsConditionEraLongTerm
#' @param conceptIdsConditionEraMediumTerm
#' @param conceptIdsConditionEraShortTerm
#' @param conceptIdsConditionEraOverlapping
#' @param conceptIdsConditionEraStartLongTerm
#' @param conceptIdsConditionEraStartMediumTerm
#' @param conceptIdsConditionEraStartShortTerm
#' @param conceptIdsConditionGroupEraAnyTimePrior
#' @param conceptIdsConditionGroupEraLongTerm
#' @param conceptIdsConditionGroupEraMediumTerm
#' @param conceptIdsConditionGroupEraShortTerm
#' @param conceptIdsConditionGroupEraOverlapping
#' @param conceptIdsConditionGroupEraStartLongTerm
#' @param conceptIdsConditionGroupEraStartMediumTerm
#' @param conceptIdsConditionGroupEraStartShortTerm
#' @param conceptIdsDrugExposureAnyTimePrior
#' @param conceptIdsDrugExposureLongTerm
#' @param conceptIdsDrugExposureMediumTerm
#' @param conceptIdsDrugExposureShortTerm
#' @param conceptIdsDrugEraAnyTimePrior
#' @param conceptIdsDrugEraLongTerm
#' @param conceptIdsDrugEraMediumTerm
#' @param conceptIdsDrugEraShortTerm
#' @param conceptIdsDrugEraOverlapping
#' @param conceptIdsDrugEraStartLongTerm
#' @param conceptIdsDrugEraStartMediumTerm
#' @param conceptIdsDrugEraStartShortTerm
#' @param conceptIdsDrugGroupEraAnyTimePrior
#' @param conceptIdsDrugGroupEraLongTerm
#' @param conceptIdsDrugGroupEraMediumTerm
#' @param conceptIdsDrugGroupEraShortTerm
#' @param conceptIdsDrugGroupEraOverlapping
#' @param conceptIdsDrugGroupEraStartLongTerm
#' @param conceptIdsDrugGroupEraStartMediumTerm
#' @param conceptIdsDrugGroupEraStartShortTerm
#' @param conceptIdsProcedureOccurrenceAnyTimePrior
#' @param conceptIdsProcedureOccurrenceLongTerm
#' @param conceptIdsProcedureOccurrenceMediumTerm
#' @param conceptIdsProcedureOccurrenceShortTerm
#' @param conceptIdsDeviceExposureAnyTimePrior
#' @param conceptIdsDeviceExposureLongTerm
#' @param conceptIdsDeviceExposureMediumTerm
#' @param conceptIdsDeviceExposureShortTerm
#' @param conceptIdsMeasurementAnyTimePrior
#' @param conceptIdsMeasurementLongTerm
#' @param conceptIdsMeasurementMediumTerm
#' @param conceptIdsMeasurementShortTerm
#' @param conceptIdsMeasurementValueAnyTimePrior
#' @param conceptIdsMeasurementValueLongTerm
#' @param conceptIdsMeasurementValueMediumTerm
#' @param conceptIdsMeasurementValueShortTerm
#' @param conceptIdsMeasurementRangeGroupAnyTimePrior
#' @param conceptIdsMeasurementRangeGroupLongTerm
#' @param conceptIdsMeasurementRangeGroupMediumTerm
#' @param conceptIdsMeasurementRangeGroupShortTerm
#' @param conceptIdsObservationAnyTimePrior
#' @param conceptIdsObservationLongTerm
#' @param conceptIdsObservationMediumTerm
#' @param conceptIdsObservationShortTerm
#' @param useCharlsonIndex
#' @param useDcsi
#' @param useChads2
#' @param useChads2Vasc
#' @param useHfrs
#' @param useDistinctConditionCountLongTerm
#' @param useDistinctConditionCountMediumTerm
#' @param useDistinctConditionCountShortTerm
#' @param useDistinctIngredientCountLongTerm
#' @param useDistinctIngredientCountMediumTerm
#' @param useDistinctIngredientCountShortTerm
#' @param useDistinctProcedureCountLongTerm
#' @param useDistinctProcedureCountMediumTerm
#' @param useDistinctProcedureCountShortTerm
#' @param useDistinctMeasurementCountLongTerm
#' @param useDistinctMeasurementCountMediumTerm
#' @param useDistinctMeasurementCountShortTerm
#' @param useDistinctObservationCountLongTerm
#' @param useDistinctObservationCountMediumTerm
#' @param useDistinctObservationCountShortTerm
#' @param useVisitCountLongTerm
#' @param useVisitCountMediumTerm
#' @param useVisitCountShortTerm
#' @param useVisitConceptCountLongTerm
#' @param useVisitConceptCountMediumTerm
#' @param useVisitConceptCountShortTerm
#' @param longTermStartDays
#' @param mediumTermStartDays
#' @param shortTermStartDays
#' @param endDays
#'
#' @return
#' An object of type \code{tableSpecification}, to be used in other functions.
#'
#' @export
setTableSpecification <- function(useDemographicsGender = FALSE,
                                  useDemographicsAge = FALSE,
                                  useDemographicsAgeGroup = FALSE,
                                  useDemographicsRace = FALSE,
                                  useDemographicsEthnicity = FALSE,
                                  useDemographicsIndexYear = FALSE,
                                  useDemographicsIndexMonth = FALSE,
                                  useDemographicsPriorObservationTime = FALSE,
                                  useDemographicsPostObservationTime = FALSE,
                                  useDemographicsTimeInCohort = FALSE,
                                  useDemographicsIndexYearMonth = FALSE,
                                  conceptIdsConditionOccurrenceAnyTimePrior = c(),
                                  conceptIdsConditionOccurrenceLongTerm = c(),
                                  conceptIdsConditionOccurrenceMediumTerm = c(),
                                  conceptIdsConditionOccurrenceShortTerm = c(),
                                  conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior = c(),
                                  conceptIdsConditionOccurrencePrimaryInpatientLongTerm = c(),
                                  conceptIdsConditionOccurrencePrimaryInpatientMediumTerm = c(),
                                  conceptIdsConditionOccurrencePrimaryInpatientShortTerm = c(),
                                  conceptIdsConditionEraAnyTimePrior = c(),
                                  conceptIdsConditionEraLongTerm = c(),
                                  conceptIdsConditionEraMediumTerm = c(),
                                  conceptIdsConditionEraShortTerm = c(),
                                  conceptIdsConditionEraOverlapping = c(),
                                  conceptIdsConditionEraStartLongTerm = c(),
                                  conceptIdsConditionEraStartMediumTerm = c(),
                                  conceptIdsConditionEraStartShortTerm = c(),
                                  conceptIdsConditionGroupEraAnyTimePrior = c(),
                                  conceptIdsConditionGroupEraLongTerm = c(),
                                  conceptIdsConditionGroupEraMediumTerm = c(),
                                  conceptIdsConditionGroupEraShortTerm = c(),
                                  conceptIdsConditionGroupEraOverlapping = c(),
                                  conceptIdsConditionGroupEraStartLongTerm = c(),
                                  conceptIdsConditionGroupEraStartMediumTerm = c(),
                                  conceptIdsConditionGroupEraStartShortTerm = c(),
                                  conceptIdsDrugExposureAnyTimePrior = c(),
                                  conceptIdsDrugExposureLongTerm = c(),
                                  conceptIdsDrugExposureMediumTerm = c(),
                                  conceptIdsDrugExposureShortTerm = c(),
                                  conceptIdsDrugEraAnyTimePrior = c(),
                                  conceptIdsDrugEraLongTerm = c(),
                                  conceptIdsDrugEraMediumTerm = c(),
                                  conceptIdsDrugEraShortTerm = c(),
                                  conceptIdsDrugEraOverlapping = c(),
                                  conceptIdsDrugEraStartLongTerm = c(),
                                  conceptIdsDrugEraStartMediumTerm = c(),
                                  conceptIdsDrugEraStartShortTerm = c(),
                                  conceptIdsDrugGroupEraAnyTimePrior = c(),
                                  conceptIdsDrugGroupEraLongTerm = c(),
                                  conceptIdsDrugGroupEraMediumTerm = c(),
                                  conceptIdsDrugGroupEraShortTerm = c(),
                                  conceptIdsDrugGroupEraOverlapping = c(),
                                  conceptIdsDrugGroupEraStartLongTerm = c(),
                                  conceptIdsDrugGroupEraStartMediumTerm = c(),
                                  conceptIdsDrugGroupEraStartShortTerm = c(),
                                  conceptIdsProcedureOccurrenceAnyTimePrior = c(),
                                  conceptIdsProcedureOccurrenceLongTerm = c(),
                                  conceptIdsProcedureOccurrenceMediumTerm = c(),
                                  conceptIdsProcedureOccurrenceShortTerm = c(),
                                  conceptIdsDeviceExposureAnyTimePrior = c(),
                                  conceptIdsDeviceExposureLongTerm = c(),
                                  conceptIdsDeviceExposureMediumTerm = c(),
                                  conceptIdsDeviceExposureShortTerm = c(),
                                  conceptIdsMeasurementAnyTimePrior = c(),
                                  conceptIdsMeasurementLongTerm = c(),
                                  conceptIdsMeasurementMediumTerm = c(),
                                  conceptIdsMeasurementShortTerm = c(),
                                  conceptIdsMeasurementValueAnyTimePrior = c(),
                                  conceptIdsMeasurementValueLongTerm = c(),
                                  conceptIdsMeasurementValueMediumTerm = c(),
                                  conceptIdsMeasurementValueShortTerm = c(),
                                  conceptIdsMeasurementRangeGroupAnyTimePrior = c(),
                                  conceptIdsMeasurementRangeGroupLongTerm = c(),
                                  conceptIdsMeasurementRangeGroupMediumTerm = c(),
                                  conceptIdsMeasurementRangeGroupShortTerm = c(),
                                  conceptIdsObservationAnyTimePrior = c(),
                                  conceptIdsObservationLongTerm = c(),
                                  conceptIdsObservationMediumTerm = c(),
                                  conceptIdsObservationShortTerm = c(),
                                  useCharlsonIndex = FALSE,
                                  useDcsi = FALSE,
                                  useChads2 = FALSE,
                                  useChads2Vasc = FALSE,
                                  useHfrs = FALSE,
                                  useDistinctConditionCountLongTerm = FALSE,
                                  useDistinctConditionCountMediumTerm = FALSE,
                                  useDistinctConditionCountShortTerm = FALSE,
                                  useDistinctIngredientCountLongTerm = FALSE,
                                  useDistinctIngredientCountMediumTerm = FALSE,
                                  useDistinctIngredientCountShortTerm = FALSE,
                                  useDistinctProcedureCountLongTerm = FALSE,
                                  useDistinctProcedureCountMediumTerm = FALSE,
                                  useDistinctProcedureCountShortTerm = FALSE,
                                  useDistinctMeasurementCountLongTerm = FALSE,
                                  useDistinctMeasurementCountMediumTerm = FALSE,
                                  useDistinctMeasurementCountShortTerm = FALSE,
                                  useDistinctObservationCountLongTerm = FALSE,
                                  useDistinctObservationCountMediumTerm = FALSE,
                                  useDistinctObservationCountShortTerm = FALSE,
                                  useVisitCountLongTerm = FALSE,
                                  useVisitCountMediumTerm = FALSE,
                                  useVisitCountShortTerm = FALSE,
                                  useVisitConceptCountLongTerm = FALSE,
                                  useVisitConceptCountMediumTerm = FALSE,
                                  useVisitConceptCountShortTerm = FALSE,
                                  longTermStartDays = -365,
                                  mediumTermStartDays = -180,
                                  shortTermStartDays = -30,
                                  endDays = 0){

  covariateSetting <- FeatureExtraction::createCovariateSettings(useDemographicsGender = useDemographicsGender,
                                                                 useDemographicsAge = useDemographicsAge,
                                                                 useDemographicsAgeGroup = useDemographicsAgeGroup,
                                                                 useDemographicsRace = useDemographicsRace,
                                                                 useDemographicsEthnicity = useDemographicsEthnicity,
                                                                 useDemographicsIndexYear = useDemographicsIndexYear,
                                                                 useDemographicsIndexMonth = useDemographicsIndexMonth,
                                                                 useDemographicsPriorObservationTime = useDemographicsPriorObservationTime,
                                                                 useDemographicsPostObservationTime = useDemographicsPostObservationTime,
                                                                 useDemographicsTimeInCohort = useDemographicsTimeInCohort,
                                                                 useDemographicsIndexYearMonth = useDemographicsIndexYearMonth,

                                                                 useConditionOccurrenceAnyTimePrior = as.logical(length(conceptIdsConditionOccurrenceAnyTimePrior)),
                                                                 useConditionOccurrenceLongTerm = as.logical(length(conceptIdsConditionOccurrenceLongTerm)),
                                                                 useConditionOccurrenceMediumTerm = as.logical(length(conceptIdsConditionOccurrenceMediumTerm)),
                                                                 useConditionOccurrenceShortTerm = as.logical(length(conceptIdsConditionOccurrenceShortTerm)),
                                                                 useConditionOccurrencePrimaryInpatientAnyTimePrior = as.logical(length(conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior)),
                                                                 useConditionOccurrencePrimaryInpatientLongTerm = as.logical(length(conceptIdsConditionOccurrencePrimaryInpatientLongTerm)),
                                                                 useConditionOccurrencePrimaryInpatientMediumTerm = as.logical(length(conceptIdsConditionOccurrencePrimaryInpatientMediumTerm)),
                                                                 useConditionOccurrencePrimaryInpatientShortTerm = as.logical(length(conceptIdsConditionOccurrencePrimaryInpatientShortTerm)),
                                                                 useConditionEraAnyTimePrior = as.logical(length(conceptIdsConditionEraAnyTimePrior)),
                                                                 useConditionEraLongTerm = as.logical(length(conceptIdsConditionEraLongTerm)),
                                                                 useConditionEraMediumTerm = as.logical(length(conceptIdsConditionEraMediumTerm)),
                                                                 useConditionEraShortTerm = as.logical(length(conceptIdsConditionEraShortTerm)),
                                                                 useConditionEraOverlapping = as.logical(length(conceptIdsConditionEraOverlapping)),
                                                                 useConditionEraStartLongTerm = as.logical(length(conceptIdsConditionEraStartLongTerm)),
                                                                 useConditionEraStartMediumTerm = as.logical(length(conceptIdsConditionEraStartMediumTerm)),
                                                                 useConditionEraStartShortTerm = as.logical(length(conceptIdsConditionEraStartShortTerm)),
                                                                 useConditionGroupEraAnyTimePrior = as.logical(length(conceptIdsConditionGroupEraAnyTimePrior)),
                                                                 useConditionGroupEraLongTerm = as.logical(length(conceptIdsConditionGroupEraLongTerm)),
                                                                 useConditionGroupEraMediumTerm = as.logical(length(conceptIdsConditionGroupEraMediumTerm)),
                                                                 useConditionGroupEraShortTerm = as.logical(length(conceptIdsConditionGroupEraShortTerm)),
                                                                 useConditionGroupEraOverlapping = as.logical(length(conceptIdsConditionGroupEraOverlapping)),
                                                                 useConditionGroupEraStartLongTerm = as.logical(length(conceptIdsConditionGroupEraStartLongTerm)),
                                                                 useConditionGroupEraStartMediumTerm = as.logical(length(conceptIdsConditionGroupEraStartMediumTerm)),
                                                                 useConditionGroupEraStartShortTerm = as.logical(length(conceptIdsConditionGroupEraStartShortTerm)),
                                                                 useDrugExposureAnyTimePrior = as.logical(length(conceptIdsDrugExposureAnyTimePrior)),
                                                                 useDrugExposureLongTerm = as.logical(length(conceptIdsDrugExposureLongTerm)),
                                                                 useDrugExposureMediumTerm = as.logical(length(conceptIdsDrugExposureMediumTerm)),
                                                                 useDrugExposureShortTerm = as.logical(length(conceptIdsDrugExposureShortTerm)),
                                                                 useDrugEraAnyTimePrior = as.logical(length(conceptIdsDrugEraAnyTimePrior)),
                                                                 useDrugEraLongTerm = as.logical(length(conceptIdsDrugEraLongTerm)),
                                                                 useDrugEraMediumTerm = as.logical(length(conceptIdsDrugEraMediumTerm)),
                                                                 useDrugEraShortTerm = as.logical(length(conceptIdsDrugEraShortTerm)),
                                                                 useDrugEraOverlapping = as.logical(length(conceptIdsDrugEraOverlapping)),
                                                                 useDrugEraStartLongTerm = as.logical(length(conceptIdsDrugEraStartLongTerm)),
                                                                 useDrugEraStartMediumTerm = as.logical(length(conceptIdsDrugEraStartMediumTerm)),
                                                                 useDrugEraStartShortTerm = as.logical(length(conceptIdsDrugEraStartShortTerm)),
                                                                 useDrugGroupEraAnyTimePrior = as.logical(length(conceptIdsDrugGroupEraAnyTimePrior)),
                                                                 useDrugGroupEraLongTerm = as.logical(length(conceptIdsDrugGroupEraLongTerm)),
                                                                 useDrugGroupEraMediumTerm = as.logical(length(conceptIdsDrugGroupEraMediumTerm)),
                                                                 useDrugGroupEraShortTerm = as.logical(length(conceptIdsDrugGroupEraShortTerm)),
                                                                 useDrugGroupEraOverlapping = as.logical(length(conceptIdsDrugGroupEraOverlapping)),
                                                                 useDrugGroupEraStartLongTerm = as.logical(length(conceptIdsDrugGroupEraStartLongTerm)),
                                                                 useDrugGroupEraStartMediumTerm = as.logical(length(conceptIdsDrugGroupEraStartMediumTerm)),
                                                                 useDrugGroupEraStartShortTerm = as.logical(length(conceptIdsDrugGroupEraStartShortTerm)),
                                                                 useProcedureOccurrenceAnyTimePrior = as.logical(length(conceptIdsProcedureOccurrenceAnyTimePrior)),
                                                                 useProcedureOccurrenceLongTerm = as.logical(length(conceptIdsProcedureOccurrenceLongTerm)),
                                                                 useProcedureOccurrenceMediumTerm = as.logical(length(conceptIdsProcedureOccurrenceMediumTerm)),
                                                                 useProcedureOccurrenceShortTerm = as.logical(length(conceptIdsProcedureOccurrenceShortTerm)),
                                                                 useDeviceExposureAnyTimePrior = as.logical(length(conceptIdsDeviceExposureAnyTimePrior)),
                                                                 useDeviceExposureLongTerm = as.logical(length(conceptIdsDeviceExposureLongTerm)),
                                                                 useDeviceExposureMediumTerm = as.logical(length(conceptIdsDeviceExposureMediumTerm)),
                                                                 useDeviceExposureShortTerm = as.logical(length(conceptIdsDeviceExposureShortTerm)),
                                                                 useMeasurementAnyTimePrior = as.logical(length(conceptIdsMeasurementAnyTimePrior)),
                                                                 useMeasurementLongTerm = as.logical(length(conceptIdsMeasurementLongTerm)),
                                                                 useMeasurementMediumTerm = as.logical(length(conceptIdsMeasurementMediumTerm)),
                                                                 useMeasurementShortTerm = as.logical(length(conceptIdsMeasurementShortTerm)),
                                                                 useMeasurementValueAnyTimePrior = as.logical(length(conceptIdsMeasurementValueAnyTimePrior)),
                                                                 useMeasurementValueLongTerm = as.logical(length(conceptIdsMeasurementValueLongTerm)),
                                                                 useMeasurementValueMediumTerm = as.logical(length(conceptIdsMeasurementValueMediumTerm)),
                                                                 useMeasurementValueShortTerm = as.logical(length(conceptIdsMeasurementValueShortTerm)),
                                                                 useMeasurementRangeGroupAnyTimePrior = as.logical(length(conceptIdsMeasurementRangeGroupAnyTimePrior)),
                                                                 useMeasurementRangeGroupLongTerm = as.logical(length(conceptIdsMeasurementRangeGroupLongTerm)),
                                                                 useMeasurementRangeGroupMediumTerm = as.logical(length(conceptIdsMeasurementRangeGroupMediumTerm)),
                                                                 useMeasurementRangeGroupShortTerm = as.logical(length(conceptIdsMeasurementRangeGroupShortTerm)),
                                                                 useObservationAnyTimePrior = as.logical(length(conceptIdsObservationAnyTimePrior)),
                                                                 useObservationLongTerm = as.logical(length(conceptIdsObservationLongTerm)),
                                                                 useObservationMediumTerm = as.logical(length(conceptIdsObservationMediumTerm)),
                                                                 useObservationShortTerm = as.logical(length(conceptIdsObservationShortTerm)),

                                                                 useCharlsonIndex = useCharlsonIndex,
                                                                 useDcsi = useDcsi,
                                                                 useChads2 = useChads2,
                                                                 useChads2Vasc = useChads2Vasc,
                                                                 useHfrs = useHfrs,
                                                                 useDistinctConditionCountLongTerm = useDistinctConditionCountLongTerm,
                                                                 useDistinctConditionCountMediumTerm = useDistinctConditionCountMediumTerm,
                                                                 useDistinctConditionCountShortTerm = useDistinctConditionCountShortTerm,
                                                                 useDistinctIngredientCountLongTerm = useDistinctIngredientCountLongTerm,
                                                                 useDistinctIngredientCountMediumTerm = useDistinctIngredientCountMediumTerm,
                                                                 useDistinctIngredientCountShortTerm = useDistinctIngredientCountShortTerm,
                                                                 useDistinctProcedureCountLongTerm = useDistinctProcedureCountLongTerm,
                                                                 useDistinctProcedureCountMediumTerm = useDistinctProcedureCountMediumTerm,
                                                                 useDistinctProcedureCountShortTerm = useDistinctProcedureCountShortTerm,
                                                                 useDistinctMeasurementCountLongTerm = useDistinctMeasurementCountLongTerm,
                                                                 useDistinctMeasurementCountMediumTerm = useDistinctMeasurementCountMediumTerm,
                                                                 useDistinctMeasurementCountShortTerm = useDistinctMeasurementCountShortTerm,
                                                                 useDistinctObservationCountLongTerm = useDistinctObservationCountLongTerm,
                                                                 useDistinctObservationCountMediumTerm = useDistinctObservationCountMediumTerm,
                                                                 useDistinctObservationCountShortTerm = useDistinctObservationCountShortTerm,
                                                                 useVisitCountLongTerm = useVisitCountLongTerm,
                                                                 useVisitCountMediumTerm = useVisitCountMediumTerm,
                                                                 useVisitCountShortTerm = useVisitCountShortTerm,
                                                                 useVisitConceptCountLongTerm = useVisitConceptCountLongTerm,
                                                                 useVisitConceptCountMediumTerm = useVisitConceptCountMediumTerm,
                                                                 useVisitConceptCountShortTerm = useVisitConceptCountShortTerm,

                                                                 longTermStartDays = longTermStartDays,
                                                                 mediumTermStartDays = mediumTermStartDays,
                                                                 shortTermStartDays = shortTermStartDays,
                                                                 endDays = endDays)


  label <- c()
  analysisId <- c()
  covariateIds <- c()
  if(useDemographicsGender){
    label <- c(label, "Gender")
    analysisId <- c(analysisId, 1)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsAge){
    label <- c(label, "Age")
    analysisId <- c(analysisId, 2)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsAgeGroup){
    label <- c(label, "Age Group")
    analysisId <- c(analysisId, 3)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsRace){
    label <- c(label, "Race")
    analysisId <- c(analysisId, 4)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsEthnicity){
    label <- c(label, "Ethnicity")
    analysisId <- c(analysisId, 5)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsIndexYear){
    label <- c(label, "Index year")
    analysisId <- c(analysisId, 6)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsIndexMonth){
    label <- c(label, "Index month")
    analysisId <- c(analysisId, 7)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsPriorObservationTime){
    label <- c(label, "Prior obesrvation time")
    analysisId <- c(analysisId, 8)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsPostObservationTime){
    label <- c(label, "Post observation time")
    analysisId <- c(analysisId, 9)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsTimeInCohort){
    label <- c(label, "Time in cohort")
    analysisId <- c(analysisId, 10)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsIndexYearMonth){
    label <- c(label, "Index year month")
    analysisId <- c(analysisId, 11)
    covariateIds <- c(covariateIds, "")
  }
  if(length(conceptIdsConditionOccurrenceAnyTimePrior)){
    label <- c(label, "ConditionOccurrenceAnyTimePrior")
    analysisId <- c(analysisId, 101)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrenceAnyTimePrior,101),collapse=","))
  }
  if(length(conceptIdsConditionOccurrenceLongTerm)){
    label <- c(label, "ConditionOccurrenceLongTerm")
    analysisId <- c(analysisId, 102)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrenceLongTerm,102),collapse=","))
  }
  if(length(conceptIdsConditionOccurrenceMediumTerm)){
    label <- c(label, "ConditionOccurrenceMediumTerm")
    analysisId <- c(analysisId, 103)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrenceMediumTerm,103),collapse=","))
  }
  if(length(conceptIdsConditionOccurrenceShortTerm)){
    label <- c(label, "ConditionOccurrenceShortTerm")
    analysisId <- c(analysisId, 104)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrenceShortTerm,104),collapse=","))
  }

  if(length(conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior)){
    label <- c(label, "ConditionOccurrencePrimaryInpatientAnyTimePrior")
    analysisId <- c(analysisId, 105)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior,105),collapse=","))
  }

  if(length(conceptIdsConditionOccurrencePrimaryInpatientLongTerm)){
    label <- c(label, "ConditionOccurrencePrimaryInpatientLongTerm")
    analysisId <- c(analysisId, 106)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrencePrimaryInpatientLongTerm,106),collapse=","))
  }
  if(length(conceptIdsConditionOccurrencePrimaryInpatientMediumTerm)){
    label <- c(label, "ConditionOccurrencePrimaryInpatientMediumTerm")
    analysisId <- c(analysisId, 107)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrencePrimaryInpatientMediumTerm,107),collapse=","))
  }

  if(length(conceptIdsConditionOccurrencePrimaryInpatientShortTerm)){
    label <- c(label, "ConditionOccurrencePrimaryInpatientShortTerm")
    analysisId <- c(analysisId, 108)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrencePrimaryInpatientShortTerm,108),collapse=","))
  }

  if(length(conceptIdsConditionEraAnyTimePrior)){
    label <- c(label, "ConditionEraAnyTimePrior")
    analysisId <- c(analysisId, 201)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraAnyTimePrior,201),collapse=","))
  }

  if(length(conceptIdsConditionEraLongTerm)){
    label <- c(label, "ConditionEraLongTerm")
    analysisId <- c(analysisId, 202)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraLongTerm,202),collapse=","))
  }

  if(length(conceptIdsConditionEraMediumTerm)){
    label <- c(label, "ConditionEraMediumTerm")
    analysisId <- c(analysisId, 203)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraMediumTerm,203),collapse=","))
  }

  if(length(conceptIdsConditionEraShortTerm)){
    label <- c(label, "ConditionEraShortTerm")
    analysisId <- c(analysisId, 204)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraShortTerm,204),collapse=","))
  }
  if(length(conceptIdsConditionEraOverlapping)){
    label <- c(label, "ConditionEraOverlapping")
    analysisId <- c(analysisId, 205)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraOverlapping,205),collapse=","))
  }
  if(length(conceptIdsConditionEraStartLongTerm)){
    label <- c(label, "ConditionEraStartLongTerm")
    analysisId <- c(analysisId, 206)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraStartLongTerm,206),collapse=","))
  }
  if(length(conceptIdsConditionEraStartMediumTerm)){
    label <- c(label, "ConditionEraStartMediumTerm")
    analysisId <- c(analysisId, 207)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraStartMediumTerm,207),collapse=","))
  }
  if(length(conceptIdsConditionEraStartShortTerm)){
    label <- c(label, "ConditionEraStartShortTerm")
    analysisId <- c(analysisId, 208)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraStartShortTerm,208),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraAnyTimePrior)){
    label <- c(label, "ConditionGroupEraAnyTimePrior")
    analysisId <- c(analysisId, 209)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraAnyTimePrior,209),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraLongTerm)){
    label <- c(label, "ConditionGroupEraLongTerm")
    analysisId <- c(analysisId, 210)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraLongTerm,210),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraMediumTerm)){
    label <- c(label, "ConditionGroupEraMediumTerm")
    analysisId <- c(analysisId, 211)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraMediumTerm,211),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraShortTerm)){
    label <- c(label, "ConditionGroupEraShortTerm")
    analysisId <- c(analysisId, 212)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraShortTerm,212),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraOverlapping)){
    label <- c(label, "ConditionGroupEraOverlapping")
    analysisId <- c(analysisId, 213)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraOverlapping,213),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraStartLongTerm)){
    label <- c(label, "ConditionGroupEraStartLongTerm")
    analysisId <- c(analysisId, 214)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraStartLongTerm,214),collapse=","))
  }

  if(length(conceptIdsConditionGroupEraStartMediumTerm)){
    label <- c(label, "ConditionGroupEraStartMediumTerm")
    analysisId <- c(analysisId, 215)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraStartMediumTerm,215),collapse=","))
  }

  if(length(conceptIdsConditionGroupEraStartShortTerm)){
    label <- c(label, "ConditionGroupEraStartShortTerm")
    analysisId <- c(analysisId, 216)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraStartShortTerm,216),collapse=","))
  }

  if(length(conceptIdsDrugExposureAnyTimePrior)){
    label <- c(label, "DrugExposureAnyTimePrior")
    analysisId <- c(analysisId, 301)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugExposureAnyTimePrior,301),collapse=","))
  }

  if(length(conceptIdsDrugExposureLongTerm)){
    label <- c(label, "DrugExposureLongTerm")
    analysisId <- c(analysisId, 302)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugExposureLongTerm,302),collapse=","))
  }
  if(length(conceptIdsDrugExposureMediumTerm)){
    label <- c(label, "DrugExposureMediumTerm")
    analysisId <- c(analysisId, 303)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugExposureMediumTerm,303),collapse=","))
  }
  if(length(conceptIdsDrugExposureShortTerm)){
    label <- c(label, "DrugExposureShortTerm")
    analysisId <- c(analysisId, 304)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugExposureShortTerm,304),collapse=","))
  }
  if(length(conceptIdsDrugEraAnyTimePrior)){
    label <- c(label, "DrugEraAnyTimePrior")
    analysisId <- c(analysisId, 401)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraAnyTimePrior,401),collapse=","))
  }
  if(length(conceptIdsDrugEraLongTerm)){
    label <- c(label, "DrugEraLongTerm")
    analysisId <- c(analysisId, 402)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraLongTerm,402),collapse=","))
  }
  if(length(conceptIdsDrugEraMediumTerm)){
    label <- c(label, "DrugEraMediumTerm")
    analysisId <- c(analysisId, 403)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraMediumTerm,403),collapse=","))
  }

  if(length(conceptIdsDrugEraShortTerm)){
    label <- c(label, "DrugEraShortTerm")
    analysisId <- c(analysisId, 404)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraShortTerm,404),collapse=","))
  }

  if(length(conceptIdsDrugEraOverlapping)){
    label <- c(label, "DrugEraOverlapping")
    analysisId <- c(analysisId, 405)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraOverlapping,405),collapse=","))
  }

  if(length(conceptIdsDrugEraStartLongTerm)){
    label <- c(label, "DrugEraStartLongTerm")
    analysisId <- c(analysisId, 406)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraStartLongTerm,406),collapse=","))
  }

  if(length(conceptIdsDrugEraStartMediumTerm)){
    label <- c(label, "DrugEraStartMediumTerm")
    analysisId <- c(analysisId, 407)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraStartMediumTerm,407),collapse=","))
  }

  if(length(conceptIdsDrugEraStartShortTerm)){
    label <- c(label, "DrugEraStartShortTerm")
    analysisId <- c(analysisId, 408)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraStartShortTerm,408),collapse=","))
  }

  if(length(conceptIdsDrugGroupEraAnyTimePrior)){
    label <- c(label, "DrugGroupEraAnyTimePrior")
    analysisId <- c(analysisId, 409)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraAnyTimePrior,409),collapse=","))
  }
  if(length(conceptIdsDrugGroupEraLongTerm)){
    label <- c(label, "DrugGroupEraLongTerm")
    analysisId <- c(analysisId, 410)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraLongTerm,410),collapse=","))
  }

  if(length(conceptIdsDrugGroupEraMediumTerm)){
    label <- c(label, "DrugGroupEraMediumTerm")
    analysisId <- c(analysisId, 411)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraMediumTerm,411),collapse=","))
  }
  if(length(conceptIdsDrugGroupEraShortTerm)){
    label <- c(label, "DrugGroupEraShortTerm")
    analysisId <- c(analysisId, 412)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraShortTerm,412),collapse=","))
  }

  if(length(conceptIdsDrugGroupEraOverlapping)){
    label <- c(label, "DrugGroupEraOverlapping")
    analysisId <- c(analysisId, 413)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraOverlapping,413),collapse=","))
  }

  if(length(conceptIdsDrugGroupEraStartLongTerm)){
    label <- c(label, "DrugGroupEraStartLongTerm")
    analysisId <- c(analysisId, 414)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraStartLongTerm,414),collapse=","))
  }
  if(length(conceptIdsDrugGroupEraStartMediumTerm)){
    label <- c(label, "DrugGroupEraStartMediumTerm")
    analysisId <- c(analysisId, 415)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraStartMediumTerm,415),collapse=","))
  }
  if(length(conceptIdsDrugGroupEraStartShortTerm)){
    label <- c(label, "DrugGroupEraStartShortTerm")
    analysisId <- c(analysisId, 416)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraStartShortTerm,416),collapse=","))
  }

  if(length(conceptIdsProcedureOccurrenceAnyTimePrior)){
    label <- c(label, "ProcedureOccurrenceAnyTimePrior")
    analysisId <- c(analysisId, 501)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsProcedureOccurrenceAnyTimePrior,501),collapse=","))
  }
  if(length(conceptIdsProcedureOccurrenceLongTerm)){
    label <- c(label, "ProcedureOccurrenceLongTerm")
    analysisId <- c(analysisId, 502)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsProcedureOccurrenceLongTerm,502),collapse=","))
  }
  if(length(conceptIdsProcedureOccurrenceMediumTerm)){
    label <- c(label, "ProcedureOccurrenceMediumTerm")
    analysisId <- c(analysisId, 503)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsProcedureOccurrenceMediumTerm,503),collapse=","))
  }

  if(length(conceptIdsProcedureOccurrenceShortTerm)){
    label <- c(label, "ProcedureOccurrenceShortTerm")
    analysisId <- c(analysisId, 504)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsProcedureOccurrenceShortTerm,504),collapse=","))
  }

  if(length(conceptIdsDeviceExposureAnyTimePrior)){
    label <- c(label, "DeviceExposureAnyTimePrior")
    analysisId <- c(analysisId, 601)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDeviceExposureAnyTimePrior,601),collapse=","))
  }
  if(length(conceptIdsDeviceExposureLongTerm)){
    label <- c(label, "DeviceExposureLongTerm")
    analysisId <- c(analysisId, 602)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDeviceExposureLongTerm,602),collapse=","))
  }
  if(length(conceptIdsDeviceExposureMediumTerm)){
    label <- c(label, "DeviceExposureMediumTerm")
    analysisId <- c(analysisId, 603)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDeviceExposureMediumTerm,603),collapse=","))
  }

  if(length(conceptIdsDeviceExposureShortTerm)){
    label <- c(label, "DeviceExposureShortTerm")
    analysisId <- c(analysisId, 604)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDeviceExposureShortTerm,604),collapse=","))
  }

  if(length(conceptIdsMeasurementAnyTimePrior)){
    label <- c(label, "MeasurementAnyTimePrior")
    analysisId <- c(analysisId, 701)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementAnyTimePrior,701),collapse=","))
  }

  if(length(conceptIdsMeasurementLongTerm)){
    label <- c(label, "MeasurementLongTerm")
    analysisId <- c(analysisId, 702)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementLongTerm,702),collapse=","))
  }
  if(length(conceptIdsMeasurementMediumTerm)){
    label <- c(label, "MeasurementMediumTerm")
    analysisId <- c(analysisId, 703)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementMediumTerm,703),collapse=","))
  }

  if(length(conceptIdsMeasurementShortTerm)){
    label <- c(label, "MeasurementShortTerm")
    analysisId <- c(analysisId, 704)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementShortTerm,704),collapse=","))
  }

  if(length(conceptIdsMeasurementValueAnyTimePrior)){
    label <- c(label, "MeasurementValueAnyTimePrior")
    analysisId <- c(analysisId, 705)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementValueAnyTimePrior,705),collapse=","))
  }

  if(length(conceptIdsMeasurementValueLongTerm)){
    label <- c(label, "MeasurementValueLongTerm")
    analysisId <- c(analysisId, 706)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementValueLongTerm,706),collapse=","))
  }
  if(length(conceptIdsMeasurementValueMediumTerm)){
    label <- c(label, "MeasurementValueMediumTerm")
    analysisId <- c(analysisId, 707)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementValueMediumTerm,707),collapse=","))
  }

  if(length(conceptIdsMeasurementValueShortTerm)){
    label <- c(label, "MeasurementValueShortTerm")
    analysisId <- c(analysisId, 708)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementValueShortTerm,708),collapse=","))
  }

  if(length(conceptIdsMeasurementRangeGroupAnyTimePrior)){
    label <- c(label, "MeasurementRangeGroupAnyTimePrior")
    analysisId <- c(analysisId, 709)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementRangeGroupAnyTimePrior,709),collapse=","))
  }

  if(length(conceptIdsMeasurementRangeGroupLongTerm)){
    label <- c(label, "MeasurementRangeGroupLongTerm")
    analysisId <- c(analysisId, 710)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementRangeGroupLongTerm,710),collapse=","))
  }
  if(length(conceptIdsMeasurementRangeGroupMediumTerm)){
    label <- c(label, "MeasurementRangeGroupMediumTerm")
    analysisId <- c(analysisId, 711)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementRangeGroupMediumTerm,711),collapse=","))
  }

  if(length(conceptIdsMeasurementRangeGroupShortTerm)){
    label <- c(label, "MeasurementRangeGroupShortTerm")
    analysisId <- c(analysisId, 712)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementRangeGroupShortTerm,712),collapse=","))
  }

  if(length(conceptIdsObservationAnyTimePrior)){
    label <- c(label, "ObservationAnyTimePrior")
    analysisId <- c(analysisId, 801)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsObservationAnyTimePrior,801),collapse=","))
  }
  if(length(conceptIdsObservationLongTerm)){
    label <- c(label, "ObservationLongTerm")
    analysisId <- c(analysisId, 802)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsObservationLongTerm,802),collapse=","))
  }
  if(length(conceptIdsObservationMediumTerm)){
    label <- c(label, "ObservationMediumTerm")
    analysisId <- c(analysisId, 803)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsObservationMediumTerm,803),collapse=","))
  }
  if(length(conceptIdsObservationShortTerm)){
    label <- c(label, "ObservationShortTerm")
    analysisId <- c(analysisId, 804)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsObservationShortTerm,804),collapse=","))
  }

  if(useCharlsonIndex){
    label <- c(label, "CharlsonIndex")
    analysisId <- c(analysisId, 901)
    covariateIds <- c(covariateIds, "")
  }
  if(useDcsi){
    label <- c(label, "Dcsi")
    analysisId <- c(analysisId, 902)
    covariateIds <- c(covariateIds, "")
  }
  if(useChads2){
    label <- c(label, "Chads2")
    analysisId <- c(analysisId, 903)
    covariateIds <- c(covariateIds, "")
  }
  if(useChads2Vasc){
    label <- c(label, "Chads2Vasc")
    analysisId <- c(analysisId, 904)
    covariateIds <- c(covariateIds, "")
  }
  if(useHfrs){
    label <- c(label, "Hfrs")
    analysisId <- c(analysisId, 926)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctConditionCountLongTerm){
    label <- c(label, "DistinctConditionCountLongTerm")
    analysisId <- c(analysisId, 905)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctConditionCountMediumTerm){
    label <- c(label, "DistinctConditionCountMediumTerm")
    analysisId <- c(analysisId, 906)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctConditionCountShortTerm){
    label <- c(label, "DistinctConditionCountShortTerm")
    analysisId <- c(analysisId, 907)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctIngredientCountLongTerm){
    label <- c(label, "DistinctIngredientCountLongTerm")
    analysisId <- c(analysisId, 908)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctIngredientCountMediumTerm){
    label <- c(label, "DistinctIngredientCountMediumTerm")
    analysisId <- c(analysisId, 909)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctIngredientCountShortTerm){
    label <- c(label, "DistinctIngredientCountShortTerm")
    analysisId <- c(analysisId, 910)
    covariateIds <- c(covariateIds, "")
  }

  if(useDistinctProcedureCountLongTerm){
    label <- c(label, "DistinctProcedureCountLongTerm")
    analysisId <- c(analysisId, 911)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctProcedureCountMediumTerm){
    label <- c(label, "DistinctProcedureCountMediumTerm")
    analysisId <- c(analysisId, 912)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctProcedureCountShortTerm){
    label <- c(label, "DistinctProcedureCountShortTerm")
    analysisId <- c(analysisId, 913)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctMeasurementCountLongTerm){
    label <- c(label, "DistinctMeasurementCountLongTerm")
    analysisId <- c(analysisId, 914)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctMeasurementCountMediumTerm){
    label <- c(label, "DistinctMeasurementCountMediumTerm")
    analysisId <- c(analysisId, 915)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctMeasurementCountShortTerm){
    label <- c(label, "DistinctMeasurementCountShortTerm")
    analysisId <- c(analysisId, 916)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctObservationCountLongTerm){
    label <- c(label, "DistinctObservationCountLongTerm")
    analysisId <- c(analysisId, 917)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctObservationCountMediumTerm){
    label <- c(label, "DistinctObservationCountMediumTerm")
    analysisId <- c(analysisId, 918)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctObservationCountShortTerm){
    label <- c(label, "DistinctObservationCountShortTerm")
    analysisId <- c(analysisId, 919)
    covariateIds <- c(covariateIds, "")
  }
  if(useVisitCountLongTerm){
    label <- c(label, "VisitCountLongTerm")
    analysisId <- c(analysisId, 920)
    covariateIds <- c(covariateIds, "")
  }
  if(useVisitCountMediumTerm){
    label <- c(label, "VisitCountMediumTerm")
    analysisId <- c(analysisId, 921)
    covariateIds <- c(covariateIds, "")
  }
  if(useVisitCountShortTerm){
    label <- c(label, "VisitCountShortTerm")
    analysisId <- c(analysisId, 922)
    covariateIds <- c(covariateIds, "")
  }

  if(useVisitConceptCountLongTerm){
    label <- c(label, "VisitConceptCountLongTerm")
    analysisId <- c(analysisId, 923)
    covariateIds <- c(covariateIds, "")
  }

  if(useVisitConceptCountMediumTerm){
    label <- c(label, "VisitConceptCountMediumTerm")
    analysisId <- c(analysisId, 924)
    covariateIds <- c(covariateIds, "")
  }

  if(useVisitConceptCountShortTerm){
    label <- c(label, "VisitConceptCountShortTerm")
    analysisId <- c(analysisId, 925)
    covariateIds <- c(covariateIds, "")
  }

  tableSpec <- data.frame(label = label,
                          analysisId = analysisId,
                          covariateIds = covariateIds,
                          stringsAsFactors = F)
  result <- list(tableSpec = tableSpec, covariateSetting = covariateSetting)
  class(result) <- "tableSpecification"

  return(result)
}

