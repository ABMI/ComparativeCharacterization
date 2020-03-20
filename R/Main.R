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

#' Execute the Study
comparativeCharacterization <- function(connectionDetails,
                                        cdmDatabaseSchema,
                                        cohortDatabaseSchema = cdmDatabaseSchema,
                                        cohortTable = "cohort",
                                        oracleTempSchema = NULL,
                                        outputFolder,
                                        minCellCount = 0,
                                        targetCohortId,
                                        #stratifyingSetting = NULL,
                                        outcomeCohortIds = NULL,
                                        tableSpecification = tableSpecification,
                                        sampleSize = NULL){
  if(is.null(outcomeCohortIds)){
    covData <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
                                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                                     cohortTable = cohortTable,
                                                     cohortId = targetCohortId,
                                                     rowIdField = "subject_id",
                                                     covariateSettings = tableSpecification$covariateSetting,
                                                     aggregated = TRUE)
    table1 <- FeatureExtraction::createTable1(covData,
                                              specifications  =tableSpecification$tableSpec,
                                              output = "two column",
                                              percentDigits = 1,
                                              valueDigits = 1,
                                              stdDiffDigits = 2)

    exportFolder <- file.path(outputFolder, "export", "table1")
    if (!file.exists(exportFolder)) {
      dir.create(exportFolder, recursive = TRUE)
    }

    if(class(table1)=="list"){
      write.csv(table1[[1]],file.path(exportFolder,sprintf("t%d_base_char_binary.csv", targetCohortId)))
      write.csv(table1[[2]],file.path(exportFolder,sprintf("t%d_base_char_cont.csv", targetCohortId)))
    }else{
      write.csv(table1,file.path(exportFolder,sprintf("t%d_base_char.csv", targetCohortId)))
    }

  }
}


