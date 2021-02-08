getUniqueId <- function(Names, idstaken, idrange=NULL){
  #This functioncreates a unique(random) id for a covariate 
  #It is borrowed from Tom Seinen's package Triton 
  ##ParallelLogger::logInfo("\t\tCreating unique covariate ids")
  Names <- as.data.frame(Names)
  colnames(Names) <- "name"
  uniqueNames <- as.data.frame(unique(Names))
  colnames(uniqueNames) <- "name"
  ## generate the ids
  if(is.null(idrange)){idrange <- c(1, 2147482)}
  uniqueNames$ids <- as.integer(paste0((as.integer(runif(n = nrow(uniqueNames), min = idrange[1], max = idrange[2]))), 999))
  ## change the ids if duplicated or taken
  while((length(uniqueNames$ids[duplicated(uniqueNames$ids)])>0) || (!all(is.na(match(uniqueNames$ids,idstaken))))) {
    dubs <- duplicated(uniqueNames$ids) # duplicated ids
    taken <- !is.na(match(uniqueNames$ids,idstaken)) # ids that are already taken
    both <- dubs | taken # combine both
    uniqueNames$ids[both] <- as.integer(paste0((as.integer(runif(n = length(both[both]), min = 0, max = 2147482))), 999)) # create new id for those
  }
  Names <- dplyr::left_join(Names, uniqueNames, by = "name")
  return(Names$ids)
}


toCovariateData <- function(inputFile, objectWithIds){
  
  inputfile = vroom::vroom(file = inputFile, col_names = FALSE, trim_ws = TRUE, progress = TRUE)
  rowIds <- objectWithIds$rowId
  
  if (any(stringi::stri_detect_fixed(inputfile$V1, "#SID", max_count = 1)) == FALSE) {
    stop("The input file provided does not contain sequence IDs")
  }
  
  covariateTidy <- inputfile %>%
    dplyr::mutate(rowId = gsub(x = X1, pattern = ".*SID: ", replacement = ""), 
                  Sequences = gsub(x = X1, pattern =".#.*", replacement = "")) %>%
    dplyr::select(Sequences, rowId) %>%
    tidyr::separate_rows(rowId, sep = " ") %>%
    dplyr::mutate(covariateValue = 1) %>%
    dplyr::arrange(as.numeric(rowId))  
  
  # Making rowId numeric
  covariateTidy$rowId <- as.numeric(covariateTidy$rowId)
  
  # Fixing names of sequences
  covariateTidy$Sequences <- gsub(x = covariateTidy$Sequences, pattern = "-1", replacement = "=>")
  covariateTidy$Sequences <- gsub(x = covariateTidy$Sequences,pattern = "=>$", replacement = "")
  
  # Constructing unique covariate Ids
  uniqueSeqs <- unique(covariateTidy$Sequences)
  uniqueCovariates <- data.frame(covariateName = uniqueSeqs) 
  uniqueCovariateIds <- uniqueCovariates %>%
    dplyr::mutate(covariateId = as.numeric(getUniqueId(Names = covariateName, idstaken = NULL)))
  
  # Assign the true patient id to rowId
  
  #trueRowIds <- as.data.frame(unique(objectWithIds$rowId))
  #colnames(trueRowIds) <- "trueRowIds"
  
  #trueRowIds <- trueRowIds %>%
  #  dplyr::mutate(row_no = dplyr::row_number(), 
  #                java_row_id = row_no - 1)
  
  trueRowIds <- objectWithIds %>%
    select("rowId", "SPMFrowId") 
  
  trueIdDf <- data.frame("rowId" = unique(trueRowIds$rowId),
                         "SPMFrowId" = unique(trueRowIds$SPMFrowId))
  
  
 
  
  #outputRowIds <- as.data.frame(unique(covariateTidy$rowId))
 # colnames(outputRowIds) <- "outputRowIds"
  #idData <- dplyr::bind_cols(outputRowIds, trueRowIds) #%>%
    #dplyr::rename("outputRowIds" = ...1, 
    #              "trueRowIds" = ...2) 
  
  
  # include unique ids in the data
  covariateDataFp <- dplyr::left_join(x = covariateTidy, y = uniqueCovariateIds, by = c("Sequences" = "covariateName"))
  
  # include true row ids
  covariateDataFp <- dplyr::left_join(x = covariateDataFp, y= trueIdDf, by = c("rowId" = "SPMFrowId"))
  
  # Constructing covariateData's object $covariates
  covariates <- covariateDataFp %>%
    dplyr::select(rowId, covariateId, covariateValue) #%>%
   # dplyr::rename("rowId" = "trueRowIds")
  
  # Constructing covariateData's object $covariateRef
  covariateRef <- uniqueCovariateIds %>%
    dplyr::mutate(analysisId = 999, 
                  conceptId = 0)
  
  # Constructing covariateData's object $analysisRef
  analysisRef <- data.frame(analysisId = 999, 
                            analysisName = "FrequentPatterns", 
                            domainId = "FP",
                            startDay = 0, 
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y",
                            stringsAsFactors = TRUE)
  
  metadata <- list()
  result <- Andromeda::andromeda(covariates = covariates, 
                                 covariateRef = covariateRef, 
                                 analysisRef = analysisRef)
  attr(result, "metadata") <- metadata
  class(result) <- "CovariateData"
  return(result)
}

appendCovariateData<- function(tempCovariateData,covariateData){
  ##==## appends covariate objects ##==##
  if (is.null(covariateData)) {
    covariateData <- tempCovariateData
  } else {
    if (hasData(covariateData$covariates)) {
      if (hasData(tempCovariateData$covariates)) {
        Andromeda::appendToTable(covariateData$covariates, tempCovariateData$covariates)
      }
    } else if (hasData(tempCovariateData$covariates)) {
      covariateData$covariates <- tempCovariateData$covariates
    }
    if (hasData(covariateData$covariatesContinuous)) {
      if (hasData(tempCovariateData$covariatesContinuous)) {
        Andromeda::appendToTable(covariateData$covariatesContinuous, tempCovariateData$covariatesContinuous)
      } else if (hasData(tempCovariateData$covariatesContinuous)) {
        covariateData$covariatesContinuous <- tempCovariateData$covariatesContinuous
      }
    }
    Andromeda::appendToTable(covariateData$covariateRef, tempCovariateData$covariateRef)
    Andromeda::appendToTable(covariateData$analysisRef, tempCovariateData$analysisRef)
    for (name in names(attr(tempCovariateData, "metaData"))) {
      if (is.null(attr(covariateData, "metaData")[name])) {
        attr(covariateData, "metaData")[[name]] <- attr(tempCovariateData, "metaData")[[name]]
      } else {
        attr(covariateData, "metaData")[[name]] <- list(attr(covariateData, "metaData")[[name]],
                                                        attr(tempCovariateData, "metaData")[[name]])
      }
    }
  }
  return(covariateData)
}

hasData <- function(data) {
  ##==## checks if data has data ##==##
  return(!is.null(data) && (data %>% count() %>% pull()) > 0)
}

