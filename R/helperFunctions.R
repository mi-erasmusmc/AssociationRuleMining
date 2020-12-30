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
  
  inputfile = read.delim(inputFile, header = FALSE, blank.lines.skip = TRUE)
  rowIds <- objectWithIds$rowId
  
  if (any(stringi::stri_detect_fixed(inputfile$V1, "#SID", max_count = 1)) == FALSE) {
    stop("The input file provided does not contain sequence IDs")
  }
  
  covariateTidy <- inputfile %>%
    dplyr::mutate(rowId = stringr::str_replace_all(V1, ".*SID: ", ""), 
                  Sequences = stringr::str_replace_all(V1, ".#.*", "")) %>%
    dplyr::select(Sequences, rowId) %>%
    tidyr::separate_rows(rowId, sep = " ") %>%
    dplyr::mutate(covariateValue = 1) %>%
    dplyr::arrange(as.numeric(rowId))  
  
  # Making rowId numeric
  covariateTidy$rowId <- as.numeric(covariateTidy$rowId)
  
  # Fixing names of sequences
  covariateTidy$Sequences <- stringr::str_replace_all(covariateTidy$Sequences, "-1", "=>")
  covariateTidy$Sequences <- stringr::str_replace_all(covariateTidy$Sequences, "=>$", "")
  
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
