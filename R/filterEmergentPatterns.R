#' @importFrom arulesSequences %in%
#' @importFrom arulesSequences labels
#' @importFrom arulesSequences support
#' 
#' @export
filterCommonPatterns <- function(patternsObjectOne, 
                                 patternsObjectTwo, 
                                 absoluteDifference, 
                                 transactionsNegative, 
                                 transactionsPositive, 
                                 keepDiscriminative){
  
  if (absoluteDifference < 0 && absoluteDifference > 1 ) {
    stop("Absolute difference should be between 0 and 1 indicating the relative difference that should exist to include a common pattern in the final set.")
  }
  
  #commonPatterns <- arulesSequences::labels(patternsObjectOne[patternsObjectOne %in% patternsObjectTwo])
  commonPatternsOne <- patternsObjectOne[patternsObjectOne %in% patternsObjectTwo]
  commonPatternsTwo <- patternsObjectTwo[patternsObjectTwo %in% patternsObjectOne]
  ParallelLogger::logInfo(paste("Common patterns in both objects are", length(commonPatternsOne), "."))
  
  # commonSequences <-  tibble(sequence = labels(commonPatterns))
  
  df1 <- as(commonPatternsOne, "data.frame") 
  df2 <- as(commonPatternsTwo, "data.frame")
  commonSequences <- merge(df1, df2, by = "sequence")
  # commonSequences$neg <- support(commonPatterns, transactionsNegative)
  # commonSequences$pos <- arulesSequences::support(commonPatterns, transactionsPositive)
  commonSequences$difference <- abs(commonSequences$support.x - commonSequences$support.y)
  # Instead of keeping perfectly discriminative patterns, which happens in this section:
  # retain redundant sequences
  redundantSequences <- commonSequences %>% dplyr::filter(difference < absoluteDifference)
  partialDiscriminativeSequences <- commonSequences %>% dplyr::filter(difference >= absoluteDifference)
  `%notin%` <- Negate(`%in%`)
  if (keepDiscriminative == FALSE){
  ParallelLogger::logInfo(paste0("Identified ", nrow(redundantSeq), " redundant sequences and will remove.\n Identified ", nrow(partialDiscriminativeSequences), " sequences with absolute difference greater or equal to ", absoluteDifference, "."))
  s11 <- subset(patternsObjectOne, labels(patternsObjectOne) %in% partialDiscriminativeSequences$sequence)
  s22 <- subset(patternsObjectTwo, labels(patternsObjectTwo) %in% partialDiscriminativeSequences$sequence)
  # we will keep only those patterns that appear in both groups but with different frequency in the two groups 
  } else {
    # we will keep only those patterns that appear with different frequency in the two groups + also those appearing in one group not in the other.
  # discriminativeSeq <- commonSequences %>% filter(difference >= absoluteDifference)
  ParallelLogger::logInfo(paste0("Identified ", nrow(redundantSequences), " redundant sequences with absolute support difference less than ", absoluteDifference, " . Will remove."))
  s11 <- subset(patternsObjectOne, labels(patternsObjectOne) %notin% redundantSequences$sequence)
  s22 <- subset(patternsObjectTwo, labels(patternsObjectTwo) %notin% redundantSequences$sequence)
  }
  result <- list(s11, s22)
  
  return(result)
  
}
