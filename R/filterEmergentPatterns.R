#' @importFrom arulesSequences %in%
#' @importFrom arulesSequences labels
#' @importFrom arulesSequences support
#' 
#' @export
filterCommonPatterns <- function(patternsObjectOne, 
                                 patternsObjectTwo, 
                                 absoluteDifference, 
                                 transactionsNegative, 
                                 transactionsPositive){
  
  if (absoluteDifference < 0 && absoluteDifference > 1 ) {
    stop("Absolute difference should be between 0 and 1 indicating the relative difference that should exist to include a common pattern in the final set.")
  }
  
  #commonPatterns <- arulesSequences::labels(patternsObjectOne[patternsObjectOne %in% patternsObjectTwo])
  commonPatterns <- patternsObjectOne[patternsObjectOne %in% patternsObjectTwo]
  ParallelLogger::logInfo(paste("Common patterns in both objects are", length(commonPatterns), "."))
  
  commonSequences <-  tibble(sequence = labels(commonPatterns))
  commonSequences$neg <- support(commonPatterns, transactionsNegative)
  commonSequences$pos <- arulesSequences::support(commonPatterns, transactionsPositive)
  commonSequences$difference <- abs(commonSequences$pos - commonSequences$neg)
  # retain redundant sequences
  redundantSeq <- commonSequences %>% filter(difference < absoluteDifference)
  `%notin%` <- Negate(`%in%`)
  s11 <- subset(patternsObjectOne, labels(patternsObjectOne) %notin% redundantSeq$sequence)
  s22 <- subset(patternsObjectTwo, labels(patternsObjectTwo) %notin% redundantSeq$sequence)
  
  result <- list(s11, s22)
  
  return(result)
  
}
