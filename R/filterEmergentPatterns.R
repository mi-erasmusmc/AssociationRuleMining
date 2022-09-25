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
  # retain redundant sequences
  redundantSeq <- commonSequences %>% filter(difference < absoluteDifference)
  ParallelLogger::logInfo(paste("Identified", nrow(redundantSeq), "redundant sequences and will remove."))
  `%notin%` <- Negate(`%in%`)
  s11 <- subset(patternsObjectOne, labels(patternsObjectOne) %notin% redundantSeq$sequence)
  s22 <- subset(patternsObjectTwo, labels(patternsObjectTwo) %notin% redundantSeq$sequence)
  
  result <- list(s11, s22)
  
  return(result)
  
}
