getDatabaseSummaryForFrequentPatterns <- function(data = data){
  # input data should be the output of getInputFileForFrequentPatterns
  
  itemFrequency <- data %>% 
    group_by(covariateLabel) %>%
    summarise('itemFrequency' = n())
  
  sequenceLength <- data %>%
    group_by(rowId) %>%
    summarise("sequenceLength" = n())
  
  eventLength <- data %>%
    group_by(rowId, eventId) %>%
    summarise("eventLength" = n()) %>%
    ungroup() 
#    %>% 
#    group_by(rowId) %>%
#    summarise("meanEventLengthPerSequence" = mean(eventLength), 
#              "maxEventLengthPerSequence" = max(eventLength), 
#              "minEventLengthPerSequence" = min(eventLength))
  
  vec1 <- summarise(itemFrequency, 
                    'Total items' = sum(itemFrequency),
                    'Average item frequency' = mean(itemFrequency), 
                    'Standard deviation of item freq.' = sd(itemFrequency), 
                    'Median item frequency' = median(itemFrequency),
                    '25th percentile' = quantile(itemFrequency)[[2]],
                    '75th percentile' = quantile(itemFrequency)[[4]],
                    'Max item frequency' = max(itemFrequency), 
                    'Min item frequency' = min(itemFrequency), 
                    'Distinct items' = length(unique(covariateLabel)))
  
  vec2 <- sequenceLength %>%
    summarise('Total sequences' = length(unique(rowId)), 
              "Average events per sequence" = mean(sequenceLength),
              'Standard deviation of events per sequence' = sd(sequenceLength),
              'Median length of sequences' = median(sequenceLength),
              '25th percentile' = quantile(sequenceLength)[[2]],
              '75th percentile' = quantile(sequenceLength)[[4]],
              "Max sequence length" = max(sequenceLength), 
              "Min sequence length" = min(sequenceLength)
    )
  
 vec3 <- eventLength %>%
   summarise("Average event size" = mean(eventLength),
             'Standard deviation of event size' = sd(eventLength),
             "Median event length" = median(eventLength),
             "5th percentile" = quantile(eventLength)[[2]],
             "75th percentile" = quantile(eventLength)[[4]],
             "Max event length" = max(eventLength), 
             "Min event length" = min(eventLength))
  
  return(list(t(vec1), t(vec2), t(vec3)
              ))
}