getNamesFromCovariateId <- function(data, covariateDataObject, fileToSave){
  cDO = covariateDataObject
  names.df <- cDO$covariateRef %>% dplyr::collect()
  names.df$covariateId <- as.character(names.df$covariateId)
  names.df$covariateLabel <-  stringr::str_replace(names.df$covariateName, ".*: ", "")
  names.df$covariateLabel2 <- stringr::str_replace_all(names.df$covariateLabel, " ", "_")
  names.df <-names.df %>% 
    arrange(conceptId) %>%
    mutate(SPMFinputId = match(covariateLabel, unique(covariateLabel)), 
           SPMFnameID = paste("@ITEM", SPMFinputId, covariateLabel2, sep = "="))
  write.table(x = "@CONVERTED_FROM_TEXT", paste(fileToSave), quote = FALSE, row.names = FALSE, col.names = FALSE)
  write.table(names.df$SPMFnameID, paste(fileToSave), quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  data$covariateId <- as.character(data$covariateId)
  data2 <- tidyr::separate_rows(data, covariateId)
  df_input <- dplyr::inner_join(data2, names.df, by = "covariateId")
  
  SPMFrowId <- data.frame(trueId = unique(data2$rowId)) %>%
    dplyr::arrange(trueId) %>%
    dplyr::mutate(SPMFrowId = dplyr::row_number() - 1)
  
  df_input <- dplyr::inner_join(df_input, SPMFrowId, by = c("rowId" = "trueId")) %>%
    dplyr::arrange(rowId)
  # Filtering useful variables
  #  df_input2 <- dplyr::select(df_input, c(rowId, eventId, SIZE, covariateLabel))
  #  return(df_input2)
  #row_id <- unique(df_input$rowId)
  #result <- list(df_input, row_id)
  #return(result)
  return(df_input)
}

# getNamesFromCovariateId <- function(data, covariateDataObject, fileToSave){
#   # cDO = covariateDataObject
#   names.df <- as_tibble(covariateDataObject$covariateRef) %>%
#     dplyr::mutate(covariateId = as.character(covariateId), 
#                   covariateLabel = stringr::str_replace(names.df$covariateName, ".*: ", ""), 
#                   covariateLabel2 = stringr::str_replace_all(names.df$covariateLabel, " ", "_")) %>%
#     dplyr::arrange(conceptId) %>%
#     dplyr::mutate(SPMFinputId = match(covariateLabel, unique(covariateLabel)), 
#                   SPMFnameID = paste("@ITEM", SPMFinputId, covariateLabel2, sep = "=")) 
#   # names.df$covariateId <- as.character(names.df$covariateId)
#   # names.df$covariateLabel <-  stringr::str_replace(names.df$covariateName, ".*: ", "")
#   # names.df$covariateLabel2 <- stringr::str_replace_all(names.df$covariateLabel, " ", "_")
#   # names.df <-names.df %>% 
#   #   arrange(conceptId) %>%
#   #   mutate(SPMFinputId = match(covariateLabel, unique(covariateLabel)), 
#   #          SPMFnameID = paste("@ITEM", SPMFinputId, covariateLabel2, sep = "="))
#   write.table(x = "@CONVERTED_FROM_TEXT", paste(fileToSave), quote = FALSE, row.names = FALSE, col.names = FALSE)
#   write.table(names.df$SPMFnameID, paste(fileToSave), quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
#   
#   # data$covariateId <- as.character(data$covariateId)
#   # data <- data %>%
#   #   dplyr::mutate(covariateId = as.character(covariateId))
#     
#   data2 <- tidyr::separate_rows(data, covariateId)
#   df_input <- dplyr::inner_join(data2, names.df, by = "covariateId")
#   
#   SPMFrowId <- data.frame(trueId = unique(data2$rowId)) %>%
#     dplyr::arrange(trueId) %>%
#     dplyr::mutate(SPMFrowId = dplyr::row_number() - 1)
#   
#   df_input <- dplyr::inner_join(df_input, SPMFrowId, by = c("rowId" = "trueId")) %>%
#     dplyr::arrange(rowId)
#   # Filtering useful variables
#   #  df_input2 <- dplyr::select(df_input, c(rowId, eventId, SIZE, covariateLabel))
#   #  return(df_input2)
#   #row_id <- unique(df_input$rowId)
#   #result <- list(df_input, row_id)
#   #return(result)
#   return(df_input)
# }
