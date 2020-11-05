getNamesFromCovariateId <- function(data, covariateDataObject){
  dataframe=data
  cDO = covariateDataObject
  names.df <- as.data.frame(cDO$covariateRef)
  names.df$covariateId <- as.character(names.df$covariateId)
  names.df$covariateLabel <-  stringr::str_replace(names.df$covariateName, ".*: ", "")
  names.df$covariateLabel2 <- stringr::str_replace_all(names.df$covariateLabel, " ", "_")
  names.df <-names.df %>% 
    arrange(conceptId) %>%
    mutate(SPMFinputId = match(covariateLabel, unique(covariateLabel)), 
           SPMFnameID = paste("@ITEM", SPMFinputId, covariateLabel2, sep = "="))
  write.table(x = "@CONVERTED_FROM_TEXT", "data/processed/inputForSPMF/eunomia_MI.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
  write.table(names.df$SPMFnameID, "data/processed/inputForSPMF/eunomia_MI.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  dataframe$covariateId <- as.character(dataframe$covariateId)
  df_input <- dplyr::inner_join(dataframe, names.df, by = "covariateId")
  # Filtering useful variables
  #  df_input2 <- dplyr::select(df_input, c(rowId, eventId, SIZE, covariateLabel))
  #  return(df_input2)
  return(df_input)
}