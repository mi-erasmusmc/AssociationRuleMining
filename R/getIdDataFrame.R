getIdDataFrame <- function(inputFile){
  inputfile = read.delim(inputFile, header = FALSE)
  y <- inputfile %>%
    dplyr::mutate(id = stringr::str_replace_all(V1, ".*SID: ", ""), 
                  Seqs = stringr::str_replace_all(V1, ".#.*", "")) %>%
    dplyr::select(Seqs, id) %>%
    tidyr::separate_rows(id, sep = " ") %>%
    dplyr::mutate(val=1) %>% 
    tidyr::pivot_wider(names_from = Seqs, values_from = val) %>% 
    dplyr::mutate(across(-id, ~replace_na(.x, 0)))%>%
    dplyr::arrange(as.numeric(id))
  
  y$id <- as.numeric(y$id) + 1  # Adding one to match Java's and R's enum type

  return(y)
}
