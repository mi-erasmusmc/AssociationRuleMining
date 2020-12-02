getIdDataFrame <- function(inputFile){
  inputfile = read.delim(inputFile, header = FALSE)
  
  if (any(stringi::stri_detect_fixed(inputfile$V1, "#SID", max_count = 1)) == FALSE) {
    stop("The input file provided does not contain sequence IDs")
  }
  
  y <- inputfile %>%
    dplyr::mutate(id = stringr::str_replace_all(V1, ".*SID: ", ""), 
                  Seqs = stringr::str_replace_all(V1, ".#.*", "")) %>%
    dplyr::select(Seqs, id) %>%
    tidyr::separate_rows(id, sep = " ") %>%
    dplyr::mutate(val=1) %>% 
    tidyr::pivot_wider(names_from = Seqs, values_from = val) %>% 
    dplyr::mutate(dplyr::across(-id, ~tidyr::replace_na(.x, 0))) %>%
    dplyr::arrange(as.numeric(id)) #%>%
    #dplyr::mutate(across(-id, ~ifelse(.x==1, TRUE,FALSE)))
  
  y$id <- as.numeric(y$id) + 1  # Adding one to match Java's and R's enum type
  names(y) <- stringr::str_replace_all(names(y), "-1", "=>")
  names(y) <- stringr::str_replace_all(names(y), "=>$", "")

  return(y)
}
