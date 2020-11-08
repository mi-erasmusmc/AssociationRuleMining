getIdDataFrame <- function(inputFile){
  inputfile = read.delim(inputFile, header = FALSE)
  y <- inputfile %>%
    mutate(id = stringr::str_replace_all(V1, ".*SID: ", ""), 
           Seqs = stringr::str_replace_all(V1, ".#.*", "")) %>%
    select(Seqs, id) %>%
    separate_rows(id, sep = " ") %>%
    mutate(val=1) %>% 
    pivot_wider(names_from = Seqs, values_from = val) %>% 
    mutate(across(-id, ~replace_na(.x, 0)))%>%
    arrange(as.numeric(id))
  
  y$id <- as.numeric(y$id) + 1  # Adding one to match Java's and R's enum type

  return(y)
}
