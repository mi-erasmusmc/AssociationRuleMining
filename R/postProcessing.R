#' Reducing length of Frequent Patterns
#' 
#' \code{reduceClosedPatterns} Removes repetitive concepts from Frequent Patterns.
#' 
#' It takes as input the tibble resulted from `runFrequentPatterns()` and removes repetitive concepts from the extracted patterns
#' 
#' @param data A tibble as returned from `runFrequentPatterns()`.
#' 
#' @export
# reduceClosedPatterns <- function(data){
#   res1 <- data %>%
#     dplyr::mutate(Seqs = stringr::str_replace_all(Sequence, pattern = " ", replacement = "")) %>%
#     dplyr::select(Seqs, Count, Support) %>%
#     dplyr::mutate(seq_id = row_number()) %>%
#     splitstackshape::cSplit(., "Seqs", sep = "=>", type.convert = FALSE,direction = "long") %>%
#     dplyr::group_by(seq_id) %>%
#     dplyr::mutate(elem_seq_id = row_number(),
#            length = dplyr::n()) %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by(seq_id) %>%
#     dplyr::mutate(consecutive = sequence(rle(as.character(Seqs))$lengths)) %>%
#     dplyr::filter(consecutive == 1) %>%
#     dplyr::mutate(new_seq_length = n()) %>%
#     dplyr::ungroup()%>% 
#     dplyr::arrange(seq_id, elem_seq_id) %>% 
#     dplyr::group_by(seq_id) %>%
#     dplyr::mutate(new_whole_sequence = paste0(Seqs, collapse = "=>")) %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by(new_whole_sequence) %>%
#     dplyr::mutate(included_seqs = paste0(seq_id, collapse = ", ")) %>%
#     dplyr::slice_max(Count, with_ties = FALSE) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(new_whole_sequence, Count, Support, included_seqs, seq_id) %>%
#     dplyr::rename(Seqs = new_whole_sequence)
#   
#   return(res1)
# }

reduceClosedPatterns <- function(inputFile){
  inputfile = vroom::vroom(file = inputFile, col_names = FALSE, col_types = "c", trim_ws = TRUE, progress = TRUE, delim = "///" )
  
  res <-   inputfile %>% # part 1: preparing input 
    dplyr::mutate(rowId = gsub(x = X1, pattern = ".*SID: ", replacement = ""), 
                  Sequences = gsub(x = X1, pattern =".#.*", replacement = ""), 
                  Seq_id  = row_number(), 
                  Count = stringr::str_match(inputfile$X1, "#SUP: \\s*(.*?)\\s*#SID:")[,2],
                  Seqs = str_replace_all(Sequences, pattern = " ", replacement = "")) %>%
    #select(Seqs, Count, Support) %>%
    #mutate(Seq_id = row_number()) %>%
    cSplit(., "Seqs", sep = "-1", type.convert = FALSE,direction = "long") %>%
    dplyr::group_by(Seq_id) %>%
    dplyr::mutate(elem_seq_id = dplyr::row_number(),
                  length = dplyr::n()) %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(Seq_id) %>% # part 2: removing repeating concepts
    dplyr::mutate(consecutive = sequence(rle(as.character(Seqs))$lengths)) %>%
    dplyr::filter(consecutive == 1) %>%
    dplyr::mutate(new_seq_length = dplyr::n()) %>%
    dplyr::ungroup()%>% 
    dplyr::arrange(Seq_id, elem_seq_id) %>% # Part 3: Creating new sequences
    dplyr::group_by(Seq_id) %>%
    dplyr::mutate(new_whole_sequence = paste0(Seqs, collapse = "=>")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(new_whole_sequence) %>% # part 4: selecting rowIds
    dplyr::mutate(included_seqs = paste0(Seq_id, collapse = ", "), 
                  new_row_Ids = paste0(rowId, collapse = " ")) %>%
    dplyr::slice_max(Count, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(new_whole_sequence, Count, included_seqs, Seq_id, rowId, new_row_Ids) %>% # part5: preparing output
    tidyr::separate_rows(new_row_Ids, sep = " ") %>%
    dplyr::rename(Seqs = new_whole_sequence) %>%
    dplyr::group_by(Seqs, new_row_Ids) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Seqs) %>% # Adding new count
    dplyr::mutate(new_Count = dplyr::n())
  
  return(res)
  
}