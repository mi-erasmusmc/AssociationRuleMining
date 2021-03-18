prepareResult <- function(data = data, plot = c("Sankey")) {
  if (plot == "Sankey") {
    resdf <- data %>%
      mutate(Seqs = str_replace_all(Sequence, pattern = " ", replacement = "")) %>%
      select(Seqs, Count, Support) %>%
      mutate(seq_id = row_number()) %>%
      cSplit(., "Seqs", sep = "=>", type.convert = FALSE,direction = "long") %>%
      group_by(seq_id) %>%
      mutate(elem_seq_id = row_number(),
             length = n()) %>%
      ungroup() 
    
    seqIdvec <- resdf %>%
      filter(length != 1) %>%
      group_by(Seqs, elem_seq_id) %>%
      slice_max(length, with_ties = FALSE) %>%
      ungroup() %>%
      select(seq_id)
    
  } else {
    stop("Plot not supported at the moment. Please choose one of the available graphs.")
  }
  res <- list(resdf, seqIdvec)
  return(res)
}


extractFPs <- function(data = data, seqId = seqId){
  final_seqs <- seqId
  final_seqs_df <- data %>%
    filter(seq_id %in% final_seqs) %>%
    group_by(seq_id) %>%
    mutate(new_seq = paste0(Seqs, collapse = "=>")) %>%
    select(new_seq)
  
  return(final_seqs_df)
}

countSequences <- function(inputFile, finalSeqs, objectWithIds){
  iddf <- getIdDataFrame(inputFile = inputFile, objectWithIds = objectWithIds)
  
  iddf_long <- iddf %>%
    pivot_longer(!rowId, names_to = "Sequences", values_to = "count") %>%
    filter(count == 1) %>%
    mutate(Seqs = str_replace_all(Sequences, pattern = " ", replacement = "")) %>%
    mutate(Seqs = str_replace_all(Seqs, pattern = "_", replacement = "")) %>%
    select(rowId, Seqs, count) %>%
    filter(Seqs %in% finalSeqs) %>%
    cSplit(., "Seqs", sep = "=>", type.convert = FALSE,direction = "wide") %>%
    select(- c(count))
  
  iddf_2 <- as.data.frame(iddf_long)
  
  iddf_2[-1] <- t(apply(iddf_2[-1], 1, FUN = function(x) {
    i1 <- range(which(!is.na(x)))
    i2 <- seq_along(x)
    i3 <- which(!i2 %in% i1[1]:i1[2])
    c(x[i3], x[setdiff(i2, i3)])
  }))
  
  iddf.plot <- data.frame()
  
  for (i in 3:ncol(iddf_2)) {
    
    ord.cache <- iddf_2 %>%
      group_by(iddf_2[, i-1], iddf_2[, i]) %>%
      summarise(n = length(unique(rowId))) %>%
      ungroup
    
    ord.cache <- ord.cache %>% drop_na()
    
    colnames(ord.cache)[1:2] <- c('from', 'to')
    
    # adding tags to carts
    ord.cache$from <- paste(ord.cache$from, '(', i-1, ')', sep='')
    ord.cache$to <- paste(ord.cache$to, '(', i, ')', sep='')
    
    iddf.plot <- rbind(iddf.plot, ord.cache)
  }
  
  return(iddf.plot)
}

plot.sankey <- function(data = data, plot = c("Sankey"), inputFile, objectWithIds ){
  prep <- prepareResult(data, plot = "Sankey")
  prep2 <- extractFPs(prep[[1]], prep[[2]]$seq_id)
  prep3 <- countSequences(inputFile = inputFile, objectWithIds = objectWithIds, finalSeqs = prep2$new_seq)
  return(prep3)
} 