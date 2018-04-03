pairwise_intersect <- function(list_sets) {
  set_names <- names(list_sets)
  pairwise_matrix <- matrix(ncol=length(set_names), nrow=length(set_names))
  
  rownames(pairwise_matrix) <- set_names
  colnames(pairwise_matrix) <- set_names

  for(set_name_a in set_names){
    set_a <- list_sets[[set_name_a]]
    set_a <- unique(set_a[!is.na(set_a)])

    for(set_name_b in set_names){
      set_b <- list_sets[[set_name_b]]
      set_b <- unique(set_b[!is.na(set_b)])
      inter_count <- length(intersect(set_a, set_b))
      pairwise_matrix[set_name_a,set_name_b] <- as.integer(inter_count)
    }
  }
  return(pairwise_matrix)
}
