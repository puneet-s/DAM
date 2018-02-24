DTM_2_AdjMatrix = function(patent_dtm)
{
  # Takes 300 most common terms
  # Retuens 80*80 Adjacency Matrix
  terms_in_dtm <- apply(patent_dtm, 2, sum)
  which_terms_frequent <- order(terms_in_dtm, decreasing = TRUE)
  tsum <- terms_in_dtm[which_terms_frequent]
  frequencies_of_most_common_terms <- as.data.frame(round(tsum[1:300],0))
  list_of_common_terms <- attributes(frequencies_of_most_common_terms)$row.names
  dtm1 = as.matrix(patent_dtm)   # need it as a regular matrix for matrix ops like %*% to apply
  subset_of_dtm <- dtm1[TRUE, list_of_common_terms]
  adj.mat = t(subset_of_dtm) %*% subset_of_dtm    # making a square symmatric term-term matrix 
  diag(adj.mat) = 0     # no self-references. So diag is 0.
  a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
  adj.mat = as.matrix(adj.mat[a0[1:80], a0[1:80]])   # taking the top 50 rows and cols only
  return(adj.mat)
}
