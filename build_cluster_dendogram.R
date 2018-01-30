display_dendogram = function(data_frame)
{
  mydata = data_frame
  d <- dist(mydata, method = "euclidean") # distance matrix
  fit <- hclust(d, method = "ward.D") 
  
  plot(fit) # display dendogram
  
}
