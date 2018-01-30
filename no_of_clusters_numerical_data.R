function(data_frame)
{
  
  mydata=data_frame
  set.seed(seed = 0000)   # set seed for reproducible work
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))  # wss is within group sum of squares
  
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,  centers = i)$withinss)  
  
  # checking model fit for 2 to 15 clusters
  # note use of kmeans() func
  
  plot(1:15, wss, type="b", 
       xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
}
