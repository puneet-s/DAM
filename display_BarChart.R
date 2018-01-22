#--------------------------------------------------------#
## Step :     # Display BarChart                         #
#--------------------------------------------------------#
display_barchart <- function(dtm)
  
{        
  require(ggplot2)
  a0 = apply(dtm, 2, sum)
  a1 = order(a0, decreasing = TRUE)
  tsum = a0[a1]
  
  # plot barchart for top tokens
  test = as.data.frame(round(tsum[1:15],0))
  
  # windows()  # New plot window
  require(ggplot2)
  ggplot(test, aes(x = rownames(test), y = test)) + 
    geom_bar(stat = "identity", fill = "Blue") +
    geom_text(aes(label = test), vjust= -0.20) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
} # func ends