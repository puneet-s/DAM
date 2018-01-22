Build_dtm_dtmidf = function (clean.text)
  
{
  
  library(dplyr)
  library(tidytext)
  require(topicmodels)
  
  
  x = clean.text
  text_df <- data_frame(line = 1:length(x), text = x)
  
  
  text_words <- text_df %>%
    unnest_tokens(word, text) %>%
    count(line, word) 
  
  text_words_idf <- text_words %>%
    bind_tf_idf(word, line, n)
  
  
  dtm <-text_words%>%
    cast_dtm(line, word, n)
  
  dtm_idf <- text_words_idf %>%
    cast_dtm(line, word, tf_idf)
  
  out = list (dtm, dtm_idf)
  return (out)
  
}
