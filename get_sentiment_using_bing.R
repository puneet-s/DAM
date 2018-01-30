########## Finding sentiment of document using bing #############
### Input text as list of strings#####
### Output 4 col tibble: sentiment, index, n, method #########
function (list_of_strings)
{
  try(require(tidytext) || install.packages("tidytext"))
  try(require(tidyr)    || install.packages("tidyr"))
  try(require(dplyr)    || install.packages("dplyr"))
  require(tidytext)
  require(tidyr)
  require(dplyr)
  
  x = list_of_strings
  textdf = data_frame(text = x)  
  senti.bing = textdf %>%
    mutate(linenumber = row_number()) %>%   # build line num variable
    ungroup() %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment, index = linenumber %/% 1, sort = FALSE) %>%
    mutate(method = "bing")    # creates a column with method name
  
  return(senti.bing)
  
}
