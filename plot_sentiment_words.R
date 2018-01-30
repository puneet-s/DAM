########## Plot words that contribute sentiment #############
### input text as list of strings###############
### output: Display plot of words########################
function (list_of_strings)
{
  try(require(tidytext) || install.packages("tidytext"))
  try(require(tidyr)    || install.packages("tidyr"))
  try(require(dplyr)    || install.packages("dplyr"))
  try(require(ggplot2)  || install.packages("ggplot2"))
  require(tidytext)
  require(tidyr)
  require(dplyr)
  require(ggplot2)
  
  x = list_of_strings
  textdf = data_frame(text = x)
  bing_word_counts <- textdf %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  bing_word_counts %>%
    filter(n > 3) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Contribution to sentiment")
  
}
