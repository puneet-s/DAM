replace_bigram = function(corpus, min_freq = 2){  # corpus has 1 unnamed character colm
  
  library(tidyverse)
  library(tidytext)
  library(stringr)
  
  # first filter out stopwords - c("of ", "the ", " and").
  corpus_df <- corpus %>% data_frame() %>% rename(text=".") %>%
    mutate( text = str_replace_all(text, " of ", " "),
            text = str_replace_all(text, " and ", " "),
            text = str_replace_all(text, "[Tt]he", " "),
            text = str_replace_all(text, "\\\\s+", "\\s"))
  
  textdf = data.frame(docID=seq(1:nrow(corpus_df)), text=corpus, stringsAsFactors=FALSE)
  
  # Unnesting bigrams
  a0 = textdf %>% 	
    # bigram-tokenize, count and filter by freq
    unnest_tokens(ngram, text, token = "ngrams", n = 2) 
  head(a0)
  
  # creating frequent bigrams for replacement
  a1 = a0 %>% 
    count(ngram, sort=TRUE) %>% filter(n >= min_freq) %>% 
    separate(ngram, c("word1", "word2"), sep=" ", remove=FALSE) %>% 
    
    # drop all stopwords in the bigrams of interest
    dplyr::filter(!word1 %in% stop_words$word) %>%
    dplyr::filter(!word2 %in% stop_words$word) %>%
    
    unite(bigram1, c("word1", "word2"), sep="_")	 %>% 
    dplyr::select(ngram, bigram1)    # dplyr:: coz MASS also has select()
  a1
  
  # merging the 2 above dfs
  a2 = left_join(a0, a1, by=c("ngram" = "ngram")) %>%
    separate(ngram, c("word1", "word2"), sep=" ", remove=FALSE) %>%
    dplyr::select(-ngram) # %>% mutate(out_colm = bigram1)
  head(a2)
  
  ## using logical colms to solve repeats wala problem
  a400 = (is.na(a2$bigram1))
  # a400a = which(!a400)   # orig bigram locations
  a2$bigram1[a400] = a2$word1[a400]
  head(a2)
  
  a401 = which(!a400)  # orig bigram locations
  a402 = a401 + 1
  if (max(a402) > nrow(a2)) { a402[length(a402)] = nrow(a2) }
  
  a403 = (a2$docID[a401] == a2$docID[a402])   # is bigram inside the document vs at its boundary?
  
  # what if there are consecutive bigrams?  
  a403a = (a403)*(!(a402 %in% a401))  # bigrams are inside docs and NOT consecutive
  
  a404 = a402*a403a
  a405 = a404[(a404 > 0)]  # these are the extra terms or repeats to be dropped.
  
  a2$bigram1[a405] = ""
  
  # use logical-colms to solve token-repeats in consecutive bigrams
  a403b = (a403)*(a402 %in% a401)  # consec bigrams inside docs, logical colm
  a404b = a402*a403b       
  a405b = a404b[(a404b > 0)]    # row_nums of consec, inside bigrams
  
  # subroutine to drop middle-wala repeating token
  a405c = a405b -1    # first bigram ka location
  newgram = a2$bigram1
  newgram[a405c] = paste(a2$word1[a405c], a2$word1[a405b], a2$word1[a405b+1], sep="_")
  newgram[a405b] = ""
  a2$bigram1 = newgram
  
  # using colm-logicals to solve last-word-dropoff wala problem
  a500 = a2$docID
  a501 = c(a500[2:length(a500)], a500[length(a500)])	
  a502 = which(a500 != a501)    # docID boundaries
  a503 = !(a502 %in% a401)   # these are the ones to insert
  a503a = a502[a503]    
  
  a2$bigram1[a503a] = paste(a2$bigram1[a503a], a2$word2[a503a])
  
  # rebuilding corpus, now at doc layer
  doc_corpus = data.frame(docID = numeric(), text = character(), stringsAsFactors=FALSE)
  a201 = unique(a2$docID)
  
  for (i2 in a201){
    a200 = a2[a2$docID == i2,] 	
    doc_corpus[i2, 1] = a200$docID[1]	
    doc_corpus[i2, 2] = str_c(a200$bigram1, collapse=" ")
    
    if (i2 %% 1000 == 0) {cat(i2, " docs processed\n")}
  }    # i2 ends
  
  return(doc_corpus) }
