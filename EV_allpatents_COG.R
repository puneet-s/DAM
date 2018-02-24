library(dplyr)
library(tidyr)
library(tidytext)
library(topicmodels)
library(tm)
library(igraph)
library(data.table)

#library("bigalgebra")
#library("irlba")


patent_data_company <- data.frame()
patent_data_company <- read.csv(file.choose())
company_name <- names(which.max(table(patent_data_company$organization)))
patent_data_company <- mutate(patent_data_company, company_name = company_name)
patent_data_company <- mutate(patent_data_company, year = year((as.Date(patent_data_company$date, origin="1899-12-30"))))
patent_data_company$patent_text <- as.character(patent_data_company$patent_text)
names(patent_data_company)[6] <- "text"
patent_data_company$text <- text_clean(patent_data_company$text)
patent_data_company$text <- patent_data_company$text$text

patent_data_company$text <- replace_bigram(patent_data_company$text)
patent_data_company$text <- patent_data_company$text$text
patent_data_company_words <- patent_data_company %>% unnest_tokens(word, text, to_lower = TRUE)
patent_data_company_words <- patent_data_company_words %>% anti_join(stop_words)

more_stop_words <- subset(patent_data_company_words, nchar(as.character(word)) <= 2)
patent_data_company_words <- patent_data_company_words %>% anti_join(more_stop_words)

#patent_stop_words <- data.frame(word = c('comprises', 'inserted', 'method', 'appratus', 'determined', 'includes', 'provided', 'driven', 'hub', 'response','invention', 'true','involved','offers', 'input','control','direction','surface'))
patent_stop_words <- data.frame(word = c('driven', 'hub', 'response','invention', 'true','involved','offers', 'input','control','direction','surface','set','operation','equal','object','step','amount','set','process','time','degree','result','ing','detected','detector','difference','system','device','connected','detecting','controlling','accordance','producing','target','detection','changing','setting','ation','output','steps','plurality','formed','arranged','start','position','configured','pre_ed'))

patent_data_company_words <- patent_data_company_words %>% anti_join(patent_stop_words)
patent_data_company_words <- patent_data_company_words %>% unite(document, company_name, patent_id, remove = FALSE)

patent_data_parts <- list()

#memory.limit()
#memory.limit(size=12000)

#patent_data_company_words <- mutate(patent_data_company_words, year = year((as.Date(patent_data_company_words$date, origin="1899-12-30"))))
patent_data_parts<-split(patent_data_company_words, cut(patent_data_company_words$year, c(1976, 1981, 1986, 1991, 1996, 1998, 2001, 2003, 2006, 2008, 2010, 2012, 2014,2017), include.lowest=TRUE))

#try_for_calc <- patent_data_parts[[12]]
#patent_dtm <- try_for_calc %>% count(document, word, sort = TRUE) %>% ungroup()%>% cast_dtm(document, word, n)
#dtm1 <- as.matrix(patent_dtm)

# require(ggplot2)
#terms_in_dtm <- apply(patent_dtm, 2, sum)
#head(terms_in_dtm)
#which_terms_frequent <- order(terms_in_dtm, decreasing = TRUE)
#tsum <- terms_in_dtm[which_terms_frequent]
#frequencies_of_most_common_terms <- as.data.frame(round(tsum[1:100],0))
#list_of_common_terms <- attributes(frequencies_of_most_common_terms)$row.names
#subset_of_dtm <- dtm1[TRUE, list_of_common_terms]


#> View(tsum)
#> test <- as.data.frame(round(tsum[1:15],0))
#> # windows()  # New plot window
#  >   require(ggplot2)
#> ggplot(test, aes(x = rownames(test), y = test)) + 
#  +     geom_bar(stat = "identity", fill = "Blue") +
#  +     geom_text(aes(label = test), vjust= -0.20) + 
#  +     theme(axis.text.x = element_text(angle = 90, hjust = 1))




dtm1_t <- t(dtm1)
options(bigalgebra.mixed_airthmetic_returns_R_matrix=FALSE)
big_dtm <- as.big.matrix(dtm1)
big_dtm2 <- as.big.matrix(dtm1_t)
system.time(adj_mat <- big_dtm %*% big_dtm2) 
adj_mat1 <- as.matrix(adj_mat)
diag(adj_mat1) <- 0     # no self-references. So diag is 0.
a0 <- order(apply(adj_mat1, 2, sum), decreasing = T)   # order cols by descending colSum
adj.mat2 <- as.matrix(adj_mat1[a0[1:50], a0[1:50]])   # taking the top 50 rows and cols only
adj.mat2%>% distill.cog(str_c(as.character(try_for_calc$organization[5213]), '2014-2016'),8,8)



system.time({
  for (i in 1:13){
    get_cog_for_time_period(patent_data_parts[[i]], names(patent_data_parts[i]))
  }
}) 


for (i in 4:12) {
  system.time({get_cog_for_time_period(patent_data_parts[[12]], names(patent_data_parts[12]))
 })
}

system.time({get_cog_for_time_period(patent_data_parts[[4]], names(patent_data_parts[4]))
})
  
  
get_cog_for_time_period = function(patent_data_for_period, time_period) 
  {
  patent_data_for_period %>% count(document, word, sort = TRUE) %>% ungroup()%>% cast_dtm(document, word, n) %>% DTM_2_AdjMatrix () %>% distill.cog(str_c('Toyota', time_period),6,6)
  gc()
}


gctorture(on = TRUE)

for (i in 1:length(patent_data_parts)){
  get_cog_for_time_period(patent_data_parts[[i]], names(patent_data_parts[i]))
  gc()
}


DTM_2_AdjMatrix = function(patent_dtm)
{
  terms_in_dtm <- apply(patent_dtm, 2, sum)
  which_terms_frequent <- order(terms_in_dtm, decreasing = TRUE)
  tsum <- terms_in_dtm[which_terms_frequent]
  frequencies_of_most_common_terms <- as.data.frame(round(tsum[1:150],0))
  list_of_common_terms <- attributes(frequencies_of_most_common_terms)$row.names
  dtm1 = as.matrix(patent_dtm)   # need it as a regular matrix for matrix ops like %*% to apply
  subset_of_dtm <- dtm1[TRUE, list_of_common_terms]
  adj.mat = t(subset_of_dtm) %*% subset_of_dtm    # making a square symmatric term-term matrix 
  diag(adj.mat) = 0     # no self-references. So diag is 0.
  a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
  adj.mat = as.matrix(adj.mat[a0[1:80], a0[1:80]])   # taking the top 50 rows and cols only
  return(adj.mat)
}











#DTM_2_AdjMatrix = function(dtm) {
#       require(microbenchmark)
#       require(RcppEigen)  
#       dtm1 <- as.matrix(dtm)   # need it as a regular matrix for matrix ops like %*% to apply
#       t_of_dtm1 <- t(dtm1)
#       microbenchmark(adj.mat <-  eigenMapMatMult(dtm1, t_of_dtm1))
#       #adj.mat <- t(dtm1) %*% dtm1    # making a square symmatric term-term matrix 
#       diag(adj.mat) <- 0     # no self-references. So diag is 0.
#       a0 <- order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
#       adj.mat <- as.matrix(adj.mat[a0[1:50], a0[1:50]])   # taking the top 50 rows and cols only
#       return(adj.mat)
#       gc()
#    }

text_clean <- function(text_column){
  text_in_func <- text_column  %>% data_frame() %>% rename(text=".")%>%
    mutate(text = str_replace_all(text, "include", " "),
          text = str_replace_all(text, "[Mm]ethod", " "),
          text = str_replace_all(text, "[Cc]alculate", " "),
          text = str_replace_all(text, "[Aa]pparatus", " "),
          text = str_replace_all(text, "compris", " "),
          text = str_replace_all(text, "insert", " "),
          text = str_replace_all(text, "[Dd]etermin", " "),
          text = str_replace_all(text, "[Pp]rovid", " "),
          text = str_replace_all(text, "change", " "),
          text = str_replace_all(text, "base", " "))
  return(text_in_func)
}


##########################################
#############  REPLACE BIGRAMS ###########
###########################################

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
###### End   replace_bigram ################




#################### Distill.cog#######################
######################################################

distill.cog = function(adj_matrix, # input TCM ADJ MAT
         title, # title for the graph
         central_nodes,    # no. of central nodes
         max_connections){  # max no. of connections  
  library(igraph)
  mat1 = adj_matrix
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  s = central_nodes
  k1 = max_connections
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
}


dtm_to_adj_matrix = function(subset_of_dtm)
{
  dtm1 = subset_of_dtm   # need it as a regular matrix for matrix ops like %*% to apply
  adj.mat = t(dtm1) %*% dtm1    # making a square symmatric term-term matrix 
  diag(adj.mat) = 0     # no self-references. So diag is 0.
  a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
  adj.mat = as.matrix(adj.mat[a0[1:50], a0[1:50]])   # taking the top 50 rows and cols only
  return(adj.mat)
}  
  




for (i in 1:length(patent_data_parts)){
  patent_data_parts[[i]] %>% count(document, word, sort = TRUE) %>% ungroup()%>% cast_dtm(document, word, n) %>% DTM_2_AdjMatrix () %>% distill.cog(str_c(patent_data_parts[[i]]$organization[5613],names(patent_data_parts[i])),8,8)
}
  


z <- x %>% cast_dtm(document, word, n)
patent_data_parts[[1]] %>% count(patent_id, word, sort = TRUE) %>% ungroup() %>% cast_dtm(document, word, n) %>% DTM_2_AdjMatrix () %>% distill.cog('Toyota_patents',8,8)



patent_data_company_list <- split(patent_data_company_words_bigram, patent_data_company_words_bigram$year)
no_of_years <- length(patent_data_company_list)
divide_into_parts = function (list, no_of_parts){
  list_in_func <- list
  no_of_parts <- no_of_parts
  list_to_fill <- list()
  size_of_buckets <- length(list_in_func)%/% no_of_parts
  
}
parts_of_date <- list()
for (j in 1:42){
print(j%%10)##try[[j%%10]] <- (rbindlist(patent_data_company_list[j:j+10]))
}





patent_word_counts <- patent_data_company_words_bigram %>%
  count(patent_id, word, sort = TRUE) %>%
  ungroup()




patent_data_industry <- data.frame()
patent_data_industry <- read.csv(file.choose())
patent_data_industry <- mutate(patent_data_industry, title = "EV")
patent_data_industry <- mutate (patent_data_industry, chapter = 1:nrow(patent_data_industry))
patent_industry_chapter <- patent_data_industry %>% unite(document, title, chapter)
names(patent_industry_chapter)[4] <- "patent_text"
patent_industry_chapter$patent_text <- as.character(patent_industry_chapter$patent_text)
names(patent_industry_chapter)[4] <- "text"
by_chapter_word <- patent_industry_chapter%>% unnest_tokens(word, text, to_lower = TRUE)
patent_chapter_word_clean <- by_chapter_word %>% anti_join(stop_words)
more_stop_words <- subset(patent_chapter_word_clean, nchar(as.character(word)) <= 2)
patent_chapter_word_clean <- patent_chapter_word_clean %>% anti_join(more_stop_words)

patent_word_counts <- patent_chapter_word_clean %>%
  count(document, word, sort = TRUE) %>%
  ungroup()
patent_word_counts
patents_dtm <- patent_word_counts %>% cast_dtm(document, word, n)
patents_dtm
patents_dtm %>% DTM_2_AdjMatrix () %>% distill.cog('EV_Patents',8,8)