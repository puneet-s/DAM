#--------------------------------------------------------#
## Step 2: Create DTM and sparse_dtm using text2vec package             #
#--------------------------------------------------------#

build_dtm_dtmTfidf <- function(x){   # x is cleaned corpus
  require(text2vec)
  tok_fun = word_tokenizer  # using word & not space tokenizers
  it_0 = itoken( x,
                 #preprocessor = text.clean,
                 tokenizer = tok_fun,
                 ids = data$id,
                 progressbar = T)
  
  vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
                            ngram = c(2L, 2L))
  
  pruned_vocab = prune_vocabulary(vocab,  # filters input vocab & throws out v frequent & v infrequent terms
                                  term_count_min = 1)
  
  vectorizer = vocab_vectorizer(pruned_vocab) #  creates a text vectorizer func used in constructing a dtm/tcm/corpus
  
  dtm_0  = create_dtm(it_0, vectorizer) # high-level function for creating a document-term matrix
  
  # Sort bi-gram with decreasing order of freq
  tsum = as.matrix(t(slam::rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
  tsum = tsum[order(tsum, decreasing = T),]       # terms in decreasing order of freq
  
  #-------------------------------------------------------
  # Code bi-grams as unigram in clean text corpus
  #-------------------------------------------------------
  
  text2 = x
  text2 = paste("",text2,"")
  
  pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; 
  
  i = 0
  for (term in names(tsum)){
    i = i + 1
    focal.term = gsub("_", " ",term)        # in case dot was word-separator
    replacement.term = term
    text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2)
    setTxtProgressBar(pb, i)
  }
  
  
  it_m = itoken(text2,     # function creates iterators over input objects to vocabularies, corpora, DTM & TCM matrices
                # preprocessor = text.clean,
                tokenizer = tok_fun,
                ids = data$id,
                progressbar = T)
  
  vocab = create_vocabulary(it_m)     # vocab func collects unique terms and corresponding statistics
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = 1)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  # dim(dtm_m)
  dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
  a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
  dtm = dtm[a0,]                  # drop empty docs
  
  tfidf = TfIdf$new() # define tfidf model
  dtm_tfidf = fit_transform(dtm_m, tfidf)
  
  out = list(dtm = dtm, dtm_sparse = dtm_m, dtm_tfidf=dtm_tfidf)
  
  return(out)  # output is list of length 3 containing dtm and a sparse dtm representation and dtm_tfidf.
  
} # build_dtm_dtmTfidf func ends