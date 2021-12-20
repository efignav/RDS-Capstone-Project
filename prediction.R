library(data.table)
library(stringr)
library(tm)
library(qdap)

load("unigrams.RData")
load("bigrams.RData")
load("trigrams.RData")

setkey(unigrams, terms)
setkey(bigrams, word1, word2)
setkey(trigrams, word1, word2, word3)

preprocess_input = function(vector) {
  vector = gsub("[´’‘]", "'", vector)
  vector = qdap::replace_contraction(vector, replace = '')
  vector = gsub("^c", "", vector) # remove the preceding c
  vector = gsub('http\\S+\\s*', '', vector)  # remove URL
  vector = gsub('[[:punct:]]', '', vector)
  vector = gsub(pattern="[^a-zA-Z]", replacement=' ', vector)  # remove all but normal characters
  vector = tm::stripWhitespace(vector)
  vector = tolower(vector)
  vector = gsub(" won t ", " will not ", vector)
  vector = gsub(" can t ", " can not ", vector)
  vector = gsub("n t ", " not ", vector)
  vector = gsub("'s", '', vector) # genitive
  vector = gsub(" s ", " ", vector) # genitive removal
  vector = gsub(" u ", " you ", vector)
  
  return(vector)
}

get_next_word = function(input, unigramDT, bigramDT, trigramDT, maxResults = 3) {
  input = preprocess_input(input)
  if(input == '' | input == "na na") 
    return("Empty input. No results!")
  
  input = unlist(strsplit(input, split = " "))
  
  if (length(input) >= 2) {
    trigram_result = trigrams[list(input[length(input)-1], input[length(input)])][order(-probability)]
    trigram_result = na.omit(trigram_result[1:maxResults,])
    
    if (nrow(trigram_result) >= 1) {
      trigram_result = trigram_result[,list(word3, probability)]
      
      names(trigram_result) = c('Next Word', 'Probability')
      return(trigram_result)
    }
  } 
  
  if (length(input) >= 1) {
    bigram_result = bigrams[list(input[length(input)])][order(-probability)]
    bigram_result = na.omit(bigram_result[1:maxResults,])
    
    if (nrow(bigram_result) >= 1) {
      bigram_result = bigram_result[,list(word2, probability)]
      
      names(bigram_result) = c('Next Word', 'Probability')
      return(na.omit(bigram_result[1:maxResults,]))
    } 
  }
  
  unigram_result = unigrams[order(-probability)][1:maxResults,]
  unigram_result = unigram_result[,list(terms, probability)]
  
  names(unigram_result) = c('Next Word', 'Probability')
  return(unigram_result)
}
