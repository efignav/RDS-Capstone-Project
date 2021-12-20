store_ngram_components = function(tabla, n_gram, col_name, k){
  tabla[, col_name] = sapply(strsplit(n_gram, ' '), function(arg) arg[k])
  return(tabla)
}

# get the discount value D

discount = function(fDT) {
  D = nrow(fDT[freq == 1])/(nrow(fDT[freq == 1]) + 2*nrow(fDT[freq == 2]))
  return(D)
}

mergeDTs = function (dt1, dt2){
  dt1 = dt1[order(terms)]
  dt2 = dt2[order(terms)]
  
  # update frequency for the already present terms in dt1
  dt1$freq[dt1$terms %in% dt2$terms] = dt1$freq[dt1$terms %in% dt2$terms] + 
    dt2$freq[dt2$terms %in% dt1$terms]
  
  # keep the new terms from dt2
  list = list(dt1, dt2[!(dt2$terms %in% dt1$terms),])
  dt3 = rbindlist(list)
  return(dt3)
}

# sample as a %
getSmpl = function(fData, pSmplSize) {
  nData = length(fData)
  smplSize = as.integer(pSmplSize*nData)/100
  
  # sample without replacement
  smplIndx = sample(1:nData, size = smplSize, replace = F)
  smplData = fData[smplIndx]
  return(smplData)
}

preprocessString = function(vector) {
  vector = gsub("[´’‘]", "'", vector)
  vector = qdap::replace_contraction(vector, replace = '')
  vector = gsub("'s", '', vector)
  # remove URLs from the string
  vector = gsub('http\\S+\\s*', '', vector)
  vector = gsub('[[:punct:]]', '', vector)
  # remove all but normal characters
  vector = gsub(pattern="[^a-zA-Z]", replacement=' ', vector)
  vector = tolower(vector)
  vector = gsub(" won t ", " will not ", vector)
  vector = gsub(" can t ", " can not ", vector)
  vector = gsub("n t ", " not ", vector)
  vector = gsub(" s ", " ", vector)
  vector = gsub(" u ", " you ", vector)
  
  return(vector)
}

nGrams = function(corpusDF, grams) {
  ngram = NGramTokenizer(corpusDF, 
                         Weka_control(min = grams, max = grams,
                                      delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngram = data.frame(table(ngram))
  ngram = ngram[order(ngram$Freq,decreasing = T),]
  colnames(ngram) = c("String", "Count")
  return(ngram)
}

reduceUnigram = function (unigramDT, coverage) {
  unigramDT = unigramDT[order(-freq)]
  frequency = 0
  requiredFrequency = coverage * sum(unigramDT$freq)
  
  for (i in 1:nrow(unigramDT)){
    if (frequency >= requiredFrequency)
    {
      return(unigramDT[1:i,])
    }
    
    frequency = frequency + unigramDT[i,]$freq
  }
  
  return (unigramDT)
}

get_freq_sum = function(sentence, fDT1) {
  # split up the sentence to its words
  words = unlist(strsplit(sentence, split = ' '))
  sum = sum(sapply(words, function(x) fDT1[.(x)]$freq), na.rm = T)
  return(sum)
}

mergeDFs = function (df1, df2){
  df1 = df1[order(df1$terms),]
  df2 = df2[order(df2$terms),]
  
  df1$freq[df1$terms %in% df2$terms] = df1$freq[df1$terms %in% df2$terms] + df2$freq[df2$terms %in% df1$terms]
  df3 = rbind(df1, df2[!(df2$terms %in% df1$terms),])
  df3
}
