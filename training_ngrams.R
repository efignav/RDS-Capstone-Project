rm(list = ls())
gc(verbose = F, full = F)

options(warn = -1) # ignore warning messages in for-loop

suppressWarnings(library(tm))
suppressWarnings(library(tau))
suppressWarnings(library(stringr))
suppressWarnings(library(R.utils))
suppressWarnings(library(SnowballC))
suppressWarnings(library(RWeka))
suppressWarnings(library(plyr))
suppressWarnings(library(markdown))
suppressWarnings(library(ggplot2))
suppressWarnings(library(MASS))
suppressWarnings(library(data.table))
suppressWarnings(library(qdap))

source('helper_functions.R')

# Define file names with relative folder path
wdpath = "C:/Users/enrique figueroa/Desktop/InProgress"
setwd(wdpath)

blogs_name = "en_US/en_US.blogs.txt"
tweets_name = "en_US/en_US.twitter.txt"
news_name = "en_US/en_US.news.txt"

blogs = readLines(blogs_name, encoding = "UTF-8", skipNul = F)
rm(blogs_name)
blogs_sample = getSmpl(blogs, 12.5)
rm(blogs)

tweets = readLines(tweets_name,  encoding = "UTF-8", skipNul=T)
rm(tweets_name)
tweets_sample = getSmpl(tweets, 12.5)
rm(tweets)

news = readLines(news_name, encoding = "UTF-8", skipNul = F)
rm(news_name)
news_sample = getSmpl(news, 12.5)
rm(news)

# clean memory
rm(blogs, tweets, news)
gc(verbose = F, full = F)

# merge datasets
merged_datasets =  c(tweets_sample, news_sample, blogs_sample)

# clean memory
rm(tweets_sample, news_sample, blogs_sample)
gc(verbose = F, full = T)

merged_datasets = preprocessString(merged_datasets)

# best chunk size with a processor wit only 8 GB RAM
chunk_size = 10000
chunk_n = as.integer(length(merged_datasets)/chunk_size)

bad_words = readLines("badwords.txt")
bad_words = preprocessString(bad_words)

#======================================
# clean a process all data in chunks
#--------------------------------------
chunk_name_df1 = "en_US/data_tmp/FreqDF1/df1_"
chunk_name_df2 = "en_US/data_tmp/FreqDF2/df2_"
chunk_name_df3 = "en_US/data_tmp/FreqDF3/df3_"
chunk_name_df4 = "en_US/data_tmp/FreqDF4/df4_"

for (i in 1:chunk_n ) {
  j = (i-1)*chunk_size+1
  k = i*chunk_size
  
  chunk_corpus = VCorpus(VectorSource(merged_datasets[j:k]))
  chunk_corpus = tm_map(chunk_corpus, removeWords, bad_words)
  chunk_corpus = tm_map(chunk_corpus, stripWhitespace)
  
  chunk_df = data.frame(text=unlist(sapply(chunk_corpus, `[`, "content")), 
                         stringsAsFactors = F)
  
  # create the n-grams
  unigram = nGrams(chunk_df, 1)
  bigram = nGrams(chunk_df, 2)
  trigram = nGrams(chunk_df, 3)
  fourgram = nGrams(chunk_df, 4)
  
  write.table(unigram, paste(chunk_name_df1,i,sep=""), col.names = T)
  write.table(bigram, paste(chunk_name_df2,i,sep=""), col.names = T)
  write.table(trigram, paste(chunk_name_df3,i,sep=""), col.names = T)
  write.table(fourgram, paste(chunk_name_df4,i,sep=""), col.names = T)
  
  cat(i, " ")
  # if(i>5) break #temporal
}

setwd(wdpath)

# clean memory
rm (chunk_corpus, chunk_df, freq1, tdm1, freq2, tdm2, freq3, tdm3, freq4, tdm4)
rm(merged_datasets, bad_words)
rm(unigram, bigram, trigram, fourgram) # <==
gc(verbose = F, full = F)

#==============================================
# merge processed chunks
# ---------------------------------------------

chunk_name1 = "en_US/data_tmp/FreqDF1/DF1_"
chunk_name2 = "en_US/data_tmp/FreqDF2/DF2_"
chunk_name3 = "en_US/data_tmp/FreqDF3/DF3_"
chunk_name4 = "en_US/data_tmp/FreqDF4/DF4_"

dt1_tmp = data.table(terms=NA, freq=NA)[numeric(0), ]
dt2_tmp = data.table(terms=NA, freq=NA)[numeric(0), ]
dt3_tmp = data.table(terms=NA, freq=NA)[numeric(0), ]
dt4_tmp = data.table(terms=NA, freq=NA)[numeric(0), ]

for (i in 1:chunk_n ) {
  # read saved chunks
  chunk_name_df1 = paste(chunk_name1, i, sep="")
  chunk_dt1 = fread(chunk_name_df1, header = F, select = c(2:3), 
               col.names = c('terms', 'freq'))
  chunk_name_df2 = paste(chunk_name2,i,sep="")
  chunk_dt2 = fread(chunk_name_df2, header = F, select = c(2:3),
               col.names = c('terms', 'freq'))
  chunk_name_df3 = paste(chunk_name3,i,sep="")
  chunk_dt3 = fread(chunk_name_df3, header = F, select = c(2:3),
               col.names = c('terms', 'freq'))
  chunk_name_df4 = paste(chunk_name4,i,sep="")
  chunk_dt4 = fread(chunk_name_df4, header = F, select = c(2:3),
               col.names = c('terms', 'freq'))
  
  # merge chunks 
  dt1_merged = mergeDTs(dt1_tmp, chunk_dt1)
  dt1_tmp = dt1_merged

  dt2_merged = mergeDTs(dt2_tmp, chunk_dt2)
  dt2_tmp = dt2_merged
  
  dt3_merged = mergeDTs(dt3_tmp, chunk_dt3)
  dt3_tmp = dt3_merged
  
  dt4_merged = mergeDTs(dt4_tmp, chunk_dt4)
  dt4_tmp = dt4_merged

  cat(i, " ")
  # if(i>5) break #temporal
}

# save merged datasets
file1_name = "en_US/data_tmp/FreqDF/fDF1_final"
file2_name = "en_US/data_tmp/FreqDF/fDF2_final"
file3_name = "en_US/data_tmp/FreqDF/fDF3_final"
file4_name = "en_US/data_tmp/FreqDF/fDF4_final"

file1_name_RData = "en_US/data_tmp/FreqDF/fDF1_final.RData"
file2_name_RData = "en_US/data_tmp/FreqDF/fDF2_final.RData"
file3_name_RData = "en_US/data_tmp/FreqDF/fDF3_final.RData"
file4_name_RData = "en_US/data_tmp/FreqDF/fDF4_final.RData"

dt1_merged = dt1_merged[with(dt1_merged, order(-freq)), ]
write.table(dt1_merged, file1_name, col.names = T)
save(dt1_merged, file = file1_name_RData);

dt2_merged = dt2_merged[with(dt2_merged, order(-freq)), ]
write.table(dt2_merged, file2_name, col.names = T)
save(dt2_merged, file = file2_name_RData);

dt3_merged = dt3_merged[with(dt3_merged, order(-freq)), ]
write.table(dt3_merged, file3_name, col.names = T)
save(dt3_merged, file = file3_name_RData);

dt4_merged = dt4_merged[with(dt4_merged, order(-freq)), ]
write.table(dt4_merged, file4_name, col.names = T)
save(dt4_merged, file = file4_name_RData);

# clean memory
rm(chunk_dt1, chunk_dt2, chunk_dt3, chunk_dt4)
rm(dt1_tmp, dt2_tmp, dt3_tmp, dt4_tmp)
rm(dt1_merged, dt2_merged, dt3_merged, dt4_merged)
rm(file1_name, file2_name, file3_name, file4_name)
rm(chunk_name1, chunk_name2, chunk_name3, chunk_name4)
rm(chunk_name_df1, chunk_name_df2, chunk_name_df3, chunk_name_df4)
rm(file1_name_RData, file2_name_RData, file3_name_RData, file4_name_RData)
rm(i, j, k)
gc(verbose = F, full = F)

#=====================================================
# store n-gram component into respective tables

file1_pre_processed = "en_US/data_tmp/FreqDF/fDF1_final"
file2_pre_processed = "en_US/data_tmp/FreqDF/fDF2_final"
file3_pre_processed = "en_US/data_tmp/FreqDF/fDF3_final"
file4_pre_processed = "en_US/data_tmp/FreqDF/fDF4_final"

DT1 = fread(file1_pre_processed, select = c(2:3), col.names = c('terms', 'freq'))
DT2 = fread(file2_pre_processed, select = c(2:3), col.names = c('terms', 'freq'))
DT3 = fread(file3_pre_processed, select = c(2:3), col.names = c('terms', 'freq'))
DT4 = fread(file4_pre_processed, select = c(2:3), col.names = c('terms', 'freq'))

rm(file1_pre_processed, file2_pre_processed, file3_pre_processed, file4_pre_processed)

DT2 = store_ngram_components(DT2, n_gram = DT2$terms, "word1",1)
DT2 = store_ngram_components(DT2, n_gram = DT2$terms, "word2",2)

DT3 = store_ngram_components(DT3, n_gram = DT3$terms, "word1",1)
DT3 = store_ngram_components(DT3, n_gram = DT3$terms, "word2",2)
DT3 = store_ngram_components(DT3, n_gram = DT3$terms, "word3",3)

DT4 = store_ngram_components(DT4, n_gram = DT4$terms, "word1",1)
DT4 = store_ngram_components(DT4, n_gram = DT4$terms, "word2",2)
DT4 = store_ngram_components(DT4, n_gram = DT4$terms, "word3",3)
DT4 = store_ngram_components(DT4, n_gram = DT4$terms, "word4",4)

# discount values
D2 = discount(DT2)
D3 = discount(DT3)

setkey(DT1, terms)
setkey(DT2, word1, word2)
setkey(DT3, word1, word2, word3)
setkey(DT4, word1, word2, word3, word4)

# prune the sets
DT1 = reduceUnigram(DT1, 0.9)
DT2 = DT2[DT2$freq > 4,]
DT3 = DT3[DT3$freq > 4,]
DT4 = DT4[DT4$freq > 2,]
gc(verbose = F, full = F)

setkey(DT1, terms)
setkey(DT2, word1, word2)
setkey(DT3, word1, word2, word3)
setkey(DT4, word1, word2, word3, word4)

#=================================
# calculate n-gram probabilities
# --------------------------------
source('kneser_ney_preprocess.R')

DT3 = set_trigram_probabilities(DT1, DT2, DT3, D2, D3, 1000)
DT2 = set_bigram_probabilities(DT1, DT2, DT3, D2, 1000)
DT1 = set_unigram_probabilities(DT1, DT2, 1000)

unigrams = na.omit(DT1)
bigrams = na.omit(DT2)
trigrams = na.omit(DT3)
fourgrams = na.omit(DT4)

save(unigrams, file="en_US/data_tmp/FreqDF/UNIGRAMS.RData");
save(bigrams, file="en_US/data_tmp/FreqDF/BIGRAMS.RData");
save(trigrams, file="en_US/data_tmp/FreqDF/TRIGRAMS.RData");
save(fourgrams, file="en_US/data_tmp/FreqDF/FOURGRAMS.RData");
save(D2, D3, file="constants.RData")
