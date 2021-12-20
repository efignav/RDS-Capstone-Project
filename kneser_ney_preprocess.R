# Based on code from NMayer github repository

unigram_probability = function(fDT1, fDT2, inStr, higher = F) {
  last_word = word(inStr, -1, sep = "\\s+")
  
  preceding = fDT2[J(unique(word1), last_word), nomatch=0]
  
  if (higher) {
    probability = nrow(preceding)/nrow(fDT2)
    return(probability)
  } else {
    count_wi = fDT1[.(last_word)]$freq
    count_all = sum(fDT1$freq)
    probability = count_wi/count_all
  }
}

bigram_probability = function(fDT1, fDT2, fDT3, D2, inStr, higher = F) {
  first = word(inStr, -2, sep = "\\s+")
  sec = word(inStr, -1, sep = "\\s+")
  
  preceding = fDT3[J(unique(word1), first, sec), nomatch=0]
  
  count_2 = fDT2[.(first, sec)]$freq
  
  if(is.na(count_2)) {
    return(unigram_probability(fDT1, fDT2, inStr))
  } else {
    c_penultimate = fDT1[.(first)]$freq

    N_penultimate = nrow(fDT2[.(first)])
    
    N_star_pen_star = nrow(fDT3[J(unique(word1), 
                                   first),
                                 nomatch = 0])
    # unigram probability
    unigram_prob = unigram_probability(fDT1, fDT2, inStr, higher = T)
    bigram_prob_1 = ((nrow(preceding) - D2)/N_star_pen_star) +
      (D2/N_star_pen_star)*N_penultimate*unigram_prob
    bigram_prob_2 = ((count_2-D2)/c_penultimate) +
      (D2/c_penultimate)*N_penultimate*unigram_prob
    
    if (higher) {
      return(bigram_prob_1)
    } else {
      return(bigram_prob_2)
    }
  }
}

trigram_probability = function(inStr, fDT1, fDT2, fDT3, D2, D3) {
  
  firsttri = word(inStr, -3, sep = "\\s+")
  sectri = word(inStr, -2, sep = "\\s+")
  tritri = word(inStr, -1, sep = "\\s+")
  
  count_3 = fDT3[.(firsttri, sectri, tritri)]$freq
  
  if(is.na(count_3)) {
    return(bigram_probability(fDT1, fDT2, fDT3, D2, inStr))
  } else {
    c_two_preceding = fDT2[.(firsttri, sectri)]$freq

    N_two_preceding = nrow(fDT3[.(firsttri, sectri)])
    
    bigram_prob = bigram_probability(fDT1, fDT2,fDT3, D2, inStr, higher = T)
    trigram_prob = ((count_3-D3)/c_two_preceding) +
      (D3/c_two_preceding)*N_two_preceding*bigram_prob
    return(trigram_prob)
  }
}

set_trigram_probabilities <- function(fDT1, fDT2, fDT3, D2, D3, splitsize=1000){
  ndiv = as.integer(nrow(fDT3)/splitsize)
  
  fDT3_prob <- data.table(probability = rep(0, nrow(fDT3)))
  for (i in 1:ndiv) {
    start <- (i-1)*splitsize + 1
    end <- i*splitsize
    fDT3_prob[start:end,] <- sapply(fDT3[start:end,]$terms, trigram_probability,
                                    fDT1 = fDT1, fDT2 = fDT2, fDT3 = fDT3, D2 = D2,
                                    D3 = D3)
    cat(i, " ")
    # if(i>5) break #temporal
  }
  start <- ndiv*splitsize + 1
  end <- nrow(fDT3)
  fDT3_prob[start:end,] <- sapply(fDT3[start:end,]$terms, trigram_probability,
                                  fDT1 = fDT1, fDT2 = fDT2, fDT3 = fDT3, D2 = D2,
                                  D3 = D3)
  fDT3 <- cbind(fDT3, fDT3_prob)
  return(fDT3)
}

set_bigram_probabilities <- function(fDT1, fDT2, fDT3, D2, splitsize=1000) {
  ndiv = as.integer(nrow(fDT2)/splitsize)

  fDT2_prob <- data.table(probability = rep(0, nrow(fDT2)))
  for (i in 1:ndiv) {
    start <- (i-1)*splitsize + 1
    end <- i*splitsize
    fDT2_prob[start:end,] <- sapply(fDT2[start:end,]$terms, bigram_probability,
                                    fDT1 = fDT1, fDT2 = fDT2, fDT3 = fDT3, 
                                    D2 = D2)
    cat(i, " ")
    # if(i>5) break #temporal
  }
  start <- ndiv*splitsize + 1
  end <- nrow(fDT2)
  fDT2_prob[start:end,] <- sapply(fDT2[start:end,]$terms, bigram_probability,
                                  fDT1 = fDT1, fDT2 = fDT2, fDT3 = fDT3, 
                                  D2 = D2)
  fDT2 <- cbind(fDT2, fDT2_prob)
  return(fDT2)
}

set_unigram_probabilities <- function(fDT1, fDT2, splitsize=1000) {
  ndiv = as.integer(nrow(fDT1)/splitsize)

  fDT1_prob <- data.table(probability = rep(0, nrow(fDT1)))
  for (i in 1:ndiv) {
    start <- (i-1)*splitsize + 1
    end <- i*splitsize
    fDT1_prob[start:end,] <- sapply(fDT1[start:end,]$terms, unigram_probability,
                                    fDT1 = fDT1, fDT2 = fDT2)
    cat(i, " ")
    # if(i>5) break #temporal
  }
  start <- ndiv*splitsize + 1
  end <- nrow(fDT1)
  fDT1_prob[start:end,] <- sapply(fDT1[start:end,]$terms, unigram_probability,
                                  fDT1 = fDT1, fDT2 = fDT2)
  fDT1 <- cbind(fDT1, fDT1_prob)
  return(fDT1)
}
