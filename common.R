#
# libraries
#

library('openNLP')
library('NLP')
library('lda')
library('Snowball')


#
# functions
#

BOWFromFile <- function(file) {
  # read a file and return a vector of tokens
  
  # read the file line by line
  text <- scan(file, encoding='utf8', sep='\n', what='character')
  
  # paste the lines together
  text <- paste(text, collapse=' ')
  
  # split on non-word characters
  words <- unlist(strsplit(text, '\\W+'))
  
  # if non-words begin the string, first element is null
  if (words[1] == '') { 
    words <- words[2:length(words)]
  }
  
  # lowercase
  words <- tolower(words)
  
  return(words)
}

WordFreqsFromBOW <- function(bow) {
  # calculate frequencies per 1000 words from vector of tokens
  
  # get counts for each unique token
  tab.freq <- table(bow)
  
  # normalize per 1000 tokens
  tab.freq <- 1000 * tab.freq / sum(tab.freq)
  
  # sort from most frequent to least
  tab.freq <- sort(tab.freq, decreasing=TRUE)
  
  return(tab.freq)
}

Fingerprint <- function(freqs, tokens) {
  # return a "fingerprint" of token frequencies
  
  fingerprint <- freqs[tokens]
  fingerprint <- ifelse(is.na(fingerprint), 0, fingerprint)
  
  names(fingerprint) <- tokens
  return(fingerprint)
}

# type-token ratio for bag of words

Ttr <- function(bow) {
  
  types = length(unique(bow))
  tokens = length(bow)
  
  return(types/tokens)
}

#
# annotate time plots
#

ann.date <- function(d, label=NA) {
  
  abline(v=d, col='grey25')
  if (! is.na(label)) {
    
    mtext(label, side=3, at=d, las=2, line=0.5, cex=.6)
  }
}

ann.dateRange <- function(start, end, label=NA) {
  
  top <- par()$usr[3]
  bottom <- par()$usr[4]
  
  rect(start, bottom, end, top, col='grey85', border=NA)
  if (! is.na(label)) {
    
    mtext(label, side=3, at=mean(c(start, end)), las=2, line=0.5, cex=.6)
  }
}


#
# executed statements
#

stoplist <- scan(file='stoplist.txt', what='character', encoding='utf8', sep=' ')
stoplist <- append(stoplist, '000')