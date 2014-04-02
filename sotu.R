#
# statements to be executed
#

corpus.dir <- 'state_of_the_union'

# load metadata
metadata <- read.table(file.path(corpus.dir, 'md.date-international.txt'), encoding='utf8', sep='\t', header=TRUE, stringsAsFactors=FALSE)
metadata$date <- as.Date(metadata$date)
metadata$president <- as.factor(metadata$president)

# order by date
metadata <- metadata[order(metadata$date),]

# ingest the corpus
corpus <- lapply(file.path(corpus.dir, metadata$file), BOWFromFile)

# calculate word frequencies per doc
doc.word.freq <- lapply(corpus, WordFreqsFromBOW)

# calculate corpus-wide frequencies
corpus.word.freq <- WordFreqsFromBOW(unlist(corpus))

# create a 'fingerprint' of top word frequencies
fingerprint <- corpus.word.freq[1:5]

# a large table of fingerprint freqs for all samples
tab.fingerprint <- t(sapply(doc.word.freq, Fingerprint, tokens=names(fingerprint)))

# pca on same
pca.fingerprint <- prcomp(tab.fingerprint)


#
# topic modelling
#

# pruned and stemmed corpus
corpus.prep <- lapply(corpus, function(v) { v[-which(v %in% stoplist)] })
# corpus.prep <- lapply(corpus.prep, SnowballStemmer)

# create docstrings 

lda.doclines <- lapply(corpus.prep, paste, collapse=' ')
lda.corpus <- lexicalize(lda.doclines)

set.seed(1)
K <- 5
num.iterations<-250

lda.model <- lda.collapsed.gibbs.sampler(lda.corpus$documents, K, lda.corpus$vocab, num.iterations, 0.1, 0.1, compute.log.likelihood=TRUE)
top.words <- top.topic.words(lda.model$topics, 25, by.score=TRUE)


#
# plots
#

# 1. freq curve for 'fingerprint' words

# freq.range <- range(sapply(doc.word.freq, function(x) {return(range(x[fingerprint]))}))
# 
# plot(corpus.word.freq[fingerprint], ylim=freq.range, type='n', xaxt='n')
# axis(1, 1:length(fingerprint), labels=names(fingerprint))
# 
# lines(doc.word.freq[[1]][fingerprint], type='b')


# 2. pca plots

# plot(pca.fingerprint$x, col=metadata$president, pch=metadata$president)

# 3. topic plots

plot(metadata$date, lda.model$document_sums[1,], type='n', xlab='Date', ylab='Topic Strength', yaxt='n', ylim=range(lda.model$document_sums))
for (i in 1:K) { 
 lines(metadata$date, lda.model$document_sums[i,], col=i, lty=i)
}

topic.labels <- paste(apply(top.words[1:5,], 2, paste, collapse=' '), '...')
legend('topleft', legend=topic.labels, col=c(1:K), lty=c(1:K), cex=.7, y.intersp=.4, text.width=(as.integer(max(metadata$date)-min(metadata$date))/2.5))


#
# nouns-only topics
#

BOWFromPOSFile <- function(file, pos=c('NN', 'NNS')) {
  
  tok.table <- read.table(file, header=TRUE, sep='\t', quote='', stringsAsFactors=FALSE)
  
  tokens <- tok.table$token[tok.table$pos %in% pos]
  tokens <- tolower(tokens)
  
  return(tokens)
}

corpus <- lapply(file.path(corpus.dir, 'pos_tagged', metadata$file), BOWFromPOSFile)



ann.date(as.Date('1929-10-28'), 'Black Monday')
