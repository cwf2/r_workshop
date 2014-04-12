#
# statements to be executed
#

# load some pre-defined functions
source('common.R')

# load metadata
corpus.dir <- 'brownings'
metadata <- read.table(file.path(corpus.dir, 'index.txt'), encoding='utf8', sep='\t', header=TRUE)

# ingest the corpus
corpus <- lapply(file.path(corpus.dir, metadata$file), BOWFromFile)

# calculate word frequencies per doc
doc.word.freq <- lapply(corpus, WordFreqsFromBOW)

# calculate corpus-wide frequencies
corpus.word.freq <- WordFreqsFromBOW(unlist(corpus))

# create a 'fingerprint' of top word frequencies
fingerprint <- corpus.word.freq[1:100]

# a large table of fingerprint freqs for all samples
tab.fingerprint <- t(sapply(doc.word.freq, Fingerprint, tokens=names(fingerprint)))


#
# plots
#

# pronoun use

plot(tab.fingerprint[,'it'], 
     col=unclass(metadata$from), 
     pch=unclass(metadata$from),
     ann=FALSE)
title(
     main='Frequency of "it" in Elizabeth\n and Robert Browning\'s Letters',
     xlab='Letter ID',
     ylab='Count / 1000 Words')
legend('topright',
       legend=levels(metadata$from),
       pch=1:2,
       col=1:2)

# show mean frequencies
# abline(h=mean(tab.fingerprint[metadata$from=='E.B.B.', 'it']), col=1)
# abline(h=mean(tab.fingerprint[metadata$from=='R.B.', 'it']), col=2)

# 
# pca plots
# 
# for (n in seq(from=5, to=200, by=5)) {
# 
#   # create a 'fingerprint' of top word frequencies
#   fingerprint <- corpus.word.freq[1:n]
#   
#   # a large table of fingerprint freqs for all samples
#   tab.fingerprint <- t(sapply(doc.word.freq, Fingerprint, tokens=names(fingerprint)))
#   
#   # pca on same
#   pca.fingerprint <- prcomp(tab.fingerprint)
# 
#   # plot
#   plot(pca.fingerprint$x[,2], col=unclass(metadata$from), pch=unclass(metadata$from))
#   title(main=paste('top', n, 'words'))
#   legend('bottomleft', legend=levels(metadata$from), pch=1:2, col=1:2)  
# }
