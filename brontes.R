
corpus.dir <- 'brontes'

# load metadata

file.metadata <- file.path(corpus.dir, 'metadata.txt')
metadata <- read.table(file.metadata, sep='\t', header=TRUE, colClasses=c('character', 'factor', 'character', 'factor'))

# load corpus
corpus <- lapply(file.path(corpus.dir, paste(metadata$file, '.txt', sep='')), BOWFromFile)

# calculate word frequencies per doc
doc.word.freq <- lapply(corpus, WordFreqsFromBOW)

# calculate corpus-wide frequencies
corpus.word.freq <- WordFreqsFromBOW(unlist(corpus))

# create a 'fingerprint' of top word frequencies
fingerprint <- corpus.word.freq[1:5]

# a large table of fingerprint freqs for all samples
tab.fingerprint <- t(sapply(doc.word.freq, Fingerprint, tokens=names(fingerprint)))

# pca
pca.fingerprint <- prcomp(tab.fingerprint)

# author/genre plot
par(mar=c(6, 4, 4, 8) + .1)
plot(pca.fingerprint$x, col=unclass(metadata$type), pch=unclass(metadata$author))
legend('right', legend=c('anne prose', 'charlotte prose', 'emily prose', 'anne verse', 'charlotte verse', 'emily verse'), pch=c(1:3,1:3), col=c(1,1,1,2,2,2), xpd=TRUE, inset=c(-.45,0), cex=.9)

