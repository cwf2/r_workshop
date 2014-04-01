#
# statements to be executed
#

corpus.dir <- 'brownings'
file.list <- file.path(corpus.dir, dir(corpus.dir, pattern='\\d+.txt'))

# load metadata

metadata <- read.table(file.path(corpus.dir, 'index.txt'), encoding='utf8', sep='\t', header=TRUE)

# ingest the corpus
corpus <- lapply(file.list, BOWFromFile)

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
# plots
#

# pronoun use

rb <- hist(tab.fingerprint[metadata$from=='R.B.', 'i'], plot=FALSE)
ebb <- hist(tab.fingerprint[metadata$from=='E.B.B.', 'i'], plot=FALSE)
plot(rb, main='I', xlab='use/1000 words', angle=45, density=15, col=1)
lines(ebb, angle=-45, density=30, col=2)
legend('topright', c('R.B.', 'E.B.B'), density=c(15, 30), angle=c(45, -45), fill=c(1,2))


rb <- hist(tab.fingerprint[metadata$from=='R.B.', 'you'], plot=FALSE)
ebb <- hist(tab.fingerprint[metadata$from=='E.B.B.', 'you'], plot=FALSE)
plot(ebb, main='you', xlab='use/1000 words', angle=45, density=15, col=2)
lines(rb, angle=45, density=15, col=1)
legend('topright', c('R.B.', 'E.B.B'), density=c(15, 30), angle=c(45, -45), fill=c(1,2))
