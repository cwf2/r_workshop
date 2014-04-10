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

# ingest the corpus - plain text
# corpus <- lapply(file.path(corpus.dir, metadata$file), BOWFromFile)
# ingest the corpus from files pre-parsed for POS
corpus <- lapply(file.path(corpus.dir, 'pos_tagged', metadata$file), BOWFromPOSFile)

#
# topic modelling
#

# delete words from stoplist
corpus.prep <- lapply(corpus, function(v) { v[-which(v %in% stoplist)] })
# apply an English stemmer
# corpus.prep <- lapply(corpus.prep, SnowballStemmer)

# create "documents" of the format required
# by the topic-modelling package

lda.doclines <- lapply(corpus.prep, paste, collapse=' ')
lda.corpus <- lexicalize(lda.doclines)

# set random number seed
set.seed(1)

# set number of topics
K <- 5

# set number of iterations
num.iterations<-250

# generate model
lda.model <- lda.collapsed.gibbs.sampler(lda.corpus$documents, K, lda.corpus$vocab, num.iterations, 0.1, 0.1, compute.log.likelihood=TRUE)

# extract top words from the model
top.words <- top.topic.words(lda.model$topics, 25, by.score=TRUE)


#
# topic plots
#

# set up basic plot area
plot.new()
plot.window(xlim=range(metadata$date), ylim=range(lda.model$document_sums))

# add annotations
notes <- read.table(file.path(corpus.dir, 'interesting.dates.txt'), header=TRUE, sep='\t', stringsAsFactors=FALSE)
notes$start <- as.Date(notes$start)
notes$end <- as.Date(notes$end)

for (i in 1:nrow(notes)) { ann.dateRange(start=notes$start[i], end=notes$end[i], label=notes$name[i])}

# add axes
box()
axis.Date(side=1, x=metadata$date, lwd=0, lwd.ticks=1)

# draw curve for each topic
for (i in 1:K) { 
 lines(metadata$date, lda.model$document_sums[i,], col=i, lty=i)
}

# create legend
topic.labels <- paste(apply(top.words[1:5,], 2, paste, collapse=' '), '...')
legend('topleft', legend=topic.labels, col=c(1:K), lty=c(1:K), cex=.7, y.intersp=.4, text.width=(as.integer(max(metadata$date)-min(metadata$date))/2.5))
