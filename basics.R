#
# R Basics
#

# operators

2 + 3  # R returns the answer at the console
7 - 6
5 * 2
6 / 3
2 ^ 5

# functions

sqrt(9)    # functions take arguments in parens
sin(1)
floor(5.12)

# get help on something
?floor

# assignment

a <- 5   # what happens in the "environment" window?
2 -> b

a + b
sqrt(a + 2 * b)

#
# vectors
#

# a vector of length 1

a <- 42
class(a)

b <- 'Hello, World!'
class(b)

c <- TRUE
class(c)

# longer vectors

a <- 1:10
a <- 50:1
a <- seq(from=0, to=24, by=6)

a <- c(2, 3, 5, 7, 11, 13, 17)
b <- c('Sing', 'a', 'song', 'of', 'sixpence')
c <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

d <- c('bob', 'bob', 'alice', 'carol', 'alice')
d <- factor(d)
class(d)
as.numeric(d)

# subscripting vectors

a <- seq(from=0, to=50, by=5)
a
a[1]
a[-1]
a[1:5]
a[c(4,5,6)]
a[a > 20]

# note what happens when we combine things of different classes
l <- c(1, 2, 'five', TRUE, 8)
class(l)

# lists

l <- list(1, 2, 'five', TRUE, 8)
l
class(l)
class(l[[1]])
class(l[[4]])

# data frames
#  - this is one of the most useful data structures

my.table <- data.frame(trial=1:4, outcome=c('win', 'lose', 'win', 'draw'))

# read a data frame from an external text file
metadata <- read.table(
  file=file.path('brontes', 'metadata.txt'),
  sep='\t',
  header=TRUE,
  stringsAsFactors=FALSE
  )

# subscripting a data frame

metadata[1,1]      # by row and column
metadata[3,]       # by row
metadata[,4]       # by column
metadata[, 'type'] # by name
metadata$type
metadata[, c('author', 'title')]

#
# some simple graphs
#

# plot one variable
x <- seq(from=0, to=6, by=0.1)
plot(x)

# a function of one variable
plot(sin(x))

# two variables
y <- sin(x)
plot(x, y)
title(main='a sine function')

# add a second set of data to the existing graph
points(x, sin(x-1), col=2, pch=2)

# make a nice legend
legend('topright', 
       legend=c('sin(x)', 'sin(x-1)'),
       col=c(1,2),
       pch=c(1,2))

#
# working with text files
# 

# repeated just in case we want to start from this point
metadata <- read.table(file=file.path('brontes', 'metadata.txt'), sep='\t', header=TRUE, stringsAsFactors=FALSE)

# combine directory and file names into a path
my.file <- file.path('brontes', paste(metadata[1,'file'], '.txt', sep=''))

# read the file line by line
text <- scan(file=my.file, what='character', sep='\n', encoding='utf8')

length(text)
head(text)

# paste the lines together
text <- paste(text, collapse=' ')
length(text)

# split into words on non-word characters
#  -produces a list by default
words <- strsplit(text, '\\W+')

# simplify to a vector
words <- unlist(words)

# how many words?
length(words)

# how many unique words (types?)
length(unique(words))
type.token.ratio <- length(unique(words))/length(words)
type.token.ratio

# standardize orthography a little
words <- tolower(words)
type.token.ratio <- length(unique(words))/length(words)
type.token.ratio

# summarize all the token counts
tab.freq <- table(words)
tab.freq

sum(tab.freq) == length(words)

# frequency per 1000 tokens
tab.freq <- 1000 * tab.freq / sum(tab.freq)
tab.freq

# sort from most frequent to least
tab.freq <- sort(tab.freq, decreasing=TRUE)

# take a random sample
sam <- sample(words, 100)
sam

# a replicable random sample
set.seed(19810111)
sam <- sample(words, 100)
sam

# a "more random" sample?
set.seed(format(Sys.time(), '%s'))
sam <- sample(words, 100)
sam

# look at subsets
sam=='and'
which(sam=='and')
length(which(sam=='and'))

# NB: in math operations, TRUE evaluates as 1 and FALSE as 0
sum(sam=='and')

#
# interlude: defining a function
#

get.freq <- function(token) {
  # calculate a token's frequency per 1000 words
  
  count <- length(which(words==token))
  all.tokens <- length(words)
  freq <- count/all.tokens
  
  return(freq * 1000)
}

# now we can use our function

get.freq('and')
get.freq('i')

# apply it to a big sample
sam <- sample(words, 20000)

word.freq <- sapply(unique(sam), get.freq)
word.freq <- sort(word.freq, decreasing=TRUE)
word.freq[1:25]

# grab a whole set of samples
sam <- t(replicate(20, sample(words, 100)))

# another way of breaking up the text
length(words)
sample.size <- 5000

sample.id <- ceiling(seq_along(words)/sample.size)
sam <- split(words, sample.id)

str(sam)

sam <- sam[sapply(sam, length)==5000]
str(sam)

#
# interlude: a function with 2 arguments
#

get.freq <- function(token, sample=words) {
  # calculate a token's frequency per 1000 words
  #  - by default, calculate for all words;
  #    or allow user to specify a sample
  
  count <- length(which(sample==token))
  all.tokens <- length(sample)
  freq <- count/all.tokens
  
  return(freq * 1000)
}

# word frequencies for arbitrary samples

get.freq('and')
get.freq('and', sam[[1]])

# plot frequencies by sample
freq.and <- sapply(sam, function(s) {get.freq(token='and', sample=s)})
freq.i <- sapply(sam, function(s) {get.freq(token='i', sample=s)})

plot(freq.and)

#
# put the plot together piece by piece
#

# initialize a new plot
plot.new()

# define the x and y ranges
plot.window(ylim=range(freq.and, freq.i), xlim=c(0, 37))

# draw an outline and axes
box()
axis(1)
axis(2)

# add the data series
points(freq.and, col=1, pch=1)
points(freq.i, col=2, pch=2)

# titles
title(main='frequency of two function words', xlab='sample id', ylab='count/1000 words')

# legend
legend('topleft', legend=c('and', 'i'), col=c(1,2), pch=c(1,2))

#
# tidy up a little
#

# remove unwanted data structures
my.table
rm(my.table)
my.table

# remove the whole list at once
rm(list=ls())

#
# a look at the Brownings
#

# some functions I prepared ahead of time
source('common.R')

# load metadata
corpus.dir <- 'brownings'
metadata <- read.table(file.path(corpus.dir, 'index.txt'), encoding='utf8', sep='\t', header=TRUE)
metadata$file <- as.character(metadata$file)

# a function to turn a file into a bag of words
my.file <- file.path(corpus.dir, metadata[1,'file'])
bag.of.words <- BOWFromFile(my.file)
bag.of.words

# now apply that function to the list of files in metadata
corpus <- lapply(file.path(corpus.dir, metadata$file), BOWFromFile)

# turn a bag of words into word frequencies
corpus[[3]]
WordFreqsFromBOW(corpus[[3]])

# calculate word frequencies per doc in corpus
#  - NB: set of tokens is different for each doc
doc.word.freq <- lapply(corpus, WordFreqsFromBOW)

# calculate corpus-wide frequencies
corpus.word.freq <- WordFreqsFromBOW(unlist(corpus))

# create a set of top word frequencies in corpus
top.words <- names(corpus.word.freq[1:5])
top.words

# a function to return a "fingerprint"
# made up of key word frequencies
Fingerprint(corpus.word.freq, tokens=top.words)
Fingerprint(doc.word.freq[[1]], tokens=top.words)

# a large table of fingerprint freqs for all samples
tab.fingerprint <- data.frame(t(sapply(doc.word.freq, Fingerprint, tokens=top.words)))
tab.fingerprint

#
# does the fingerprint distinguish Elizabeth from Robert?
#

classes <- as.numeric(metadata$from)

plot(tab.fingerprint$it, col=classes, pch=classes)
legend('topright', legend=levels(metadata$from), pch=1:2, col=1:2)

# identify individual points
identify(tab.fingerprint[,'it'])

# show mean frequencies
abline(h=mean(tab.fingerprint[metadata$from=='E.B.B.', 'it']), col=1)
abline(h=mean(tab.fingerprint[metadata$from=='R.B.', 'it']), col=2)


# combine two features

plot(x=tab.fingerprint$the, y=tab.fingerprint$and, col=classes, pch=classes)
legend('topright', legend=levels(metadata$from), pch=1:2, col=1:2)

#
# combining more than 2 features 
#

pca.fingerprint <- prcomp(tab.fingerprint)$x
plot(pca.fingerprint, col=classes, pch=classes)
legend('topright', legend=levels(metadata$from), pch=1:2, col=1:2)

#
# how many words make a good fingerprint?
#  -- this is a good time to hit "Clear All" in the plots pane

for (n in seq(from=5, to=200, by=5)) {

  # create a 'fingerprint' of top word frequencies
  top.words <- names(corpus.word.freq[1:n])
  
  # a large table of fingerprint freqs for all samples
  tab.fingerprint <- data.frame(t(sapply(doc.word.freq, Fingerprint, tokens=top.words)))
  
  # pca on same
  pca.fingerprint <- prcomp(tab.fingerprint)$x

  # plot
  plot(pca.fingerprint, col=unclass(metadata$from), pch=unclass(metadata$from))
  title(main=paste('top', n, 'words'))
  legend('bottomleft', legend=levels(metadata$from), pch=1:2, col=1:2)  
}
