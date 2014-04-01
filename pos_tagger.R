# create annotators

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_token_annotator <- Maxent_POS_Tag_Annotator()

selectPOS <- function(x, pos = c('NN', 'NNS', 'NNP', 'NNPS')) {
  ifelse(x$features[[1]]$POS %in% pos, TRUE, FALSE)
}

extractTokensTags <- function(text, ann) {
  tok <- substr(text, ann$start, ann$end)
  pos <- ann$features[[1]]$POS
  
  return(list(token=tok, pos=pos))
}

extractTokensTagsFromFile <- function(file) {
  # read a file and return a vector of noun tokens
  cat(paste('annotate: reading', file, '\n'), file=stderr())
  
  # read the file line by line
  text <- scan(file, encoding='utf8', sep='\n', what='character')
  
  # paste the lines together
  text <- paste(text, collapse=' ')
  
  # annotate
  cat('annotate: identifying sentences\n', file=stderr())
  ann <- annotate(text, sent_token_annotator)
  
  cat('annotate: identifying words\n', file=stderr())
  ann <- annotate(text, word_token_annotator, ann)
  
  cat('annotate: pos tagging\n', file=stderr())
  ann <- annotate(text, pos_token_annotator, ann)
  
  # extract word tokens with pos tags
  cat('annotate: extracting tokens with pos tags\n', file=stderr())
  tokens <- sapply(ann[ann$type=='word'], function(x) {extractTokensTags(text=text, ann=x)})
  tokens <- t(tokens)
  
  return(tokens)
}
