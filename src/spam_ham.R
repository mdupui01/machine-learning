library(tm)
library(ggplot2)

spam.path <- "/Users/marcdupuis/src/machine_learning/data/ML_for_Hackers-master/03-Classification/data/spam/"
spam2.path <- "/Users/marcdupuis/src/machine_learning/data/ML_for_Hackers-master/03-Classification/data/spam_2/"
easyham.path <- "/Users/marcdupuis/src/machine_learning/data/ML_for_Hackers-master/03-Classification/data/easy_ham/"
easyham2.path <- "/Users/marcdupuis/src/machine_learning/data/ML_for_Hackers-master/03-Classification/data/easy_ham_2/"
hardham.path <- "/Users/marcdupuis/src/machine_learning/data/ML_for_Hackers-master/03-Classification/data/hard_ham/"
hardman.path2 <- "/Users/marcdupuis/src/machine_learning/data/ML_for_Hackers-master/03-Classification/data/hard_ham_2/"

get.msg <- function(path) {
  con <- file(path, open='rt', encoding="latin1")
  text <- readLines(con)
  msg <- text[seq(which(text == "")[1]+1, length(text),1)]
  close(con)
  return(paste(msg, collapse="\n"))
}

spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds")]
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path,p,sep="")))

spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)), stringsAsFactors=FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i) {length(which(spam.matrix[i,] >0))/ncol(spam.matrix)})
spam.density <- spam.df$frequency/sum(spam.df$frequency)

spam.df <- transform(spam.df, density=spam.density,
                     occurrence=spam.occurrence)

