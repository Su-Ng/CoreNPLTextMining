
#read in file
textdata <- read.delim("osha.txt", header=TRUE, sep="\t", quote = "", stringsAsFactors = FALSE)
#get the movie stories
text1 <- textdata[, 3]

library(coreNLP)

initCoreNLP(type="english_fast")

getNouns = function (x) {
  tok <- getToken(annotateString(x))
  lem <- unlist(tok[startsWith(tok[, "POS"], "N"), "lemma"])
}

lemmaN <- sapply(text1, getNouns)
head(lemmaN)

save(lemmaN, file="nouns.RData")
load("nouns.RData")

library(wordnet)
setDict("C:/Program Files (x86)/WordNet/2.1/dict")

#how to get hyponyms(the children words) of a term 
hyponyms <- function(x){
  filter <- getTermFilter("ExactMatchFilter", x, TRUE)
  terms <- getIndexTerms("NOUN", 1, filter)
  synsets <- getSynsets(terms[[1]])
  related <- tryCatch(
    getRelatedSynsets(synsets[[1]], "~"),
    error = function(condition) {
      if (condition$message == "RcallMethod: invalid object parameter")
        message("No direct hyponyms found")
      else
        stop(condition)
      return(NULL)
    }
  )
  if (is.null(related))
    return(NULL)
  return(unlist(sapply(related, getWord)))
}

hyponyms("external body part")

body_terms<-c(hyponyms("external body part"),hyponyms("body part"))

library(tm)

#create corpus using noun lemmas
vector1 <- VectorSource(lemmaN)
corpus1 <- VCorpus(vector1)
dtm <- DocumentTermMatrix(corpus1, control=list(weighting=weightBin,
                                                removeNumbers=TRUE,
                                                dictionary=body_terms,
                                                stemming=FALSE,
                                                stopwords=c("area","process","system","body")))
#get frequency of each term
freq <- colSums(as.matrix(dtm))
#sort frequency in order
sor<-sort(freq,decreasing = TRUE)
head(sor)
hyponyms("worker")
hyponyms("employee")
occupation_terms<-c(hyponyms("worker"),hyponyms("employee"))
dtm2 <- DocumentTermMatrix(corpus1, control=list(weighting=weightBin,
                                                 removeNumbers=TRUE,
                                                 dictionary=occupation_terms,
                                                 stemming=FALSE,
                                                 stopwords=c("employee")
                                                 ))

freq2 <- colSums(as.matrix(dtm2))
#sort frequency in order
sor2<-sort(freq2,decreasing = TRUE)
head(sor2)
