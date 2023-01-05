source("read_in_raw_complaints.R")
source("clean_complaints.R")

# activate packages
library(xml2)
library(rvest)
library(lexRankr)
library(textmineR)
library(tidyverse)
library(quanteda)
library(igraph)
library(here)
# activate klippy for copy-to-clipboard button
klippy::klippy()




# removed blanks but also complaint 13, 14 because summmariser gives an error

sample <- data_nometa %>% 
  filter(!complaint_id %in% c(18, 60, 8))


sentence_summariser <- function(complaint_id) {
  top = lexRankr::lexRank(sample$text[complaint_id],
                          # only 1 article; repeat same docid for all of input vector
                          docId = complaint_id,
                          # return 3 sentences
                          n = 3,
                          continuous = TRUE,
                          sentencesAsDocs = TRUE,
                          threshold = 0.01)
                          
  
  return(top)
}


top_sentence_summary <- sapply(sample$complaint_id, sentence_summariser) %>% 
  t(.) %>% 
  as.data.frame(.)

# perform lexrank for top 3 sentences
top3sentences = lexRankr::lexRank(sample,
                                  # only 1 article; repeat same docid for all of input vector
                                  docId = 2,
                                  # return 3 sentences
                                  n = 3,
                                  continuous = TRUE, 
                                  sentencesAsDocs = TRUE)

