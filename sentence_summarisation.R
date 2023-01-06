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
library(purrr)
# activate klippy for copy-to-clipboard button
klippy::klippy()




# removed blanks but also complaint 13, 14 because summmariser gives an error

sample <- data_nometa %>% 
  filter(!complaint_id %in% c(18, 60, 8, 16, 39))


sentence_summariser <- function(complaint_id) {
  input <- sample$text[sample$complaint_id == complaint_id]

  top = lexRankr::lexRank(input,
                          # only 1 article; repeat same docid for all of input vector
                          docId = complaint_id,
                          # return 3 sentences
                          n = 3,
                          continuous = TRUE,
                          sentencesAsDocs = TRUE)
  print(complaint_id)
  
  return(top)
}

test <- map_dfr(sample$complaint_id, sentence_summariser) %>% 
  mutate(docId = as.numeric(docId))

output <- left_join(test, sample, by = c("docId" = "complaint_id"))

write.csv(output, "top3sentences.csv", row.names = FALSE)

top_sentence <- output %>% 
  group_by(docId) %>% 
  slice_max(order_by = value)


write.csv(top_sentence, "topsentences.csv", row.names = FALSE)
