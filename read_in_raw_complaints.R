library(tidyverse)
library(tools)
library(pdftools)
library(msgxtractr)
library(qdapTools)

# Set path to sample cases
set_path <- "C:\\Users\\emercado\\OneDrive - Ofsted\\September 2022\\Peer on peer abuse complaints\\Sample cases\\Sample cases\\"
  
file_info <- data.frame(filenames = c(list.files(path = set_path)), 
                        extensions = file_ext(c(list.files(path = set_path)))) %>% 
  mutate(row_complaint_id = row_number())

# function to read in pdf

pdf_reader <- function(complaint_row_number) {
  complaint_text = pdf_text(pdf = paste0(set_path, file_info$filenames[complaint_row_number]))
  complaint_text = paste(unlist(complaint_text), collapse = " ### ")
}


# filter for pdf complaints only
pdf_files <-  file_info %>% filter(extensions == "pdf")

# Read in pdf complaints text as a data frame

pdfcomplaints_text <- data.frame(complaint = pdf_files$filenames,
                                 complaint_id = pdf_files$row_complaint_id)

pdf_text_summary <- left_join(pdfcomplaints_text,
                              data_frame(text = sapply(pdf_files$row_complaint_id, pdf_reader),
                                         complaint_id = pdf_files$row_complaint_id),
                              by = c("complaint_id")) 

# Read in msg files text as data frame ------------------------------------

# function to read in msg files
msg_reader <- function(complaint_row_number) {
  complaint_text = read_msg(path = paste0(set_path, file_info$filenames[complaint_row_number]))
  
  df_msg_text <- data.frame(text = 
                              subset(complaint_text[["body"]][["text"]], nchar(as.character(complaint_text[["body"]][["text"]]))>2))
  
  df_msg_text$text <- as.character(df_msg_text$text)
 # Encoding(df_msg_text$text) <- "UTF-8"
  return(df_msg_text)
}

# filter msg complaints only
msg_files <-  file_info %>% filter(extensions == "msg")

msgcomplaints_text <- data.frame(complaint = msg_files$filenames,
                                 complaint_id = msg_files$row_complaint_id)

msg_text_summary <- left_join(msgcomplaints_text, 
                              data_frame(text = sapply(msg_files$row_complaint_id, msg_reader), 
                                         complaint_id = msg_files$row_complaint_id), 
                              by = c("complaint_id")) #%>% 
  

msg_text_summary$text <- unlist(msg_text_summary$text)


# Read in docx complaints -------------------------------------------------

docx_files <- file_info %>% filter(extensions == "docx")

# Function to read in docx complaints
docx_reader <- function(complaint_row_number) {
  complaint_text = read_docx(file = paste0(set_path, file_info$filenames[complaint_row_number]))
  
  df_docx_text <- data.frame(complaint = file_info$filenames[complaint_row_number],
                             complaint_id = complaint_row_number, 
                            text = complaint_text) %>% 
    group_by(complaint, complaint_id) %>% 
    summarise(text = paste(text, collapse = " ### "))

  return(df_docx_text)
}

docxcomplaints_text <- data.frame(complaint = docx_files$filenames,
                                 complaint_id = docx_files$row_complaint_id)

docx_text_summary <- sapply(docx_files$row_complaint_id, docx_reader) %>%
  t(.) %>% 
  as.data.frame(.) %>% 
  mutate(complaint_id = as.numeric(complaint_id)) #%>% 
  
docx_text_summary$complaint <- unlist(docx_text_summary$complaint) 
docx_text_summary$text <- unlist(docx_text_summary$text)


all_complaints <- bind_rows(pdf_text_summary, msg_text_summary, docx_text_summary) 

# Resolves issues with apostrophes etc. 
Encoding(all_complaints$text) <-  "UTF-8"
  
write.csv(all_complaints, 
            "C:\\Users\\emercado\\OneDrive - Ofsted\\September 2022\\Peer on peer abuse complaints\\Sample cases\\raw_complaints_text.csv", row.names = FALSE)

