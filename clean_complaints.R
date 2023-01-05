source("read_in_raw_complaints.R")
library(janitor)

data <- all_complaints


data <- clean_names(data)


# transform feedback to lower case
data$text <- char_tolower(data$text)


# Additional cleanup: complaints about schools forms ----------------------

data_no_form_info <- data %>% 
  mutate(text = gsub(pattern = 
                       "([0-9])([0-9])(\\/)([0-9])([0-9])(\\/)(20)([0-9])([0-9])((.|\\n)*)(cention suite)((.|\\n)*)(please provide a summary of your complaint)",
                     replacement = "", text, perl = TRUE)) %>% 
  # get rid of all text on form below this line:
  mutate(text = gsub(pattern = 
                       "(please confirm that you understand and agree to the above.)(.|\\n)*",
                     replacement = "", text, perl = TRUE)) %>% 
  # for tidying forms 
  mutate(text = gsub(pattern = 
                       "what action have you taken and what was the response\\?",
                     replacement = "", text, perl = TRUE)) %>% 
  
  # This is specific cleanup for CAS-387032 which is laid out in an unusual way
  mutate(text = gsub(pattern = "([0-9])([0-9])(\\/)([0-9])([0-9])(\\/)(20)([0-9])([0-9])((.|\\n)*)(cention suite)((.|\\n){0,1000})(hi katherine)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern = "([0-9])([0-9])(\\/)([0-9])([0-9])(\\/)(20)([0-9])([0-9])(\\s)(cention suite)((.|\\n)*)(kind regards)((.|\\n)*)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern = ">*",
                     replacement = "", text, perl = TRUE)) %>% 
  # To clean special case of CAS-417720
  mutate(text = gsub(pattern = "((.|\\n)*)(cv22 7jt)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern = "(yes i give permission)((.|\\n)*)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern = "(https:\\/\\/ofsted.cust.cention.se)((.|\\n)*)(\\/false)",
                     replacement = "", text, perl = TRUE)) %>% 
  # Special case 452900
  mutate(text = gsub(pattern = "(\\s*)(kent case management ltd)((.|\\n)*)(recolo uk\\nlimited)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern = "(yours sincerely)((.|\\n)*)(kent case)((.|\\n)*)(page 2 of 2)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern = "(kent case management)((.|\\n)*)(page )([1-9])(of [1-9])",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern = "(1\\) ofsted will share information)(.|\\n)*", 
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern = "(https:\\/\\/ofsted.cust)(.|\\n)*(cention workflow)",
                     replacement = "", text, perl = TRUE))



write.csv(data_no_form_info, "test.csv", row.names = FALSE) 



# Additional cleanup .msg text --------------------------------------------
data_msg_clean <- data_no_form_info %>% 
  mutate(text = gsub(pattern =
                       "(.|\\n)*(to:)((.|\\n){0,100})(@ofsted.gov.uk)(.*)(\\n)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern =
                       "(<http:\\/\\/ofsted)(.|\\n)*(please provide a summary of your complaint)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern =
                       "(\\s*)(subject:)((.|\\n)*)(<http)(.|\\n)*(add your comments below)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern =
                       "(subject:)([^\\t|\\n]+)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern =
                       "(<http:\\/\\/ofsted-contacts.)(.|\\n)*(copyright)",
                     replacement = "", text, perl = TRUE)) %>% 
  
  mutate(text = gsub(pattern =
                       "(website: www.gov.uk)(.|\\n)*(question:)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern =
                       "(get outlook for )(.|\\n)*",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern =
                       "(<https:\\/\\/eur03.safelinks)(.|\\n)*(=0)",
                     replacement = "", text, perl = TRUE)) %>%
  mutate(text = gsub(pattern = "(for windows 10)",
                     replacement = "", text, perl = TRUE)) %>%
  mutate(text = gsub(pattern = "(for windows 10)",
                     replacement = "", text, perl = TRUE)) %>%
  mutate(text = gsub(pattern = "(-{2,100})|(_{2,100})",
                     replacement = "", text, perl = TRUE))



write.csv(data_msg_clean, "test_msg.csv", row.names = FALSE) 


# Additional cleansing: docx ----------------------------------------------
data_doc_clean <- data_msg_clean %>%
  mutate(text = gsub(pattern =
                       "(.|\\n)*(risks and protective factors ###)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern =
                       "(### advice provided ###)(.|\\n)*",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern =
                       "(hi.*,)(\\s###)",
                     replacement = "", text, perl = TRUE)) %>% 
  mutate(text = gsub(pattern =
                       "(best regards| yours sincerely)( ###)(.|\\n)*",
                     replacement = "", text, perl = TRUE))





#

# Remove 'metadata' from the start of complaint text ----------------------
# These are lines that are replicated in several samples that summarise category of concern, whether it is a qualifying complaint etc.
# Also some about the outcome of the complaint eg retained for inspection

data_nometa <- data_doc_clean %>% 
  mutate(text = gsub(pattern = "(this is a qualifying )(complaint|notification)(\\s)(and it must be considered as part of the next inspection. therefore, you must contact the complaints about schools team in line with our guidance for all information.)",
                     replacement = "", text)) %>%
  mutate(text = gsub(pattern = "this complaint contained aspects of a safeguarding nature therefore you will also require information in relation to the outcome of this.",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "(category\\s|cat\\s|cas\\s)([0-9].[0-9])(.*?)(safeguarding)",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "(category\\s|cat\\s|cas\\s)([0-9])(.*?)(concerns)",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "(category\\s)([0-9])(.*?)(safeguarding)",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "(linked to cas-)([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "(11a)(.*?)(inspection|nfd|retained)",
                     replacement = "", text)) %>% 
  # Date removal
  mutate(text = gsub(pattern = "([0-9])([0-9])(\\/|\\.)([0-9])([0-9])(\\/|\\.)(20)([0-9])([0-9])",
                     replacement = "", text)) %>%
  mutate(text = gsub(pattern = "([0-9])([0-9])(\\/|\\.)([0-9])([0-9])(\\/|\\.)([0-9])([0-9])",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "([0-9])(\\/|\\.)(([0-9])([0-9])|([0-9]))(\\/|\\.)(20)([0-9])([0-9])",
                     replacement = "", text)) %>%
  mutate(text = gsub(pattern = "([0-9])(\\/|\\.)(([0-9])([0-9])|([0-9]))(\\/|\\.)([0-9])([0-9])",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "(rd)(\\s)(decision:)(.*?)(\\.)",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "this has been scheduled for",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "(aiver|waiver|avier)(agreed)",
                     replacement = "", text)) %>% 
  mutate(text = gsub(pattern = "(declaration\\n)(.|\\n)*(1\\) ofsted)(.|\\n)*",
                     replacement = "", text)) %>% 
  ## Abbreviations
  # ht = 'headteacher'
  mutate(text = gsub(pattern = "(\\bht\\b)",
                     replacement = "headteacher", text)) %>% 
  # sg = 'safeguarding' so replace abbreviation with this so can be counted together
  mutate(text = gsub(pattern = "(\\bsg\\b)",
                     replacement = "safeguarding", text)) %>% 
  mutate(text = ifelse(complaint == "CAS-453368.docx", 
                       gsub(pattern = "(\\bd\\b)", replacement = "", text), text)) %>% 
  mutate(text = ifelse(complaint == "CAS-453368.docx", 
                       gsub(pattern = "(\\bgd\\b)", replacement = "granddaughter", text), text)) %>% 
  mutate(text = ifelse(complaint == "CAS-453368.docx", 
                       gsub(pattern = "(\\bag\\b)", replacement = "another girl", text), text)) 



