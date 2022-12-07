library(tidyverse)
library(stringi)
library(tm)
library(mgsub)
library(tidytext)
print("successfully load the packages")

# Read file ------
df <- read_csv('df_close_new.csv')
name <- unique(df["name"])

# Load "Stop Words" from the tidytext package
data("stop_words")

df.text <- df %>%  select(name,text_clean)
  
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(df.text$text_clean)
df.text$text_clean <- sapply(df.text$text_clean,function(row) iconv(row,"latin1","ASCII",sub = " "))
  
# Lowecase all text
df.text$text_clean <- tolower(df.text$text_clean)

# Remove numbers in the text
df.text$text_clean  <- removeNumbers(df.text$text_clean )
# Remove punctuations in the text
df.text$text_clean  <- removePunctuation(df.text$text_clean )

# make wasn't=was not, can't=can not, etc..
df.text$text_clean <- gsub("wasn[\u2019']t", "was not", df.text$text_clean)
df.text$text_clean <- gsub("won[\u2019']t", "will not", df.text$text_clean)
df.text$text_clean <- gsub("can[\u2019']t", "can not", df.text$text_clean)
df.text$text_clean <- gsub("didn[\u2019']t", "did not", df.text$text_clean)
df.text$text_clean <- gsub("don[\u2019']t", "do not", df.text$text_clean)
df.text$text_clean <- gsub("I[\u2019']m", "I am", df.text$text_clean)
df.text$text_clean <- gsub("[\u2019']ve", " have", df.text$text_clean) 
df.text$text_clean <- gsub("[\u2019|']s", "", df.text$text_clean)
df.text$text_clean <- gsub("[\u2019']re", " are", df.text$text_clean)
df.text$text_clean <- gsub("[\u2019']ll", " will", df.text$text_clean)
  
# If you view common typos during your analysis, fix them here.
df.text$text_clean<- gsub("canceling", "cancelling", df.text$text_clean)
df.text$text_clean <- gsub("cancellation", "cancelling", df.text$text_clean)
  
# Fix Negations
# Create a list to identify the sentiment shifters in the text
negation.words <- c("not",
                      "no",
                      "without",
                      "never",
                      "bad",
                      "none",
                      "never",
                      "nobody",
                      "nowhere",
                      "neither",
                      "nothing"
)
  
# Run the following to view Shifted sentiments sorted by polarity point
shifted.words <- df.text %>% 
  unnest_tokens(bigram, text_clean, token = "ngrams", n = 2) %>%  
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% negation.words & !word2 %in% stop_words$word)%>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word")) %>%
  mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
  mutate(score = sentiment * n) %>%
  mutate(word2 = reorder(word2, score))
  
# Pick the most effective sentiment shifters
negated.phrases <- c("never bad", 
                     "never disappointed",
                     "never issue",
                     "never disappoints",
                     "nothing fancy",
                     "nothing wrong",
                     "never fails",
                     "without doubt",
                     "never disappoint",
                     "bad bad",
                     "nothing bad",
                     "nothing spectacular",
                     "never recommend",
                     "not bad",
                     "not waste",
                     "not worth",
                     "not bother",
                     "not impressed",
                     "not fast",
                     "not clean",
                     "not fresh"
)


  
# Find synonyms for the phrases above to replace
synonyms <- c("good",
              "satisfied",
              "satisfied",
              "satisfied",
              "boring",
              "satisfied", 
              "satisfied",
              "good",
              "satisfied",
              "good",
              "good",
              "normal",
              "bad",
              "good",
              "satisfied",
              "expensive",
              "satisfied",
              "boring",
              "slow",
              "messed",
              "stale"
)
  
# Replace the negations with their synonyms.  
df.text <- mgsub(df.text$text_clean, negated.phrases, synonyms) %>%
  dplyr::as_data_frame() %>%
  rename(text = value)
  
df.text["name"] = df["name"]
#df.text["business_id"] = df["business_id"]
#df.text["stars_x"] = df["stars_x"]
#df.text["stars_y"] = df["stars_y"]
write.csv(df.text, "cleaned_reviews.csv", row.names = FALSE)

# if you want to ignore words that are frequent but doesn't help, add them to this list.
ignore.words <- data_frame(word = c("service","sauce","food", "time", "chicken","fries", "im","burger","sandwich","staff","location"))
  
# create the words freq table
word.freq.table<- df.text %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
  

