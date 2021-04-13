# USED files:
#            full_geomerged_df.csv (from part 1: internal merge)
#            lemmatized.csv (from Jupyter "spacen met spacy" SpaCy)
#            stopwords.txt
# 
# WRITE files:
#            for_spacy.csv 
#            full_geomerged_df_2.csv (for part 3)

library(tidytext)
library(dplyr)
library(qdap)




input <- read.csv("full_geomerged_df.csv")

brazil_df <- input
sanity_brazil <- input

brazil_df <- brazil_df %>%
  mutate(
    review_comment_message = as.character(review_comment_message),
    review_comment_title = as.character(review_comment_title)
  )

# --------------------------------- #
# Pre-Spacy Sentence-level cleaning #
# --------------------------------- #

# For message
brazil_df  <- brazil_df %>%
  mutate(
    # Remove punctuation 
    review_comment_message = gsub("[[:punct:]]+"," ", review_comment_message),
    # Remove digits
    review_comment_message = gsub("[[:digit:]]+"," ", review_comment_message),
    # remove double characters except ss and rr
    review_comment_message = gsub("([a-q t-z])\\1+", "\\1", 
                                  review_comment_message, 
                                  perl = TRUE),
    # Get rid of line break strings
    review_comment_message = gsub("\r?\n|\r", " ", review_comment_message)
  )

# for title
brazil_df  <- brazil_df %>%
  mutate(
    # Remove punctuation 
    review_comment_title = gsub("[[:punct:]]+"," ", review_comment_title),
    # Remove digits
    review_comment_title = gsub("[[:digit:]]+"," ", review_comment_title),
    # remove double characters except ss and rr
    review_comment_title = gsub("([a-q t-z])\\1+", "\\1", 
                                review_comment_title, 
                                perl = TRUE),
    # Get rid of line break strings
    review_comment_title = gsub("\r?\n|\r", " ", review_comment_title)
  )

# Remove extremely short comments except for "ok"
brazil_df <- brazil_df %>%
  mutate(
    review_comment_message = ifelse(
      nchar(review_comment_message) < 3 & review_comment_message != "ok",
      "",
      review_comment_message)
  )

# Convert to lower
brazil_df <- brazil_df %>%
  mutate(
    review_comment_message = tolower(review_comment_message),
    review_comment_title = tolower(review_comment_title)
  )

# Record comment length before transformations
brazil_df <- brazil_df %>%
  mutate(
    # total characters in the string
    bef_nchar = nchar(brazil_df$review_comment_message),
    # total number of separations (words) in the string 
    bef_nwords = lengths(strsplit(brazil_df$review_comment_message, " ")),
    # average number of letters per word 
    nchar_perword = nchar(brazil_df$review_comment_message) / lengths(strsplit(brazil_df$review_comment_message, " "))
  )

# write for "Spacen met Spacy.ipynb"
to_write <- brazil_df %>%
  select(review_id, review_comment_message)

write.csv(to_write, "for_spacy.csv", row.names = FALSE)

# ------------------- #
# Post-SpaCy cleaning #
# ------------------- #

# Load data from "Spacen met Spacy.ipynb"
lemmatized <- read.csv('lemmatized.csv')

# Create index column that will help with merge
indiced_gsp <- brazil_df
indiced_gsp$index <- c(0: (nrow(indiced_gsp) - 1))

# Merge by index
brazil_df <- merge(indiced_gsp, lemmatized, 
                          by.x = 'index',
                          by.y = 'index',
                          all.x = TRUE)

# Get rid of unnecessary columns
brazil_df <- brazil_df %>%
  mutate(review_comment_message = sentence) %>%
  select(
    - X,
    - sentence,
    - index
  )

# Turn into string again
brazil_df <- brazil_df %>%
  mutate(
    review_comment_message = as.character(review_comment_message),
    review_comment_title = as.character(review_comment_title)
  )

# standardize comments to ASCII encoding representation
brazil_df <- brazil_df %>%
  mutate(
    # Convert message
    review_comment_message = iconv(
      review_comment_message, 
      to = "ASCII//TRANSLIT"),
    # Convert title
    review_comment_title = iconv(
      review_comment_title,
      to = "ASCII//TRANSLIT")
  )

# tidy text format shows word frequencies
text_df <- tibble(line = 1:nrow(brazil_df), 
                  text = as.character(brazil_df$review_comment_message))

new_text_df <- text_df %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)


# ---------------------------------------- #
# individual word-level cleaning: prep dic #
# ---------------------------------------- #

dic <- vector()

# add word length column, needed for filtering super short words and stuff
new_text_df <- new_text_df %>%
  mutate(
    word_length = nchar(word)
  )

# df to capture the words
super_short_words <- new_text_df %>%
  filter(word_length < 3) 
# add to dic
dic <- c(dic, super_short_words$word)
# Moving forward, we can filter these out of the tidy df
new_text_df <- new_text_df %>%
  filter(!word_length < 3) 

# Words like "adoreiiiiiiiiiiii"
weird_words <- new_text_df %>%
  filter(grepl('([a-z\\d])\\1\\1', word))
# add to dic
dic <- c(dic, weird_words$word)
# Moving forward, we can filter these out of the tidy df
new_text_df <- new_text_df %>%
  filter(!grepl('([a-z\\d])\\1\\1', word))

# Words with no vowels
no_vowels <- new_text_df %>%
  filter(!grepl('[aeiou]+', word))
# add to dic
dic <- c(dic, no_vowels$word)
# Moving forward, we can filter these out of the tidy df
new_text_df <- new_text_df %>%
  filter(grepl('[aeiou]+', word))


# Words with only vowels
on_vowels <- new_text_df %>%
  filter(!grepl('[^aeiou]+', word))
# add to dic
dic <- c(dic, on_vowels$word)
# Moving forward, we can filter these out of the tidy df
new_text_df <- new_text_df %>%
  filter(grepl('[^aeiou]+', word))


# external stopwords document
port_stopwords <- read.table("stopwords.txt", sep = "", header=F)
# standardize them
port_stopwords <- port_stopwords %>%
  mutate(V1 = iconv(V1, to = "ASCII//TRANSLIT"))
# Keep nao
port_stopwords <- port_stopwords %>%
  filter(!grepl('nao', V1))
# add to dic
dic <- c(dic, port_stopwords$V1)

# Keep only unique elements in dic
dic <- unique(dic)

# ----------------------------------------------- #
# individual word-level cleaning: filter with dic #
# ----------------------------------------------- #

empty_df <- as.data.frame(brazil_df$review_id)
empty_df$iterator <- c(1:nrow(empty_df))
empty_df$new_stuff <- 0 

iterator <- 1

for (i in brazil_df$review_comment_message) {
  print(paste(rm_stopwords(i, dic)[[1]], collapse = " "))
  empty_df$new_stuff[iterator] <- paste(rm_stopwords(i, dic)[[1]], collapse = " ")
  iterator = iterator + 1
}
brazil_df$review_comment_message <- empty_df$new_stuff


write.csv(brazil_df,"full_geomerged_df_2.csv", row.names = FALSE)
