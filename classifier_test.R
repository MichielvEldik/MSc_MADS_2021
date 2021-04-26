library(dplyr)
library(tm)
input <- read.csv("full_geomerged_df_4.csv")
brazil_df <- input

length(unique(brazil_df$review_id))
brazil_df$review_comment_message <- as.character(brazil_df$review_comment_message)


#listje <- c("receber", <- should be stemmed! 
#            "aguar",
#            "ainda",
#            "faltou",
#            "faltar",
#            "incompleto")

# use stems because we've got grepl.
listje <- c("receb",
            "aguar",
            "ainda",
            "faltou",
            "faltar",
            "incompleto",
            "nunca chegar",
            "chegar",
            "entregar",
            "nao entregar",
            "antar do prazo",
            "prazo"
            )


workable_sample <- brazil_df %>% select(review_id, 
                                        review_comment_message,
                                        message_and_title,
                                        review_score)
workable_sample$freight_issue <- 0
for (i in listje){
  workable_sample$freight_issue <- ifelse(
    grepl(i, workable_sample$message_and_title), 
    1, 
    workable_sample$freight_issue)
}
# Check out 
nonzeros <- workable_sample[workable_sample$freight_issue == 1,]
zeros <- workable_sample[workable_sample$freight_issue == 0,]

# -------------------------- #
# Naive Bayes classification #
# -------------------------- #
sms_corpus <- VCorpus(VectorSource(workable_sample$review_comment_message))
print(sms_corpus)

inspect(sms_corpus[4])

as.character(sms_corpus[[4]])







# ---------

weird_words <- brazil_df %>%
  filter(grepl('([a-z\\d])\\1\\1', review_comment_message))
