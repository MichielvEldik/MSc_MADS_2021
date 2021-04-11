# USED files:
#            full_geomerged_df_2.csv (from part 2: Lemma)
# 
# WRITE files:
#            full_geomerged_df_3.csv (for part 4)

library(lubridate)
library(dplyr)


input <- read.csv("full_geomerged_df_2.csv")
geo_sao_paolo_df <- input


# --------- #
# Date data #
# --------- #

# To date format
geo_sao_paolo_df <- geo_sao_paolo_df %>%
  mutate(
    review_creation_date = as.Date(review_creation_date,
                                      format = "%Y-%m-%d %H:%M:%S"),
    review_answer_timestamp = as.Date(review_answer_timestamp,
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_purchase_timestamp = as.Date(order_purchase_timestamp, 
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_approved_at = as.Date(order_approved_at,
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_delivered_carrier_date = as.Date(order_delivered_carrier_date,
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_delivered_customer_date = as.Date(order_delivered_customer_date,
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_estimated_delivery_date = as.Date(order_estimated_delivery_date,
                                      format = "%Y-%m-%d %H:%M:%S")
        )

# derived variables
geo_sao_paolo_df <- geo_sao_paolo_df %>%
  mutate(
    diff_est_deliv = order_estimated_delivery_date - order_delivered_customer_date,
    diff_pur_est  = order_estimated_delivery_date - order_purchase_timestamp,
    diff_pur_deliv = order_delivered_customer_date - order_purchase_timestamp,
    diff_rev_crea_ans = review_creation_date - review_answer_timestamp,
    diff_rev_est_ans = order_estimated_delivery_date - review_answer_timestamp,
    diff_rev_deliv_ans = order_delivered_customer_date - review_answer_timestamp
        )

geo_sao_paolo_df <- geo_sao_paolo_df %>%
  mutate(
    # show weekday of a date
    review_sent_dow = wday(review_creation_date, label = TRUE),
    review_answer_dow = wday(review_answer_timestamp, label = TRUE),
    # Dummy variable weekend yes/no
    review_sent_wknd = ifelse(
      review_sent_dow == 'zo' | review_sent_dow == 'za', 1, 0),
    review_answer_wknd = ifelse(
      review_answer_dow == 'zo' | review_answer_dow == 'za', 1, 0)
        )

geo_sao_paolo_df<- geo_sao_paolo_df %>%
  mutate(
    y_2016 = ifelse(year(order_purchase_timestamp) == '2016', 1, 0),
    y_2017 = ifelse(year(order_purchase_timestamp) == '2017', 1, 0),
    y_2018 = ifelse(year(order_purchase_timestamp) == '2016', 1, 0)
        )


# ----------------------------- #
# Message and title derivatives #
# ----------------------------- #


# Create dummy for message vs no message
geo_sao_paolo_df <- geo_sao_paolo_df %>%
  mutate(review_comment_message = as.character(review_comment_message)) %>%
  mutate(message_length = nchar(review_comment_message)) %>%
  mutate(message_bool = ifelse(message_length == 0, 0, 1)) %>%
  mutate(message_bool = as.integer(message_bool))

# Create a dummy variable for title message
geo_sao_paolo_df <- geo_sao_paolo_df %>%
  mutate(review_comment_title = as.character(review_comment_title)) %>%
  mutate(title_length = nchar(review_comment_title)) %>%
  mutate(title_bool = ifelse(title_length == 0, 0, 1)) %>%
  mutate(title_bool = as.integer(title_bool))

# Create a dummy variable for title OR, AND message
geo_sao_paolo_df <- geo_sao_paolo_df %>%
  mutate(title_or_message = ifelse(message_bool == 1 | title_bool == 1, 1, 0)) %>%
  mutate(title_and_message = ifelse(message_bool == 1 & title_bool == 1, 1, 0))

# Top2box transformation
geo_sao_paolo_df <- geo_sao_paolo_df %>%
  mutate(top2box = ifelse(review_score > 3, 1, 0))


# ----------------------------- #
#      Product derivatives      #
# ----------------------------- #

# Work in progress! 
table(geo_sao_paolo_df$product_category_name)

product_cats <- geo_sao_paolo_df %>%
  group_by(product_category_name) %>%
  summarise(freq = n())



# ----------------- #
# customer history  #
# ----------------- #

# Work in progress



# ---------- #
# Write file #
# ---------- #

write.csv(geo_sao_paolo_df, "full_geomerged_df_3.csv")




