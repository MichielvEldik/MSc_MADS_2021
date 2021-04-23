# USED files:
#            full_geomerged_df_2.csv (from part 2: Lemma)
# 
# WRITE files:
#            full_geomerged_df_3.csv (for part 4)

library(lubridate)
library(dplyr)


input <- read.csv("full_geomerged_df_2.csv")
brazil_df <- input


# --------- #
# Date data #
# --------- #

# To date format
brazil_df <- brazil_df %>%
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

brazil_df <- brazil_df %>%
  mutate(
    diff_est_deliv = order_estimated_delivery_date - order_delivered_customer_date,
    diff_pur_est  = order_estimated_delivery_date - order_purchase_timestamp,
    diff_pur_deliv = order_delivered_customer_date - order_purchase_timestamp,
    diff_rev_crea_ans = review_creation_date - review_answer_timestamp,
    diff_rev_est_ans = order_estimated_delivery_date - review_answer_timestamp,
    diff_rev_deliv_ans = order_delivered_customer_date - review_answer_timestamp
        )

brazil_df <- brazil_df %>%
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

brazil_df<- brazil_df %>%
  mutate(
    y_2016 = ifelse(year(order_purchase_timestamp) == '2016', 1, 0),
    y_2017 = ifelse(year(order_purchase_timestamp) == '2017', 1, 0),
    y_2018 = ifelse(year(order_purchase_timestamp) == '2016', 1, 0)
        )


# ----------------------------- #
# Message and title derivatives #
# ----------------------------- #


# Create dummy for message vs no message
brazil_df <- brazil_df %>%
  mutate(review_comment_message = as.character(review_comment_message)) %>%
  mutate(message_length = nchar(review_comment_message)) %>%
  mutate(message_bool = ifelse(message_length == 0, 0, 1)) %>%
  mutate(message_bool = as.integer(message_bool))

brazil_df <- brazil_df %>%
  mutate(message_bool = ifelse(is.na(message_bool) == TRUE, 0, message_bool))

# Create a dummy variable for title message
brazil_df <- brazil_df %>%
  mutate(review_comment_title = as.character(review_comment_title)) %>%
  mutate(title_length = nchar(review_comment_title)) %>%
  mutate(title_bool = ifelse(title_length == 0, 0, 1)) %>%
  mutate(title_bool = as.integer(title_bool))

# Create a dummy variable for title OR, AND message
brazil_df <- brazil_df %>%
  mutate(title_or_message = ifelse(message_bool == 1 | title_bool == 1, 1, 0)) %>%
  mutate(title_and_message = ifelse(message_bool == 1 & title_bool == 1, 1, 0))

# Top2box transformation
brazil_df <- brazil_df %>%
  mutate(top2box = ifelse(review_score > 3, 1, 0))


# ----------------------------- #
#      Product derivatives      # ---- (in progress) -----------------------
# ----------------------------- #

# Work in progress! 
table(brazil_df$product_category_name)





# Taxonomy Donal Vitaliano, 2007
search_goods <- c("furniture_bedroom",
                  "furniture_living_room",
                  "furniture_bedroom",
                  "office_furniture",
                  
                  )
experience_goods <- c("auto",
                      "food",
                      "food_drink",
                      "home_appliances",
                      "home_appliances_2",
                      )

credence_goods <- c("health_beauty",
                    "")


product_cats <- brazil_df %>%
  group_by(product_category_name) %>%
  summarise(freq = n(),
            mes_ratio = mean(message_bool),
            mean_price = mean(max_price),
            spread_sd = sd(max_price),
            char_length = mean(bef_nchar)) # this is wrong, as it takes into asccount many zeros


# ----------------- #
# Regions variables #
# ----------------- #

c_north <- c("AC","AP","AM","PA", "RO", "RR", "TO")
c_south <- c("SC", "RS", "PR")
c_southeast <- c("SP", "RJ", "MG", "ES")
c_northeast <- c("AL", "BA", "CE", "MA", "RN", "SE", "PI", "PB", "PE")
c_centerwest <- c("MT", "MS", "GO", "DF")

brazil_df$north <- 0
brazil_df$south <- 0
brazil_df$southeast <- 0
brazil_df$northeast <- 0
brazil_df$centerwest <- 0

brazil_df <- brazil_df %>% 
  mutate(north = ifelse(customer_state %in% c_north, 1,0),
         south = ifelse(customer_state %in% c_south, 1,0),
         southeast = ifelse(customer_state %in% c_southeast, 1,0),
         northeast = ifelse(customer_state %in% c_northeast, 1,0),
         centerwest = ifelse(customer_state %in% c_centerwest, 1,0),
        )

# ----------------- #
# customer history  #
# ----------------- #

# Work in progress

# ---------- #
# Write file #
# ---------- #

write.csv(brazil_df, "full_geomerged_df_3.csv")




