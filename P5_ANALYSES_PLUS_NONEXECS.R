library(car)
# Hello, world
input <- read.csv("full_geomerged_df_5.csv")
brazil_df <- input





hey <- glm(bef_message_bool 
           ~ log(new_idhm)
           + freight_issue_bool
           #+ new_urbanity
           + top2box
           + review_sent_wknd
           + udh_indicator
           + item_count
           + max_price
           + search_goods
           + experience_goods
           + intimate_goods
           + south
           + north
           + southeast
           + northeast
           + y_2016
           + y_2017
           , 
           data = brazil_df, 
           family = binomial(link = "logit")
           )
summary(hey)
vif(hey)

summary(lm(new_idhm ~ new_young_ratio, data = brazil_df))

summary(lm(new_urbanity ~ freight_issue_bool, data = brazil_df))


summary(lm(new_urbanity ~ freight_issue_bool, data = brazil_df))

summary(glm(freight_issue_bool ~ new_idhm, 
            data = brazil_df, 
            family = binomial(link = "logit")))


(udh_indicator is maybe a good swap for urbanity)