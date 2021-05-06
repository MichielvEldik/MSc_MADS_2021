library(car)
library(dplyr)
library(sampleSelection)
library(sjstats)
library(ggplot2)

library(lme4)


# Load data ------------------------------------------------------------------ #
input <- read.csv("full_geomerged_df_5.csv")
brazil_df <- input


# Descriptives continuous ---------------------------------------------------- #

# New_IDHM
# --------
idhm_histogram <- ggplot(
  data = brazil_df,
  aes(new_idhm)) +
  geom_histogram(bins = 100)
plot(idhm_histogram)

ggplot(brazil_df) +
  aes(x = "", y = new_idhm) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# New_urbanity
# ------------
urban_histogram <- ggplot(
  data = brazil_df,
  aes(new_urbanity)) +
  geom_histogram(bins = 100)
plot(urban_histogram)

ggplot(brazil_df) +
  aes(x = "", y = new_urbanity) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# Young ratio
# -----------
young_histogram <- ggplot(
  data = brazil_df,
  aes(new_young_ratio)) +
  geom_histogram(bins = 100)
plot(young_histogram)

ggplot(brazil_df) +
  aes(x = "", y = new_young_ratio) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# Max Price 
# ---------
maxprice_histogram <- ggplot(
  data = brazil_df,
  aes(max_price)) + 
  geom_histogram(bins = 100)
plot(maxprice_histogram)

ggplot(brazil_df) +
  aes(x = "", y = max_price) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# State counts --------------------------------------------------------------- #



# Mean centering ------------------------------------------------------------- #
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
brazil_df$new_idhm <- center_scale(brazil_df$new_idhm)
brazil_df$new_urbanity <- center_scale(brazil_df$new_urbanity)
brazil_df$new_young_ratio <- center_scale(brazil_df$new_young_ratio)
brazil_df$max_price <- center_scale(brazil_df$max_price)

# Factor type ----------------------------------------------------------------- #

cols <- c("bef_message_bool",
          "max_price_disc",
          "item_count_disc",
          "urbanity_disc",
          "freight_issue_bool",
          "review_score",
          "north",
          "northeast",
          "centerwest",
          "south",
          "southeast",
          "y_2016",
          "y_2017",
          "y_2018",
          "year",
          "top2box",
          "experience_goods",
          "search_goods",
          "intimate_goods",
          "review_sent_wknd",
          "review_answer_wknd",
          "sent_sun",
          "sent_mon",
          "sent_tue",
          "sent_wed",
          "sent_thu",
          "sent_fri",
          "sent_sat",
          "title_bool",
          "title_or_message",
          "title_and_message",
          "title_nor_message")

brazil_df[,cols] <- lapply(brazil_df[cols], function(x) as.factor(x))

# Missingness? ---------------------------------------------------------------- #
colSums(is.na(brazil_df))

brazil_df <- brazil_df %>%
  filter(order_status == "delivered")

# Formula Definition ---------------------------------------------------------- #

# Variable selection
dv_var <- "bef_message_bool"
iv_var <- c("review_score",
            "year",
            "new_idhm",
            "new_urbanity",
            "new_young_ratio",
            "region",
            "review_sent_dow",
            "item_count",
            "experience_goods",
            "intimate_goods",
            "max_price"
           )

iv_var_int <- c("1",
                "review_score",
                "year",
                "new_idhm",
                "new_urbanity",
                "new_young_ratio",
                "region"
               )

state_mei <- "(1 | customer_state)" # mixed effects on intercept 
city_mei <- "(1 | customer_city)"   # mixed effects on intercept
city_state_ram <- "(1 + new_idhm | customer_state) + (1 + new_idhm | customer_city)"
city_ram <- "(1 + new_idhm | customer_city)"
city_ram_2 <- "(1 + new_idhm + new_urbanity | customer_city)"
city_ram_3 <- "(1 + new_idhm + new_young_ratio | customer_city)"


main_city <- paste(dv_var, 
                   paste(
                     paste(iv_var_int, collapse = " + "), city_mei, 
                     sep = " + "), 
                   sep = " ~ ")

main_state <- paste(dv_var, 
                   paste(
                     paste(iv_var_int, collapse = " + "), state_mei, 
                     sep = " + "), 
                   sep = " ~ ")

main_citystate <- paste(dv_var, 
                        paste(
                          paste(iv_var_int, collapse = " + "), paste(state_mei, city_mei, sep = " + "),
                          sep = " + "), 
                        sep = " ~ ")


main_sing <- paste(dv_var, paste(iv_var, collapse =  " + "), sep = " ~ ")

random_eff_citystate <- paste(dv_var,
                              paste(
                                paste(iv_var_int, collapse = " + "),
                                city_state_ram,
                                sep = " + "
                              ), sep = " ~ ")

random_eff_city <- paste(dv_var,
                         paste(
                           paste(iv_var_int, collapse = " + "),
                           city_ram,
                           sep = " + "
                         ), sep = " ~ ")


random_eff_city_2 <- paste(dv_var,
                           paste(
                             paste(iv_var_int, collapse = " + "),
                             city_ram_2,
                             sep = " + "
                           ), sep = " ~ ")

random_eff_city_3 <- paste(dv_var,
                           paste(
                             paste(iv_var_int, collapse = " + "),
                             city_ram_3,
                             sep = " + "
                           ), sep = " ~ ")



# Define fit function --------------------------------------------------------- #

me_function <- function(formu_la, dat_a)
{
  model <- glmer(formula = formu_la,
                 family = binomial(link = "logit"),
                 data = dat_a,
                 control = glmerControl(
                   optimizer = "bobyqa", 
                   optCtrl = list(maxfun=2e5))
                )
  return(model)
}

logit_function <- function(formu_la, dat_a)
{
  model <- glm(formula = formu_la,
               data = dat_a,
               family = binomial(link = "logit")
               )
  return(model)
}


# Fit models ------------------------------------------------------------------ #

# Single models
# -------------
fit_single_logit <- glm(main_sing,
                             data = brazil_df,
                             family = binomial(link = "logit"))
summary(fit_single_logit)
AIC(fit_single_logit)

# There is no
fit_single_interactions <- glm(bef_message_bool ~
                                 region*review_score,
                               data = brazil_df,
                               family = binomial(link = "logit"))
summary(fit_single_interactions)

fit_single <- glm(bef_message_bool ~
                                 region + review_score + ,
                               data = brazil_df,
                               family = binomial(link = "logit"))
summary(fit_single)




anova(fit_single_interactions, fit_single)


fit_city <- me_function(main_city, brazil_df)
summary(fit_city)

fit_state <- me_function(main_state, brazil_df)
summary(fit_state)

fit_state_city <- me_function(main_citystate, brazil_df)
summary(fit_state_city)


random_eff <- me_function(random_eff_citystate, brazil_df)
summary(random_eff)

random_eff_city <- me_function(random_eff_city, brazil_df) # BEST SO FAR
summary(random_eff_city)


random_eff_city_2 <- me_function(random_eff_city_2, brazil_df)
summary(random_eff_city_2)

random_eff_city_3 <- me_function(random_eff_city_3, brazil_df)
summary(random_eff_city_3)

AIC(fit_city)
AIC(fit_city_state)
AIC(fit_state)


anova(fit_state, fit_city)

# One level is enough
anova(fit_state_city, fit_city)

# HDI random effect slightly better fit
anova(fit_city, random_eff_city)

# Adding urbanity does not improve  
anova(random_eff_city, random_eff_city_2)

# Adding young ratio does not improve either
anova(random_eff_city, random_eff_city_3)



# Mixed effects versus fixed effects ------------------------------------------ #



# Define train and test sets -------------------------------------------------- #

# 75% of the sample size
train_size <- floor(0.75 * nrow(brazil_df))

set.seed(777)
train_ind <- sample(seq_len(nrow(brazil_df)), size = train_size)

train <- brazil_df[train_ind, ]
test <- brazil_df[-train_ind, ]



# Validation ------------------------------------------------------------------ #

validation_function <- function(fitted_model, test_set){
  probabilities <- fitted_model %>% predict(test_set, type = "response")
  predicted.classes <- ifelse(probabilities > 0.50, 1, 0)
  predicted.classes
  
  probies <- cbind(as.character(test_set$bef_message_bool), predicted.classes)
  probies <- as.data.frame(probies)
  
  probies <- probies %>%
    mutate(true_positives = ifelse(V1 == "1" & predicted.classes == "1", 1, 0),
           true_negatives = ifelse(V1 == "0" & predicted.classes == "0", 1, 0),
           hit_rate = ifelse(true_positives == "1" | true_negatives == "1", 1, 0)
    )
  
  hit_rate <- mean(probies$hit_rate, na.rm = TRUE)
  
  return(hit_rate)
}

fit_single_logit <- glm(main_sing,
                        data = train,
                        family = binomial(link = "logit"))

hit_rate_1 <- validation_function(fit_single_logit, test)
print(hit_rate_1)  
  
random_eff_city <- me_function(random_eff_city, train)
summary(random_eff_city)
hit_rate_2 <- validation_function(random_eff_city, test)


probabilities <- random_eff_city %>% predict(test, type = "response")





probies <- probies %>%
  select(bef_message_bool, predicted.classes)

mean(predicted.classes == test$bef_message_bool)


mean(predicted.classes == test$bef_message_bool)

fitted.results <- predict(fit_single_logit,
                          newdata = test)
fitted.results
