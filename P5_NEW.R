library(car)
library(dplyr)
library(sampleSelection)
library(sjstats)
library(ggplot2)
library(lme4)
library(tictoc)
library(GLMMadaptive)
library(glmmTMB)

# Load data ------------------------------------------------------------------ #
input <- read.csv("full_geomerged_df_5.csv")
brazil_df <- input

# Descriptives continuous (before scaling) ----------------------------------- #

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
  aes(x = "", y = log(max_price)) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

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


# Fix order 
brazil_df <- brazil_df %>%
  mutate(item_count_disc = factor(item_count_disc, 
                                  levels = c("single", "multiple", "large")))
# fix order
brazil_df <- brazil_df %>%
  mutate(hdi_class_col = factor(hdi_class_col, levels = c("low_medium", 
                                                          "high", 
                                                          "very high")))

# Get ird of stuff ------------------------------------------------------------ #

# only keep delivered ones
brazil_df <- brazil_df %>%
  filter(order_status == "delivered")

# Get rid of NA in DV
colSums(is.na(brazil_df))
brazil_df <- brazil_df %>%
  filter(!is.na(bef_message_bool))


# -- Check: how many people have more than 2? ---------------------------------# 

more_than_2 <- brazil_df %>% 
  group_by(customer_unique_id) %>% 
  summarise(count = n())

more_than_2 <- more_than_2 %>% 
  mutate(two_or_one = ifelse(count >2, 0, 1), 
         one = ifelse(count == 1, 1, 0))

mean(more_than_2$two_or_one)
mean(more_than_2$one)

# Data Discriptives  -------------------------------------------------------- #

ggplot(data  = brazil_df,
       aes(x = region,
           y = new_idhm)) + 
  geom_violin() + 
  stat_summary(new_idhm=mean, 
               geom="point", 
               shape=23, 
               size=2)




mean(brazil_df$bef_message_bool)




ggplot(data  = brazil_df,
       aes(x = region,
           y = log(new_young_ratio))) + geom_violin()

ggplot(data  = brazil_df,
       aes(x = hdi_class,
           y = log(new_young_ratio))) + geom_violin()



prop.table(table(brazil_df[brazil_df$region == "north" ,]$bef_message_bool, 
      brazil_df[brazil_df$region == "north" ,]$hdi_class_col), margin = 2)


prop.table(table(brazil_df[brazil_df$region == "south" ,]$bef_message_bool, 
                 brazil_df[brazil_df$region == "south" ,]$hdi_class_col), margin = 2)

prop.table(table(brazil_df[brazil_df$region == "southeast" ,]$bef_message_bool, 
                 brazil_df[brazil_df$region == "southeast" ,]$hdi_class_col), margin = 1)

# Getting some general insights on message_bool (has to be int though)
mean(brazil_df[brazil_df$region == "southeast",]$bef_message_bool)
mean(brazil_df[brazil_df$region == "north",]$bef_message_bool)
mean(brazil_df[brazil_df$region == "south",]$bef_message_bool)
mean(brazil_df[brazil_df$region == "centerwest",]$bef_message_bool)
mean(brazil_df[brazil_df$region == "northeast",]$bef_message_bool)

mean(brazil_df[brazil_df$region == "south",]$new_idhm)
mean(brazil_df[brazil_df$region == "north",]$new_idhm)
mean(brazil_df[brazil_df$region == "centerwest",]$new_idhm)
mean(brazil_df[brazil_df$region == "southeast",]$new_idhm)

mean(as.integer(brazil_df$bef_message_bool))

# Creating the big table ------------------------------------------------------
pop <- brazil_df %>%
  group_by(region, hdi_class_col, bef_message_bool) %>%
  select(region, hdi_class_col, bef_message_bool) %>%
  summarise(n = n())

ggplot(pop, aes(fill=bef_message_bool, y=n, x=hdi_class_col)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap( ~ region, scales = "free")


ggplot(pop, aes(fill=bef_message_bool, y=n, x=hdi_class_col)) + 
  geom_bar(position="fill", stat="identity") +
  facet_wrap( ~ region, scales = "free") 

zero_low <- sum(pop[pop$hdi_class_col == "low_medium" & pop$bef_message_bool == 0,]$n)
zero_high <- sum(pop[pop$hdi_class_col == "high" & pop$bef_message_bool == 0,]$n)
zero_veryhigh <- sum(pop[pop$hdi_class_col == "very high" & pop$bef_message_bool == 0,]$n)

one_low <- sum(pop[pop$hdi_class_col == "low_medium" & pop$bef_message_bool == 1,]$n)
one_high <- sum(pop[pop$hdi_class_col == "high" & pop$bef_message_bool == 1,]$n)
one_veryhigh <- sum(pop[pop$hdi_class_col == "very high" & pop$bef_message_bool == 1,]$n)

pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA


# Add Full dist
# -------------
pop <- pop %>%
  mutate(region = as.character(region),
         hdi_class_col = as.character(hdi_class_col),
         bef_message_bool = as.character(bef_message_bool))

pop[31:36,1] <- "full"
pop[31:36,2] <- c("low_medium", "low_medium",
                  "high", "high",
                  "very high", "very high")
pop[31:36,3] <- c("0", "1",
                  "0", "1",
                  "0", "1")
pop[31:36,4] <- c(zero_low, one_low,
                  zero_high, one_high,
                  zero_veryhigh, one_veryhigh)
pop <- pop %>%
  mutate(region = as.factor(region),
         hdi_class_col = as.factor(hdi_class_col),
         bef_message_bool = as.factor(bef_message_bool))

pop <- pop %>%
  mutate(region = factor(region, levels = c("centerwest",
                                            "north",
                                            "northeast",
                                            "south",
                                            "southeast",
                                            "full")),
         hdi_class_col = factor(hdi_class_col, levels = c("low_medium",
                                                   "high",
                                                   "very high")))
to_go <- c(2,
           4,
           6,
           8,
           10,
           12,
           14,
           16,
           18,
           20,
           22,
           24,
           26,
           28,
           30,
           32,
           34,
           36)

new_vec <- rep("0", length(to_go))

counter <- 1
for (i in to_go){
  outcome <- pop[i,"n"] / (pop[i,"n"] +  pop[i-1,"n"])
  new_vec[counter] <- outcome
  counter <- counter + 1
}

round(new_vec[[2]], digits = 2)

test <- c("", paste(as.character(round(new_vec[[1]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[2]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[3]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[4]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[5]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[6]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[7]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[8]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[9]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[10]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[11]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[12]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[13]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[14]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[15]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[16]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[17]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[18]], digits = 2)), "%", sep = ""))

# Do visual again
ggplot(pop, aes(fill=bef_message_bool, y=n, x=hdi_class_col)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 3.3, aes(label = test, family = "serif"), vjust = -1) + 
  ylab("count (n)") +
  theme_bw() +
  xlab("Human Development Index Category") +
  theme(text=element_text(size=13,  family="serif")) +
  facet_wrap( ~ region, 
              scales = "free",
              labeller =labeller(region = c(
                "centerwest" = "centerwest (n = 3,537)",
                "north" = "north (n = 1,779)",
                "northeast" = "northeast (n = 8,996)",
                "south" = "south (n = 13,736)",
                "southeast" = "southeast (n = 64,714)",
                "full" = "full (n = 92,762)"))) 


# Define train and test sets -------------------------------------------------- #

# 75% of the sample size
train_size <- floor(0.75 * nrow(brazil_df))

set.seed(777)
train_ind <- sample(seq_len(nrow(brazil_df)), size = train_size)

train <- brazil_df[train_ind, ]
test <- brazil_df[-train_ind, ]

colSums(is.na(test))
colSums(is.na(train))

# Validation function definition --------------------------------------------- #

validation_function <- function(fitted_model, test_set, multilevel, probability){
  if (multilevel == 1)
  {
    probabilities <- fitted_model %>% predict(test_set, 
                                              type = "response",
                                              allow.new.levels = TRUE)
  } 
  else 
  {
    probabilities <- fitted_model %>% predict(test_set, 
                                              type = "response",
                                              allow.new.levels = FALSE)
  }
  # Perform classification based on probability
  predicted.classes <- ifelse(probabilities > probability, 1, 0)
  predicted.classes
  
  # bind 
  probies <- cbind(as.character(test_set$bef_message_bool), predicted.classes)
  probies <- as.data.frame(probies)
  
  probies <- probies %>%
    mutate(true_positives = ifelse(V1 == "1" & predicted.classes == "1", 1, 0),
           true_negatives = ifelse(V1 == "0" & predicted.classes == "0", 1, 0),
           false_positives = ifelse(V1 == "0" & predicted.classes == "1", 1, 0),
           false_negatives = ifelse(V1 == "1" & predicted.classes == "0", 1, 0),
           hit_rate = ifelse(true_positives == "1" | true_negatives == "1", 1, 0)
    )
  
  # Compute hitrate
  hit_rate <- mean(probies$hit_rate, na.rm = TRUE)
  
  return(hit_rate)
}


# Zero-Inflated Negative Binomial Mixed Effects Model ------------------------ #
# https://www.r-bloggers.com/2018/08/zero-inflated-poisson-and-negative-binomial-models-with-glmmadaptive/



gm1 <- mixed_model(bef_nchar ~ new_idhm, 
                   random = ~ 1 | customer_city, 
                   data = brazil_pos,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ new_idhm)
summary(gm1)

gm2 <- mixed_model(bef_nwords ~ new_idhm, 
                   random = ~ 1 | customer_city, 
                   data = brazil_pos,
                   family = zi.poisson(), 
                   zi_fixed = ~ new_idhm)

summary(gm2)

gm3 <- mixed_model(bef_nchar ~ new_idhm, 
                   random = ~ 1 | customer_city, 
                   data = brazil_df,
                   family = hurdle.negative.binomial(), 
                   zi_fixed = ~ new_idhm)


summary(gm3)

gm3 <- mixed_model(log(bef_nchar) ~ new_idhm, 
                   random = ~ 1 | customer_city, 
                   data = brazil_df,
                   family = hurdle.lognormal(), 
                   zi_fixed = ~ new_idhm)


summary(gm3)



hm2 <- update(gm3, zi_random = ~ 1 | customer_city)


gm4 <- mixed_model(bef_nchar ~ new_idhm, 
                   random = ~ 1 | customer_city, 
                   data = brazil_pos,
                   family = hurdle.poisson(), 
                   zi_fixed = ~ new_idhm)


summary(gm4)


anova(gm1, gm2, test = FALSE)

library(MASS)
library(pscl)
summary(m2 <- glm.nb(bef_nchar ~ new_idhm + camper, data = brazil_df))


M4 <- zeroinfl(bef_nchar ~ new_idhm |  new_idhm,
               dist = 'negbin',
               data = brazil_df)
summary(M4)

M5 <- hurdle(bef_nchar ~ new_idhm |  new_idhm,
               dist = 'negbin',
               data = brazil_pos)
summary(M5)




fit_hnbinom1 <- update(fit_zinbinom1_bs,
                       ziformula=~.,
                       data=Owls,
                       family=list(family="truncated_nbinom1",link="log"))




library(glmm)
library(glmmADMB)
library(R2admb)

# https://stats.stackexchange.com/questions/250563/r-hurdle-models-with-date-as-non-nested-random-effect
# binairy part
hurP1 <- glmmadmb(bef_nchar ~ new_idhm + (1 | customer_city), 
                  data = brazil_df, family = "binomial")

# truncated at 0 part
hurP2 <- glmmadmb(value ~ product * sex * morph + (1 | Date),
                  data = subset(data2, value > 0), family = "truncnbinom1")


# Check for linearity assumption --------------------------------------------- #

lin_assum_func <- function(variable_name){
  dv_var <- "bef_message_bool"
  iv_var <- variable_name
  form <- paste(dv_var, iv_var, sep = "~")
  
  
  
  fit_1 <- glm(bef_message_bool ~ variable_name,
               data = train, 
               family = binomial(link = "logit"))
  
  data_tjes <- fit_1$model[2]
  probies <- predict(fit_1, type =  "response")
  logies <- log(probies / (1 - probies))
  
  beidies <- cbind(data_tjes, logies)
  
  plot(beidies$variable_name ~ jitter(beidies$logies, 4))
  
}


lin_assum_func('max_price')


###

main_sing


brazil_pos <- brazil_df[brazil_df$review_score == 4 | brazil_df$review_score == 5 | brazil_df$review_score == 3,]
brazil_neg <- brazil_df[brazil_df$review_score == 1 | brazil_df$review_score == 2,]
 

  
fit_1 <- glm(bef_message_bool 
             ~ new_idhm
             + review_score
             + region
             + new_young_ratio
             + above_median
             + new_urbanity,
             data = test, 
             family = binomial(link = "logit"))

summary(fit_1)
data_tjes <- fit_1$model["new_young_ratio"]
probies <- predict(fit_1, type =  "response")
logies <- log(probies / (1 - probies))

beidies <- cbind(data_tjes, logies)

plot(beidies[,1] ~ jitter(beidies$logies, 4))




brazil_df <- brazil_df %>%
  mutate(quantiles_idhm = ntile(new_idhm, 8),
         quantiles_idhm = as.factor(quantiles_idhm))

fit_1 <- glm(bef_message_bool ~ quantiles_idhm,
             data = brazil_df, family = binomial(link = "logit"))




summary(fit_1)
cofs <- summary(fit_1)$coefficients[2:8,1]
plot(cofs)


# try glmer
model <- glmer(bef_message_bool 
               ~ new_idhm
               + region
               + new_young_ratio
               + above_median
               + review_score
               + (1 | customer_city),
               family = binomial(link = "logit"),
               data = brazil_pos,
               control = glmerControl(
                 optimizer = "bobyqa", 
                 optCtrl = list(maxfun=2e5)))

summary(model)

data_tjes <- fit_1$model["new_young_ratio"]


model <- glmer(bef_message_bool 
               ~ new_idhm
               + region
               + new_young_ratio
               + above_median
               + review_score
               + intimate_goods
               + experience_goods
               + (1 | customer_city),
               family = binomial(link = "logit"),
               data = brazil_neg,
               control = glmerControl(
                 optimizer = "bobyqa", 
                 optCtrl = list(maxfun=2e5)))

summary(model)


# Does hdi depend on review_score? 

prop.table(table(brazil_df$review_score, brazil_df$hdi_class_col), margin = 2)

pyrex <- lm(log(new_idhm) 
            ~ review_score 
            + freight_issue_bool 
            + other_issue, 
            data = brazil_df)
ols_plot_resid_qq(pyrex)
plot(pyrex)
plot(cooks.distance(pyrex))


hi <- aov(log(new_idhm) ~ review_score, data = brazil_df)
plot(hi, 2)
  
hi_bye <- aov(new_idhm ~ freight_issue_bool, data = brazil_df)  
summary(hi_bye)
  
# Mean centering ------------------------------------------------------------- #
center_scale <- function(x) {
  scale(x, scale = TRUE)
}

# apply it
brazil_df$new_idhm <- center_scale(brazil_df$new_idhm)
brazil_df$new_urbanity <- center_scale(brazil_df$new_urbanity)
brazil_df$new_young_ratio <- center_scale(brazil_df$new_young_ratio)
# brazil_df$max_price <- center_scale(brazil_df$max_price)

# Outliers ------------------------------------------------------------------- #

hist(brazil_df$max_price)

maximum_price <- brazil_df %>%
  select(max_price)


# Above median variable ------------------------------------------------------- #

# Summarise per product
cat_1 <- brazil_df %>%
  group_by(product_category_name) %>%
  summarise(mean = mean(max_price),
            median = median(max_price),
            sd = sd(max_price),
            unique_count = unique(n()))
# get rid of NA
cat_1 <- cat_1[1:71,]
# Keep only the interesting ones
cat_1 <- cat_1 %>%
  select(product_category_name,
         mean, 
         median)
# merge with original dataframe
brazil_df <- merge(brazil_df, cat_1,
                    by.x = "product_category_name",
                    by.y = "product_category_name",
                    all.x = TRUE)
# calculate derived variables 
brazil_df <- brazil_df %>%
  mutate(above_median = ifelse(max_price > median, 1, 0),
         above_median = as.factor(above_median),
         above_mean = ifelse(max_price > mean, 1, 0),
         above_mean = as.factor(above_mean))
# Check out the end product...
table(brazil_df$above_median)

str(brazil_df$above_median)


# Numeric culture ------------------------------------------------------------- #

brazil_df$individualism <- 0

brazil_df <- brazil_df %>%
  mutate(individualism = ifelse(region == "south", 6, individualism),
         individualism = ifelse(region == "southeast", 2, individualism),
         individualism = ifelse(region == "centerwest", 0, individualism),
         individualism = ifelse(region == "northeast", -1, individualism),
         individualism = ifelse(region == "north", -5, individualism))


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
            "item_count_disc",
            "experience_goods",
            "intimate_goods",
            "max_price",
            "above_median"
           )

iv_var_int <- c("1",
                "review_score",
                "year",
                "new_idhm",
                "new_urbanity",
                "new_young_ratio",
                "region",
                "experience_goods",
                "intimate_goods",
                "item_count_disc",
                "max_price",
                "review_sent_dow"
               )

state_mei <- "(1 | customer_state)" # mixed effects on intercept 
city_mei <- "(1 | customer_city)"   # mixed effects on intercept
city_state_ram <- "(1 + new_idhm | customer_state) + (1 + new_idhm | customer_city)"
city_ram <- "(1 + new_idhm | customer_city)"
city_ram_2 <- "(1 + new_idhm + new_urbanity | customer_city)"
city_ram_3 <- "(1 + new_idhm + new_young_ratio | customer_city)"

rev_score <- "(1  | customer_city) + (1 | review_score)"


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

main_sing <- paste(dv_var, 
                   paste(iv_var, collapse =  " + "), 
                   sep = " ~ ")

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

random_eff_city_4 <- paste(dv_var,
                           paste(
                             paste(iv_var_int, collapse = " + "),
                             rev_score,
                             sep = " + "
                           ), sep = " ~ ")


random_eff_basic <- paste(dv_var, rev_score, sep = " ~ " )


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

# Single model
# -------------
fit_single_logit <- glm(main_sing,
                             data = brazil_df,
                             family = binomial(link = "logit"))
summary(fit_single_logit)
AIC(fit_single_logit)





blue_jeans <- glm(bef_message_bool 
            ~ region
            + review_score
            + new_idhm
            + year
            + new_urbanity
            + new_young_ratio
            + experience_goods
            + intimate_goods
            + item_count_disc
            + review_sent_dow
            + region*above_median, 
            data = brazil_df, family = binomial(link = "logit"))

summary(blue_jeans)

blue_jeans <- glm(bef_message_bool ~ region*above_median, 
                  data = brazil_df, family = binomial(link = "logit"))

summary(blue_jeans)

blue_jeans_3 <- glm(bef_message_bool ~ region*above_mean, 
                  data = brazil_df, family = binomial(link = "logit"))

summary(blue_jeans_3)


blue_jeans_2 <- glm(bef_message_bool ~ region + above_median, 
                    data = brazil_df, family = binomial(link = "logit"))

AIC(blue_jeans)
AIC(blue_jeans_2)

anova(blue_jeans, blue_jeans_2)



# fit on customer city
# --------------------
fit_city <- me_function(main_city, brazil_df)
summary(fit_city)
AIC(fit_city)


fit_random <- me_function(random_eff_basic, brazil_df)



fit_single_logit <- glm(main_sing,
                        data = brazil_df,
                        family = binomial(link = "logit"))

summary(fit_single_logit)



anova(fit_city, fit_single_logit)


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


anova(fit_city, fit_single_logit)

# One level is enough
anova(fit_state_city, fit_city)

# HDI random effect slightly better fit
anova(fit_city, random_eff_city)

# Adding urbanity does not improve  
anova(random_eff_city, random_eff_city_2)

# Adding young ratio does not improve either
anova(random_eff_city, random_eff_city_3)



# Mixed effects versus fixed effects ------------------------------------------ #

# Good tutorial
# https://www.youtube.com/watch?v=6MexZiX-2W8
# --------------------------------------------

# No random effects at all
# ------------------------
fit_single_logit <- glm(main_sing,
                        data = train,
                        family = binomial(link = "logit"))
summary(fit_single_logit)

hit_rate_1 <- validation_function(fit_single_logit, 
                                  test,
                                  multilevel = 0,
                                  probability = 0.58)
print(hit_rate_1)  


# Random effects only 
# -------------------
fit_city <- me_function(main_city, train)
summary(fit_city)

hit_rate_city <- validation_function(fit_city,
                                     test,
                                     multilevel = 1,
                                     probability = 0.575)
print(hit_rate_city)

AIC(fit_city)

# City State
# ----------

fit_state_city <- me_function(main_citystate, train)
summary(fit_state_city)

hit_rate_statecity <- validation_function(fit_state_city,
                                     test,
                                     multilevel = 1,
                                     probability = 0.566)
print(hit_rate_statecity)


anova(fit_city, fit_state_city) # No evidence that state offers better fit.
anova(fit_single_logit, fit_city, test = "Chisq" )


# Mixed random effects at hdi 
# ---------------------------
random_eff_city <- me_function(random_eff_city, train)
summary(random_eff_city)





# Singularity? NO!
tt <- getME(random_eff_city,"theta")
ll <- getME(random_eff_city,"lower")
min(tt[ll==0])






model <- glmer(formula = random_eff_city,
               family = binomial(link = "logit"),
               data = train,
               control = glmerControl(
                 optimizer = "bobyqa", 
                 optCtrl = list(maxfun=2e5))
               )
hit_rate_2 <- validation_function(model,
                                  test,
                                  multilevel = 1,
                                  probability = 0.65)
print(hit_rate_2)



summary(model)
  




hit_rate_2 <- validation_function(model, test, type = "response")


probabilities <- model %>% predict(test, type = "response", allow.new.levels = TRUE)
predicted.classes <- ifelse(probabilities > 0.55, 1, 0)
predicted.classes

probies <- cbind(as.character(test$bef_message_bool), predicted.classes)
probies <- as.data.frame(probies)

probies <- probies %>%
  mutate(true_positives = ifelse(V1 == "1" & predicted.classes == "1", 1, 0),
         true_negatives = ifelse(V1 == "0" & predicted.classes == "0", 1, 0),
         hit_rate = ifelse(true_positives == "1" | true_negatives == "1", 1, 0)
  )

hit_rate <- mean(probies$hit_rate, na.rm = TRUE)
hit_rate




probies <- probies %>%
  select(bef_message_bool, predicted.classes)

mean(predicted.classes == test$bef_message_bool)


mean(predicted.classes == test$bef_message_bool)

fitted.results <- predict(fit_single_logit,
                          newdata = test)
fitted.results



# Heckmann -------------------------------------------------------------------


myprobit    <- probit(lfp ~ age + I(age^2) + faminc + kids + educ - 1, x = TRUE, 
                      iterlim = 30, data=Mroz87)

imrData     <- invMillsRatio(myprobit) # same as yours in this particular case
Mroz87$IMR1 <- imrData$IMR1

outeqn1     <- lm(wage ~ -1 + exper + I(exper^2) + educ + city + IMR1, 
                  data=Mroz87, subset=(lfp==1))



# Variable that doesn't affect length but that does affect the other



truncated <- brazil_df[brazil_df$bef_nchar > 0,]
library(MASS)
m1 <- glm.nb(bef_nchar ~ new_idhm + region + review_score, data = truncated)


m1_residuals <- residuals(m1)


qqnorm(m1_residuals, pch = 1, frame = FALSE)
qqline(m1_residuals, col = "steelblue", lwd = 2)

residuals(m1)












# Current best version
heckman <- selection(selection = bef_message_bool 
                     ~ region
                     + review_score
                     + new_idhm
                     + year
                     + new_urbanity
                     + new_young_ratio
                     + experience_goods
                     + intimate_goods
                     + item_count_disc
                     + review_sent_dow
                     + region*above_median, 
                     
                     outcome = bef_nchar
                     ~ region
                     + review_score
                     + new_idhm
                     + year
                     + new_urbanity
                     + new_young_ratio
                     + intimate_goods
                     + item_count_disc
                     + above_median,
                     data = brazil_pos, method = "2step")
summary(heckman)
out_residuals <- residuals(heckman, part = "outcome")

nona_residuals <- out_residuals[!is.na(out_residuals)]

qqnorm(nona_residuals, pch = 1, frame = FALSE)
qqline(nona_residuals, col = "steelblue", lwd = 2)

hist(nona_residuals)


hist(sqrt(brazil_df[brazil_df$bef_nchar > 0,]$bef_nwords))


qqPlot(nona_residuals)




heckman$coefficients
plot(heckman$invMillsRatio)

heckman$invMillsRatio

millies <- heckman$invMillsRatio

length(millies[millies > 0.7])


data_tjes <- heckman$param
data_tjes



data("Mroz87")
Mroz87$kids <- (Mroz87$kids5 + Mroz87$kids618 > 0) # Like bef message bool 



# Heckman
heckman <- selection(selection = lfp 
                     ~ age 
                     + I(age^2) 
                     + faminc 
                     + kids 
                     + educ,
                     
                     outcome = wage 
                     ~ exper 
                     + I(exper^2) 
                     + educ 
                     + city, 
                     data = Mroz87, method = "2step")
summary(heckman)

# Manual 
seleqn1 <- glm(lfp 
               ~ age 
               + I(age^2) 
               + faminc 
               + kids 
               + educ, 
               family=binomial(link="probit"), data=Mroz87)
summary(seleqn1)

# Calculate inverse Mills ratio by hand ##
Mroz87$IMR <- dnorm(seleqn1$linear.predictors)/pnorm(seleqn1$linear.predictors)


# Outcome equation correcting for selection ## ==> correct estimates, wrong SEs
outeqn1 <- lm(wage 
              ~ exper 
              + I(exper^2) 
              + educ 
              + city 
              + IMR, 
              data=Mroz87, subset=(lfp==1))
summary(outeqn1)



Mroz87$kids <- (Mroz87$kids5 + Mroz87$kids618 > 0) # Like bef message bool 






# -----------
heckman <- selection(selection = lfp 
                     ~ age 
                     + I(age^2) 
                     + faminc 
                     + kids 
                     + educ,
                     
                     outcome = wage 
                     ~ exper 
                     + I(exper^2) 
                     + educ 
                     + city, 
                     data = Mroz87, method = "2step")
summary(heckman)


data("Mroz87")

# Correct way
myprobit    <- probit(lfp ~ 
                        age + 
                        I(age^2) + 
                        faminc + 
                        kids + 
                        educ, 
                      x = TRUE, 
                      iterlim = 30, 
                      data=Mroz87)
summary(myprobit)

imrData     <- invMillsRatio(myprobit) # same as yours in this particular case
probit_lp = predict(myprobit)
mills0 = dnorm(probit_lp)/pnorm(probit_lp)

Mroz87$IMR1 <- imrData$IMR1


outeqn1     <- lm(wage 
                  ~ -1 
                  + exper 
                  + I(exper^2) 
                  + educ 
                  + city 
                  + IMR1, 
                  data=Mroz87, subset=(lfp==1))
summary(outeqn1)


# https://m-clark.github.io/models-by-example/heckman-selection.html


model <- glmer(formula = bef_message_bool 
               ~ region
               + review_score
               + new_idhm
               + year
               + new_urbanity
               + new_young_ratio
               + experience_goods
               + intimate_goods
               + item_count_disc
               + review_sent_dow
               + (1 | customer_city),
               family = binomial(link = "probit"),
               data = train,
               control = glmerControl(
                 optimizer = "bobyqa", 
                 optCtrl = list(maxfun=2e5))
)

summary(model)

laylow <- cbind(train, probabilities)

# Any differences? Don't think so....
probit_lp = predict(model)
probabilities <- model %>% predict(train, type = "response",allow.new.levels = TRUE)

mills0 = dnorm(probit_lp)/pnorm(probit_lp) # this works correctly

# Somewhat multimodal; is this a concern? 
hist(mills0)

train$mills <- mills0

# How about the correlated errors between latent and non-latent?

truncated <- train[train$bef_message_bool == 1,]


simpy_glm <- lm(bef_nchar ~ new_idhm + mills, data = truncated)
summary(simpy_glm)
AIC(simpy_glm)
plot(simpy_glm)

m1_residuals <- residuals(simpy_glm)

qqnorm(m1_residuals, pch = 1, frame = FALSE)
qqline(m1_residuals, col = "steelblue", lwd = 2)
hist(m1_residuals)


library(MASS)
simpy_glms <- glm.nb(bef_nchar ~ new_idhm + mills, data = truncated)
AIC(simpy_glms)



m1_residuals <- residuals(simpy_glms)

qqnorm(m1_residuals, pch = 1, frame = FALSE)
qqline(m1_residuals, col = "steelblue", lwd = 2)
hist(m1_residuals)

pois_glms <- glm(bef_nchar ~ new_idhm + mills, family="poisson", data=truncated)
AIC(pois_glms)

m1_residuals <- residuals(pois_glms)

qqnorm(m1_residuals, pch = 1, frame = FALSE)
qqline(m1_residuals, col = "steelblue", lwd = 2)



gamma_glms <- glm(bef_nchar ~ new_idhm + mills, family="Gamma", data=truncated)
AIC(gamma_glms)

m1_residuals <- residuals(gamma_glms)

qqnorm(m1_residuals, pch = 1, frame = FALSE)
qqline(m1_residuals, col = "steelblue", lwd = 2)
