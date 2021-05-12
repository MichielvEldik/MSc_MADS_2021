library(car)
library(dplyr)
library(sampleSelection)
library(sjstats)
library(ggplot2)
library(lme4)
library(stats)
library(tictoc)
library(GLMMadaptive)
library(glmmTMB)
library(stargazer)
library(MASS)
library(pscl)
library(glmm)
library(glmmADMB)
library(R2admb)
library(lmtest)
library(margins)

# Load data ------------------------------------------------------------------ 
input <- read.csv("full_geomerged_df_5.csv")
brazil_df <- input

# PART 1: DATA ENGINEERING
################################################################################
# Factor type ----------------------------------------------------------------- 

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

# WOrk in progress (for missingness) -----------------------------------------
# Order status to "delivered"
table(brazil_df$order_status)
non_delivered <- brazil_df %>%
  filter(!order_status == "delivered") # Though we should look at what causes non-deliveries.


# Category names
nrow(non_delivered)
colSums(is.na(non_delivered))

# MISSINGNESS ----------------------------------------------------------------- 


# only keep delivered ones
brazil_df <- brazil_df %>%
  filter(order_status == "delivered")

brazil_df <- brazil_df %>%
  filter(! is.na(product_category_name))

brazil_df <- brazil_df %>%
  filter(!is.na(bef_message_bool))

colSums(is.na(brazil_df))



# Above median variable ------------------------------------------------------- 

all_names <- levels(brazil_df$product_category_name)

hi <-brazil_df[brazil_df$product_category_name == all_names[1],]
hi$product_category_name



# Summarise per product
cat_1 <- brazil_df %>%
  group_by(product_category_name) %>%
  summarise(mean = mean(max_price),
            median = median(max_price),
            sd = sd(max_price),
            count = n())

# get rid of NA
cat_1 <- cat_1[1:71,]

# view
stargazer(cat_1, type = "text")

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


# Mean centering before analyses  --------------------------------------------- 
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
brazil_df$mc_new_idhm <- center_scale(brazil_df$new_idhm)
brazil_df$mc_new_young_ratio <- center_scale(brazil_df$new_young_ratio)


# PART 2: FIRST INSIGHTS
################################################################################
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


# PART 3: SUBSETTING 
################################################################################
# Robustness checks part -------------------------------------------------------













# Define train and test sets -------------------------------------------------- 
# Regular split ---------------------------------------------------------------
train_size <- floor(0.75 * nrow(brazil_df))

set.seed(777)
train_ind <- sample(seq_len(nrow(brazil_df)), size = train_size)

train <- brazil_df[train_ind, ]
test <- brazil_df[-train_ind, ]

colSums(is.na(test))
colSums(is.na(train))



# Pos/neg split ---------------------------------------------------------------
train_pos <- train[
  train$review_score == 4 | 
  train$review_score == 5 | 
  train$review_score == 3,]
train_neg <- train[
  train$review_score == 1 | 
  train$review_score == 2,]

test_pos <- train[
  test$review_score == 4 | 
  test$review_score == 5 | 
  test$review_score == 3,]
test_neg <- train[
  test$review_score == 1 | 
  test$review_score == 2,]


# PART 4: MODELING -------------------------------------------------------------
################################################################################

# Find evidence for mixed model ------------------------------------------------
# (4.1.) Fit null model
null_model <- glm(formula = bef_message_bool ~ 1,
                  data = train,
                  family = binomial(link = "logit")
                  )
AIC(null_model)
summary(null_model)
model_or <- exp(coef(null_model)) # Odds ratio (checks out)
probability_success <- nrow(train[train$bef_message_bool == 1,]) / nrow(train)
probability_failure <- nrow(train[train$bef_message_bool == 0,]) / nrow(train)
manual_or <- probability_success / probability_failure # Checks out

# (4.2.) Nested null model with city
nested_null_model_1 <- glmer(
  formula = bef_message_bool ~ 1 + (1 | customer_city),
  family = binomial(link = "logit"),
  data = train,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
    )
  )
AIC(nested_null_model_1)
summary(nested_null_model_1)
all_intercepts <- coef(nested_null_model_1)
all_intercepts <- all_intercepts[["customer_city"]]
mean_intercept <- mean(all_intercepts$`(Intercept)`) 
hist(all_intercepts$`(Intercept)`)
var(all_intercepts$`(Intercept)`)
# Comparison
lrtest(nested_null_model_1, null_model) # nested model has better fit

# (4.3.) Nested null model with city and state
nested_null_model_2 <- glmer(
  formula = bef_message_bool ~ 1 + (1 | customer_city) + (1 | customer_state),
  family = binomial(link = "logit"),
  data = train,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
AIC(nested_null_model_2)
summary(nested_null_model_2)
all_intercepts <- coef(nested_null_model_2)
all_intercepts <- all_intercepts[["customer_state"]]
mean_intercept <- mean(all_intercepts$`(Intercept)`) 
hist(all_intercepts$`(Intercept)`)
var(all_intercepts$`(Intercept)`)
mean(all_intercepts$`(Intercept)`)
dispersion_index <- sd(all_intercepts$`(Intercept)`) / mean(all_intercepts$`(Intercept)`)

# Comparison
lrtest(nested_null_model_1, nested_null_model_2) # Multilevel 

# FULL Comparisons
stargazer(null_model, nested_null_model_1, nested_null_model_2,  type = "text", title = "Table 1. Results")


# Do the same with full models ------------------------------------------------


# (4.4.) Fit model on full train
full_model <- glm(formula = bef_message_bool 
                  ~ 1
                  + mc_new_idhm
                  + region
                  + urbanity_disc
                  + mc_new_young_ratio
                  + review_score
                  + year
                  + order_status # What to do with this one? 
                  + udh_indicator
                  + intimate_goods
                  + experience_goods
                  + review_sent_wknd
                  + item_count_disc
                  + above_median
                  + region*above_median,
                  data = train,
                  family = binomial(link = "logit")
)
AIC(full_model)
summary(full_model)
vif(full_model)


train$bef_message_bool <- as.integer(train$bef_message_bool) - 1

# (4.5.) Fit probit model on full train
full_prob_model <- glm(formula = bef_message_bool 
                  ~ mc_new_idhm
                  + urbanity_disc
                  + mc_new_young_ratio
                  + review_score
                  + year
                  + udh_indicator
                  + intimate_goods
                  + experience_goods
                  + review_sent_wknd
                  + item_count_disc
                  + above_median,
                  data = train[train$region == "south",],
                  family = binomial(link = "probit")
)
summary(full_prob_model)
#termplot(full_prob_model)

used_data <- cbind(full_prob_model$model$bef_message_bool, fitted.values(full_prob_model))
used_data <- as.data.frame(used_data)
used_data <- used_data %>%
  mutate(V3 = ifelse(V1 == 1, V1-V2, V2))

hist(used_data$V3)
residuals(full_prob_model, type = "response")



nested_null_model_2 <- glmer(
  formula = bef_message_bool ~ 1 + mc_new_idhm +(1 | customer_city) + (1 | customer_state),
  family = binomial(link = "logit"),
  data = train,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
used_data <- cbind(nested_null_model_2$model$bef_message_bool, fitted.values(nested_null_model_2))
used_data <- as.data.frame(used_data)
used_data <- used_data %>%
  mutate(V3 = ifelse(V1 == 1, V1-V2, V2))

hist(used_data$V3)




plot(density(rstandard(full_prob_model, type='deviance')))
lines(density(resid(m1, type='response')), col='red')

scatter.smooth(rstandard(full_prob_model, type='deviance'), col='gray')

scatter.smooth(predict(full_prob_model, type='response'), rstandard(full_prob_model, type='deviance'), col='gray')

probies <- full_prob_model %>% predict(test, type = "response", allow.new.levels = TRUE)
hist(probies)

both <- cbind(probies, train[! is.na(train$product_category_name),]$bef_message_bool)

hist(residuals(full_prob_model))
summary(full_probit_model)
vif(full_probit_model)




# (4.5.) Fit null model on fullpositives only
pos_model <- glm(formula = bef_message_bool 
                  ~ 1
                  + mc_new_idhm
                  + region
                  + urbanity_disc
                  + mc_new_young_ratio
                  + review_score
                  + year
                  + intimate_goods
                  + experience_goods
                  + review_sent_wknd
                  + item_count_disc
                  + above_median
                  + region*above_median,
                  data = train_pos,
                  family = binomial(link = "logit")
)
AIC(pos_model)
summary(pos_model)
vif(pos_model)



# (4.6.) Fit null model on negatives only
neg_model <- glm(formula = bef_message_bool 
                 ~ 1
                 + mc_new_idhm
                 + region
                 + urbanity_disc
                 + mc_new_young_ratio
                 + review_score
                 + year
                 + intimate_goods
                 + experience_goods
                 + review_sent_wknd
                 + item_count_disc
                 + above_median
                 + region*above_median,
                 data = train_neg,
                 family = binomial(link = "logit")
)
AIC(neg_model)
summary(neg_model)
vif(neg_model)

stargazer(pos_model, neg_model, type = "text")



