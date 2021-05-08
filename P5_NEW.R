library(car)
library(dplyr)
library(sampleSelection)
library(sjstats)
library(ggplot2)
library(lme4)
library(tictoc)


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


# Mean centering ------------------------------------------------------------- #
center_scale <- function(x) {
  scale(x, scale = TRUE)
}

# apply it
brazil_df$new_idhm <- center_scale(brazil_df$new_idhm)
brazil_df$new_urbanity <- center_scale(brazil_df$new_urbanity)
brazil_df$new_young_ratio <- center_scale(brazil_df$new_young_ratio)
brazil_df$max_price <- center_scale(brazil_df$max_price)


# Get order right ------------------------------------------------------------- #

brazil_df <- brazil_df %>%
  mutate(hdi_class_col = factor(hdi_class_col, levels = c("low_medium", 
                                                          "high", 
                                                          "very high")))


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

brazil_df <- brazil_df %>%
  mutate(item_count_disc = factor(item_count_disc, 
                                  levels = c("single", "multiple", "large")))




# Get ird of stuff ------------------------------------------------------------ #


brazil_df <- brazil_df %>%
  filter(order_status == "delivered")

# Get rid of NA in DV
colSums(is.na(brazil_df))
brazil_df <- brazil_df %>%
  filter(!is.na(bef_message_bool))


# State counts --------------------------------------------------------------- #

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

dfr_prop <- brazil_df %>% 
  count(region, hdi_class_col, message_bool) %>%          
  mutate(prop = prop.table(n)) 

hoi <- as.data.frame(dfr_prop)

gg_prop <- ggplot(data = hoi
                  , aes(x = hdi_class_col, y = prop, fill = message_bool)) + 
  geom_bar(stat = 'identity', 
           position = 'dodge', 
           alpha = 2/3) +
  facet_wrap(~region, scales = "free")

gg_prop
 

specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Stacked
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")
  
  
# Try new

pop <- brazil_df %>% 
  count(region, hdi_class_col, message_bool)



# ________________ THIS IS THE GOOD STUFF_______________________________________
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

test <- c("", as.character(round(new_vec[[1]], digits = 2)),
          "", as.character(round(new_vec[[2]], digits = 2)),
          "", as.character(round(new_vec[[3]], digits = 2)),
          "", as.character(round(new_vec[[4]], digits = 2)),
          "", as.character(round(new_vec[[5]], digits = 2)),
          "", as.character(round(new_vec[[6]], digits = 2)),
          "", as.character(round(new_vec[[7]], digits = 2)),
          "", as.character(round(new_vec[[8]], digits = 2)),
          "", as.character(round(new_vec[[9]], digits = 2)),
          "", as.character(round(new_vec[[10]], digits = 2)),
          "", as.character(round(new_vec[[11]], digits = 2)),
          "", as.character(round(new_vec[[12]], digits = 2)),
          "", as.character(round(new_vec[[13]], digits = 2)),
          "", as.character(round(new_vec[[14]], digits = 2)),
          "", as.character(round(new_vec[[15]], digits = 2)),
          "", as.character(round(new_vec[[16]], digits = 2)),
          "", as.character(round(new_vec[[17]], digits = 2)),
          "", as.character(round(new_vec[[18]], digits = 2)))

# Do visual again
ggplot(pop, aes(fill=bef_message_bool, y=n, x=hdi_class_col)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 3, aes(label = test, family = "serif"), vjust = -1) + 
  ylab("count (n)") +
  xlab("Human Development Index Category") +
  theme(text=element_text(size=11,  family="serif")) +
  facet_wrap( ~ region, 
              scales = "free",
              labeller =labeller(region = c(
                "centerwest" = "centerwest (n = 3,537)",
                "north" = "north (n = 1,779)",
                "northeast" = "northeast (n = 8,996)",
                "south" = "south (n = 13,736)",
                "southeast" = "southeast (n = 64,714)",
                "full" = "full (n = 92,762)"))) 

# _____________________________________________________________________________

library(glmmTMB)

  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'cyl', y = 'pct', fill = 'gear')


ggplot(pop, 
       aes(fill=message_bool, 
           y=n, 
           x=hdi_class_col)) + 
  geom_bar(position="fill",  stat="bin")

geom_text(stat = 'count',
          position = position_dodge(.9), 
          vjust = -0.5, 
          size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'cyl', y = 'pct', fill = 'gear')
  


ggplot(pop, aes(y = n, message_bool)) + geom_bar()


library(car)

ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
  geom_bar()

ggplot(brazil_df, aes(hdi_class_col, ), fill = bef_message_bool) +
  geom_bar(position="fill", stat="identity")



ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="fill", stat="identity")




specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)




sect <- brazil_df %>%
  select(region, hdi_class_col, message_bool)
library(tidyr)


# Outliers ------------------------------------------------------------------- #

hist(brazil_df$max_price)

maximum_price <- brazil_df %>%
  select(max_price)


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
            "max_price"
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



# fit on customer city
# --------------------
fit_city <- me_function(main_city, brazil_df)
summary(fit_city)
AIC(fit_city)





fit_single_logit <- glm(main_sing,
                        data = fit_city@frame,
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



# Define train and test sets -------------------------------------------------- #

# 75% of the sample size
train_size <- floor(0.75 * nrow(brazil_df))

set.seed(777)
train_ind <- sample(seq_len(nrow(brazil_df)), size = train_size)

train <- brazil_df[train_ind, ]
test <- brazil_df[-train_ind, ]

colSums(is.na(test))
colSums(is.na(train))

# Validation ------------------------------------------------------------------ #

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
