library(car)
library(dplyr)
library(sampleSelection)
# Hello, world
input <- read.csv("full_geomerged_df_5.csv")
brazil_df <- input

cols <- c("bef_message_bool",
          "hdi_class",
          "max_price_disc",
          "item_count_disc",
          "urbanity_disc",
          "freight_issue_bool",
          "intimate_goods",
          "north",
          "northeast",
          "centerwest",
          "south",
          "southeast",
          "y_2016",
          "y_2017",
          "y_2018",
          "top2box",
          "experience_goods",
          "search_goods")

brazil_df[,cols] <- lapply(brazil_df[cols], function(x) as.factor(x))

# [MISSIGNESS] ----------------------------------------------------------- #

length(unique(brazil_df$review_id))

colSums(is.na(brazil_df))

isna <- brazil_df[is.na(brazil_df$product_id.y),]


# (1) Get rid of most canceled, unfinished ones. 
brazil_df <- brazil_df %>%
  filter(! is.na(product_id.y))

colSums(is.na(brazil_df))

# (2) One weird bef_nchar
brazil_df <- brazil_df %>%
  filter(! is.na(bef_nchar))

colSums(is.na(brazil_df))

# (3) Missing product_id information

miss_product <- brazil_df[is.na(brazil_df$product_category_name),]

brazil_df <- brazil_df %>%
  filter(! is.na(product_category_name))

colSums(is.na(brazil_df))


# [last variable transformations] -------------------------------------------- #

brazil_df <- brazil_df %>%
  mutate(other_issue = ifelse(diff_est_deliv < 1, 1, 0))


# As factor score
brazil_df <- brazil_df %>%
  mutate(review_score = as.factor(review_score))



brazil_df <- brazil_df %>%

  mutate(north_vs_south = ifelse(north == 1 | northeast == 1, "all_north", ""),
         north_vs_south = ifelse(south == 1 | southeast == 1, "all_south", north_vs_south),
         north_vs_south = ifelse(centerwest == 1, "centerwest", north_vs_south),
         north_vs_south = as.factor(north_vs_south)
        )
table(brazil_df$north_vs_south)

table(brazil_df$hdi_class)


# HDI class has to be changed
brazil_df <- brazil_df %>%
  mutate(hdi_class = as.character(hdi_class),
         hdi_class = ifelse(hdi_class == "low", "medium", hdi_class),
         hdi_class = as.factor(hdi_class))


table(brazil_df$hdi_class)

brazil_df$hdi_class <- factor(brazil_df$hdi_class, levels = c("medium", "high", "very high"))

levels(brazil_df$hdi_class)


brazil_df$max_price_disc <- factor(brazil_df$max_price_disc, levels = c("low", "medium", "high"))

brazil_df$north_vs_south <- factor(brazil_df$north_vs_south, levels = c("centerwest", "all_south", "all_north"))


brazil_df <- brazil_df %>%
  mutate(sent_sun = ifelse(review_sent_dow == "zo", 1, 0),
         sent_mon = ifelse(review_sent_dow == "ma", 1, 0))

# Waste of time, relatively few who have commented more than once
hoi <- brazil_df %>% group_by(customer_unique_id) %>% summarise(n = n())


# Filter out freight comments
brazil_df <- brazil_df %>%
  filter(freight_issue_bool == 0 & other_issue == 0)


# -----[ Subsetting ] -------------------------------------------------------- #

subset_1 <- brazil_df %>%
  filter(north_vs_south == 'all_north')

subset_2 <- brazil_df %>%
  filter(north_vs_south == 'all_south')

subset_3 <- brazil_df %>%
  filter(north_vs_south == 'centerwest')

subset_1 <- subset_1[sample(nrow(subset_1), 5459, replace = FALSE, prob = NULL),]
subset_2 <- subset_2[sample(nrow(subset_2), 5459, replace = FALSE, prob = NULL),]
subset_3 <- subset_3[sample(nrow(subset_3), 5459, replace = FALSE, prob = NULL),]


brazil_df <- rbind(subset_1, subset_2)
brazil_df <- rbind(brazil_df, subset_3)


# discretize even further because too little in low. 
# Best to consider conditional gaussian network though!
# Can also include length in that case.
# Lose less information. 
brazil_df$hdi_class <- 0
brazil_df <- brazil_df %>%
  mutate(hdi_class = ifelse(new_idhm < 0.700, "low medium", ""),
         hdi_class = ifelse(new_idhm > 0.699 & new_idhm < 0.800, "high", hdi_class),
         hdi_class = ifelse(new_idhm > 0.799, "very high",  hdi_class),
         hdi_class = as.factor(hdi_class)
  )

# ---------------------------------------------------------------------------- #
hey <- glm(bef_message_bool 
           ~ new_idhm
           + freight_issue_bool
           + urbanity_disc
           + top2box
           + review_sent_wknd
           + udh_indicator
           + max_price
           + search_goods
           + experience_goods
           + intimate_goods
           #+ north_vs_south
           , 
           data = brazil_df[brazil_df$north_vs_south == "centerwest" | brazil_df$north_vs_south == "all_north" ,], 
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



hey_2 <- glm(bef_message_bool 
           ~ scale(new_idhm)
           + urbanity_disc
           + review_score
           + north_vs_south
           + other_issue
           + experience_goods
           + intimate_goods
           + product_weight_g
           + y_2018
           #+ scale(new_young_ratio)
           + sent_sun
           + item_count
           + log(max_price)
           , 
           data = brazil_df, 
           family = binomial(link = "logit")
)
summary(hey_2)
vif(hey_2)

hey_3 <- lm(log(bef_nchar)
            ~ log(new_idhm)
            + north_vs_south
            # + intimate_goods
            #+ other_issue
            #+ experience_goods
            # + item_count
            #+ top2box
            #+ urbanity_disc
            + review_score
           # + log(max_price),
            data = brazil_df[brazil_df$bef_nchar > 0,])

summary(hey_3)
vif(hey_3)
hist(hey_3$residuals)



heckman <- selection(selection = bef_message_bool 
                     ~ scale(new_idhm)
                     + urbanity_disc
                     + north_vs_south
                     + review_score
                     + other_issue
                     + experience_goods
                     + intimate_goods
                     + product_weight_g
                     + y_2018
                     + sent_sun
                     + item_count
                     + log(max_price), 
                     
                     outcome = log(bef_nchar) 
                     ~ log(new_idhm) 
                     + north_vs_south 
                     + review_score
                     + log(max_price),
                     data = brazil_df, method = "2step")
summary(heckman)


plot(brazil_df[brazil_df$bef_nchar > 0,]$bef_nchar, 
     scale(brazil_df[brazil_df$bef_nchar > 0,]$new_idhm))


# 75% of the sample size
train_size <- floor(0.75 * nrow(brazil_df))

set.seed(777)
train_ind <- sample(seq_len(nrow(brazil_df)), size = train_size)

train <- brazil_df[train_ind, ]
test <- brazil_df[-train_ind, ]


mod_fit <- glm(bef_message_bool 
                  ~ scale(new_idhm)
                  + urbanity_disc
                  + review_score
                  + north_vs_south
                  + experience_goods
                  + intimate_goods
                  + product_weight_g
                  + y_2017
                  + y_2018
                  #+ scale(new_young_ratio)
                  + sent_sun
                  + item_count
                  + log(max_price)
                  , 
                  data = train, 
                  family = binomial(link = "logit")
              )
summary(mod_fit)

# Deviance table
#anova(mod_fit, test="Chisq")


probabilities <- mod_fit %>% predict(test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
predicted.classes

probies <- cbind(test, predicted.classes)

probies <- probies %>%
  select(bef_message_bool, predicted.classes)

mean(predicted.classes == test$bef_message_bool)




fitted.results <- predict(mod_fit,
                          newdata = test)


# Data exploration ----------------------------------------------------------- #


# Age is approximmately normal
hist(brazil_df$new_young_ratio)

# HDI kind of normal but with slight tail
hist(brazil_df$new_idhm)
hist(log(brazil_df$new_idhm))

# Urbanity is completely, highly skewed
hist(brazil_df$new_urbanity)
tail(table(brazil_df$new_urbanity))



# Strong correlation between age and HDI
plot(brazil_df$new_young_ratio, brazil_df$new_idhm)


# Strong correlation with IDH and anafalbetism



# State counts & Uncertainty propagation ------------------------------------- #

table(brazil_df$north_vs_south, brazil_df$hdi_class)
table(brazil_df$north_vs_south, brazil_df$urbanity_disc)
table(brazil_df$hdi_class, brazil_df$urbanity_disc)

fit$intimate_goods



hist(brazil_df$new_urbanity)


state_count_1 <- brazil_df %>%
  filter(north_vs_south == "centerwest") %>%
  filter(top2box == 1) %>%
  filter(freight_issue_bool == 0) %>%
  filter(intimate_goods == 0)

table(state_count_1$hdi_class, state_count_1$bef_message_bool)

library(ggplot2)




rbinom(n = 1, size = 346, prob = 0.306)

medium <- data.frame(length = rbinom(n = 365, size = 1, prob = 0.306))

high <- data.frame(length = rbinom(n = 1597, size = 1, prob = 0.22))


medium$veg <- 'medium'
high$veg <- 'high'

# and combine into your new data frame vegLengths
vegLengths <- rbind(medium, high)

ggplot(vegLengths, aes(length, fill = veg)) + geom_density(alpha = 0.2)



# Bayes Nets ----------------------------------------------------------------- #

library(bnlearn)
library(qgraph)

# sample
s1_brazil_df <- brazil_df[sample(nrow(brazil_df), 70000, replace = FALSE, prob = NULL),]

s1_brazil_df <- brazil_df

selection <- c("hdi_class", 
               "intimate_goods",
               "bef_message_bool",
               "experience_goods",
               "urbanity_disc",
               "freight_issue_bool",
               "top2box",
               "north_vs_south")

s1_brazil_df <- s1_brazil_df[,selection]


# Bnlearn does not like integers.
# Bnlearn does accept numeric


# ----- [Tutorial YouTube] --------------------------------------------------- #

Whitelist <- matrix(c(
  "hdi_class", "bef_message_bool"
),, 2 ,byrow=TRUE)
colnames(Whitelist) <- c("from", "to")
Whitelist

Blacklist <- matrix(c(
  "bef_message_bool", "intimate_goods",
  "bef_message_bool", "hdi_class",
  "bef_message_bool", "urbanity_disc",
  "bef_message_bool", "experience_goods",
  "bef_message_bool", "north_vs_south",
  "bef_message_bool", "top2box",
  "bef_message_bool", "freight_issue_bool",
  
  "freight_issue_bool", "bef_message_bool",
  # Nothing can cause north or noortheast membership
  
  "experience_goods", "north_vs_south",
  "hdi_class", "north_vs_south",
  "intimate_goods", "north_vs_south",
  "urbanity_disc", "north_vs_south",
  "experience_goods", "north_vs_south",
  "top2box", "north_vs_south",
  
  # HDICLASS
  "experience_goods", "hdi_class",
  "hdi_class", "hdi_class",
  "intimate_goods", "hdi_class",
  "urbanity_disc", "hdi_class",
  "experience_goods", "hdi_class",
  "top2box", "hdi_class",
  
  
  "experience_goods", "intimate_goods",
  "intimate_goods", "experience_goods"

),, 2 ,byrow=TRUE)
colnames(Blacklist) <- c("from", "to")
Blacklist


# Estiamte dag
# incremental association markov Blanket
Res <- gs(s1_brazil_df,
            blacklist = Blacklist)

bnlearn:::print.bn(Res)

# d-seperation
dsep(Res, "top2box", "freight_issue_bool", "bef_message_bool")

Labels <- c(
  "hdi_class", 
  "intimate_goods",
  "bef_message_bool",
  "experience_goods",
  "urbanity_disc",
  "freight_issue_bool",
  "top2box",
  "north_vs_south"
  )




qgraph(Res, nodeNames = Labels, legend.cex = 0.5,
       asize = 3, edge.color = "black")

# Res <- set.arc(Res, from = "intimate_goods", to = "experience_goods")




qgraph(Res, nodeNames = Labels, legend.cex = 0.5,
       asize = 3, edge.color = "black")

# MLE
fit <- bn.fit(Res, s1_brazil_df)

bn.bayes <- bn.fit(Res, data = s1_brazil_df, method = "bayes", iss = 10)
bn.bayes$top2box

# Hdi class too little data
fit$bef_message_bool

bn.bayes$bef_message_bool

# query
cpquery(fit,
        (bef_message_bool == "0"),
        (urbanity_disc == "1"),
        method = "lw"
       )


brazil_df %>%
  filter(! hdi_class == "low")


# Bootstrap
str_network <- boot.strength(
  s1_brazil_df,
  algorithm = "tabu",
  algorithm.args = list(
    blacklist = Blacklist,
    whitelist = Whitelist,
    tabu = 10
  ))

filtered_s1_brazil_df <- s1_brazil_df %>%
  select(-hdi_class)

hi <- bn.boot(data = filtered_s1_brazil_df, R = 2, m = 500, algorithm = "gs",
        statistic = arcs)
hi[[2]]

filtered_Blacklist <- matrix(c(
    "bef_message_bool", "intimate_goods",
    "bef_message_bool", "urbanity_disc",
    "bef_message_bool", "experience_goods",
    "bef_message_bool", "south",
    "bef_message_bool", "freight_issue_bool",
    "bef_message_bool", "top2box",
    # Nothing can cause north or noortheast membership
    
    "experience_goods", "south",
    "intimate_goods", "south",
    "urbanity_disc", "south",
    "experience_goods", "south",
    "freight_issue_bool", "south",
    "top2box", "south",
    
    "intimate_goods", "experience_goods",
    "experience_goods", "intimate_goods"
    
  ),, 2 ,byrow=TRUE)
colnames(filtered_Blacklist) <- c("from", "to")
  
filtered_Blacklist
  


table(brazil_df$north_vs_south, brazil_df$experience_goods)

bt_str <- boot.strength(filtered_s1_brazil_df, 
                        R = 200,
                        m = nrow(filtered_s1_brazil_df) %/% 1.08,
                        algorithm.args=list(blacklist=filtered_Blacklist),
                        algorithm = "gs")


bt_str

avg30 <- averaged.network(bt_str, threshold = 0.30)

gR <- strength.plot(
  avg30,
  bt_str,
  shape = "rectangle",
  render = FALSE,
  layout = "dot"
)
plot(gR)

fit$intimate_goods

library(rgraphviz)


# ----- [hybrid bayes nets with HydeNet] ------------------------------------- #
# https://cran.r-project.org/web/packages/HydeNet/vignettes/GettingStartedWithHydeNet.html
library(coda)
library(rjags)
library(BiocManager)
library(graph)
library(HydeNet)

# transform into factors
mtcars2 <- transform(mtcars,
                     cyl = factor(cyl),
                     gear=factor(gear),
                     am = factor(am))

# Network Construction
carNet <- HydeNetwork(~ cyl
                      + disp | cyl
                      + hp | disp
                      + wt
                      + gear
                      + mpg | disp*hp*wt*gear,
                      data=mtcars2)
# Visualization
plot(carNet)

# ----- [ normal bayes net with bnlearn] ------------------------------------- #
# https://www.r-bloggers.com/2018/09/bayesian-network-example-with-the-bnlearn-package/
library(bnlearn)
library(qgraph)
library(visNetwork)





structure <- empty.graph(c("hdi", "intimate_goods", "bef_message_bool"))

modelstring(structure) <- "[hdi][intimate_goods][bef_message_bool|hdi:intimate_goods]"

plot.network <- function(structure, ht = "400px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}
plot.network(structure)


s1_brazil_sub <- s1_brazil[ais$sport %in% c("Netball", "Tennis", "W_Polo"), c("high_hc", "high_hg", "sport")]
ais.sub$sport <- factor(ais.sub$sport)
bn.mod <- bn.fit(structure, data = ais.sub)
bn.mod



# Different tutorial
# https://dirmeier.github.io/bayesian-networks-introduction/index.html#2



