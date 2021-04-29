library(car)
library(dplyr)
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


# As factor score
brazil_df <- brazil_df %>%
  mutate(review_score = as.factor(review_score))



brazil_df <- brazil_df %>%
  mutate(north_and_northeast = ifelse(north == 1 | northeast == 1, 1,0))



# ---------------------------------------------------------------------------- #
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



hey_2 <- glm(bef_message_bool 
           ~ hdi_class
           + urbanity_disc
           + freight_issue_bool
           #+ top2box
           + review_score
           + north
           + northeast 
           + south
           + southeast
           + search_goods
           + experience_goods
           + intimate_goods
           + max_price_disc
           + item_count_disc
           , 
           data = brazil_df, 
           family = binomial(link = "logit")
)
summary(hey_2)
vif(hey_2)



# Bayes Nets ----------------------------------------------------------------- #

library(bnlearn)
library(qgraph)

# sample
s1_brazil_df <- brazil_df[sample(nrow(brazil_df), 70000, replace = FALSE, prob = NULL),]

s1_brazil_df <- brazil_df

selection <- c("hdi_class", 
               "intimate_goods",
               "bef_message_bool",
               "south",
               "experience_goods",
               "urbanity_disc",
               "freight_issue_bool",
               "top2box",
               "northeast")

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
  "bef_message_bool", "south",
  "bef_message_bool", "freight_issue_bool",
  "bef_message_bool", "top2box",
  "bef_message_bool", "northeast",
  # Nothing can cause north or noortheast membership
  
  "experience_goods", "south",
  "hdi_class", "south",
  "intimate_goods", "south",
  "urbanity_disc", "south",
  "experience_goods", "south",
  "freight_issue_bool", "south",
  "top2box", "south",
  "northeast", "south",
  
  "experience_goods", "northeast",
  "hdi_class", "northeast",
  "intimate_goods", "northeast",
  "urbanity_disc", "northeast",
  "experience_goods", "northeast",
  "freight_issue_bool", "northeast",
  "top2box", "northeast",
  "south", "northeast",
  
  # HDICLASS
  "experience_goods", "hdi_class",
  "hdi_class", "hdi_class",
  "intimate_goods", "hdi_class",
  "urbanity_disc", "hdi_class",
  "experience_goods", "hdi_class",
  "freight_issue_bool", "hdi_class",
  "top2box", "hdi_class"

),, 2 ,byrow=TRUE)
colnames(Blacklist) <- c("from", "to")
Blacklist


# Estiamte dag
# incremental association markov Blanket
Res <- iamb(s1_brazil_df,
            blacklist = Blacklist)

bnlearn:::print.bn(Res)

Labels <- c(
  "hdi_class", 
  "intimate_goods",
  "bef_message_bool",
  "south",
  "experience_goods",
  "urbanity_disc",
  "freight_issue_bool",
  "top2box",
  "northeast"
  )




qgraph(Res, nodeNames = Labels, legend.cex = 0.5,
       asize = 3, edge.color = "black")

Res <- set.arc(Res, from = "intimate_goods", to = "experience_goods")

fit <- bn.fit(Res, s1_brazil_df)





fit$freight_issue_bool

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



