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
           + top2box
           + north
           + northeast 
           + south
           + southeast
           , 
           data = brazil_df, 
           family = binomial(link = "logit")
)
summary(hey_2)




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



