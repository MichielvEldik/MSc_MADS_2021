library(car)
library(dplyr)
library(sampleSelection)
library(sjstats)



# Load data ------------------------------------------------------------------ #
input <- read.csv("full_geomerged_df_5.csv")
brazil_df <- input



# Mean centering ------------------------------------------------------------- #
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
brazil_df$new_idhm <- center_scale(brazil_df$new_idhm)
brazil_df$new_urbanity <- center_scale(brazil_df$new_urbanity)
brazil_df$new_young_ratio <- center_scale(brazil_df$new_young_ratio)

# Fix the class variable order ----------------------------------------------- #

