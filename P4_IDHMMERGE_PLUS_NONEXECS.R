# USED files:
#            full_geomerged_df_3.csv (from part 2: from derived)
#            data (3).xlsx (from Atlas.bi)
# 
# WRITE files:
#            full_geomerged_df_4.csv (for part 5)


library(readxl)
library(tidyr)
library(dplyr)
library(corrplot)
library(FNN)


# ------------- #
# Load Datasets # -------------------------------------------------------------
# ------------- #

# internal data
input <- read.csv("full_geomerged_df_3.csv")
brazil_df <- input

# external data | municip level
brazil_municip <- read_excel("./atlas_data/brazil_municipal.xlsx")

# external data | udh (neighborhood) level
sp_udh <- read_excel("./atlas_data/sao_paulo_udh.xlsx")


# ------------------------------- #
# Cleaning and deriving variables # -------------------------------------------
# ------------------------------- #

# Function is called inside column_fixer function
# Purpose: put data-level indicator for column name (state, municip, udh) 
locality_names <- function(input_data, data_level) {
  
  if (data_level == 'state') {
    level_indicator <- 'st.'
  } else if (data_level == 'municip') {
    level_indicator <- 'mc.'
  } else {
    level_indicator <- 'udh.'
  }
  
  old_colnames <- colnames(input_data)
  new_colnames <- c()
  countertje <- 1
  
  for (i in old_colnames) {
    new_colnames[countertje] <- paste(level_indicator, i, sep = "")
    countertje <- countertje + 1
  }
  
  input_data <- setNames(input_data, new_colnames)
  
  return(input_data)
}
# General purpose (works for all data levels)
# Purpose: cleans atlas Brasil external data
column_fixer <- function(new_data, data_level){
  new_data <- new_data %>%
    mutate(Territorialidades = as.character(Territorialidades),
           Territorialidades = iconv(Territorialidades, to = 'ASCII//TRANSLIT'),
           Territorialidades = tolower(Territorialidades)
    ) %>%
    filter(! grepl("elabor", Territorialidades)
    ) %>%
    filter(! grepl("brasil", Territorialidades)
    ) %>%
    filter(! grepl("fontes:", Territorialidades)
    ) %>%
    drop_na(Territorialidades
    ) %>%
    mutate(
      urbanity = `População urbana 2010` / `População total 2010`,
      rurality = `População rural 2010` / `População total 2010`,
      rurality = ifelse(is.na(`População rural 2010`) == TRUE, 0, rurality)
    ) %>% 
    mutate(cumul_age_24 = 
             `População masculina de 0 a 4 anos de idade 2010` +
             `População masculina de 5 a 9 anos de idade 2010` +
             `População masculina de 10 a 14 anos de idade 2010` + 
             `População masculina de 15 a 19 anos de idade 2010` + 
             `População masculina de 20 a 24 anos de idade 2010`
    ) %>%
    select(
      - `População masculina de 0 a 4 anos de idade 2010`,
      - `População masculina de 5 a 9 anos de idade 2010`,
      - `População masculina de 10 a 14 anos de idade 2010`,
      - `População masculina de 15 a 19 anos de idade 2010`,
      - `População masculina de 20 a 24 anos de idade 2010`,
      - `População masculina de 25 a 29 anos de idade 2010`,
      - `População masculina de 30 a 34 anos de idade 2010`
    ) %>%
    mutate(young_ratio = cumul_age_24 / `População total 2010`)
  
  new_data <- locality_names(new_data, data_level)
  
  return(new_data)
}

# Salling function for various level datasets
sp_udh <- column_fixer(sp_udh, 'udh')
brazil_municip <- column_fixer(brazil_municip, 'municip')


# --------- #
# udh query # ----------------------------------------------------------------
# --------- #

get_cors <- function(datasetje){
  counter <- 1 
  subbie_df <- datasetje
  for (i in subbie_df$udh.Territorialidades){
    tryCatch({
      retrieved_cors <- geocode_OSM(i)
      subbie_df$udh.long[counter] <- retrieved_cors[["coords"]][["x"]]
      subbie_df$udh.lat[counter] <- retrieved_cors[["coords"]][["y"]]
    },
    error = function(e){
      subbie_df$udh.long[counter] <- 0
      subbie_df$udh.lat[counter] <- 0
    })
    print(i)
    counter <- counter + 1
  }
  subbie_df <- subbie_df %>%
    mutate(udh.lat = ifelse(udh.lat == 0, NA, udh.lat),
           udh.long = ifelse(udh.long == 0, NA, udh.long))
  return(subbie_df)
}

coordinate_retriever <- function(input_udh_data) {
  # Clean text to avoid trouble in OSM queries
  input_udh_data <- input_udh_data %>%
    mutate(
      udh.Territorialidades = gsub(":.*", "", udh.Territorialidades),
      udh.Territorialidades = ifelse(grepl("/", udh.Territorialidades), gsub("/.*", "", udh.Territorialidades), udh.Territorialidades),
      udh.Territorialidades = gsub("\\s*\\([^\\)]+\\)","", udh.Territorialidades),
      udh.Territorialidades = paste(udh.Territorialidades, "(Sao Paulo)", sep = " "),
      udh.Territorialidades = gsub("\\s+", " ", udh.Territorialidades) # whitespaces
    )
  # drop non uniques 
  input_udh_data <- input_udh_data[!duplicated(input_udh_data$udh.Territorialidades), ]
  # add longitude and latitude columns (empty)
  input_udh_data$udh.lat <- 0
  input_udh_data$udh.long <- 0
  # Call the query function
  input_udh_data_retrieved_cors <- get_cors(input_udh_data)
  # Return dataset
  return(input_udh_data_retrieved_cors)
}

# call function with datasets
sp_udh <- coordinate_retriever(sp_udh)
write.csv(sp_udh, "./udh_queried_data/sao_paulo_queried.csv")


# ---------------------------- #
# udh merge with internal data # ----------------------------------------------------------------
# ---------------------------- #

udh_merge_ex_with_in <- function(internal_data, external_data, city) {
  
  external_data <- external_data[
    !is.na(external_data$udh.lat) | !is.na(external_data$udh.long),]
  
  internal_data <- internal_data[
    internal_data$customer_city == city & !is.na(internal_data$centroid_lat),]
  
  nn2 <- get.knnx(
    external_data[,c("udh.lat", "udh.long")], 
    internal_data[,c("centroid_lat", "centroid_long")], 
    2)
  
  internal_data$local_index <- c(1:nrow(internal_data)) 
  external_data$local_index <- c(1:nrow(external_data))
  
  internal_data$index_other_data <- nn2$nn.index[,1]
  internal_data$dist_lat <- nn2$nn.dist[,1]
  internal_data$dist_long <- nn2$nn.dist[,2]
  internal_data$total_distancjes <- internal_data$dist_lat + internal_data$dist_long
  
  married <- merge(internal_data,
                   external_data,
                   by.x = "index_other_data",
                   by.y = "local_index")
  return(married)
}

sao_paulo_udh_merged <- udh_merge_ex_with_in(brazil_df, sp_udh, 'sao paulo')



# ---------------------------------- #
# NEXT: merge with original dataset  # ----------------------------------------
# ---------------------------------- #




# ---------------------------------- #
# NEXT: municipal level data thing   # ----------------------------------------
# ---------------------------------- #

brazil_df <- brazil_df %>%
  mutate(customer_city = paste(customer_city, "(", sep = " "),
         customer_city = paste(customer_city, customer_state, sep = ""),
         customer_city = paste(customer_city, ")", sep=""))

brazil_df$customer_city <- tolower(brazil_df$customer_city)

merry <- merge(brazil_df,
               my_data,
               by.x = 'customer_city',
               by.y = 'Territorialidades',
               all.x = TRUE)


nrow(merry[is.na(merry$`IDHM 2010`),])











# ------------------------------ #
# old stuff that can possibly go # --------------------------------------------
# ------------------------------ #

# diff function 

# original dataset
sp_sp <- brazil_df[brazil_df$customer_city == 'sao paulo',]

# Get NAs
colSums(is.na(sallgoodman))
colSums(is.na(sp_sp))

# Get rid of NAs in both datasets
sallgoodman <- sallgoodman[!is.na(sallgoodman$lat) | !is.na(sallgoodman$long),]
sp_sp <- sp_sp[!is.na(sp_sp$centroid_lat) | !is.na(sp_sp$centroid_long),]


nn2 <- get.knnx(sallgoodman[,c("lat", "long")], 
                sp_sp[,c("centroid_lat", "centroid_long")], 2)

# Create index columns for merge
sp_sp$local_index <- c(1:nrow(sp_sp)) 
sallgoodman$local_index <- c(1:nrow(sallgoodman))

sp_sp$index_other_data <- nn2$nn.index[,1]
sp_sp$dist_lat <- nn2$nn.dist[,1]
sp_sp$dist_long <- nn2$nn.dist[,2]
sp_sp$total_distancjes <- sp_sp$dist_lat + sp_sp$dist_long

married <- merge(sp_sp,
                 sallgoodman,
                 by.x = "index_other_data",
                 by.y = "local_index"
)

big_cities <- c("sao paulo",
               "rio de janeiro",
               "belo horizonte",
               "distrito federal",
               "porto alegre",
               "fortaleza",
               "recife",
               "salvador",
               "curitiba",
               "campinas",
               "et cetera")

my_data <- my_data[-c(1,2),]

hey <- glm(message_bool ~ `IDHM 2010`, data = merry, family = 'binomial')

yoyo <- glm(message_bool ~ 
              `IDHM 2010` + 
              top2box + 
              max_price +
              y_2018 +
              y_2017 + 
              item_count +
              review_sent_wknd +
              #review_sent_dow +
              product_weight_g + 
              north +
              south  + 
              #southeast + 
              northeast
            , data = merry,  family='binomial')
summary(yoyo)



