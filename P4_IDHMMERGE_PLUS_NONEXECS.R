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
fortaleza_udh <- read_excel("./atlas_data/fortaleza_udh.xlsx")
recife_udh <- read_excel("./atlas_data/recife_udh.xlsx")
rio_dj_udh <- read_excel("./atlas_data/rio_dj_udh.xlsx")
salvador_udh <- read_excel("./atlas_data/salvador_udh.xlsx")
porto_alegre_udh <- read_excel("./atlas_data/porto_alegre_udh.xlsx")
natal_udh <- read_excel("./atlas_data/natal_udh.xlsx")
maceio_udh <- read_excel("./atlas_data/maceio_udh.xlsx")
belo_horizonte_udh <- read_excel("./atlas_data/belo_horizonte_udh.xlsx")
campinas_udh <- read_excel("./atlas_data/campinas_udh.xlsx")
curitiba_udh <- read_excel("./atlas_data/curitiba_udh.xlsx")
belem_udh <- read_excel("./atlas_data/belem_udh.xlsx")
goiania_udh <- read_excel("./atlas_data/goiania_udh.xlsx")
grande_vitoria_udh <- read_excel("./atlas_data/grande vitoria_udh.xlsx")
florianopolis_udh <- read_excel("./atlas_data/florianopolis_udh.xlsx")

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
fortaleza_udh <- column_fixer(fortaleza_udh, "udh")
recife_udh <- column_fixer(recife_udh, "udh")
rio_dj_udh <- column_fixer(rio_dj_udh, "udh")
salvador_udh <- column_fixer(salvador_udh, "udh")
porto_alegre_udh <- column_fixer(porto_alegre_udh, "udh")
natal_udh <- column_fixer(natal_udh, "udh")
maceio_udh <- column_fixer(maceio_udh, "udh")
belo_horizonte_udh <- column_fixer(belo_horizonte_udh, "udh")
campinas_udh <- column_fixer(campinas_udh, "udh")
curitiba_udh <- column_fixer(curitiba_udh, "udh")
belem_udh <- column_fixer(belem_udh, "udh")
goiania_udh <- column_fixer(goiania_udh, "udh")
grande_vitoria_udh <- column_fixer(grande_vitoria_udh, "udh")
florianopolis_udh <- column_fixer(florianopolis_udh, "udh")

# Full 
brazil_municip <- column_fixer(brazil_municip, 'municip')


# --------- #
# udh query # ----------------------------------------------------------------
# --------- #

# function that uses names to query coordinates and returns them to main func.
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

# main function, cleans before query and calls query function
coordinate_retriever <- function(input_udh_data, city_name) {
  # Clean text to avoid trouble in OSM queries
  input_udh_data <- input_udh_data %>%
    mutate(
      udh.Territorialidades = gsub(":.*", "", udh.Territorialidades),
      udh.Territorialidades = ifelse(
        grepl("/", udh.Territorialidades), 
        gsub("/.*", "", udh.Territorialidades), 
        udh.Territorialidades),
      udh.Territorialidades = gsub("\\s*\\([^\\)]+\\)","", udh.Territorialidades),
      udh.Territorialidades = paste(udh.Territorialidades, city_name, sep = " "),
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
sp_udh <- coordinate_retriever(sp_udh, 'sao_paulo')
fortaleza_udh <- coordinate_retriever(fortaleza_udh, 'fortaleza')
recife_udh <- coordinate_retriever(recife_udh, 'recife')
rio_dj_udh <- coordinate_retriever(rio_dj_udh, 'rio de janeiro')
salvador_udh <- coordinate_retriever(salvador_udh, "salvador")
porto_alegre_udh <- coordinate_retriever(porto_alegre_udh, "porto alegre")
natal_udh <- coordinate_retriever(natal_udh, "natal")
maceio_udh <- coordinate_retriever(maceio_udh, "maceio")
belo_horizonte_udh <- coordinate_retriever(belo_horizonte_udh, "belo horizonte")
campinas_udh <- coordinate_retriever(campinas_udh, "campinas")
curitiba_udh <- coordinate_retriever(curitiba_udh, "curitiba")
belem_udh <- coordinate_retriever(belem_udh, "belem")
goiania_udh <- coordinate_retriever(goiania_udh, "goiania")
grande_vitoria_udh <- coordinate_retriever(grande_vitoria_udh, "vitoria")
florianopolis_udh <- coordinate_retriever(florianopolis_udh, "florianopolis")


# write as csv to avoid doing things over and over again
write.csv(sp_udh, "./udh_queried_data/sao_paulo_udh_queried.csv")
write.csv(fortaleza_udh, "./udh_queried_data/fortaleza_udh_queried.csv")
write.csv(recife_udh, "./udh_queried_data/recife_udh_queried.csv")
write.csv(rio_dj_udh, "./udh_queried_data/rio_dj_udh_queried.csv")
write.csv(salvador_udh, "./udh_queried_data/salvador_udh_queried.csv")
write.csv(porto_alegre_udh, "./udh_queried_data/porto_alegre_udh_queried.csv")
write.csv(natal_udh, "./udh_queried_data/natal_udh_queried.csv")
write.csv(maceio_udh, "./udh_queried_data/maceio_udh_queried.csv")
write.csv(belo_horizonte_udh, "./udh_queried_data/belo_horizonte_udh_queried.csv")
write.csv(curitiba_udh, "./udh_queried_data/curtiba_udh_queried.csv")
write.csv(belem_udh, "./udh_queried_data/belem_udh_queried.csv")
write.csv(goiania_udh, "./udh_queried_data/goiania_udh_queried.csv")
write.csv(grande_vitoria_udh, "./udh_queried_data/grande_vitoria_udh_queried.csv")
write.csv(florianopolis_udh, "./udh_queried_data/florianopolis_udh_queried.csv")


# ---------------------------- #
# udh merge with internal data # ----------------------------------------------
# ---------------------------- #

# function that combines udh-level dataset with data from internal
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
fortaleza_udh_merged <- udh_merge_ex_with_in(brazil_df, fortaleza_udh, 'fortaleza')
recife_udh_merged <- udh_merge_ex_with_in(brazil_df, recife_udh, 'recife')
rio_dj_udh_merged <- udh_merge_ex_with_in(brazil_df, rio_dj_udh, 'rio de janeiro')
salvador_udh_merged <- udh_merge_ex_with_in(brazil_df, salvador_udh, 'salvador')
porto_alegre_udh_merged <- udh_merge_ex_with_in(brazil_df, porto_alegre_udh, 'porto alegre')
natal_udh_merged <- udh_merge_ex_with_in(brazil_df, natal_udh, 'natal')
maceio_udh_merged <- udh_merge_ex_with_in(brazil_df, maceio_udh, 'maceio')
belo_horizonte_udh_merged <- udh_merge_ex_with_in(brazil_df, belo_horizonte_udh, 'belo horizonte')
curitiba_udh_merged <- udh_merge_ex_with_in(brazil_df, curitiba_udh, 'curitiba')
belem_udh_merged <- udh_merge_ex_with_in(brazil_df, belem_udh, 'belem')
goiania_udh_merged <- udh_merge_ex_with_in(brazil_df, goiania_udh, 'goiania')
grande_vitoria_udh_merged <- udh_merge_ex_with_in(brazil_df, grande_vitoria_udh, 'vitoria')
florianopolis_udh_merged <- udh_merge_ex_with_in(brazil_df, florianopolis_udh, 'florianopolis')


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



