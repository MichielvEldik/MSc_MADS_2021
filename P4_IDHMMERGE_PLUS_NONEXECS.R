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

input <- read.csv("full_geomerged_df_3.csv")
brazil_df <- input


new_data <- read_excel("full_atlas_data.xlsx")

# All nas
colSums(is.na(new_data))

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



# corrplot
corretje <- cor(new_data[,c("IDHM 2010",
                            "urbanity",
                            "young_ratio")])

corrplot(corretje, method = "circle")


# -----------------------------------------------
# Lowest possible data on HDI stuff

lowest_sp <- read_excel("lowest_saopaulo.xlsx")

# Get rid of 'Brasil'
lowest_sp <- lowest_sp %>% 
  filter(! grepl("Brasil", Territorialidades))


lowest_sp <- lowest_sp %>%
  mutate(
    Territorialidades = gsub(":.*", "", Territorialidades),
    Territorialidades = ifelse(grepl("/", Territorialidades), gsub("/.*", "", Territorialidades), Territorialidades),
    Territorialidades = gsub("\\s*\\([^\\)]+\\)","", Territorialidades),
    Territorialidades = paste(Territorialidades, "(Sao Paulo)", sep = " "),
    Territorialidades = gsub("\\s+", " ", Territorialidades) # whitespaces
  )
# drop non uniques
lowest_sp <- lowest_sp[!duplicated(lowest_sp$Territorialidades), ]

lowest_sp <- lowest_sp %>%
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


lowest_sp$lat <- 0
lowest_sp$long <- 0


get_cors <- function(datasetje){
  counter <- 1 
  subbie_df <- datasetje
  for (i in subbie_df$Territorialidades){
    tryCatch({
      retrieved_cors <- geocode_OSM(i)
      subbie_df$long[counter] <- retrieved_cors[["coords"]][["x"]]
      subbie_df$lat[counter] <- retrieved_cors[["coords"]][["y"]]
    },
    error = function(e){
      subbie_df$long[counter] <- 0
      subbie_df$lat[counter] <- 0
    })
    print(i)
    counter <- counter + 1
  }
  subbie_df <- subbie_df %>%
    mutate(lat = ifelse(lat == 0, NA, lat),
           long = ifelse(long == 0, NA, long))
  return(subbie_df)
}

sallgoodman <- get_cors(lowest_sp)


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

# Done !
# -------------------------




# -------- Next up ----------#
## ---- Nearest neighbor -- ##
## ---- Deal with weird coordinates -- ## 



sp_sp$ind_to_merge <- c(1:nrow(sp_sp)) 




# coordinates(sp_sp) = c('centroid_long', 'centroid_lat')
# coordinates(sallgoodman) = c('long', 'lat')

nn1 <- get.knnx(sp_sp[,2:3], sallgoodman[,2:3], 2)
nn2 <- get.knnx(sallgoodman[,2:3], sp_sp[,2:3], 2)

length(nn1$nn.index[,1])
length(nn2$nn.index[,1])


sp_sp$inexertjes <- nn2$nn.index[,1]
sp_sp$distancjes_1 <- nn2$nn.dist[,1]
sp_sp$distancjes_2 <- nn2$nn.dist[,2]
sp_sp$total_distancjes <- sp_sp$distancjes_1 + sp_sp$distancjes_2

sallgoodman$own_index <- c(1:nrow(sallgoodman)) 


# sallgoodman$ind_to_merge <- nn1$nn.index[,1]


married <- merge(sp_sp,
                 sallgoodman,
                 by.x = "inexertjes",
                 by.y = "own_index"
                 )


# -----------------------------------


nn1 = get.knnx(coordinates(sp_sp), coordinates(geo_sao_paolo_df), 1)

ii = nn1$nn.index[,1]
ii

new_guy_df <- as.data.frame(geo_sao_paolo_df)
new_guy_2 <- as.data.frame(sp_map_2)







# --------------------------------


hiii <- sapply(subbie_df ,function(x) gsub("\\.",",", as.character(x)))

gsub("[^:]*$", "", subbie_df$subbie)

subbie_df$subbie <- gsub(":.*", "", subbie_df$subbie)


x <- rep(0, 10)
y <- rep(0, 10)

counter <- 1
for (i in lowest_sp$Territorialidades[1:10]) {
  gui <- geocode_OSM(i)
  x[counter] <- gui[["coords"]][["x"]]
  y[counter] <- gui[["coords"]][["y"]]
  print(i)
  counter <- counter + 1 
}


# --------------------------- # 
#  Big cities in Brazil       #   
# --------------------------- #

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




# ------------------------------------------ #
# ATTENTION! THIS DEPENDS ON DATA DOWNLOADED #
# ------------------------------------------ #

my_data <- my_data[-c(1,2),]



# ------------ #
# Continue...  #
# ------------ #

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



