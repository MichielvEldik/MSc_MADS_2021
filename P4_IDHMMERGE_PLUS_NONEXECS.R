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

input <- read.csv("full_geomerged_df_3.csv")
brazil_df <- input




# old
my_data <- read_excel("data (3).xlsx")


my_data$Territorialidades <- as.character(my_data$Territorialidades)
my_data$Territorialidades <- iconv(my_data$Territorialidades, to = 'ASCII//TRANSLIT')
my_data$Territorialidades <- tolower(my_data$Territorialidades)



# New
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

# 
# some neighborhood-level geocoding #
# 

# geocoding


library(tmaptools)


gui <- geocode_OSM("Açafrão (sao paulo)")





hi <- gui[["coords"]][["x"]]
hoi <- gui[["coords"]][["y"]]


# -----------------------------------------------

lowest_sp <- read_excel("lowest_saopaulo.xlsx")

lowest_sp <- lowest_sp %>% 
  filter(! grepl("Brasil", Territorialidades))


subbie <- lowest_sp$Territorialidades[1:100]
subbie_df <- as.data.frame(subbie)


subbie_df <- subbie_df %>%
  mutate(
    subbie = gsub(":.*", "", subbie),
    subbie = ifelse(grepl("/", subbie), gsub("/.*", "", subbie), subbie),
    subbie = gsub("\\s*\\([^\\)]+\\)","", subbie),
    subbie = paste(subbie, "(Sao Paulo)", sep = " "),
    subbie = gsub("\\s+", " ", subbie) # whitespaces
        )


subbie_df <- unique(subbie_df$subbie)
subbie_df <- as.data.frame(subbie_df)
subbie_df <- subbie_df %>%
  mutate(
    x <- 0,
    y <- 0
  )

get_cors <- function(datasetje){
  counter <- 1 
  subbie_df <- datasetje
  for (i in subbie_df$subbie_df){
    tryCatch({
      retrieved_cors <- geocode_OSM(i)
      subbie_df$x[counter] <- retrieved_cors[["coords"]][["x"]]
      subbie_df$y[counter] <- retrieved_cors[["coords"]][["y"]]
    },
    error = function(e){
      subbie_df$x[counter] <- 0
      subbie_df$y[counter] <- 0
    })
    print(i)
    counter <- counter + 1
  }
  return(subbie_df)
}

sallgoodman <- get_cors(subbie_df)


# -------------------------




# -------- Next up ----------#
## ---- Nearest neighbor -- ##
## ---- Deal with weird coordinates -- ## 


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



