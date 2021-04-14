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


gui <- geocode_OSM("Jardim Eliane Sao Paulo")










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



