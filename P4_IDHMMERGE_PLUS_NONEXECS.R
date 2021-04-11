# USED files:
#            full_geomerged_df_3.csv (from part 2: from derived)
#            data (3).xlsx (from Atlas.bi)
# 
# WRITE files:
#            full_geomerged_df_4.csv (for part 5)


library(readxl)

input <- read.csv("full_geomerged_df_3.csv")
geo_sao_paolo_df <- input


my_data <- read_excel("data (3).xlsx")
my_data$Territorialidades <- as.character(my_data$Territorialidades)
my_data$Territorialidades <- iconv(my_data$Territorialidades, to = 'ASCII//TRANSLIT')
my_data$Territorialidades <- tolower(my_data$Territorialidades)


# ------------------------------------------ #
# ATTENTION! THIS DEPENDS ON DATA DOWNLOADED #
# ------------------------------------------ #

my_data <- my_data[-c(1,2),]



# ------------ #
# Continue...  #
# ------------ #

geo_sao_paolo_df <- geo_sao_paolo_df %>%
  mutate(customer_city = paste(customer_city, "(", sep = " "),
         customer_city = paste(customer_city, customer_state, sep = ""),
         customer_city = paste(customer_city, ")", sep=""))

geo_sao_paolo_df$customer_city <- tolower(geo_sao_paolo_df$customer_city)

merry <- merge(geo_sao_paolo_df,
               my_data,
               by.x = 'customer_city',
               by.y = 'Territorialidades',
               all.x = TRUE)


nrow(merry[is.na(merry$`IDHM 2010`),])


c_north <- c("AC","AP","AM","PA", "RO", "RR", "TO")
c_south <- c("SC", "RS", "PR")
c_southeast <- c("SP", "RJ", "MG", "ES")
c_northeast <- c("AL", "BA", "CE", "MA", "RN", "SE", "PI", "PB", "PE")
c_centerwest <- c("MT", "MS", "GO", "DF")

merry$north <- 0
merry$south <- 0
merry$southeast <- 0
merry$northeast <- 0
merry$centerwest <- 0

merry <- merry %>% 
  mutate(north = ifelse(customer_state %in% c_north, 1,0),
         south = ifelse(customer_state %in% c_south, 1,0),
         southeast = ifelse(customer_state %in% c_southeast, 1,0),
         northeast = ifelse(customer_state %in% c_northeast, 1,0),
         centerwest = ifelse(customer_state %in% c_centerwest, 1,0),
        )

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



