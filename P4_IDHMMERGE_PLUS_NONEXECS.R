# USED files:
#            full_geomerged_df_3.csv (from part 2: from derived)
#            data (3).xlsx (from Atlas.bi)
# 
# WRITE files:
#            full_geomerged_df_4.csv (for part 5)


library(readxl)

input <- read.csv("full_geomerged_df_3.csv")
brazil_df <- input


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



