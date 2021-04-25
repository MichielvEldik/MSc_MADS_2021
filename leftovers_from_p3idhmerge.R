# Leftovers





# Everything below here can be moved basically. We're done!










# put metro and non_metro back together --------------------------------------

# cols should be the same for merge so check it out. 
colnames_nonmetros_1 <- colnames(non_metros_1)
colnames_metros_1 <- colnames(metros_1)

nrow(metros_1) + nrow(non_metros_1) # 95736 is a few thousand loss but fine. 


# Which cols are in metros that aren't in non_metros?
for (i in colnames_metros_1) {
  if (!i %in% colnames_nonmetros_1){
    print(i)
  }
  
}

# Step 1, clean up both dataframes:

non_metros_1 <- non_metros_1 %>%
  select(- index, - X)

metros_1 <- metros_1 %>%
  select(
    - index_other_data,
    - X.x,
    - index,
    - local_index,
    - X.y
    
  )

metros_1 <- metros_1 %>%
  select(
    
    
    - X.y
  )

non_metros_1 <- non_metros_1 %>%
  mutate(
    dist_lat = NA,
    dist_long = NA,
    total_distancjes = NA,
    udh.Territorialidades = NA
    
  )









# keytjes
keys <- read_excel("dffort.xlsx")
keys <- keys %>%
  drop_na() %>%
  mutate(new_names = as.character(new_names),
         new_names = iconv(new_names, to = 'ASCII//TRANSLIT'),
         new_names = tolower(new_names)) %>%
  select(- nummie)
keys <- keys[!duplicated(keys$new_names),]

testje <- merge(testje,
                keys,
                by.x = 'customer_city',
                by.y = 'old_names',
                all.x = TRUE)

testje <- testje %>%
  mutate(customer_city = ifelse(is.na(new_names) == TRUE, customer_city, new_names))

testje <- merge(testje,
                brazil_municip,
                by.x = 'customer_city',
                by.y = 'mc.Territorialidades',
                all.x = TRUE)




non_metros <- brazil_df %>%
  filter(!customer_city %in% metro_cities)

metros_2 <- brazil_df %>%
  filter(customer_city %in% metro_cities)

nrow(metros_1) + nrow(non_metros)


nrowtestje <- nrow(testje)
nrowtestje_2 <- nrow(testje_2)
all_rows <- nrowtestje + nrowtestje_2
what_rows <- nrowtestje + nrow(metros_1)

# ----- [] DON'T RUN ]----------------- [ OLD CODE TO GET KEYS to EXCEL] ------
testje <- brazil_df %>%
  filter(!customer_city %in% all_udh_municips)

testje <- merge(testje,
                brazil_municip,
                by.x = 'customer_city',
                by.y = 'mc.Territorialidades',
                all.x = TRUE)

non_success <- testje[is.na(testje$`mc.IDHM 2010`) == TRUE,]
namescitys <- non_success$customer_city
unique_namescitys <- unique(namescitys)
tofill <- rep(0, length(unique_namescitys))
dffort <- as.data.frame(unique_namescitys, tofill)
write.csv(dffort, "dffort.csv")

# -----------------------------------------------------------------------------




# QUESTIONS
# ----------------------- # Why testje and metros_1 rows don't add up? # ----
# ----------------------- # Check stuff distances # --------------------------

summary(sorocaba_udh_merged$total_distancjes) # 1 degree of distance ~ 111 km. 
summary(old_baixada_santista_udh_merged$total_distancjes)
table(belo_horizonte_udh_merged$customer_city)
belo_horizonte_metro_municips


# Attention, Brasilia has some too extreme oultiers! 





# -------------------------- # merge all stuff # -----------------------------

metros_1 <- rbind(sao_paulo_udh_merged, fortaleza_udh_merged)
metros_1 <- rbind(metros_1, recife_udh_merged)
metros_1 <- rbind(metros_1, rio_dj_udh_merged)
metros_1 <- rbind(metros_1, salvador_udh_merged)
metros_1 <- rbind(metros_1, porto_alegre_udh_merged)
metros_1 <- rbind(metros_1, natal_udh_merged)
metros_1 <- rbind(metros_1, maceio_udh_merged)
metros_1 <- rbind(metros_1, belo_horizonte_udh_merged)
metros_1 <- rbind(metros_1, curitiba_udh_merged)
metros_1 <- rbind(metros_1, belem_udh_merged)
metros_1 <- rbind(metros_1, goiania_udh_merged)
metros_1 <- rbind(metros_1, grande_vitoria_udh_merged)
metros_1 <- rbind(metros_1, florianopolis_udh_merged)
metros_1 <- rbind(metros_1, manaus_udh_merged)
metros_1 <- rbind(metros_1, sao_luis_udh_merged)
metros_1 <- rbind(metros_1, sorocaba_udh_merged)
metros_1 <- rbind(metros_1, baixada_santista_udh_merged)
metros_1 <- rbind(metros_1, brasilia_udh_merged)
metros_1 <- rbind(metros_1, teresina_udh_merged)
metros_1 <- rbind(metros_1, petrolina_juazeiro_udh_merged)
metros_1 <- rbind(metros_1, vale_do_rio_cuiaba_udh_merged)
metros_1 <- rbind(metros_1, vale_do_paraiba_e_litoral_norte_udh_merged)

# more stuff,
# maybe make a self-calling function


# ----------------------------------------------------------------------------






hey <- glm(message_bool 
           ~ `udh.IDHM.2010` 
           + south 
           + north
           + northeast
           + southeast
           + udh.young_ratio
           + udh.urbanity
           + top2box
           , 
           data = metros_1, family = 'binomial')
summary(hey)

vif(hey)

hey_2 <- lm(log(bef_nchar) 
            ~ log(`udh.IDHM.2010`) 
            + max_price
            + item_count
            + review_sent_wknd
            + south 
            + north
            + northeast
            + southeast
            + log(udh.urbanity + 0.1)
            
            , 
            data = metros_1[metros_1$message_bool == 1,])

summary(hey_2)
vif(hey_2)

# ---------------------------------- #
# NEXT: merge with original dataset  # ----------------------------------------
# ---------------------------------- #




# ---------------------------------- #
# NEXT: municipal level data thing   # ----------------------------------------
# ---------------------------------- #

# external data | municip level
brazil_municip <- read_excel("./atlas_data/brazil_municipal.xlsx")
brazil_municip <- column_fixer(brazil_municip, "municip")

# new_data <- brazil_municip

# Full 


brazil_df <- brazil_df %>%
  mutate(customer_city = paste(customer_city, "(", sep = " "),
         customer_city = paste(customer_city, customer_state, sep = ""),
         customer_city = paste(customer_city, ")", sep=""))

brazil_df$customer_city <- tolower(brazil_df$customer_city)

brazil_df <- merge(brazil_df,
                   brazil_municip,
                   by.x = 'customer_city',
                   by.y = 'mc.Territorialidades',
                   all.x = TRUE)


filtered <- brazil_df %>%
  filter(grepl('brasilia', customer_city))



# ----------------------------------------------------------------------------
# New stuff (experiment part) 

brazil_df <- input
sp_udh <- read.csv("./udh_queried_data/sao_paulo_udh_queried.csv")

brazil_df <- brazil_df %>%
  mutate(customer_city = paste(customer_city, "(", sep = " "),
         customer_city = paste(customer_city, customer_state, sep = ""),
         customer_city = paste(customer_city, ")", sep=""),
         customer_city = tolower(customer_city)
  )

shutup <- new_udh_merge_ex_with_in(brazil_df, sp_udh, sao_paulo_metro_municips)

sp_udh <- sp_udh[
  !is.na(sp_udh$udh.lat) | !is.na(sp_udh$udh.long),]

brazil_df <- brazil_df[brazil_df$customer_city %in% sao_paulo_metro_municips &
                         !is.na(brazil_df$centroid_lat),]


nn2 <- get.knnx(
  sp_udh[,c("udh.lat", "udh.long")], 
  brazil_df[,c("centroid_lat", "centroid_long")], 
  2)

brazil_df$local_index <- c(1:nrow(brazil_df)) 
sp_udh$local_index <- c(1:nrow(sp_udh))

brazil_df$index_other_data <- nn2$nn.index[,1]
brazil_df$dist_lat <- nn2$nn.dist[,1]
brazil_df$dist_long <- nn2$nn.dist[,2]
brazil_df$total_distancjes <- brazil_df$dist_lat + brazil_df$dist_long

married <- merge(brazil_df,
                 sp_udh,
                 by.x = "index_other_data",
                 by.y = "local_index")


# --------------------- # New function try # ---------------------------------

# function that combines udh-level dataset with data from internal
udh_merge_ex_with_in <- function(internal_data, external_data, metro_munics) {
  
  external_data <- external_data[
    !is.na(external_data$udh.lat) | !is.na(external_data$udh.long),]
  
  internal_data <- internal_data[
    internal_data$customer_city %in% metro_munics & !is.na(internal_data$centroid_lat),]
  
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


















# -----------------------------------------------------------------------------


drake <- brazil_df[
  grepl('acorizal', brazil_df$customer_city) == TRUE 
  & !is.na(brazil_df$centroid_lat),]




sao_paulo_udh_merged <- udh_merge_ex_with_in(brazil_df, sp_udh, 'sao paulo')


# -----------------------------------------------------------------------------
# 












nn2 <- get.knnx(
  sp_udh[,c("udh.lat", "udh.long")], 
  brazil_df[,c("centroid_lat", "centroid_long")], 
  2)

brazil_df$local_index <- c(1:nrow(brazil_df)) 
sp_udh$local_index <- c(1:nrow(sp_udh))

brazil_df$index_other_data <- nn2$nn.index[,1]
brazil_df$dist_lat <- nn2$nn.dist[,1]
brazil_df$dist_long <- nn2$nn.dist[,2]
brazil_df$total_distancjes <- brazil_df$dist_lat + brazil_df$dist_long

married <- merge(brazil_df,
                 sp_udh,
                 by.x = "index_other_data",
                 by.y = "local_index")




















# ----------------------------------------------------------------------------
# 

nrow(merry[is.na(merry$`IDHM 2010`),])


merry_nas <- merry[is.na(merry$`mc.IDHM 2010`),]


colSums(is.na(metros_1))









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
            , data = metros_1,  family='binomial')
summary(yoyo)