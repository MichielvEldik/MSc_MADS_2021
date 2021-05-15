library(geobr)
library(ggplot2)
library(dplyr)
state <- read_state(year=2010)
muni <- read_municipality(code_muni = 3550308, year=2010)

brazil <- read_country(year = 2010)
n <- read_neighborhood(year=2010)

metros <- read_metro_area(2010)

plot(metros[metros$name_metro == "RM São Paulo",])
# THE ONES TOO
ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM São Paulo",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Sao Paulo (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = metros_1_euclidean[metros_1_euclidean$metro == "sao_paulo"  & metros_1_euclidean$udh.lat < -23.2,]$centroid_lat, 
    x =  metros_1_euclidean[metros_1_euclidean$metro == "sao_paulo"  & metros_1_euclidean$udh.lat < -23.2,]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = metros_1_euclidean[metros_1_euclidean$metro == "sao_paulo" & metros_1_euclidean$udh.lat < -23.2,]$udh.lat, 
    x =  metros_1_euclidean[metros_1_euclidean$metro == "sao_paulo"  & metros_1_euclidean$udh.lat < -23.2,]$udh.long), col = "red", size = 0.2, shape = 23)


ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Belo Horizonte",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  geom_point(aes(
    y = metros_1_euclidean[metros_1_euclidean$metro == "belo_horizonte"  & (metros_1_euclidean$udh.lat < -19 &  metros_1_euclidean$udh.lat > -20.5) ,]$centroid_lat, 
    x =  metros_1_euclidean[metros_1_euclidean$metro == "belo_horizonte" & (metros_1_euclidean$udh.lat < -19 &  metros_1_euclidean$udh.lat > -20.5),]$centroid_long), col = "black", size = 0.4) +
  geom_point(aes(
    y = metros_1_euclidean[metros_1_euclidean$metro == "belo_horizonte" & (metros_1_euclidean$udh.lat < -19 &  metros_1_euclidean$udh.lat > -20.5),]$udh.lat, 
    x =  metros_1_euclidean[metros_1_euclidean$metro == "belo_horizonte"& (metros_1_euclidean$udh.lat < -19 &  metros_1_euclidean$udh.lat > -20.5),]$udh.long), col = "red", size = 0.4, shape = 23)






ggplot() + geom_sf(data=state, 
                   fill=(ifelse(state$name_region == "Norte", "deepskyblue", 
                                ifelse(state$name_region == "Nordeste", "deepskyblue4",
                                       ifelse(state$name_region == "Sul", "coral", 
                                              ifelse(state$name_region == "Sudeste", "deeppink3", "chartreuse"))))), color="white", size=.15, show.legend = FALSE) +
  labs(title = expression(bold("Figure 5")),
       subtitle = expression(italic("Geographic locations of Olist dataset cases in Brazil's five administritave regions"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  annotate("text", x = -48, y = -31, label = "South", col = "coral1", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -63, y = -18, label = "Centerwest", col = "chartreuse3", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -47, y = 2.5, label = "North", col = "deepskyblue1", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -32, y = -10, label = "Northeast", col = "deepskyblue4", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -37, y = -22, label = "Southeast", col = "deeppink3", family = "Times New Roman", fontface = "bold") +
  
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = brazil_df[brazil_df$centroid_lat < 10 & brazil_df$centroid_long < -30 ,]$centroid_lat, 
    x =  brazil_df[brazil_df$centroid_lat < 10 & brazil_df$centroid_long < -30 ,]$centroid_long), size = 0.07) + theme(legend.position="bottom")













muni <- read_municipality(code_muni = 3550308, year=2010)
muni_fortaleza <- read_municipality(code_muni = 2304401, year = 2010)
ggplot() +
  geom_sf(data=muni, fill="grey", color="cadetblue", size=.07, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  geom_point(aes(
    y = metros_1_euclidean[metros_1_euclidean$customer_city == "sao paulo (sp)",]$centroid_lat, 
    x =  metros_1_euclidean[metros_1_euclidean$customer_city == "sao paulo (sp)",]$centroid_long), col = "red", size = 0.01) +
  geom_point(aes(
    y = metros_1_euclidean[metros_1_euclidean$customer_city == "sao paulo (sp)",]$udh.lat, 
    x =  metros_1_euclidean[metros_1_euclidean$customer_city == "sao paulo (sp)",]$udh.long), col = "black", size = 0.01)




muni <- read_municipality(code_muni = 3550308, year=2010)
ggplot() +
  geom_sf(data=muni, fill="grey", color="grey", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  geom_point(aes(
    y = sao_paulo_udh_merged[sao_paulo_udh_merged$customer_city == "sao paulo (sp)",]$udh.lat, 
    x =  sao_paulo_udh_merged[sao_paulo_udh_merged$customer_city == "sao paulo (sp)",]$udh.long))  +
  geom_point(aes(
    y = sao_paulo_udh_merged[sao_paulo_udh_merged$customer_city == "sao paulo (sp)",]$udh.lat, 
    x =  sao_paulo_udh_merged[sao_paulo_udh_merged$customer_city == "sao paulo (sp)",]$udh.long))




ggplot() +
  geom_sf(data=brazil, fill="grey", color="yellow", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  geom_point(aes(
    y = brazil_df$centroid_lat, 
    x =  brazil_df$centroid_long), size = 0.05)


ggplot(binded_df, aes(x= new_idhm)) + 
  geom_histogram(color="black", fill="black") + 
  facet_wrap(~udh_indicator) +
  labs(title = expression(bold("Figure 3")),
       subtitle = expression(italic("Difference in frequencies of Human Development Indices between UDH (1) and municipal data (0) cases"))) +
  xlab("Human Development Index associated with the location of the related to case") + ylab("Count") +
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14))

# The one 
note = expression(paste(italic("Note: "), "variation in longitude along latitude is not adjusted for in this graph."))

ggplot() + geom_sf(data=state, 
                   fill=(ifelse(state$name_region == "Norte", "deepskyblue", 
                                        ifelse(state$name_region == "Nordeste", "deepskyblue4",
                                           ifelse(state$name_region == "Sul", "coral", 
                                              ifelse(state$name_region == "Sudeste", "deeppink3", "chartreuse"))))), color="white", size=.15, show.legend = FALSE) +
  labs(title = expression(bold("Figure 5")),
       subtitle = expression(italic("Geographic locations of Olist dataset cases in Brazil's five administritave regions"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  annotate("text", x = -48, y = -31, label = "South", col = "coral1", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -63, y = -18, label = "Centerwest", col = "chartreuse3", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -47, y = 2.5, label = "North", col = "deepskyblue1", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -32, y = -10, label = "Northeast", col = "deepskyblue4", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -37, y = -22, label = "Southeast", col = "deeppink3", family = "Times New Roman", fontface = "bold") +
  
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = brazil_df[brazil_df$centroid_lat < 10 & brazil_df$centroid_long < -30 ,]$centroid_lat, 
    x =  brazil_df[brazil_df$centroid_lat < 10 & brazil_df$centroid_long < -30 ,]$centroid_long), size = 0.07) + theme(legend.position="bottom")




c_north <- c("AC","AP","AM","PA", "RO", "RR", "TO")
c_south <- c("SC", "RS", "PR")
c_southeast <- c("SP", "RJ", "MG", "ES")
c_northeast <- c("AL", "BA", "CE", "MA", "RN", "SE", "PI", "PB", "PE")
c_centerwest <- c("MT", "MS", "GO", "DF")

brazil_df$north <- 0
brazil_df$south <- 0
brazil_df$southeast <- 0
brazil_df$northeast <- 0
brazil_df$centerwest <- 0

brazil_df <- brazil_df %>% 
  mutate(north = ifelse(customer_state %in% c_north, 1,0),
         south = ifelse(customer_state %in% c_south, 1,0),
         southeast = ifelse(customer_state %in% c_southeast, 1,0),
         northeast = ifelse(customer_state %in% c_northeast, 1,0),
         centerwest = ifelse(customer_state %in% c_centerwest, 1,0),
  )

brazil_df <- brazil_df %>% 
  mutate(region = ifelse(north == 1, "north", ""),
         region = ifelse(northeast == 1, "northeast", region),
         region = ifelse(centerwest == 1, "centerwest", region),
         region = ifelse(southeast == 1, "southeast", region),
         region = ifelse(south == 1, "south", region),
         region = ifelse(customer_state == "DF", "southeast", region), # Belongs to southeast, culturally
         region = as.factor(region))

to_merge <- brazil_df %>% select(customer_state, region)


to_merge <- unique(to_merge)


states <- left_join(state, to_merge, by = c("name_state" = "customer_state"))

ggplot() +
  geom_sf(data=states, color= "black", size=.15) +
  labs(subtitle="Life Expectancy at birth, Brazilian States, 2014", size=8) +
  geom_label(data = states, aes(customer_state, customer_state, label = ID), size = 5, fontface = "bold", 
             nudge_y = states$nudge_y) +
  
  scale_fill_distiller(palette = "Blues", name="Life Expectancy", limits = c(65,80)) +
  theme_minimal()









# Load packages
library(ggplot2)
library(sf)
# Load shapefiles
br_mun <- read_sf('./shapestuff/ggplot2_dataviz-master/shapefile','br_municipios')
br_est <- read_sf('./shapestuff/ggplot2_dataviz-master/shapefile','br_estados')
plot(br_mun)
head(br_mun)
plot(br_est["Region"]) + points(x = brazil_df[brazil_df$centroid_lat < 10 & brazil_df$centroid_long < -30 ,]$centroid_lat, 
                                y =brazil_df[brazil_df$centroid_lat < 10 & brazil_df$centroid_long < -30 ,]$centroid_long)



head(br_est)
# Setting population density classes
br_mun$DensClass <- cut(br_mun$DensPop,breaks=c(0,5,50,500,5000,Inf),
                        labels=c('< 5','5-50','50-500','500-5.000','> 5.000'))
# Plotting
ggplot()+
  geom_sf(aes(fill=DensClass),color='transparent',data=br_mun)+
  geom_sf(fill='transparent',color='white',data=br_est)+
  scale_fill_viridis_d(name='Inhab/km²',
                       guide=guide_legend(
                         direction='horizontal',
                         title.position='top',
                         title.hjust = .5,
                         label.hjust = .5,
                         label.position = 'bottom',
                         keywidth = 3,
                         keyheight = .5
                       ))+
  labs(title="Brazil's demographics",
       subtitle='Population density',
       caption=c('Source: IBGE - Censo demográfico, 2010'))+
  theme_void()+
  theme(title=element_text(face='bold'),
        legend.position = 'bottom') +
  geom_point(aes(
    y = brazil_df$centroid_lat, 
    x =  brazil_df$centroid_long))




