# USED files:
#            full_geomerged_df_3.csv (from part 2: from derived)
#            data (3).xlsx (from Atlas.bi)
#            atlas_data (udh level input + full municip)
#            udh_queried_data
#            dffort.xlsx
# 
# WRITE files:
#            udh_queried_data
#            full_geomerged_df_4.csv (for part 5)


library(readxl)
library(tidyr)
library(dplyr)
library(corrplot)
library(FNN)
library(tmaptools)

# ------------------------------- #
# Cleaning and deriving variables # -------------------------------------------
# ------------------------------- #

# Function is called inside column_fixer function
# Purpose: put data-level indicator for column name (state, municip, udh) 
locality_names <- function(input_data, data_level) {
  
  if (data_level == "state") {
    level_indicator <- "st."
  } else if (data_level == "municip") {
    level_indicator <- "mc."
  } else {
    level_indicator <- "udh."
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
  if (grepl("Brasil", head(new_data[, "Territorialidades"], n = 1)) &
      grepl("Fontes:", tail(new_data[, "Territorialidades"], n = 1))) {
    print("Ready for action.")
    
    new_data <- new_data %>%
      mutate(Territorialidades = as.character(Territorialidades),
             Territorialidades = iconv(Territorialidades, to = "ASCII//TRANSLIT"),
             Territorialidades = tolower(Territorialidades)
      ) %>%
      filter(row_number() <= n()-3
      ) %>%
      filter(!row_number() == 1
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
    
  } else {
    print("Not ready for action")
    return("Something is wrong")
     
  }
}

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

# ---------------------------- #
# udh merge with internal data # ----------------------------------------------
# ---------------------------- #

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
  internal_data$dist <- nn2$nn.dist[,1]
  
  married <- merge(internal_data,
                   external_data,
                   by.x = "index_other_data",
                   by.y = "local_index")
  return(married)
}

# ------------------------ #
# OLD udh_merge_ex_with_in # -------------------------------------------------- 
# ------------------------ #

old_udh_merge_ex_with_in <- function(internal_data, external_data, city) {
  
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

# ------------------------------------ #
# Define municipalities per metro area # -------------------------------------
# ------------------------------------ #




# Region: North
# State: Para (pa)
belem_metro_municips <- c(
  "ANANINDEUA",
  "BELÉM",
  "BENEVIDES",
  "CASTANHAL",
  "MARITUBA",
  "SANTA BÁRBARA DO PARÁ",
  "SANTA IZABEL DO PARÁ")

belem_metro_municips <- iconv(belem_metro_municips, to = "ASCII//TRANSLIT")
belem_metro_municips <- tolower(belem_metro_municips)
belem_metro_municips <- paste(belem_metro_municips, "(pa)")


# Region: North 
# State: Amazonas (am)
manaus_metro_municips <- c(
  "AUTAZES",
  "CAREIRO",
  "CAREIRO DA VÁRZEA",
  "IRANDUBA",
  "ITACOATIARA",
  "ITAPIRANGA",
  "MANACAPURU",
  "MANAQUIRI",
  "MANAUS",
  "NOVO AIRÃO",
  "PRESIDENTE FIGUEIREDO",
  "RIO PRETO DA EVA",
  "SILVES")

manaus_metro_municips <- iconv(manaus_metro_municips, to = "ASCII//TRANSLIT")
manaus_metro_municips <- tolower(manaus_metro_municips)
manaus_metro_municips <- paste(manaus_metro_municips, "(am)")


# Region: Northeast
# State: Ceara (ce) 
fortaleza_metro_municips <- c(
  "Aquiraz", 
  "Cascavel",
  "Caucaia",
  "Chorozinho",
  "Eusébio",
  "Fortaleza",
  "Guaiuba", 
  "Horizonte",
  "Itaitinga",
  "Maracanaú",
  "Maranguape", 
  "Pacajus",
  "Pacatuba",
  "Paracuru",
  "Paraipaba",
  "Pindoretama",
  "São Gonçalo do Amarante",
  "São Luís do Curu e Trairi")
fortaleza_metro_municips <- iconv(fortaleza_metro_municips, to = "ASCII//TRANSLIT")
fortaleza_metro_municips <- tolower(fortaleza_metro_municips)
fortaleza_metro_municips <- paste(fortaleza_metro_municips, "(ce)")

# Region: Northeast
# State: Pernambuco (pe)
recife_metro_municips <- c(
  "ABREU E LIMA",
  "ARAÇOIABA",
  "CABO DE SANTO AGOSTINHO",
  "CAMARAGIBE",
  "GOIANA",
  "IGARASSU",
  "ILHA DE ITAMARACÁ",
  "IPOJUCA",
  "ITAPISSUMA",
  "JABOATÃO DOS GUARARAPES",
  "MORENO",
  "OLINDA",
  "PAULISTA",
  "RECIFE",
  "SÃO LOURENÇO DA MATA")

recife_metro_municips <- iconv(recife_metro_municips, to = "ASCII//TRANSLIT")
recife_metro_municips <- tolower(recife_metro_municips)
recife_metro_municips <- paste(recife_metro_municips, "(pe)")

# Region: Northeast
# State: Bahia (ba)
salvador_metro_municips <- c(
  "CAMAÇARI",
  "CANDEIAS",
  "DIAS D’ÁVILA",
  "ITAPARICA",
  "LAURO DE FREITAS",
  "MADRE DE DEUS",
  "MATA DE SÃO JOÃO",
  "POJUCA",
  "SALVADOR",
  "SÃO FRANCISCO DO CONDE",
  "SÃO SEBASTIÃO DO PASSÉ",
  "SIMÕES FILHO",
  "VERA CRUZ")

salvador_metro_municips <- iconv(salvador_metro_municips, to = "ASCII//TRANSLIT")
salvador_metro_municips <- tolower(salvador_metro_municips)
salvador_metro_municips <- paste(salvador_metro_municips, "(ba)")

# Region: Northeast
# State: Rio Grande do Norte (rn)
natal_metro_municips <- c(
  "ARÊS",
  "CEARÁ-MIRIM",
  "EXTREMOZ",
  "GOIANINHA",
  "IELMO MARINHO",
  "MACAÍBA",
  "MAXARANGUAPE",
  "MONTE ALEGRE",
  "NATAL",
  "NÍSIA FLORESTA",
  "PARNAMIRIM",
  "SÃO GONÇALO DO AMARANTE",
  "SÃO JOSÉ DE MIPIBU",
  "VERA CRUZ")

natal_metro_municips <- iconv(natal_metro_municips, to = "ASCII//TRANSLIT")
natal_metro_municips <- tolower(natal_metro_municips)
natal_metro_municips <- paste(natal_metro_municips, "(rn)")


# Region: Northeast
# State: Alagoas (al)
maceio_metro_municips <- c(
  "ATALAIA",
  "BARRA DE SANTO ANTÔNIO",
  "BARRA DE SÃO MIGUEL",
  "COQUEIRO SECO",
  "MACEIÓ",
  "MARECHAL DEODORO",
  "MESSIAS",
  "MURICI",
  "PARIPUEIRA",
  "PILAR",
  "RIO LARGO",
  "SANTA LUZIA DO NORTE",
  "SATUBA")

maceio_metro_municips <- iconv(maceio_metro_municips, to = "ASCII//TRANSLIT")
maceio_metro_municips <- tolower(maceio_metro_municips)
maceio_metro_municips <- paste(maceio_metro_municips, "(al)")

# Region: Northeast
# State: Maranhao (ma)

sao_luis_metro_municips <- c(
  "ALCÂNTARA",
  "BACABEIRA",
  "ICATU",
  "PAÇO DO LUMIAR",
  "RAPOSA",
  "ROSÁRIO",
  "SANTA RITA",
  "SÃO JOSÉ DE RIBAMAR",
  "SÃO LUÍS",
  "AXIXÁ",
  "CACHOEIRA GRANDE",
  "MORROS",
  "PRESIDENTE JUSCELINO")

sao_luis_metro_municips <- iconv(sao_luis_metro_municips, to = "ASCII//TRANSLIT")
sao_luis_metro_municips <- tolower(sao_luis_metro_municips)
sao_luis_metro_municips <- paste(sao_luis_metro_municips, "(ma)")

# Region: Northeast
# State: Piaui (pi) e Maranhao (ma) 
teresina_metro_municips <- c(
  "altos (pi)",
  "Beneditinos (pi)",
  "Coivaras (pi)",
  "Curralinhos (pi)",
  "Demerval Lobão (pi)",
  "José de Freitas (pi)",
  "Lagoa Alegre (pi)",
  "Lagoa do Piauí (pi)",
  "Miguel Leão (pi)",
  "Monsenhor Gil (pi)",
  "Nazária (pi)",
  "Pau D'Arco do Piauí (pi)",
  "Teresina (pi)",
  "União (pi)",
  "Timon")

teresina_metro_municips <- iconv(teresina_metro_municips, to = "ASCII//TRANSLIT")
teresina_metro_municips <- tolower(teresina_metro_municips)
# No additional one due to inter-state 

# Region: Northeast
# State: Bahia (ba) e Pernambuco (pe)
petrolina_juazeiro_metro_municips <- c(
  "CASA NOVA (ba)",
  "CURAÇÁ (ba)",
  "JUAZEIRO (ba)",
  "SOBRADINHO (ba)",
  "LAGOA GRANDE (pe)",
  "OROCÓ (pe)",
  "PETROLINA (pe)",
  "SANTA MARIA DA BOA VISTA (pe)")

petrolina_juazeiro_metro_municips <- iconv(petrolina_juazeiro_metro_municips, to = "ASCII//TRANSLIT")
petrolina_juazeiro_metro_municips <- tolower(petrolina_juazeiro_metro_municips)

# Region: Central-west
# State: Goias (go)
goiania_metro_municips <- c(
  "ABADIA DE GOIÁS",
  "APARECIDA DE GOIÂNIA",
  "ARAGOIÂNIA",
  "BELA VISTA DE GOIÁS",
  "BONFINÓPOLIS",
  "BRAZABRANTES",
  "CALDAZINHA",
  "CATURAÍ",
  "GOIANÁPOLIS",
  "GOIANIRA",
  "GOIÂNIA",
  "GUAPÓ",
  "HIDROLÂNDIA",
  "INHUMAS",
  "NERÓPOLIS",
  "NOVA VENEZA",
  "SANTO ANTÔNIO DE GOIÁS",
  "SENADOR CANEDO",
  "TEREZÓPOLIS DE GOIÁS",
  "TRINDADE")

goiania_metro_municips <- iconv(goiania_metro_municips, to = "ASCII//TRANSLIT")
goiania_metro_municips <- tolower(goiania_metro_municips)
goiania_metro_municips <- paste(goiania_metro_municips, "(go)")

# Region: Central-west
# State: Distrito Federal (df)
brasilia_metro_municips <- c(
  "brasilia",
  "ceilandia",
  "guara",
  "santa maria",
  "sobradinho",
  "taguatinga")

brasilia_metro_municips <- iconv(brasilia_metro_municips, to = "ASCII//TRANSLIT")
brasilia_metro_municips <- tolower(brasilia_metro_municips)
brasilia_metro_municips <- paste(brasilia_metro_municips, "(df)")

# Region: Central-west 
# State: Mato Grosso (mt)

vale_do_rio_cuiaba_metro_municips <- c(
  "ACORIZAL",
  "CHAPADA DOS GUIMARÃES",
  "CUIABÁ",
  "NOSSA SENHORA DO LIVRAMENTO",
  "SANTO ANTÔNIO DE LEVERGER",
  "VÁRZEA GRANDE" # Last one from metro region 
)

vale_do_rio_cuiaba_metro_municips <- iconv(vale_do_rio_cuiaba_metro_municips, to = "ASCII//TRANSLIT")
vale_do_rio_cuiaba_metro_municips <- tolower(vale_do_rio_cuiaba_metro_municips)
vale_do_rio_cuiaba_metro_municips <- paste(vale_do_rio_cuiaba_metro_municips, "(mt)")

# Region: Southeast 
# State: Sao Paulo (sp)
sao_paulo_metro_municips <- c(
  "São Paulo",
  "Arujá",
  "Barueri",
  "Biritiba Mirim",
  "Caieiras",
  "Cajamar",
  "Carapicuíba",
  "Cotia",
  "Diadema",
  "Embu",
  "Embu-Guaçu",
  "Ferraz de Vasconcelos",
  "Francisco Morato",
  "Franco da Rocha",
  "Guararema",
  "Guarulhos",
  "Itapecerica da Serra",
  "Itapevi",
  "Itaquaquecetuba",
  "Jandira",
  "Juquitiba",
  "Mairiporã",
  "Mauá",
  "Mogi das Cruzes",
  "Osasco",
  "Pirapora do Bom Jesus",
  "Poá",
  "Ribeirão Pires",
  "Rio Grande da Serra",
  "Salesópolis",
  "Santa Isabel",
  "Santana do Parnaíba",
  "Santo André",
  "São Bernardo do Campo",
  "São Caetano do Sul",
  "São Lourenço da Serra Suzano",
  "Suzano",
  "Taboão da Serra",
  "Vargem Grande Paulista")
sao_paulo_metro_municips <- iconv(sao_paulo_metro_municips, to = "ASCII//TRANSLIT")
sao_paulo_metro_municips <- tolower(sao_paulo_metro_municips)
sao_paulo_metro_municips <- paste(sao_paulo_metro_municips, "(sp)")

# Region: Southeast
# State: Rio de Janeiro (rj)
rio_dj_metro_municips <- c(
  "CACHOEIRAS DE MACACU",
  "BELFORD ROXO",
  "DUQUE DE CAXIAS",
  "GUAPIMIRIM",
  "ITABORAÍ",
  "ITAGUAÍ",
  "JAPERI",
  "MAGÉ",
  "MARICÁ",
  "MESQUITA",
  "NILÓPOLIS",
  "NITERÓI",
  "NOVA IGUAÇU",
  "PARACAMBI",
  "QUEIMADOS",
  "RIO BONITO",
  "RIO DE JANEIRO",
  "SÃO GONÇALO",
  "SÃO JOÃO DE MERITI",
  "SEROPÉDICA",
  "TANGUÁ")

rio_dj_metro_municips <- iconv(rio_dj_metro_municips, to = "ASCII//TRANSLIT")
rio_dj_metro_municips <- tolower(rio_dj_metro_municips)
rio_dj_metro_municips <- paste(rio_dj_metro_municips, "(rj)")

# Region: Southeast
# State: Minas Gerais (mg)
belo_horizonte_metro_municips <- c(
  "BALDIM",
  "BELO HORIZONTE",
  "BETIM",
  "BRUMADINHO",
  "CAETÉ",
  "CAPIM BRANCO",
  "CONFINS",
  "CONTAGEM",
  "ESMERALDAS",
  "FLORESTAL",
  "IBIRITÉ",
  "IGARAPÉ",
  "ITAGUARA",
  "ITATIAIUÇU",
  "JABOTICATUBAS",
  "JUATUBA",
  "LAGOA SANTA",
  "MÁRIO CAMPOS",
  "MATEUS LEME",
  "MATOZINHOS",
  "NOVA LIMA",
  "NOVA UNIÃO",
  "PEDRO LEOPOLDO",
  "RAPOSOS",
  "RIBEIRÃO DAS NEVES",
  "RIO ACIMA",
  "RIO MANSO",
  "SABARÁ",
  "SANTA LUZIA",
  "SÃO JOAQUIM DE BICAS",
  "SÃO JOSÉ DA LAPA",
  "SARZEDO",
  "TAQUARAÇU DE MINAS",
  "VESPASIANO")

belo_horizonte_metro_municips <- iconv(belo_horizonte_metro_municips, to = "ASCII//TRANSLIT")
belo_horizonte_metro_municips <- tolower(belo_horizonte_metro_municips)
belo_horizonte_metro_municips <- paste(belo_horizonte_metro_municips, "(mg)")


# Region: Southeast
# State: Espirito Santo (es)
grande_vitoria_metro_municips <- c(
  "CARIACICA",
  "FUNDÃO",
  "GUARAPARI",
  "SERRA",
  "VIANA",
  "VILA VELHA",
  "VITÓRIA")
grande_vitoria_metro_municips <- iconv(grande_vitoria_metro_municips, to = "ASCII//TRANSLIT")
grande_vitoria_metro_municips <- tolower(grande_vitoria_metro_municips)
grande_vitoria_metro_municips <- paste(grande_vitoria_metro_municips, "(es)")


# Region: Southeast
# State: Sao Paulo (sp)
sorocaba_metro_municips <- c(
  "ALAMBARI",
  "ALUMÍNIO",
  "ARAÇARIGUAMA",
  "ARAÇOIABA DA SERRA",
  "BOITUVA",
  "CAPELA DO ALTO",
  "CERQUILHO",
  "CESÁRIO LANGE",
  "IBIÚNA",
  "IPERÓ",
  "ITAPETININGA",
  "ITU",
  "JUMIRIM",
  "MAIRINQUE",
  "PIEDADE",
  "PILAR DO SUL",
  "PORTO FELIZ",
  "SALTO",
  "SALTO DE PIRAPORA",
  "SÃO MIGUEL ARCANJO",
  "SÃO ROQUE",
  "SARAPUÍ",
  "SOROCABA",
  "TAPIRAÍ",
  "TATUÍ",
  "TIETÊ",
  "VOTORANTIM")
sorocaba_metro_municips <- iconv(sorocaba_metro_municips, to = "ASCII//TRANSLIT")
sorocaba_metro_municips <- tolower(sorocaba_metro_municips)
sorocaba_metro_municips <- paste(sorocaba_metro_municips, "(sp)")


# Region: Southeast
# State: Sao Paulo
baixada_santista_metro_municips <- c(
  "Bertioga",
  "Cubatão",
  "Guarujá",
  "Itanhaém",
  "Mongaguá",
  "Peruíbe",
  "Praia Grande",
  "Santos",
  "São Vicente")
baixada_santista_metro_municips <- iconv(baixada_santista_metro_municips, to = "ASCII//TRANSLIT")
baixada_santista_metro_municips <- tolower(baixada_santista_metro_municips)
baixada_santista_metro_municips <- paste(baixada_santista_metro_municips, "(sp)")


# Region: Southeast
# State: Sao Paulo
campinas_metro_municips <- c(
  "Americana",
  "Artur Nogueira",
  "Campinas",
  "Cosmópolis",
  "Engenheiro Coelho",
  "Holambra",
  "Hortolândia",
  "Indaiatuba",
  "Itatiba",
  "Jaguariúna",
  "Monte Mor",
  "Morungaba",
  "Nova Odessa",
  "Paulínia",
  "Pedreira",
  "Santa Bárbara d'Oeste",
  "Santo Antônio de Posse",
  "Sumaré",
  "Valinhos",
  "Vinhedo")
campinas_metro_municips <- iconv(campinas_metro_municips, to = "ASCII//TRANSLIT")
campinas_metro_municips <- tolower(campinas_metro_municips)
campinas_metro_municips <- paste(campinas_metro_municips, "(sp)")

# Region: Southeast
# State: Sao Paulo
litoral_norte_metro_municips <- c(
  "APARECIDA",
  "ARAPEÍ",
  "AREIAS",
  "BANANAL",
  "CAÇAPAVA",
  "CACHOEIRA PAULISTA",
  "CAMPOS DO JORDÃO",
  "CANAS",
  "CARAGUATATUBA",
  "CRUZEIRO",
  "CUNHA",
  "GUARATINGUETÁ",
  "IGARATÁ",
  "ILHABELA",
  "JACAREÍ",
  "JAMBEIRO",
  "LAGOINHA",
  "LAVRINHAS",
  "LORENA",
  "MONTEIRO LOBATO",
  "NATIVIDADE DA SERRA",
  "PARAIBUNA",
  "PINDAMONHANGABA",
  "PIQUETE",
  "POTIM",
  "QUELUZ",
  "REDENÇÃO DA SERRA",
  "ROSEIRA",
  "SANTA BRANCA",
  "SANTO ANTÔNIO DO PINHAL",
  "SÃO BENTO DO SAPUCAÍ",
  "SÃO JOSÉ DO BARREIRO",
  "SÃO JOSÉ DOS CAMPOS",
  "SÃO LUIZ DO PARAITINGA",
  "SÃO SEBASTIÃO",
  "SILVEIRAS",
  "TAUBATÉ",
  "TREMEMBÉ",
  "UBATUBA")

litoral_norte_metro_municips <- iconv(litoral_norte_metro_municips, to = "ASCII//TRANSLIT")
litoral_norte_metro_municips <- tolower(litoral_norte_metro_municips)
litoral_norte_metro_municips <- paste(litoral_norte_metro_municips, "(sp)")




# Region: South
# State: Rio Grande do Sul (rs)
porto_alegre_metro_municips <- c(
  "ALVORADA",
  "ARARICÁ",
  "ARROIO DOS RATOS",
  "CACHOEIRINHA",
  "CAMPO BOM",
  "CANOAS",
  "CAPELA DE SANTANA",
  "CHARQUEADAS",
  "DOIS IRMÃOS",
  "ELDORADO DO SUL",
  "ESTÂNCIA VELHA",
  "ESTEIO",
  "GLORINHA",
  "GRAVATAÍ",
  "GUAÍBA",
  "IGREJINHA",
  "IVOTI",
  "MONTENEGRO",
  "NOVA HARTZ",
  "NOVA SANTA RITA",
  "NOVO HAMBURGO",
  "PAROBÉ",
  "PORTÃO",
  "PORTO ALEGRE",
  "ROLANTE",
  "SANTO ANTÔNIO DA PATRULHA",
  "SÃO JERÔNIMO",
  "SÃO LEOPOLDO",
  "SÃO SEBASTIÃO DO CAÍ",
  "SAPIRANGA",
  "SAPUCAIA DO SUL",
  "TAQUARA",
  "TRIUNFO",
  "VIAMÃO")
porto_alegre_metro_municips <- iconv(porto_alegre_metro_municips, to = "ASCII//TRANSLIT")
porto_alegre_metro_municips <- tolower(porto_alegre_metro_municips)
porto_alegre_metro_municips <- paste(porto_alegre_metro_municips, "(rs)")


# Region: South
# State: Parana (pr)
curitiba_metro_municips <- c(
  "ADRIANÓPOLIS",
  "AGUDOS DO SUL",
  "ALMIRANTE TAMANDARÉ",
  "ARAUCÁRIA",
  "BALSA NOVA",
  "BOCAIÚVA DO SUL",
  "CAMPINA GRANDE DO SUL",
  "CAMPO DO TENENTE",
  "CAMPO LARGO",
  "CAMPO MAGRO",
  "CERRO AZUL",
  "COLOMBO",
  "CONTENDA",
  "CURITIBA",
  "DOUTOR ULYSSES",
  "FAZENDA RIO GRANDE",
  "ITAPERUÇU",
  "LAPA",
  "MANDIRITUBA",
  "PIÊN",
  "PINHAIS",
  "PIRAQUARA",
  "QUATRO BARRAS",
  "QUITANDINHA",
  "RIO BRANCO DO SUL",
  "RIO NEGRO",
  "SÃO JOSÉ DOS PINHAIS",
  "TIJUCAS DO SUL",
  "TUNAS DO PARANÁ")
curitiba_metro_municips <- iconv(curitiba_metro_municips, to = "ASCII//TRANSLIT")
curitiba_metro_municips <- tolower(curitiba_metro_municips)
curitiba_metro_municips <- paste(curitiba_metro_municips, "(pr)")


# Region: South
# State: Santa Catarina (sc)
florianopolis_metro_municips <- c(
  "ÁGUAS MORNAS",
  "ANTÔNIO CARLOS",
  "BIGUAÇU",
  "FLORIANÓPOLIS",
  "GOVERNADOR CELSO RAMOS",
  "PALHOÇA",
  "SANTO AMARO DA IMPERATRIZ",
  "SÃO JOSÉ",
  "SÃO PEDRO DE ALCÂNTARA")
florianopolis_metro_municips <- iconv(florianopolis_metro_municips, to = "ASCII//TRANSLIT")
florianopolis_metro_municips <- tolower(florianopolis_metro_municips)
florianopolis_metro_municips <- paste(florianopolis_metro_municips, "(sc)")




# all together

all_udh_municips <- baixada_santista_metro_municips
all_udh_municips <- append(all_udh_municips, belem_metro_municips)
all_udh_municips <- append(all_udh_municips, belo_horizonte_metro_municips)
all_udh_municips <- append(all_udh_municips, brasilia_metro_municips)
all_udh_municips <- append(all_udh_municips, campinas_metro_municips)
all_udh_municips <- append(all_udh_municips, curitiba_metro_municips)
all_udh_municips <- append(all_udh_municips, florianopolis_metro_municips)
all_udh_municips <- append(all_udh_municips, fortaleza_metro_municips)
all_udh_municips <- append(all_udh_municips, goiania_metro_municips)
all_udh_municips <- append(all_udh_municips, grande_vitoria_metro_municips)
all_udh_municips <- append(all_udh_municips, litoral_norte_metro_municips)
all_udh_municips <- append(all_udh_municips, maceio_metro_municips)
all_udh_municips <- append(all_udh_municips, manaus_metro_municips)
all_udh_municips <- append(all_udh_municips, natal_metro_municips)
all_udh_municips <- append(all_udh_municips, petrolina_juazeiro_metro_municips)
all_udh_municips <- append(all_udh_municips, porto_alegre_metro_municips)
all_udh_municips <- append(all_udh_municips, recife_metro_municips)
all_udh_municips <- append(all_udh_municips, rio_dj_metro_municips)
all_udh_municips <- append(all_udh_municips, salvador_metro_municips)
all_udh_municips <- append(all_udh_municips, sao_luis_metro_municips)
all_udh_municips <- append(all_udh_municips, sao_paulo_metro_municips)
all_udh_municips <- append(all_udh_municips, sorocaba_metro_municips)
all_udh_municips <- append(all_udh_municips, teresina_metro_municips)
all_udh_municips <- append(all_udh_municips, vale_do_rio_cuiaba_metro_municips)


                                                      # all above can be run! 
# ---------------------------------------------- #
# Call functions part (NO NEED TO EXECUTE AGAIN) # ----------------------------
# ---------------------------------------------- #

# external data | udh (neighborhood) level, by metrpopolitan region
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
manaus_udh <- read_excel("./atlas_data/manaus_udh.xlsx")
sao_luis_udh <- read_excel("./atlas_data/sao_luis_udh.xlsx")
sorocaba_udh <- read_excel("./atlas_data/sorocaba_udh.xlsx")
baixada_santista_udh <- read_excel("./atlas_data/baixada_santista_udh.xlsx")
brasilia_udh <- read_excel("./atlas_data/brasilia_udh.xlsx")
teresina_udh <- read_excel("./atlas_data/teresina_udh.xlsx")
petrolina_juazeiro_udh <- read_excel("./atlas_data/petrolina_juazeiro_udh.xlsx")
vale_do_rio_cuiaba_udh <- read_excel("./atlas_data/vale_do_rio_cuiaba_udh.xlsx")
vale_do_paraiba_e_litoral_norte_udh <- read_excel("./atlas_data/vale_do_paraiba_e_litoral_norte_udh.xlsx")

# Salling function for various level datasets
sp_udh <- column_fixer(sp_udh, "udh")
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
manaus_udh <- column_fixer(manaus_udh, "udh")
sao_luis_udh <- column_fixer(sao_luis_udh, "udh")
sorocaba_udh <- column_fixer(sorocaba_udh, "udh")
baixada_santista_udh <- column_fixer(baixada_santista_udh, "udh")
brasilia_udh <- column_fixer(brasilia_udh, "udh")
teresina_udh <- column_fixer(teresina_udh, "udh")
petrolina_juazeiro_udh <- column_fixer(petrolina_juazeiro_udh, "udh")
vale_do_rio_cuiaba_udh <- column_fixer(vale_do_rio_cuiaba_udh, "udh")
vale_do_paraiba_e_litoral_norte_udh <- column_fixer(vale_do_paraiba_e_litoral_norte_udh, "udh")

# call function with datasets
sp_udh <- coordinate_retriever(sp_udh, "sao paulo")
fortaleza_udh <- coordinate_retriever(fortaleza_udh, "fortaleza")
recife_udh <- coordinate_retriever(recife_udh, "recife")
rio_dj_udh <- coordinate_retriever(rio_dj_udh, "rio de janeiro")
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
manaus_udh <- coordinate_retriever(manaus_udh, "manaus")
sao_luis_udh <- coordinate_retriever(sao_luis_udh, "sao luis")
sorocaba_udh <- coordinate_retriever(sorocaba_udh, "sorocaba")
baixada_santista_udh <- coordinate_retriever(baixada_santista_udh, "baixada santista")
brasilia_udh <- coordinate_retriever(brasilia_udh, "distrito federal")
teresina_udh <- coordinate_retriever(teresina_udh, "teresina")
petrolina_juazeiro_udh <- coordinate_retriever(petrolina_juazeiro_udh, "petrolina juazeiro")
vale_do_rio_cuiaba_udh <- coordinate_retriever(vale_do_rio_cuiaba_udh, "mato grosso")
vale_do_paraiba_e_litoral_norte_udh <- coordinate_retriever(vale_do_paraiba_e_litoral_norte_udh, "litoral norte")


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
write.csv(campinas_udh, "./udh_queried_data/campinas_udh_queried.csv")
write.csv(curitiba_udh, "./udh_queried_data/curtiba_udh_queried.csv")
write.csv(belem_udh, "./udh_queried_data/belem_udh_queried.csv")
write.csv(goiania_udh, "./udh_queried_data/goiania_udh_queried.csv")
write.csv(grande_vitoria_udh, "./udh_queried_data/grande_vitoria_udh_queried.csv")
write.csv(florianopolis_udh, "./udh_queried_data/florianopolis_udh_queried.csv")
write.csv(manaus_udh, "./udh_queried_data/manaus_udh_queried.csv")
write.csv(sao_luis_udh, "./udh_queried_data/sao_luis_udh_queried.csv")
write.csv(sorocaba_udh, "./udh_queried_data/sorocaba_udh_queried.csv")
write.csv(baixada_santista_udh, "./udh_queried_data/baixada_santista_udh_queried.csv")
write.csv(brasilia_udh, "./udh_queried_data/brasilia_udh_queried.csv")
write.csv(teresina_udh, "./udh_queried_data/teresina_udh_queried.csv")
write.csv(petrolina_juazeiro_udh, "./udh_queried_data/petrolina_juazeiro_udh_queried.csv")
write.csv(vale_do_rio_cuiaba_udh, "./udh_queried_data/vale_do_rio_cuiaba_udh_queried.csv")
write.csv(vale_do_paraiba_e_litoral_norte_udh, "./udh_queried_data/vale_do_paraiba_e_litoral_norte_udh_queried.csv")



# ------------------------- # Whole process sped up # (EXECUTE)----------------

# internal data
input <- read.csv("full_geomerged_df_2.csv")
brazil_df <- input
brazil_df <- brazil_df %>%
  mutate(customer_city = paste(customer_city, "(", sep = " "),
         customer_city = paste(customer_city, customer_state, sep = ""),
         customer_city = paste(customer_city, ")", sep=""),
         
         # already done beforehand but do it again just to be certain
         customer_city = tolower(customer_city),
         customer_city = iconv(customer_city, to = "ASCII//TRANSLIT")
        )

# keytjes
keys <- read_excel("dffort.xlsx")
keys <- keys %>%
  drop_na() %>%
  mutate(new_names = as.character(new_names),
         new_names = iconv(new_names, to = "ASCII//TRANSLIT"),
         new_names = tolower(new_names)) %>%
  select(- nummie)
keys <- keys[!duplicated(keys$new_names),]

brazil_df <- merge(brazil_df,
                keys,
                by.x = "customer_city",
                by.y = "old_names",
                all.x = TRUE)
brazil_df <- brazil_df %>%
  mutate(customer_city = ifelse(is.na(new_names) == TRUE, customer_city, new_names)) %>%
  select(- new_names)



sp_udh <- read.csv("./udh_queried_data/sao_paulo_udh_queried.csv")
fortaleza_udh <- read.csv("./udh_queried_data/fortaleza_udh_queried.csv")
recife_udh <- read.csv("./udh_queried_data/recife_udh_queried.csv")
rio_dj_udh <- read.csv("./udh_queried_data/rio_dj_udh_queried.csv")
salvador_udh <- read.csv("./udh_queried_data/salvador_udh_queried.csv")
porto_alegre_udh <- read.csv("./udh_queried_data/porto_alegre_udh_queried.csv")
natal_udh <- read.csv("./udh_queried_data/natal_udh_queried.csv")
maceio_udh <- read.csv("./udh_queried_data/maceio_udh_queried.csv")
belo_horizonte_udh <- read.csv("./udh_queried_data/belo_horizonte_udh_queried.csv")
campinas_udh <- read.csv("./udh_queried_data/campinas_udh_queried.csv")
curitiba_udh <- read.csv("./udh_queried_data/curtiba_udh_queried.csv")
belem_udh <- read.csv("./udh_queried_data/belem_udh_queried.csv")
goiania_udh <- read.csv("./udh_queried_data/goiania_udh_queried.csv")
grande_vitoria_udh <- read.csv("./udh_queried_data/grande_vitoria_udh_queried.csv")
florianopolis_udh <- read.csv("./udh_queried_data/florianopolis_udh_queried.csv")
manaus_udh <- read.csv("./udh_queried_data/manaus_udh_queried.csv")
sao_luis_udh <- read.csv("./udh_queried_data/sao_luis_udh_queried.csv")
sorocaba_udh <-read.csv("./udh_queried_data/sorocaba_udh_queried.csv")
baixada_santista_udh <- read.csv("./udh_queried_data/baixada_santista_udh_queried.csv")
brasilia_udh <- read.csv("./udh_queried_data/brasilia_udh_queried.csv")
teresina_udh <- read.csv("./udh_queried_data/teresina_udh_queried.csv")
petrolina_juazeiro_udh <- read.csv("./udh_queried_data/petrolina_juazeiro_udh_queried.csv")
vale_do_rio_cuiaba_udh <- read.csv("./udh_queried_data/vale_do_rio_cuiaba_udh_queried.csv")
vale_do_paraiba_e_litoral_norte_udh <- read.csv("./udh_queried_data/vale_do_paraiba_e_litoral_norte_udh_queried.csv")


# New merge functions (RUN)
sao_paulo_udh_merged <- udh_merge_ex_with_in(brazil_df, sp_udh, sao_paulo_metro_municips)
fortaleza_udh_merged <- udh_merge_ex_with_in(brazil_df, fortaleza_udh, fortaleza_metro_municips)
recife_udh_merged <- udh_merge_ex_with_in(brazil_df, recife_udh, recife_metro_municips)
rio_dj_udh_merged <- udh_merge_ex_with_in(brazil_df, rio_dj_udh, rio_dj_metro_municips)
salvador_udh_merged <- udh_merge_ex_with_in(brazil_df, salvador_udh, salvador_metro_municips)
porto_alegre_udh_merged <- udh_merge_ex_with_in(brazil_df, porto_alegre_udh, porto_alegre_metro_municips)
natal_udh_merged <- udh_merge_ex_with_in(brazil_df, natal_udh, natal_metro_municips)
maceio_udh_merged <- udh_merge_ex_with_in(brazil_df, maceio_udh, maceio_metro_municips)
belo_horizonte_udh_merged <- udh_merge_ex_with_in(brazil_df, belo_horizonte_udh, belo_horizonte_metro_municips)
campinas_udh_merged <- udh_merge_ex_with_in(brazil_df, campinas_udh, campinas_metro_municips)
curitiba_udh_merged <- udh_merge_ex_with_in(brazil_df, curitiba_udh, curitiba_metro_municips)
belem_udh_merged <- udh_merge_ex_with_in(brazil_df, belem_udh, belem_metro_municips)
goiania_udh_merged <- udh_merge_ex_with_in(brazil_df, goiania_udh, goiania_metro_municips)
grande_vitoria_udh_merged <- udh_merge_ex_with_in(brazil_df, grande_vitoria_udh, grande_vitoria_metro_municips)
florianopolis_udh_merged <- udh_merge_ex_with_in(brazil_df, florianopolis_udh, florianopolis_metro_municips)
manaus_udh_merged <- udh_merge_ex_with_in(brazil_df, manaus_udh, manaus_metro_municips)
sao_luis_udh_merged <- udh_merge_ex_with_in(brazil_df, sao_luis_udh, sao_luis_metro_municips)
sorocaba_udh_merged <- udh_merge_ex_with_in(brazil_df, sorocaba_udh, sorocaba_metro_municips)
baixada_santista_udh_merged <- udh_merge_ex_with_in(brazil_df, baixada_santista_udh, baixada_santista_metro_municips)
brasilia_udh_merged <- udh_merge_ex_with_in(brazil_df, brasilia_udh, brasilia_metro_municips) 
teresina_udh_merged <- udh_merge_ex_with_in(brazil_df, teresina_udh, teresina_metro_municips)
petrolina_juazeiro_udh_merged <- udh_merge_ex_with_in(brazil_df, petrolina_juazeiro_udh, petrolina_juazeiro_metro_municips )
vale_do_rio_cuiaba_udh_merged <- udh_merge_ex_with_in(brazil_df, vale_do_rio_cuiaba_udh, vale_do_rio_cuiaba_metro_municips)
vale_do_paraiba_e_litoral_norte_udh_merged <- udh_merge_ex_with_in(brazil_df, vale_do_paraiba_e_litoral_norte_udh, litoral_norte_metro_municips)


# Old merge functions (for reference DONT RUN)
# old_sao_paulo_udh_merged <- old_udh_merge_ex_with_in(brazil_df, sp_udh, "sao paulo (sp)")
# old_fortaleza_udh_merged <- old_udh_merge_ex_with_in(brazil_df, fortaleza_udh, "fortaleza (ce)")
# old_recife_udh_merged <- old_udh_merge_ex_with_in(brazil_df, recife_udh, "recife (pe)")
# old_rio_dj_udh_merged <- old_udh_merge_ex_with_in(brazil_df, rio_dj_udh, "rio de janeiro (rj)")
# old_salvador_udh_merged <- old_udh_merge_ex_with_in(brazil_df, salvador_udh, "salvador (ba)")
# old_porto_alegre_udh_merged <- old_udh_merge_ex_with_in(brazil_df, porto_alegre_udh, "porto alegre (rs)")
# old_natal_udh_merged <- old_udh_merge_ex_with_in(brazil_df, natal_udh, "natal (rn)")
# old_maceio_udh_merged <- old_udh_merge_ex_with_in(brazil_df, maceio_udh, "maceio (al)")
# old_belo_horizonte_udh_merged <- old_udh_merge_ex_with_in(brazil_df, belo_horizonte_udh, "belo horizonte (mg)")
# old_campinas_udh_merged <- old_udh_merge_ex_with_in(brazil_df, campinas_udh, "campinas (sp)")
# old_curitiba_udh_merged <- old_udh_merge_ex_with_in(brazil_df, curitiba_udh, "curitiba (pr)")
# old_belem_udh_merged <- old_udh_merge_ex_with_in(brazil_df, belem_udh, "belem (pa)")
# old_goiania_udh_merged <- old_udh_merge_ex_with_in(brazil_df, goiania_udh, "goiania (go)")
# old_grande_vitoria_udh_merged <- old_udh_merge_ex_with_in(brazil_df, grande_vitoria_udh, "vitoria (es)")
# old_florianopolis_udh_merged <- old_udh_merge_ex_with_in(brazil_df, florianopolis_udh, "florianopolis (sc)")
# old_manaus_udh_merged <- old_udh_merge_ex_with_in(brazil_df, manaus_udh, "manaus (am)")
# old_sao_luis_udh_merged <- old_udh_merge_ex_with_in(brazil_df, sao_luis_udh, "sao luis (ma)")
# old_sorocaba_udh_merged <- old_udh_merge_ex_with_in(brazil_df, sorocaba_udh, "sorocaba (sp)")
# old_baixada_santista_udh_merged <- old_udh_merge_ex_with_in(brazil_df, baixada_santista_udh, "santos (sp)")


# To fix column discrepancy due to atals imports
to_get_rid_func <- function(rid_dataset) {
  rid_dataset <- rid_dataset %>%
    select(
           - udh.Taxa.de.analfabetismo...11.a.14.anos.de.idade.2010,
           - udh.Taxa.de.analfabetismo...15.a.17.anos.de.idade.2010,
           - udh.Taxa.de.analfabetismo...18.a.24.anos.de.idade.2010,
           - udh.Taxa.de.analfabetismo...15.anos.ou.mais.de.idade.2010)
}
brasilia_udh_merged <- to_get_rid_func(brasilia_udh_merged)
manaus_udh_merged <- to_get_rid_func(manaus_udh_merged)
petrolina_juazeiro_udh_merged <- to_get_rid_func(petrolina_juazeiro_udh_merged)
sao_luis_udh_merged <- to_get_rid_func(sao_luis_udh_merged)
sorocaba_udh_merged <- to_get_rid_func(sorocaba_udh_merged)
teresina_udh_merged <- to_get_rid_func(teresina_udh_merged)
vale_do_paraiba_e_litoral_norte_udh_merged <- to_get_rid_func(vale_do_paraiba_e_litoral_norte_udh_merged)
vale_do_rio_cuiaba_udh_merged <- to_get_rid_func(vale_do_rio_cuiaba_udh_merged)

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

metro_cities <- unique(metros_1$customer_city)


# split, prepare and merge (EXECUTE) ------------------------------------------
non_metros_1 <- brazil_df %>%
  filter(!customer_city %in% all_udh_municips)


brazil_municip <- read_excel("./atlas_data/brazil_municipal.xlsx")
brazil_municip <- column_fixer(brazil_municip, "municip")

# prepare for rbind later on
non_metros_1 <- non_metros_1 %>%
  select(- index) %>%
  mutate(
    dist = NA,
    udh.Territorialidades = NA,
    udh.População.total.2010 = NA,
    udh.População.rural.2010 = NA,
    udh.População.urbana.2010 = NA,
    udh.IDHM.2010 = NA,
    udh.IDHM.Educação.2010 = NA,
    udh.Taxa.de.analfabetismo...25.anos.ou.mais.de.idade.2010 = NA,
    udh.Taxa.de.analfabetismo...18.anos.ou.mais.de.idade.2010 = NA,
    udh.urbanity = NA,
    udh.rurality = NA,
    udh.cumul_age_24 = NA,
    udh.young_ratio = NA,
    udh.lat = NA,
    udh.long = NA
  )


non_metros_1 <- merge(non_metros_1,
                brazil_municip,
                by.x = "customer_city",
                by.y = "mc.Territorialidades",
                all.x = TRUE)




colSums(is.na(non_metros_1)) # only 149 NA whereas it was 876

metros_1 <- metros_1 %>%
  select(
    - index_other_data,
    - X,
    - index,
    - local_index
  ) %>%
  mutate(
    `mc.População total 2010` = NA,
    `mc.População rural 2010` = NA,
    `mc.População urbana 2010` = NA,
    `mc.IDHM 2010` = NA,
    `mc.IDHM Educação 2010` = NA,
    `mc.Taxa de analfabetismo - 25 anos ou mais de idade 2010` = NA,
    `mc.Taxa de analfabetismo - 18 anos ou mais de idade 2010` = NA,
    mc.urbanity = NA,
    mc.rurality = NA,
    mc.cumul_age_24 = NA,
    mc.young_ratio = NA
  ) %>%
  filter(
    dist < 2 # to deal with extreme distances
  )

# Put together both sides of the equation
binded_df <- rbind(metros_1, non_metros_1)
saved_binded <- binded_df


# Instances with overlap? NOPE (should be zero)
overlap <- binded_df[is.na(binded_df$mc.urbanity) == FALSE & 
                       is.na(binded_df$udh.cumul_age_24) == FALSE,]

# Take care of double NA rows.
binded_df <- subset(binded_df, # xor works because there are no overlaps on 1
                  xor( is.na(mc.young_ratio), 
                       is.na(udh.long))) # implement 
# Check if it worked (should be zero)
both_nas <- binded_df[
  is.na(binded_df$mc.young_ratio) == TRUE &
    is.na(binded_df$udh.long) == TRUE,] # check


colSums(is.na(metros_1))

# (3) Can we now combine it all into one column WITH dummy variable? 
binded_df <- binded_df %>%
  # First fill with mc data
  mutate(new_total_pop = `mc.População total 2010`,
         new_total_rural = `mc.População rural 2010`,
         new_total_urban = `mc.População urbana 2010`,
         new_idhm = `mc.IDHM 2010`,
         new_idhm_edu = `mc.IDHM Educação 2010`,
         new_anafal_25_oumais = `mc.Taxa de analfabetismo - 25 anos ou mais de idade 2010`,
         new_anafal_18_oumais = `mc.Taxa de analfabetismo - 18 anos ou mais de idade 2010`,
         new_urbanity = mc.urbanity,
         new_rurality = mc.rurality,
         new_cumul_age_24 = mc.cumul_age_24,
         new_young_ratio = mc.young_ratio
  ) %>%
  # If na, fill up with the udh data
  mutate(
    new_total_pop = ifelse(is.na(new_total_pop) == TRUE, udh.População.total.2010, new_total_pop),
    new_total_rural = ifelse(is.na(new_total_rural) == TRUE, udh.População.rural.2010, new_total_rural),
    new_total_urban = ifelse(is.na(new_total_urban) == TRUE, udh.População.urbana.2010, new_total_urban),
    new_idhm = ifelse(is.na(new_idhm) == TRUE, udh.IDHM.2010, new_idhm),
    new_idhm_edu = ifelse(is.na(new_idhm_edu) == TRUE, udh.IDHM.Educação.2010, new_idhm_edu),
    new_anafal_25_oumais = ifelse(is.na(new_anafal_25_oumais) == TRUE, udh.Taxa.de.analfabetismo...25.anos.ou.mais.de.idade.2010, new_anafal_25_oumais),
    new_anafal_18_oumais = ifelse(is.na(new_anafal_18_oumais) == TRUE, udh.Taxa.de.analfabetismo...18.anos.ou.mais.de.idade.2010, new_anafal_18_oumais),
    new_urbanity = ifelse(is.na(new_urbanity) == TRUE, udh.urbanity, new_urbanity),
    new_rurality = ifelse(is.na(new_rurality) == TRUE, udh.rurality, new_rurality),
    new_cumul_age_24 = ifelse(is.na(new_cumul_age_24) == TRUE, udh.cumul_age_24, new_cumul_age_24),
    new_young_ratio = ifelse(is.na(new_young_ratio) == TRUE, udh.young_ratio, new_young_ratio)
        ) %>%
  mutate(
    udh_indicator = ifelse(is.na(udh.urbanity) == FALSE, 1, 0)
        ) %>%
  select(
    - udh.População.total.2010,
    - udh.População.rural.2010,
    - udh.População.urbana.2010,
    - udh.IDHM.2010,
    - udh.IDHM.Educação.2010,
    - udh.Taxa.de.analfabetismo...25.anos.ou.mais.de.idade.2010,
    - udh.Taxa.de.analfabetismo...18.anos.ou.mais.de.idade.2010,
    - udh.urbanity,
    - udh.rurality,
    - udh.cumul_age_24,
    - udh.young_ratio,
    - `mc.População total 2010`,
    - `mc.População rural 2010`,
    - `mc.População urbana 2010`,
    - `mc.IDHM 2010`,
    - `mc.IDHM Educação 2010`,
    - `mc.Taxa de analfabetismo - 25 anos ou mais de idade 2010`,
    - `mc.Taxa de analfabetismo - 18 anos ou mais de idade 2010`,
    - mc.urbanity,
    - mc.rurality,
    - mc.cumul_age_24,
    - mc.young_ratio
        )

# experii <- experii %>%
#  mutate(udh_indicator = ifelse(is.na(udh.urbanity) == FALSE, 1, 0))

colSums(is.na(binded_df))

# Fine, it's the unavailable / undelivered 
absence_maxprice <- binded_df[is.na(binded_df$max_price) == TRUE,]


write.csv(binded_df, 'full_geomerged_df_4_new.csv')



