### NJ Transit - MUSA Final Project

# create notin operator
`%notin%` <- Negate(`%in%`)

#remove scientific notation
options(scipen=999)

## Download + Load Packages
pckgs <- c("tidyverse", "kableExtra", "readr", "ggplot2", "lubridate","sf")

if (any(pckgs %notin% rownames(installed.packages())==TRUE)){
  install.packages(pckgs, repos = c(CRAN = "http://cloud.r-project.org"))}

lapply(pckgs, FUN = require, character.only = TRUE)


### Read in CSV files

files <- list.files(path = "./Raw_Data/",pattern = "*.csv", full.names = T)
data <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

data$delay_binary <- ifelse(data$delay_minutes >5,1,0)

## NJ Transit only
njtransit <- data %>%
  filter(type == "NJ Transit") %>%
  mutate(id = rownames(.))

##Build hour,day,week,month columns
#ignoring year - there's only two (2018 & 2019 and I want random test/train splits)
njtransit <- njtransit %>%
  mutate(interval60 = floor_date(ymd_hms(scheduled_time), unit = "hour"),
         interval15 = floor_date(ymd_hms(scheduled_time), unit = "15 mins"),
         hour = hour(interval60),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE),
         month = month(interval60, label=TRUE))

#weekday vs. weekend
#rush hour between 6am and 10am or 4pm and 8pm
njtransit <- njtransit %>%
  mutate(weekday = recode(dotw, "Mon" = "Yes",
                          "Tue" = "Yes",
                          "Wed" = "Yes",
                          "Thu" = "Yes",
                          "Fri" = "Yes",
                          "Sat" = "No",
                          "Sun" = "No"),
          rush = ifelse(hour<10 & hour>=6 & weekday=="Yes","Yes",
                       ifelse(hour<20 & hour>=16 & weekday=="Yes","Yes","No")))

#Origin & Destination Feature

njtransit <- njtransit %>%
  group_by(train_id) %>%
  arrange(stop_sequence) %>%
  filter(row_number()==1) %>%
  mutate(origin = to) %>%
  select(train_id,origin) %>%
  right_join(njtransit,.,by="train_id") %>%
  group_by(train_id) %>%
  arrange(stop_sequence) %>%
  filter(row_number()==n()) %>%
  mutate(destination = to) %>%
  select(train_id,origin,destination) %>%
  right_join(njtransit,.,by="train_id")

#Includes Manhattan

njtransit <- njtransit %>%
  mutate(manhattan = ifelse(origin == "New York Penn Station","Yes",ifelse(destination == "New York Penn Station","Yes","No")))

#characters as factors
njtransit <- njtransit %>%
  mutate_if(., is.character, as.factor) 


### station geometries
### used long island ESRI which is recommended for NYC

station_geo <- st_read("https://opendata.arcgis.com/datasets/acf1aa71053f4bb48a1aad7034f35e48_3.geojson") %>%
  st_transform('ESRI:102318')


##Prepping for merge

station_geo$ATIS_ID <- gsub("RAIL","",station_geo$ATIS_ID) 
  
station_geo$ATIS_ID <- sub("^0+", "", station_geo$ATIS_ID)


## merge
njtransit$to_id <- as.character(njtransit$to_id)
station_geo$to_id <- station_geo$ATIS_ID 

#breaks at st_as_sf because tons of missing lat and long because these freaking IDs aren't 100% the same
njtransit_sf <- station_geo %>%
  st_drop_geometry() %>%
  select(LATITUDE, LONGITUDE,STATION,to_id) %>%
  plyr::join(njtransit,.,by="to_id",type="left",match="first") %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs=4326, agr = "constant") %>%
  st_transform('ESRI:102318')










##model 1 binary
model1 <- glm(delay_binary ~ date + line, 
              data = njtransit %>%
                filter(year == "2018"), family = binomial(link="logit"))

summary(model1)

summary(model1) %>%
  coef(.) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(.,"Variable") %>%
  mutate(Odds_Ratio = (exp(Estimate)),
         across(2:6, round, 2),
         Odds_Ratio = ifelse(Odds_Ratio >100,"NA",Odds_Ratio)) %>%
  kable(caption = "Model 1 Odds Ratios") %>%
  kable_styling("striped", full_width = F)

#model2 continuous
model2 <- lm(delay_minutes ~ stop_sequence + line, 
              data = njtransit %>%
                filter(year == "2018"))

summary(model2)

summary(model2) %>%
  coef(.) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(.,"Variable") %>%
  mutate(across(2:5, round, 2)) %>%
  kable(caption = "Model 2") %>%
  kable_styling("striped", full_width = F)

#princeon shuttle most efficient, NJ coast least efficient
#stops add time
