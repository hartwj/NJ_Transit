### NJ Transit - MUSA Final Project
x <- njtransit[rowSums(is.na(njtransit)) > 0,]

# --- Setup: Libraries ----
# create notin operator
`%notin%` <- Negate(`%in%`)

#remove scientific notation
options(scipen=999)

## Download + Load Packages
pckgs <- c("tidyverse", "kableExtra", "readr", "ggplot2", "lubridate","sf",
           "riem","tigris", "gganimate","gridExtra","knitr","tidyr")

if (any(pckgs %notin% rownames(installed.packages())==TRUE)){
  install.packages(pckgs, repos = c(CRAN = "http://cloud.r-project.org"))}

lapply(pckgs, FUN = require, character.only = TRUE)


# --- Setup: Aesthetics & Functions ----
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

# --- Data ----
### Read in CSV files

files <- list.files(path = "./Raw_Data/",pattern = "*.csv", full.names = T)
data <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

## NJ Transit only
njtransit <- data %>%
  filter(type == "NJ Transit") %>%
  mutate(id = rownames(.)) %>%
  na.omit()

njtransit$delay_binary <- ifelse(njtransit$delay_minutes >5,1,0)

## Create hour, week, day of the week, and month columns
# Ignoring year - there's only two (2018 & 2019 and I want random test/train splits)
njtransit <- njtransit %>%
  mutate(interval60 = floor_date(ymd_hms(scheduled_time), unit = "hour"),
         interval15 = floor_date(ymd_hms(scheduled_time), unit = "15 mins"),
         year = year(interval60),
         hour = hour(interval60),
         week = week(interval60),
         dotw = wday(interval60, week_start = getOption("lubridate.week.start", 1)),
         month = month(interval60))

# Weekday vs. weekend
# Rush hour between 6am and 10am or 4pm and 8pm
njtransit <- njtransit %>%
  mutate(weekday = ifelse(dotw %in% 1:5,"1","0"),
         rush = ifelse(hour<10 & hour>=6 & weekday=="1","1",
                ifelse(hour<20 & hour>=16 & weekday=="1","1","0")))

# --- Features ----
# Origin & Destination Feature
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

# Includes Manhattan Feature % Line dummies
njtransit <- njtransit %>%
  mutate(incl_manhattan = ifelse(origin == "New York Penn Station","1",ifelse(destination == "New York Penn Station","1","0")),
         incl_hoboken = ifelse(origin == "Hoboken","1",ifelse(destination == "Hoboken","1","0")),
         incl_newark = ifelse(origin == "Newark Penn Station","1",ifelse(destination == "Newark Penn Station","1","0")))

# Characters as factors & train line dummies - not doing this for now
#njtransit <- njtransit %>%
#  mutate_if(., is.character, as.factor) 

# Weather Feature
weather.Data <- riem_measures(station = "EWR", date_start = "2018-03-01", date_end = "2020-05-18")

#Trying to fix the weather NA issue: create table with every single hour
#weather.Features <- data.table(interval60 = )
#start <- ymd_hms("2018-03-01 00:00:00")
#end <- ymd_hms("2020-05-18 23:00:00")
#interval(end, start)

## Plot Newark weather data
weather.Panel <-  
  weather.Data %>%
  mutate_if(is.character, list(~replace(as.character(.), is.na(.), "0"))) %>% 
  replace(is.na(.), 0) %>%
  mutate(interval60 = floor_date(ymd_h(substr(valid, 1, 13))),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Precipitation = sum(p01i),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))



# Join weather data & add train age
njtransit <- weather.Panel %>%
  plyr::join(njtransit,.,by="interval60",type="left")

# Lol, I tried to get the ages, but like 88% of the rows were NAs so clearly something's off
#njtransit_age <- njtransit %>%
#  mutate(train_age = ifelse(train_id %in% 500:503, "48",
#                     ifelse(train_id %in% 1304:1533, "25", 
#                     ifelse(train_id %in% 1600:1609 | train_id %in% 1700:1760 | train_id %in% 5100:5134 | train_id %in% 5707:5751, "33", 
#                     ifelse(train_id %in% 4000:4032, "14", 
#                     ifelse(train_id %in% 4100:4112, "29",
#                     ifelse(train_id %in% 4113:4129, "22",
#                     ifelse(train_id %in% 4130:4144, "30",
#                     ifelse(train_id %in% 4145:4150, "26",
#                     ifelse(train_id %in% 4184:4189, "30",
#                     ifelse(train_id == "4190", "30",
#                     ifelse(train_id %in% 4191:4192, "21",
#                     ifelse(train_id %in% 4193:4194, "17",             
#                     ifelse(train_id %in% 4200:4219, "23",
#                     ifelse(train_id %in% 4300:4303, "52",
#                     ifelse(train_id %in% 4400:4414, "30",
#                     ifelse(train_id %in% 4415:4419, "25",
#                     ifelse(train_id %in% 4420:4431, "23",             
#                     ifelse(train_id %in% 4600:4628, "18",
#                     ifelse(train_id %in% 4300:4303, "52",
#                     ifelse(train_id %in% 5000:5010 | train_id %in% 5200:5205 | train_id %in% 5500:5534 , "29", 
#                     ifelse(train_id %in% 5155:5169 | train_id %in% 5220:5234, "31", 
#                     ifelse(train_id %in% 5300:5460, "17", 
#                     ifelse(train_id %in% 5011:5031 | train_id %in% 5235:5264 | train_id %in% 5535:5582, "24", 
#                     ifelse(train_id %in% 6000:6083 | train_id %in% 6200:6213 | train_id %in% 6500:6601, "15", 
#                     ifelse(train_id %in% 7000:7051 | train_id %in% 7200:7298 | train_id %in% 7500:7677, "10", 
#                     ifelse(train_id %in% 7052:7061 | train_id %in% 7678:7767, "7", NA
#                           )))))))))))))))))))))))))))

### Station geometries
### Used Long Island ESRI which is recommended for NYC
station_geo <- st_read("https://opendata.arcgis.com/datasets/acf1aa71053f4bb48a1aad7034f35e48_3.geojson") %>%
  st_transform('ESRI:102318')

## Prepping for merge
## Removing leading characters from train id(ATIS_ID)
station_geo$ATIS_ID <- gsub("RAIL","",station_geo$ATIS_ID) 
station_geo$ATIS_ID <- sub("^0+", "", station_geo$ATIS_ID)

## recode non matching IDs - thank you NJTransit
njtransit$to_id <- as.character(njtransit$to_id)

station_geo$to_id <- station_geo$ATIS_ID
station_geo <- station_geo %>%
  mutate(
    to_id = ifelse(to_id=="82", "37169", to_id),
    to_id = ifelse(to_id=="163", "32905", to_id),
    to_id = ifelse(to_id=="65", "32906", to_id),
    to_id = ifelse(to_id=="165", "38081", to_id),
    to_id = ifelse(to_id=="171", "39472", to_id),
    to_id = ifelse(to_id=="164", "37953", to_id),
    to_id = ifelse(to_id=="183", "43298", to_id),
    to_id = ifelse(to_id=="128", "38417", to_id),
    to_id = ifelse(to_id=="167", "38174", to_id),
    to_id = ifelse(to_id=="168", "38187", to_id),
    to_id = ifelse(to_id=="166", "38105", to_id),
    to_id = ifelse(to_id=="172", "39635", to_id),
    to_id = ifelse(STATION=="Wesmont", "43599", to_id)
  )

#merge station geos to njtransit
njtransit_sf <- station_geo %>%
  st_drop_geometry() %>%
  select(LATITUDE, LONGITUDE,STATION,to_id) %>%
  plyr::join(njtransit,.,by="to_id",type="left",match="first") %>%
  mutate(LATITUDE = ifelse(to=="Ramsey Main St", "41.05707", LATITUDE),
         LONGITUDE = ifelse(to=="Ramsey Main St", "-74.14220", LONGITUDE),
         STATION = ifelse(to=="Ramsey Main St", "Ramsey Main St", STATION)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs=4326, agr = "constant") %>%
  st_transform('ESRI:102318')

# --- Exploratory Analysis ----
# Plot weather features
grid.arrange(top = "Weather Data - Newark - Mar 2018 to May 2020",
             ggplot(weather.Panel, aes(interval60,Precipitation)) + geom_line() + 
               labs(title="Precipitation", x="Hour", y="Precipitation") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
               labs(title="Wind Speed", x="Hour", y="Wind Speed") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
               labs(title="Temperature", x="Hour", y="Temperature") + plotTheme())

# Run correlations btwn delay_binary and features
features <- c("delay_binary","stop_sequence","hour","week"/"month","dotw"/"weekday","rush",
              "incl_manhattan","incl_hoboken","incl_newark",
              "Temperature","Precipitation","Wind_Speed")


### Ken Questions

#1- Characters or Factors? Depends on GLM or LM?


#2- Time Lag & Spatial Lag - Do we need to do it? Spatial would require tracts joined to stations?





# --- Modeling ----
## model 1 binary
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
