### NJ Transit - MUSA Final Project

# --- Setup: Libraries ----
# create notin operator
`%notin%` <- Negate(`%in%`)

#remove scientific notation
options(scipen=999)

## Download + Load Packages
pckgs <- c("tidyverse", "kableExtra", "readr", "ggplot2", "lubridate","sf",
           "riem","tigris", "gganimate","gridExtra","knitr")

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

data$delay_binary <- ifelse(data$delay_minutes >5,1,0)

## NJ Transit only
njtransit <- data %>%
  filter(type == "NJ Transit") %>%
  mutate(id = rownames(.))

## Create hour, week, day of the week, and month columns
# Ignoring year - there's only two (2018 & 2019 and I want random test/train splits)
njtransit <- njtransit %>%
  mutate(interval60 = floor_date(ymd_hms(scheduled_time), unit = "hour"),
         interval15 = floor_date(ymd_hms(scheduled_time), unit = "15 mins"),
         hour = hour(interval60),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE),
         month = month(interval60, label=TRUE))

# Weekday vs. weekend
# Rush hour between 6am and 10am or 4pm and 8pm
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

# --- Features ----
# Weather Feature
weather.Data <- riem_measures(station = "EWR", date_start = "2018-03-01", date_end = "2020-05-31")

## Plot Newark weather data
weather.Panel <-  
  weather.Data %>%
  mutate_if(is.character, list(~replace(as.character(.), is.na(.), "0"))) %>% 
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid, 1, 13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Precipitation = sum(p01i),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

grid.arrange(top = "Weather Data - Newark - Mar 2018 to May 2020",
             ggplot(weather.Panel, aes(interval60,Precipitation)) + geom_line() + 
               labs(title="Precipitation", x="Hour", y="Percipitation") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
               labs(title="Wind Speed", x="Hour", y="Wind Speed") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
               labs(title="Temperature", x="Hour", y="Temperature") + plotTheme())



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

# Includes Manhattan Feature
njtransit <- njtransit %>%
  mutate(manhattan = ifelse(origin == "New York Penn Station","Yes",ifelse(destination == "New York Penn Station","Yes","No")))

# Characters as factors
njtransit <- njtransit %>%
  mutate_if(., is.character, as.factor) 


### Station geometries
### Used Long Island ESRI which is recommended for NYC
station_geo <- st_read("https://opendata.arcgis.com/datasets/acf1aa71053f4bb48a1aad7034f35e48_3.geojson") %>%
  st_transform('ESRI:102318')

## Prepping for merge

station_geo$ATIS_ID <- gsub("RAIL","",station_geo$ATIS_ID) 
station_geo$ATIS_ID <- sub("^0+", "", station_geo$ATIS_ID)

## merge
njtransit$to_id <- as.character(njtransit$to_id)
station_geo$to_id <- station_geo$ATIS_ID 

#breaks at st_as_sf because tons of missing lat and long because these freaking IDs aren't 100% the same
njtransit_sf <- station_geo %>%
  st_drop_geometry() %>%
  select(LATITUDE, LONGITUDE,STATION,to_id) %>%
  plyr::join(njtransit,.,by="to_id",type="left",match="first")
#%>%
 # st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs=4326, agr = "constant") %>%
  #st_transform('ESRI:102318')


#Theres about 7 stations or so that aren't being captured
x <- njtransit_sf[is.na(njtransit_sf$STATION),]








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
