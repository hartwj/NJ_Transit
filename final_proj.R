### NJ Transit - MUSA Final Project
x <- njtransit[rowSums(is.na(njtransit)) > 0,]
njtransit <- njtransit %>% select(-Temperature, -Precipitation, -Wind_Speed)

# --- Setup: Libraries ----
# create notin operator
`%notin%` <- Negate(`%in%`)

#remove scientific notation
options(scipen=999)

## Download + Load Packages
pckgs <- c("tidyverse", "kableExtra", "readr", "ggplot2", "lubridate","sf",
           "riem","tigris", "gganimate","gridExtra","knitr","tidyr","caret")

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
palette4 <- c("#981FAC","#FF006A","#FE4C35","#FE9900")
palette2 <- c("#981FAC","#FF006A")

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

# Join weather data 
njtransit <- weather.Panel %>%
  plyr::join(njtransit,.,by="interval60",type="left") %>%
  mutate(weather_na = ifelse(is.na(Temperature), "1",
                      ifelse(is.na(Precipitation), "1",
                      ifelse(is.na(Wind_Speed), "1","0"))))

njtransit[is.na(njtransit)] = 0


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
    to_id = ifelse(STATION=="Wesmont", "43599", to_id))

station_geo <- station_geo %>%
  mutate(
    Lines = ifelse(RAIL_LINE != "Main/Bergen Line" & str_detect(RAIL_LINE, "/"), NA, RAIL_LINE))

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

#distance to hubs
hub.nyc <-
  filter(station_geo, STATION == "New York Penn Station") %>%
  st_centroid()
hub.hoboken <-
  filter(station_geo, STATION == "Hoboken Terminal") %>%
  slice(n()) %>%
  st_centroid()
hub.philly <-
  filter(station_geo, STATION == "Suburban Station") %>%
  st_centroid()
hub.newark <-
  filter(station_geo, STATION == "Newark Penn Station") %>%
  slice(n()) %>%
  st_centroid()
hub.secaucus <-
  filter(station_geo, STATION == "Secaucus Junction Upper Level") %>%
  st_centroid()

njtransit_sf <- njtransit_sf %>%
  mutate(distNYC =  as.numeric(st_distance(njtransit_sf,hub.nyc)),
         distHOB =  as.numeric(st_distance(njtransit_sf,hub.hoboken)),
         distEWR =  as.numeric(st_distance(njtransit_sf,hub.newark)),
         distPHL =  as.numeric(st_distance(njtransit_sf,hub.philly)),
         distSEC =  as.numeric(st_distance(njtransit_sf,hub.secaucus)))
  

# --- Exploratory Analysis ----
# Plot weather features
weather.Panel18 <- weather.Panel %>%
  filter(interval60 >= "2018-03-01 00:00:00" & interval60 <= "2018-12-31 23:00:00")
weather.Panel19 <- weather.Panel %>%
  filter(interval60 >= "2019-01-01 00:00:00" & interval60 <= "2019-12-31 23:00:00")
weather.Panel20 <- weather.Panel %>%
  filter(interval60 >= "2020-01-01 00:00:00" & interval60 <= "2020-05-18 23:00:00")

### Recode Weather
njtransit_sf <- njtransit_sf %>%
  mutate(weather = case_when(Temperature <=32 & Precipitation < 1 ~ "Freezing",
                             Temperature <=32 & Precipitation >= 1 ~ "Snow",
                             Temperature >32 & Temperature <85 & Precipitation >=1 ~ "Rainy",
                             Temperature >32 & Temperature <85 & Precipitation <1 ~ "Clear",
                             Temperature >= 85 ~ "Hot"),
         snow = ifelse(Temperature <=32 & Precipitation >= 1, "1", "0"))

# --- Visualizations ----
# Run correlations btwn delay_binary and features
features <- c("delay_binary","stop_sequence","hour","month","weekday","rush",
              "incl_manhattan","incl_hoboken","incl_newark",
              "weather","snow")

grid.arrange(top = "Weather Data - Newark - 2019",
             ggplot(weather.Panel, aes(interval60,Precipitation)) + geom_line() + 
               labs(title="Precipitation", x="Day", y="Precipitation") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
               labs(title="Wind Speed", x="Day", y="Wind Speed") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
               labs(title="Temperature", x="Day", y="Temperature") + plotTheme())

ggplot(weather.Panel, aes(interval60,Temperature)) + 
  geom_line() + 
  geom_hline(yintercept=32, color="#64a6f1", size = 2) +
  geom_hline(yintercept=85, color="#f27a60", size = 2) +
  labs(title="Temperature (F)", x="Day", y="Temperature") + 
    plotTheme()
  
stations <- mapview::mapview(station_geo, na.col = "#f27a60", zcol = "Lines")

## Hour and Delay Minutes
#Need a palette 24 lol?
njtransit_sf %>%
  st_drop_geometry %>%
  filter(weekday==1) %>%
  group_by(hour) %>%
  summarize(mean_delay = mean(delay_minutes)) %>%
  mutate(rush = ifelse(hour<10 & hour>=6,"1",
                ifelse(hour<20 & hour>=16,"1","0"))) %>%
  ggplot(., aes(x=hour, y=mean_delay)) +   
  geom_bar(aes(fill = rush), position = "dodge", stat="identity") +
  scale_fill_manual(values = c("gray","gold")) +
  scale_x_continuous(breaks=seq(0,23,by=2))+
  labs(x="Hour", y="Average Delay (minutes)",
       title = "Feature associations with the train delays",
       subtitle = "Hour") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Rush Hour & Non Rush Hour
# there's actually no difference at the overall level
njtransit_sf %>%
  st_drop_geometry %>%
  group_by(rush) %>%
  summarize(mean_delay = mean(delay_minutes)) %>%
  ggplot(aes(rush, mean_delay, fill=rush)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  scale_fill_manual(values = c("gray","gold")) +
  labs(x="Rush Hour", y="Average Delay (minutes)", 
       title = "Feature associations with train delay",
       subtitle = "Rush Hour") +
  plotTheme() 

## Weekday?
# more delays on weekends
njtransit_sf %>%
  st_drop_geometry %>%
  group_by(weekday) %>%
  summarize(mean_delay = mean(delay_minutes)) %>%
  ggplot(aes(weekday, mean_delay, fill=weekday)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  scale_fill_manual(values = c("gray","gold")) +
  labs(x="Weekday", y="Average Delay (minutes)", 
       title = "Feature associations with train delay",
       subtitle = "Weekday (1) vs. Weekend (0)") +
  plotTheme() 

## Day of the week?
njtransit_sf %>%
  st_drop_geometry %>%
  group_by(dotw) %>%
  summarize(mean_delay = mean(delay_minutes)) %>%
  ggplot(aes(dotw, mean_delay)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  labs(y="Average Delay (minutes)", 
       title = "Feature associations with train delay",
       subtitle = "Days of the week from Mon (1) to Sun (7)") +
  scale_x_discrete(name = "Day of the Week", limits =c('1','2','3','4','5','6','7')) +
  plotTheme() 

## Month 
njtransit_sf %>%
  st_drop_geometry %>%
  group_by(month) %>%
  summarize(mean_delay = mean(delay_minutes)) %>%
  ggplot(., aes(month, mean_delay)) +   
  geom_bar(position = "dodge", stat="identity") +
  scale_fill_manual(values = palette5) +
  scale_x_continuous(breaks=seq(0,12,by=1)) + 
  labs(x="Month", y="Average Delay (minutes)",
       title = "Feature associations with the train delays",
       subtitle = "Month") +
  plotTheme()

## Weather
njtransit_sf %>%
  st_drop_geometry %>%
  group_by(weather) %>%
  summarize(mean_delay = mean(delay_minutes)) %>%
  ggplot(aes(weather, mean_delay, fill=weather)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  scale_fill_manual(values = palette5) +
  labs(x="Weather", y="Average Delay (minutes)", 
       title = "Feature associations with train delay",
       subtitle = "Weather") +
  plotTheme()

## include manhattan
#delay is slightly larger - would be cool to visualize this as 0 no,1 manhattan, 2 hoboken, 3secaucus, etc. 
njtransit_sf %>%
  st_drop_geometry %>%
  group_by(incl_manhattan) %>%
  summarize(mean_delay = mean(delay_minutes)) %>%
  ggplot(aes(incl_manhattan, mean_delay, fill=incl_philly)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  labs(x="Includes Manhattan", y="Average Delay (minutes)", 
       title = "Feature associations with train delay",
       subtitle = "Whether a train includes Philly's Suburban Station") +
  plotTheme() 

## distance to hubs
## huge outliers kinda ruin this plot - we could also run these for hoboken, seacaucus, etc. 

njtransit_sf %>%
  st_drop_geometry() %>%
  group_by(to) %>%
  summarize(mean_delay = mean(delay_minutes),
            mean_NYCdist = mean(distNYC)) %>%
  ggplot(aes(mean_NYCdist,mean_delay)) +
  geom_point()+
  stat_smooth(aes(mean_NYCdist,mean_delay), 
              method = "lm", se = FALSE, size = 1, colour="#FA7800")+
  plotTheme()

## delay by station - is it consistent?
# answer - it's not

njtransit_sf %>%
  st_drop_geometry %>%
  group_by(to) %>%
  summarize(mean_delay = mean(delay_minutes)) %>%
  ggplot(., aes(to, mean_delay)) +   
  geom_bar(position = "dodge", stat="identity") +
  scale_fill_manual(values = palette2) +
    labs(x="Station", y="Average Delay (minutes)",
       title = "Feature associations with the train delays",
       subtitle = "by Station") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## delay by station - top 10 largest offenders
#palette 10?

njtransit_sf %>%
  st_drop_geometry %>%
  group_by(to) %>%
  summarize(mean_delay = mean(delay_minutes)) %>%
  arrange(desc(mean_delay)) %>%
  slice(1:10) %>%
  ggplot(., aes(x=reorder(to, -mean_delay),y=mean_delay)) +   
  geom_bar(aes(fill=to), position = "dodge", stat="identity") +
  scale_fill_manual(values = c("#64a6f1","#64a6f1","#64a6f1","#64a6f1","#64a6f1","#64a6f1","#64a6f1","#64a6f1","#64a6f1","gray")) +
  labs(x="Station", y="Average Delay (minutes)",
       title = "Feature associations with the train delays",
       subtitle = "Top 10 Stations") + 
  plotTheme() +
  theme(legend.position = "none")










### Modeling

#The goal of this next step was to build a model that accurately predicts average station delay
#minimizing absolute error and absolute percent error. At each stage we removed outliers based on
#Cook's Distance which improved our error metrics. Importantly, our data was aggregated at the station-level for our use case.
#We used a simple 60/40 train/test split on the aggregated data. Each model has its own unique dataset with increase observations for increasing features.
#Model performance was based on out of fold test set predictions.
#`Model 1` looks solely at calendar features, while `Model 2` looks at calendar and weather features, and `Model 3` examines calendar, weather, and geographic
#features. `Model 1` had the lowest absolute error and absolute percent error, but `Model 3` was recommended for the dashboard,
#given that it is more customizable. 


# --- Modeling ----
## model 1 is station + calendar

mod1df <- njtransit_sf %>%
  st_drop_geometry() %>%
  group_by(to,hour,weekday,month) %>%
  summarize(mean_delay = mean(delay_minutes))


mod1df$to <- as.factor(mod1df$to)
mod1df$weekday <- as.factor(mod1df$weekday)
str(mod1df)

set.seed(3457)

inTrain <- createDataPartition(
  y = mod1df$mean_delay, 
  p = .60, list = FALSE)
mod1.training <- as.data.frame(mod1df[inTrain,])
mod1.test <- as.data.frame(mod1df[-inTrain,])

model1 <- lm(mean_delay ~ to + hour + weekday + month, data = (mod1.training))

#cooks distance lowers abs error and APE
mod1.training$cd <- cooks.distance(model1)

model1b <- lm(mean_delay ~ to + hour + weekday + month, data = subset(mod1.training, cd<= 4/43135))

summary(model1)

mod1.test <-
  mod1.test %>%
  mutate(Regression = "Model 1: Calendar Features",
         Delay.Predict = predict(model1b, mod1.test),
         Delay.Error = Delay.Predict - mean_delay,
         Delay.AbsError = abs(Delay.Predict - mean_delay),
         Delay.APE = (abs(Delay.Predict - mean_delay)) / Delay.Predict)
  

## Model 2 is station + calendar + weather

mod2df <- njtransit_sf %>%
  st_drop_geometry() %>%
  group_by(to,hour,weekday,month,snow) %>%
  summarize(mean_delay = mean(delay_minutes))

set.seed(3458)

inTrain <- createDataPartition(
  y = mod2df$mean_delay, 
  p = .60, list = FALSE)
mod2.training <- as.data.frame(mod2df[inTrain,])
mod2.test <- as.data.frame(mod2df[-inTrain,])

model2 <- lm(mean_delay ~ to + hour + weekday + month + snow, data = (mod2.training))

mod2.training$cd <- cooks.distance(model2)

model2b <- lm(mean_delay ~ to + hour + weekday + month + snow, data = subset(mod2.training, cd<= 4/84524))

mod2.test <-
  mod2.test %>%
  mutate(Regression = "Model 2: Calendar + Weather Features",
         Delay.Predict = predict(model2b, mod2.test),
         Delay.Error = Delay.Predict - mean_delay,
         Delay.AbsError = abs(Delay.Predict - mean_delay),
         Delay.APE = (abs(Delay.Predict - mean_delay)) / Delay.Predict)

## Model 3 - Calendar + Weather + Hub Distance

mod3df <- njtransit_sf %>%
  st_drop_geometry() %>%
  group_by(to,hour,weekday,month,weather) %>%
  summarize(mean_delay = mean(delay_minutes),
            meanNYC = mean(distNYC),
            meanHOB = mean(distHOB),
            meanEWR = mean(distEWR),
            meanPHL = mean(distPHL),
            meanSEC = mean(distSEC))

set.seed(3459)

inTrain <- createDataPartition(
  y = mod3df$mean_delay, 
  p = .60, list = FALSE)
mod3.training <- as.data.frame(mod3df[inTrain,])
mod3.test <- as.data.frame(mod3df[-inTrain,])

model3 <- lm(mean_delay ~ to + hour + weekday + month + weather + meanNYC +
               meanHOB + meanEWR + meanPHL + meanSEC, data = (mod3.training))

mod3.training$cd <- cooks.distance(model3)

model3b <- lm(mean_delay ~ to + hour + weekday + month + weather + meanNYC +
                meanHOB + meanEWR + meanPHL + meanSEC, data = subset(mod3.training, cd<= 4/84524))


mod3.test <-
  mod3.test %>%
  mutate(Regression = "Model 3: Calendar + Weather + Distance Features",
         Delay.Predict = predict(model3b, mod3.test),
         Delay.Error = Delay.Predict - mean_delay,
         Delay.AbsError = abs(Delay.Predict - mean_delay),
         Delay.APE = (abs(Delay.Predict - mean_delay)) / Delay.Predict)




### Visualizing Results

allRegressions <- 
  rbind(
    dplyr::select(mod1.test, mean_delay,starts_with("Delay"), Regression),
    dplyr::select(mod2.test, mean_delay,starts_with("Delay"), Regression),
    dplyr::select(mod3.test, mean_delay,starts_with("Delay"), Regression))
      
allRegressions %>%
  gather(Variable,Value,-Regression) %>%
  filter(Variable == "Delay.AbsError" | Variable == "Delay.APE") %>%
  group_by(Regression, Variable) %>%
  summarize(meanValue = mean(Value, na.rm = T)) %>%
  spread(Variable, meanValue) %>%
  kable(caption = "Mean Error by Model") %>%
  kable_styling("striped", full_width = F)


allRegressions %>%
  dplyr::select(Delay.Predict, mean_delay, Regression) %>%
  ggplot(aes(mean_delay, Delay.Predict)) +
  geom_point() +
  stat_smooth(aes(mean_delay, mean_delay), 
              method = "lm", se = FALSE, size = 1, colour="#FA7800") + 
  stat_smooth(aes(Delay.Predict, mean_delay), 
              method = "lm", se = FALSE, size = 1, colour="#25CB10") +
  facet_wrap(~Regression) +
  ylim(0,50)+
  labs(title="Predicted average station delay as a function of observed delays",
       subtitle="Orange line represents a perfect prediction; Green line represents actual prediction") +
  plotTheme()









