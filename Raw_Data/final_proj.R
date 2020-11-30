### NJ Transit - MUSA Final Project

# create notin operator
`%notin%` <- Negate(`%in%`)

#remove scientific notation
options(scipen=999)

## Download + Load Packages
pckgs <- c("tidyverse", "kableExtra", "readr", "ggplot2", "lubridate")

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
  filter(type == "NJ Transit")

##Build month and year columns
njtransit <- njtransit %>%
  mutate(interval60 = floor_date(ymd_hms(scheduled_time), unit = "hour"),
         interval15 = floor_date(ymd_hms(scheduled_time), unit = "15 mins"),
         hour = hour(interval60),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE))

#rush hour between 6am and 10am or 4pm and 8pm
njtransit <- njtransit %>%
  mutate(weekday = recode(dotw, "Mon" = "Yes",
                          "Tue" = "Yes",
                          "Wed" = "Yes",
                          "Thu" = "Yes",
                          "Fri" = "Yes",
                          "Sat" = "No",
                          "Sun" = "No"))

njtransit <- njtransit %>%
         mutate(rush = ifelse(hour<10 & hour>=6 & weekday=="Yes","Yes",
                       ifelse(hour<20 & hour>=16 & weekday=="Yes","Yes","No")))

#rowID and characters as factors
njtransit <- njtransit %>%
  mutate_if(., is.character, as.factor) %>%
  mutate(id = rownames(.))



#involves manhattan or 1)from nyc 2)to nyc





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
