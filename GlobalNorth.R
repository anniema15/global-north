## Data Visualization GOVT 016 Fall 2015
## Introduction to R & RStudio 
## DV 3 Project
## 
## Name: Annie Ma
## Date: Nov 3, 2015 

library(ggplot2)
library(dplyr)
library(xtermStyle)
library(RColorBrewer)
library(scales)
library(gpclib)
library(gridExtra)

rm(list = ls())

#read in the country codes and tidy the data
#select relevant columns
ccodes <- read.csv("data/OutWit tables export - ISO_3166_1_alpha_3_Wikipedia_the_free_encyclopedia.csv", 
                   header = TRUE, stringsAsFactors = FALSE) %>% 
  rename(id = `Column.4`) %>% 
  rename(Country = `Column.5`) %>% 
  select(Country, id)

#read in the GDP data 
gdp <- read.csv("data/OutWit tables export - List_of_countries_by_GDP_nominal_per_capita_Wikipedia_the_free_encyclopedia.csv", 
               header = TRUE, stringsAsFactors = FALSE)

#tidy GDP data, remove South Sudan (too new to be included)
gdp <- gdp %>% 
  filter(`Column.4` != "#Rank:—") %>% 
  rename(GDP =`Us.`) %>% 
  select(GDP, Country) %>% 
  filter(Country != "#Country:South Sudan")

#Clean the data 
gdp$Country <- gsub("#Country:", "", gdp$Country)
gdp$GDP <- gsub("#Us\\$:", "", gdp$GDP)
gdp$GDP <- gsub(",", "", gdp$GDP)

#use to find what's missing between dfs, 
#when naming countries,
#not needed when running full script 
#gdp$Country[is.na(match(gdp$Country, ccodes$Country))]

#use results of prev line to do necessary replacements 
ccodes$Country[(ccodes$Country == "Brunei Darussalam")] <- "Brunei"
ccodes$Country[(ccodes$Country == "Korea, Republic of")] <- "Korea, South"
ccodes$Country[(ccodes$Country == "Korea, Democratic People's Republic of")] <- "Korea, North"
ccodes$Country[(ccodes$Country == "Russian Federation")] <- "Russia"
ccodes$Country[(ccodes$Country == "Venezuela, Bolivarian Republic of")] <- "Venezuela"
ccodes$Country[(ccodes$Country == "Iran, Islamic Republic of")] <- "Iran"
ccodes$Country[(ccodes$Country == "Macedonia, the former Yugoslav Republic of")] <- 
  "Macedonia, Republic of"
ccodes$Country[(ccodes$Country == "Timor-Leste")] <- "East Timor"
ccodes$Country[(ccodes$Country == "Cabo Verde")] <- "Cape Verde"
ccodes$Country[(ccodes$Country == "Congo")] <- "Congo, Republic of the"
ccodes$Country[(ccodes$Country == "Bolivia, Plurinational State of")] <- 
  "Bolivia"
ccodes$Country[(ccodes$Country == "Moldova, Republic of")] <- "Moldova"
ccodes$Country[(ccodes$Country == "Viet Nam")] <- "Vietnam"
ccodes$Country[(ccodes$Country == "Sao Tome and Principe")] <-
  "São Tomé and Príncipe"
ccodes$Country[(ccodes$Country == "Syrian Arab Republic")] <-
  "Syria"
ccodes$Country[(ccodes$Country == "Lao People's Democratic Republic")] <-
  "Laos"
ccodes$Country[(ccodes$Country == "Myanmar")] <- "Burma"
ccodes$Country[(ccodes$Country == "Tanzania, United Republic of")] <-
  "Tanzania"
ccodes$Country[(ccodes$Country == "Gambia")] <- "Gambia, The"
ccodes$Country[(ccodes$Country == "Congo, the Democratic Republic of the")] <-
  "Congo, Democratic Republic of the"


#join dfs 
gdp <- left_join(gdp, ccodes, by = "Country")

#read in latitudes
loc <- read.csv("data/OutWit tables export - countries_csv_Dataset_Publishing_Language_Google_Developers.csv", 
                header = TRUE, stringsAsFactors = FALSE)
#clean the data 
loc <- loc %>% 
  select(Latitude, Longitude, Name) %>% 
  rename(Country = Name)

#use to find non-matching names
#loc$Country[is.na(match(gdp$Country, loc$Country))]

gdp$Country[(gdp$Country == "Korea, South")] <- "South Korea"
gdp$Country[(gdp$Country == "Korea, North")] <- "North Korea"
gdp$Country[(gdp$Country == "Gambia, The")] <- "Gambia"
gdp$Country[(gdp$Country == "Macedonia, Republic of")] <- "Macedonia [FYROM]"
gdp$Country[(gdp$Country == "Micronesia, Federated States of")] <- "Micronesia"
gdp$Country[(gdp$Country == "Congo, Republic of the")] <- "Congo [Republic]"
gdp$Country[(gdp$Country == "Palestine, State of")] <- "Palestinian Territories"
gdp$Country[(gdp$Country == "Congo, Democratic Republic of the")] <- "Congo [DRC]"
gdp$Country[(gdp$Country == "East Timor")] <- "Timor-Leste"
gdp$Country[(gdp$Country == "Burma")] <- "Myanmar [Burma]"

#convert GDP to numeric 
gdp$GDP <- as.numeric(gdp$GDP)

#merge data sets 
df <- left_join(gdp, loc, by = "Country")

#run stats regression on relationship between 
#log GDP and Lat
mod <- lm(log(GDP) ~ Latitude, data = df)
summary(mod)

summary(df$GDP)
#create world map df 
worldmap <- read.csv("data/HiResWorldMapWithISO3.csv") %>% 
  fortify(region = "IS03") %>% 
  filter(id != "ATA")

#get top 30 GDP 
top30 <- df %>% 
  filter(GDP>5790)

#get bottom 30 GDP 
bottom30 <- df %>% 
  filter(GDP<5790)

#merge data frames for final to map 
worldLowGDP <- full_join(worldmap, bottom30, by = "id")
worldTopGDP <- full_join(worldmap, top30, by = "id")

#map lowest GDP 
mapLowGDP <- ggplot() +
  geom_polygon(data = worldLowGDP, 
               mapping = aes(x = long, y =lat, group = group, fill = GDP), 
               color = "black") + 
  scale_fill_continuous(low = "yellow", high = "red",na.value = "grey80") + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+ 
  labs(title = "   GDP/Capita Lower than the Global Median")

#map highest GDP 
mapHighGDP <- ggplot()+ 
  geom_polygon(data = worldTopGDP, 
             mapping = aes(x = long, y =lat, group = group, fill = GDP), 
             color = "black") + 
  scale_fill_continuous(low = "light green", high = "dark blue", na.value = "grey80")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank()) + 
  ggtitle("   GDP/Capita Greater than the Global Median")

#plot the regression
regression <- ggplot() + 
  geom_point(data = df, 
              mapping = aes(x = Latitude, y = log(GDP))) + 
  geom_smooth(data = df, 
              mapping = aes(x = Latitude, y = log(GDP)), 
              method = lm) +   
  labs(xlab = "Latitude", 
  ylab = "Log(GDP per Capita in $US)") + 
  ggtitle("GDP over Increasing Latitude")+
  annotate("text", x=47, y=5.5, 
           label="R-squared: 0.157\np-value: 6.7e-9", 
           size = 4) 

#lay out the final grid plot
grid.arrange(mapHighGDP, mapLowGDP, regression, 
          layout_matrix = rbind(c(1, 3),
                              c(2, 3)), 
          top = "Visualizing a \"Global North\" — A misleading term?")
