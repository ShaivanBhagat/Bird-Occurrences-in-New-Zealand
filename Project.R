#install packages
install.packages("tidyverse")
install.packages("mapproj")
install.packages("tidymodels")

#load libraries
library(tidyverse)
library(mapproj)
library(tidymodels)

#import data
#bird data from Southland,NZ
region_birds <- read_delim("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SCIENVI303/2023h1/ass2/Birds/Southland/Southland_Birds.csv",delim=",")
bird_info <- read_delim("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SCIENVI303/2023h1/ass2/Bird_details_NZ.txt", delim="\t")

#view the datasets
view(region_birds)
view(bird_info)

#combine the two datasets into one dataframe using the left_join function
southland_birds <- region_birds %>%
  left_join(bird_info, by="common_name")

#find the number of occurrences of bird families
variable_list <- southland_birds %>%
  group_by(Family) %>%
  summarise(count = n())
view(variable_list)

#geospatial data
nz_map <- map_data("nz", region = "South.Island")

#using absolute values since inputs have a negative value (user error)
southland_birds <- southland_birds %>%
  mutate(longitude = abs(longitude))

#geospatial map plot
ggplot(data=nz_map) +
  geom_polygon(mapping=aes(x=long, y=lat, group=group)) +
  coord_map() +
  geom_point(data=southland_birds, mapping=aes(x=longitude, y=latitude, colour=Family), alpha=0.5, size=0.5) +
  facet_wrap(~Year) +
  xlab("Longitude") +
  ylab("Latitude")

#import weather data
weather <- read_delim("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SCIENVI303/2023h1/ass2/weather_data_all.csv", delim=",")

#data tidying
#filtering the weather data for the state of Southland and weather condition to temperature
weather_tidy <- weather %>%
  filter(Stats_Code == "temperature" & State == "Southland")

weather_tidy <- weather_tidy %>%
  mutate(Jul = as.character(Jul),
         Aug = as.character(Aug),
         Sep = as.character(Sep))

#combine the weather dataframes
weather_tidy <- weather_tidy %>%
  pivot_longer(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), names_to = "month", values_to = "x") %>%
  pivot_wider(names_from = Stats_Code, values_from = x) %>%
  mutate(month = ifelse(month == "Jan", 1, month), 
         month = ifelse(month == "Feb", 2, month),
         month = ifelse(month == "Mar", 3, month),
         month = ifelse(month == "Apr", 4, month),
         month = ifelse(month == "May", 5, month),
         month = ifelse(month == "Jun", 6, month),
         month = ifelse(month == "Jul", 7, month),
         month = ifelse(month == "Aug", 8, month),
         month = ifelse(month == "Sep", 9, month),
         month = ifelse(month == "Oct", 10, month),
         month = ifelse(month == "Nov", 11, month),
         month = ifelse(month == "Dec", 12, month)) %>%
  mutate(month = as.numeric(month)) %>%
  mutate(
    temperature = as.numeric(temperature))

view(weather_tidy)

#further data filtering
southland_filtered <- southland_birds %>%
  filter(Family == "Petroicidae")


#joining the two dataframes
weather_birds <- southland_filtered %>%
  left_join(weather_tidy, by=c("Year","month"))
view(weather_birds)


#plotting the data to check for correlation
#histogram
ggplot(data=weather_birds) +
  geom_histogram(mapping=aes (x=temperature)) +
  labs(title = "Distribution of Petroicidae across temperature in Southland, NZ",
       x= "Temperature",
       y="Number of birds")

#countsPlot
ggplot(data=weather_birds) +
  geom_count(mapping=aes(x=temperature, y=log10(birds_spotted_month))) +
  labs(title = "Petroicidae across different temperature, counts plot",
       x = "Temperature",
       y = "Number of birds in log10")

#line chart scatterplot
ggplot(data=weather_birds) +
  geom_point(mapping = aes(x=temperature, y=log10(birds_spotted_month), colour=Family), alpha=0.5) +
  geom_smooth(mapping=aes(x=temperature, y=log10(birds_spotted_month), colour=Family), alpha=0.5, se=FALSE) +
  labs(title = "Petroicidae across different temperatures in Southland, scatter/smooth plot",
       x = "Temperature",
       y = "Number of birds (log10)")

#Statistical Analysis
lm(log10(birds_spotted_month) ~ poly(temperature, 3), weather_birds) %>%
  tidy()


bird_map <- map_data("nz", region = "South.Island")

southland_filtered <- southland_filtered %>%
  mutate(longitude = abs(longitude))

ggplot(data=nz_map) +
  geom_polygon(mapping=aes(x=long, y=lat, group=group)) +
  coord_map() +
  geom_point(data=southland_filtered, mapping=aes(x=longitude, y=latitude, colour=Family), alpha=0.5, size=0.5) +
  facet_wrap(~Year) +
  xlab("Longitude") +
  ylab("Latitude")

