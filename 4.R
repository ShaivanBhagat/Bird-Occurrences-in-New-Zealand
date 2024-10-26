library(tidyverse)
library(mapproj)
library(tidymodels)

region_birds4 <- read_delim("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SCIENVI303/2023h1/ass2/Birds/Southland/Southland_Birds.csv",delim=",")
bird_info4 <- read_delim("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SCIENVI303/2023h1/ass2/Bird_details_NZ.txt", delim="\t")

southland_birds4 <- region_birds4 %>%
  left_join(bird_info4, by="common_name")

variable_list4 <- southland_birds4 %>%
  group_by(Family) %>%
  summarise(count = n())

nz_map <- map_data("nz", region = "South.Island")

southland_birds4 <- southland_birds4 %>%
  mutate(longitude = abs(longitude))

southland_filtered_4 <- southland_birds4 %>%
  filter(Family == "Phalacrocoracidae" | Family == "Petroicidae" | Family == "Rallidae")

ggplot(data=nz_map) +
  geom_polygon(mapping=aes(x=long, y=lat, group=group)) +
  coord_map() +
  geom_point(data=southland_filtered_4, mapping=aes(x=longitude, y=latitude, colour=Family), alpha=0.5, size=0.5) +
  facet_wrap(~Year) +
  xlab("Longitude") +
  ylab("Latitude")

weather4 <- read_delim("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SCIENVI303/2023h1/ass2/weather_data_all.csv", delim=",")

weather_tidy4 <- weather4 %>%
  filter(Stats_Code=="temperature" & State =="Southland")

weather_tidy4 <- weather_tidy4 %>%
  mutate(Jul = as.character(Jul),
         Aug = as.character(Aug),
         Sep = as.character(Sep))

weather_tidy4 <- weather_tidy4 %>%
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

weather_birds4 <- southland_filtered_4 %>%
  left_join(weather_tidy4, by=c("Year","month"))

ggplot(data=weather_birds4) +
  geom_point(mapping = aes(x=temperature, y=log10(birds_spotted_month), colour=Family), alpha=0.5) +
  geom_smooth(mapping=aes(x=temperature, y=log10(birds_spotted_month), colour=Family), alpha=0.5, se=FALSE) +
  labs(title = "Birds occurrences across different temperatures, scatter/smooth plot",
       x = "Temperature (˚C)",
       y = "Number of birds (log10)")

lm(log10(birds_spotted_month) ~ poly(temperature, 3), weather_birds4) %>%
  tidy()

ggplot(data=nz_map) +
  geom_polygon(mapping=aes(x=long, y=lat, group=group)) +
  coord_map() +
  geom_point(data=southland_filtered_4, mapping=aes(x=longitude, y=latitude, colour=Family), alpha=0.5, size=0.5) +
  facet_wrap(~Year) +
  xlab("Longitude") +
  ylab("Latitude")



