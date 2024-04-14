
#packages
install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("patchwork")

#installing packages
library(tidyverse)
library(plotly)
library(dplyr)
library(patchwork)

install.packages("conflicted")
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

#creating files
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

#joining indicator data with metadata
data_join <- full_join(unicef_indicator_2, unicef_metadata, by = join_by(time_period == year))
relationship = "many-to-many"

data_year2018 <- full_join(unicef_indicator_2, unicef_metadata, by = join_by(time_period == year)) %>%
  filter(time_period == 2018)
relationship = "many-to-many"

#Renaming Columns for visualisation 3
data_year2018 <- data_year2018 %>%
  rename("GDPperCap" = "GDP per capita (constant 2015 US$)")

data_year2018 <- data_year2018 %>%
  rename("LifeExp" = "Life expectancy at birth, total (years)")

data_year2018 <- data_year2018 %>%
  rename("Population" = "Population, total")

MapObsData_year2000 <- MapObsData_year2000 %>%
  rename("%" = "obs_value")


#Map Visualisation 1 - % of children medical help for diarrhoea in 2000
#Load map data
map_world <- map_data("world")

# Filter data for a specific year
selected_year <- 2000
ObservationData_year <- unicef_indicator_2 %>%
  filter(time_period == selected_year)

# Join with map data
map_data_year <- full_join(ObservationData_year, map_world, by = c("country" = "region"))

# Create ggplot
World_map <- ggplot(map_data_year) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "pink", high = "purple", na.value = "grey", name = "% of children") +
  labs(title = paste("% of children who sought medical help for diarrhoea in", selected_year),
       x = NULL, y = NULL) +  # Remove axis labels
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")  # Move legend to the bottom

print(World_map)


#Map Visualisation 2 - % of children medical help for diarrhoea in 2018
map_world2 <- map_data("world")

# Filter data for a specific year
selected_year <- 2018
ObservationData_year2018 <- unicef_indicator_2 %>%
  filter(time_period == selected_year)

# Join with map data
map_data_year2 <- full_join(ObservationData_year2018, map_world2, by = c("country" = "region"))

# Create ggplot
World_map2 <- ggplot(map_data_year2) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "#FFA07A", high = "#D8BFD8", na.value = "grey", name = "% of children") +
  labs(title = paste("% of children who sought medical help for diarrhoea in", selected_year),
       x = NULL, y = NULL) +  # Remove axis labels
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")  # Move legend to the bottom

print(World_map2)


#Time Series Visualisation
# % of kids affected over years, filtered to MALI only.

ObservationData_countryMali <- unicef_indicator_2 %>%
  filter(country == "Mali")

Timeseries_1 <- ObservationData_countryMali %>%
  ggplot() +
  aes(time_period, obs_value, group = sex, colour = sex) +  
  geom_line(size = 1) +  # Increase line width
  geom_point(size = 3) +  # Increase marker size
  labs(title = "How Mali has been affected by childhood diarrhoea over the years",
       x = "Year",
       y = "% of children",
       colour = "Sex") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, color = "black", face = "bold"),
        axis.title = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 10, color = "black"),
        panel.background = element_rect(fill = "white"),  # Add background color
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",  # Adjust legend position
        legend.text = element_text(size = 10)) +  # Adjust legend text size
  scale_color_manual(values = c("#D8BFD8", "#FFA07A", "grey"),
                     labels = c("Female", "Male", "Total")) +
  scale_shape_manual(values = c(16, 17)) 

ggplotly(Timeseries_1)


#Scatterplot 1 - Type of Economy VS Life Expectancy
data_year2018_sexTotal <- data_year2018 %>%
  filter(sex == "Total")

ggplot(data_year2018_sexTotal) +
  aes(GDPperCap, LifeExp, colour = country.x, size = Population) +
  geom_point(alpha = 0.2) +
  scale_x_continuous(limits = c(0, 15000)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = "Arial", color = "black"),
        axis.title = element_text(size = 10, family = "Arial", color = "black"),
        axis.text = element_text(size = 10, family = "Arial", color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "light grey")) +
  theme(legend.position = "None") +
  labs(x = "GDP per capita",
       y = "Life expectancy at birth (years)",
       title = "Type of Economy Vs Life Expectancy")


#BarChart - Worst affected countries over the last 20 years.
# Filter data for the last 20 years
recent_data <- data_year2018_sexTotal %>%
  filter(time_period >= 2002 & time_period <= 2021)

average_obs <- recent_data %>%
  group_by(country.x) %>%
  summarize(avg_obs_value = mean(obs_value, na.rm = TRUE))

# Select top 10 countries with the highest average obs_value
top_10_countries <- average_obs %>%
  top_n(10, avg_obs_value)

# Create bar chart
ggplot(top_10_countries, aes(x = reorder(country.x, -avg_obs_value), y = avg_obs_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "",
       y = "Average % of children who sought medical treatment for diarrrhoea",
       title = "Top 10 countries affected by threatening childhood diarrhoea (Last 20 Years)") +
  theme_minimal()+
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = "Arial", color = "black"))

