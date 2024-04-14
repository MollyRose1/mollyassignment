#packages
install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")

#installing packages
library(tidyverse)
library(plotly)
library(dplyr)

#creating files
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

#joining indicator data with metadata
data_join <- full_join(unicef_indicator_2, unicef_metadata)
relationship = "many-to-many"
data_join <- full_join(unicef_indicator_2, unicef_metadata, by = join_by(time_period == year))
relationship = "many-to-many"

data_join_2018both <- full_join(unicef_indicator_2, unicef_metadata, by = join_by(time_period == year)) %>%
  filter(time_period == 2018)
relationship = "many-to-many"

#Renaming Columns for visualisation 3
data_join_2018both <- data_join_2018both %>%
  rename("GDPperCap" = "GDP per capita (constant 2015 US$)")

  data_join_2018both <- data_join_2018both %>%
    rename("LifeExp" = "Life expectancy at birth, total (years)")

  data_join_2018both <- data_join_2018both %>%
    rename("Population" = "Population, total")

  
#Map Visualisation - % of children medical help for diarrhoea in 2000
map_world <- map_data("world")
data_join_2000 <- unicef_indicator_2 %>%
  filter(time_period == 2000)

map_data_join_2000 <- full_join(data_join_2000, map_world, by = c("country" = "region"))
relationship = "many-to-many"

World_map <- ggplot(map_data_join_2000) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "pink", high = "purple", na.value = "grey" ) +
  labs(title = "% of Children who sought medical help for diarrhoea in 2000") +
  theme(text = element_text(family = "serif"), 
        plot.title = element_text(size = 14, face = "bold"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
print(World_map)


#Time Series Visualisation
# % of kids affected over years, filtered to MALI only.

timesseries_plot_1_filtered <- unicef_indicator_2 %>%
  filter(country == "Mali")

timeseries_plot_1 <- timesseries_plot_1_filtered %>%
  ggplot() +
  aes(time_period, obs_value, group = sex, colour = sex) +  
  geom_line() +  # Draw lines
  geom_point() +  # Add points for max and min values
  labs(title = "How Mali has been affected by childhood diarrohea over the years",
       x = "Year",
       y = "% of children",
       colour = "Sex") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = "Arial", color = "black"),
        axis.title = element_text(size = 10, family = "Arial", color = "black"),
        axis.text = element_text(size = 10, family = "Arial", color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("pink", "purple", "blue"),
                     labels = c("Female", "Male", "Total"))
  scale_shape_manual(values = c(1, 2))  # Custom shapes for points

ggplotly(timeseries_plot_1)


#Scatterplot 1 - Type of Economy VS Life Expectancy
data_join_2018both_totalsex <- data_join_2018both %>%
  filter(sex == "Total")

ggplot(data_join_2018both_totalsex) +
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
recent_data <- data_join_2018both %>%
  filter(time_period >= 2002 & time_period <= 2021)

# Group by country and calculate the average obs_value
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
  theme_minimal()

