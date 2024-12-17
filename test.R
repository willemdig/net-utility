# Load required libraries
library(eurostat)
library(tidyverse)

# Step 1: Download the 'isoc_sk_dskl_i21' dataset
digital_skills <- get_eurostat("isoc_sk_dskl_i21", time_format = "num")

# Step 2: Filter data for Nordic countries, basic digital skills, and age groups
selected_countries_age <- digital_skills %>%
  filter(geo %in% c("NO", "SE", "DK", "FI"), 
         indic_is == "I_DSK2_B",         # Basic digital skills indicator
         ind_type %in% c("Y16_24", "Y25_54", "Y55_74")) %>%
  mutate(Age_Group = case_when(         # Create readable age group names
    ind_type == "Y16_24" ~ "16-24",
    ind_type == "Y25_54" ~ "25-54",
    ind_type == "Y55_74" ~ "55-74"
  ))

# Step 3: Create a bar graph for the most recent year
# Filter for the latest available year
latest_year <- max(selected_countries_age$TIME_PERIOD)
bar_data <- selected_countries_age %>% filter(TIME_PERIOD == latest_year)

# Step 4: Plot the grouped bar graph
ggplot(bar_data, aes(x = Age_Group, y = values, fill = geo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Basic Digital Skills by Age Group (", latest_year, ")", sep = ""),
       x = "Age Group",
       y = "Percentage of Individuals",
       fill = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom")


## result is not right most likely, probably some issue with the filtering. 
# Fun with the visualisation, though 
