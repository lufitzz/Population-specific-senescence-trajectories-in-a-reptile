###############################################################################
# PROXY FOR DEAD
###############################################################################

library(dplyr)
library(ggplot2)

## Load and Prepare Data
setwd("C:/Users/luisaf/OneDrive - University of Tasmania/Postdoc/senescence paper/2023 senescence")
data <- read.csv(file = "OcellatusNatPop_FullDataset_2025.csv", 
                 na = c("", "NA", ".", "?"))[ ,c('Year', 'FemID', 'Site')]

# Remove 2023 and 2024 as these are too recent to assess the "came back after 2 years" pattern
data <- data[data$Year <= 2022, ]

# Remove any rows with missing critical data
data <- data[complete.cases(data), ]

# Sort by FemID and Year
data <- data[order(data$FemID, data$Site, data$Year), ]

## Calculate year gaps for each individual

# Create a dataset with gap information
gap_analysis <- data %>%
  group_by(FemID, Site) %>%
  arrange(Year) %>%
  mutate(
    next_year = lead(Year),
    year_gap = next_year - Year,
    is_last_observation = is.na(next_year)) %>%
  ungroup()

# Identify females that missed 2+ years but then returned
returned_after_2yrs <- gap_analysis %>%
  filter(year_gap >= 3) %>%  # gap of 3 means 2 years missed (e.g., seen 2010, 2013 = missed 2011, 2012)
  select(FemID, Site, Year, next_year, year_gap)

# Summary by site
summary_stats <- gap_analysis %>%
  group_by(Site) %>%
  summarise(
    total_females = n_distinct(FemID),
    total_observations = n(),
    mean_gap = mean(year_gap[!is_last_observation], na.rm = TRUE),
    median_gap = median(year_gap[!is_last_observation], na.rm = TRUE),
    max_gap = max(year_gap[!is_last_observation], na.rm = TRUE),
    n_gaps_2yrs = sum(year_gap >= 3, na.rm = TRUE),
    n_gaps_1yr = sum(year_gap == 2, na.rm = TRUE),
    prop_consecutive_years = sum(year_gap == 1, na.rm = TRUE) / sum(!is_last_observation))

print(summary_stats)
print(paste0("Number of instances where females returned after 2+ years missing:"))
print(table(returned_after_2yrs$Site))

# Look at these cases in detail
print("Females that returned after missing 2+ years:")
print(returned_after_2yrs)

# Calculate recapture probability (simple approach)
# Probability of being recaptured in year t+1 given alive in year t
recapture_prob <- gap_analysis %>%
  filter(!is_last_observation) %>%
  group_by(Site) %>%
  summarise(
    prob_recap_next_year = sum(year_gap == 1) / n(),
    prob_recap_within_2yrs = sum(year_gap <= 2) / n())

print("Recapture probabilities:")
print(recapture_prob)

## For each female, what's the longest gap they had before returning?
max_gaps <- gap_analysis %>%
  filter(!is_last_observation) %>%
  group_by(FemID, Site) %>%
  summarise(max_gap_before_return = max(year_gap, na.rm = TRUE)) %>%
  ungroup()

gap_distribution <- max_gaps %>%
  group_by(Site) %>%
  count(max_gap_before_return) %>%
  mutate(prop = n / sum(n))

print("Distribution of maximum gaps before returning:")
print(gap_distribution)

## Visualization
ggplot(gap_distribution, aes(x = max_gap_before_return, y = prop, fill = Site)) +
  geom_col(position = "dodge") +
  labs(x = "Years missed before returning", 
       y = "Proportion of females",
       title = "Maximum gap length before individuals returned") +
  theme_minimal()

# Calculate the actual percentages for the supplementary text
gap_analysis %>%
  filter(!is_last_observation) %>%
  group_by(Site) %>%
  summarise(
    total_observable_gaps = n(),
    gaps_2plus_years = sum(year_gap >= 3),
    percent_2plus = (sum(year_gap >= 3) / n()) * 100)

## Check if there are temporal patterns (were early years different?)
returned_after_2yrs %>%
  group_by(Site) %>%
  summarise(
    earliest_year = min(Year),
    latest_year = max(Year),
    median_year = median(Year))
