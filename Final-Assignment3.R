getwd()
install.packages("tidyverse")
library(tidyverse)
csv_file_path <- "/Users/ralexandria/Downloads/StormEvents_details-ftp_v1.0_d2015_c20220425.csv"
Final_Cleaned_Data <- read_csv(csv_file_path)
Final_Cleaned_Data <- Final_Cleaned_Data %>%
  select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)
Final_Cleaned_Data <- Final_Cleaned_Data %>%
  arrange(STATE)
library(stringr)

Final_Cleaned_Data <- Final_Cleaned_Data %>%
  mutate(STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME))
Final_Cleaned_Data <- Final_Cleaned_Data %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)
library(stringr)
library(tidyr)

Final_Cleaned_Data <- Final_Cleaned_Data %>%
  mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 2, pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, width = 3, pad = "0")) %>%
  unite(FIPS, STATE_FIPS, CZ_FIPS, sep = "")
# Load the state dataset
data("state")

Final_Cleaned_Data <- Final_Cleaned_Data %>%
  rename_all(tolower)

# Create a new dataframe with desired columns
state_data <- data.frame(state.name = state.name,
                         area = state.area,
                         region = state.region)
# Calculate the frequency of each state
State_By_Event <- data.frame(
  STATE = unique(Final_Cleaned_Data$state),
  Frequency = table(Final_Cleaned_Data$state)
)

# View the State_By_Event dataframe
head(State_By_Event)
# Rename the "state.name" column to "STATE" in the state_data dataframe
colnames(state_data)[colnames(state_data) == "state.name"] <- "STATE"

# Merge with the state_data dataframe
merged_data <- merge(State_By_Event, state_data, by = "STATE", all.x = FALSE)

# View the merged_data dataframe
head(merged_data)
# Remove the "Frequency.Var1" column
merged_data <- merged_data %>% select(-Frequency.Var1)

# View the updated merged_data dataframe
head(merged_data)

# Rename columns with title case
colnames(merged_data) <- str_to_title(colnames(merged_data))

# Rename "Frequency.Freq" column to "Frequency"
colnames(merged_data)[colnames(merged_data) == "Frequency.freq"] <- "Frequency"

# View the updated merged_data dataframe
head(merged_data)
library(ggplot2)

# Scatterplot
ggplot(merged_data, aes(x = Area, y = Frequency, color = Region)) +
  geom_point() +
  labs(x = "Land Area (Square Miles)", y = "# of Storm Events by 2015") +
  ggtitle("Scatterplot of Frequency and Land Area by Region") +
  theme_bw()


