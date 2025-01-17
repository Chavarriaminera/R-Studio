

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(isotree)

# Suppress warnings while reading the CSV
df <- suppressWarnings(
  read_csv("C:/Users/chava/Downloads/Pregnancy-Outcomes-for-MedicaidCHIP-Beneficiaries-ages-15-to-44.csv", 
           col_types = cols(
             .default = col_guess(),  # Let `readr` guess the column types for other columns
             ServiceCount = col_number()  # Coerce a specific column to numeric
           ))
)

# Filter data for 2018 and the specific pregnancy outcome
df <- df %>% filter(Year == 2018 &
                      PregnancyOutcome == "Miscarriages / stillbirths / terminations" &
                      is.na(DataQuality))


df$Year <- substr(as.character(df$Month), 1, 4)
df$MonthOnly <- substr(as.character(df$Month), 5, 6)
df$MonthOnly<-as.integer(df$MonthOnly)

# Ensuring that service count is numeric and drop NA values in Service Count. 


# Diagnose the parsing issues
problems(df)

# Review and clean the data after loading (if necessary)
df <- df %>%
  mutate(
    ServiceCount = as.numeric(ServiceCount),  # Convert ServiceCount to numeric, set non-numeric to NA
    RatePer1000Beneficiaries = as.numeric(RatePer1000Beneficiaries)  # Ensure RatePer1000Beneficiaries is numeric
  ) %>%
  drop_na(ServiceCount, RatePer1000Beneficiaries)  # Drop rows with NA in key numeric columns



# One-hot encoded data with the original numeric data.
df$State<- as.character(df$State)



# Get the unique state names
State_Names <- data.frame(State = unique(df$State))



#Function 
Process_State_Data <- function(df, State_Name, aggregate_data) {
  # Filter data by State
  df <- df[df$State == State_Name, ]
  
  # If no data for the state, return aggregate_data
  if (nrow(df) == 0) {
    return(aggregate_data)
  }
  
  # Create the dataset with the necessary columns
  df <- data.frame(
    Month = df$MonthOnly,
    State = df$State,
    ServiceCount = df$ServiceCount
  )
  

  
  # Getting the number of rows in the dataset  
  sample_size <- nrow(df)
  
  # Initializing the isolation forest model
  iso_forest <- isolation.forest(df, ntrees = 100, sample_size = sample_size, seed = 42)
  
  # Predicting anomaly scores
  anomaly_scores <- predict(iso_forest, df, type = "score")
  
  # Adding anomaly scores to the dataframe
  df$anomaly_scores <- anomaly_scores
  
  # Set a threshold for anomalies
  threshold <- 0.60
  df$anomaly <- ifelse(df$anomaly_scores > threshold, "Yes", "No")
  
  # Append this state's data to aggregate_data
  aggregate_data <- rbind(aggregate_data, df)
  
  return(aggregate_data)
}

# For Loop 

# Initialize aggregate_data as an empty data frame before the loop
aggregate_data <- data.frame()

# Loop through all the states
for (State in State_Names$State) {
  aggregate_data <- Process_State_Data(df, State, aggregate_data)
}

#
print("Finished processing all states")

# Check distinct state counts in the aggregated data and the original filtered data

threshold_value_Red <- 0.60

# Load necessary libraries


aggregate_data$CalendarMonth<-month.name[aggregate_data$Month]


# Ensure that anomaly_scores is properly scaled or factorized if necessary
aggregate_data$anomaly <- as.factor(aggregate_data$anomaly)  # Assuming anomaly is binary "Yes" or "No"

# Create the plot
Plot <- ggplot(aggregate_data, aes(x = State, y = ServiceCount, color = anomaly)) +
  geom_point(size = 3, alpha = 0.7) +  # Plot points
  labs(title = "Service Counts by State (2018) with Anomalies Highlighted",
       x = "State",
       y = "Service Count",
       color = "Anomaly Detected") +
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate state names for readability

# Print the plot
print(Plot)
