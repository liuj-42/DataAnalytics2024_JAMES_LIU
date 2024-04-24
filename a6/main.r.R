# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(scales)
library(MASS)
library(randomForest)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Define the custom theme
Rohan_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, vjust = 1)
  )

# Read and clean the datasets
outbreak_data <- read.csv("outbreak_data.csv")
# precipitation_data <- read.csv("precipitation_data.csv")
moisture_data <- read.csv("moisture_data.csv")
temperature_data <- read.csv("temperature_data.csv")

# Mapping of African country abbreviations to full names
african_country_mapping <- data.frame(
  code = c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
           "COM", "COD", "COG", "CIV", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH",
           "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG",
           "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "REU",
           "RWA", "SHN", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN",
           "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE"),
  full_name = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cape Verde", "Cameroon", "Central African Republic", "Chad",
                "Comoros", "Democratic Republic of the Congo", "Republic of the Congo", "Ivory Coast", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia",
                "Gabon", "The Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar",
                "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Réunion",
                "Rwanda", "Saint Helena", "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan",
                "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
)

outbreak_data <- merge(outbreak_data, african_country_mapping, by.x = "country", by.y = "code", all.x = TRUE)

# Convert date columns to Date type
outbreak_data$start_date <- as.Date(outbreak_data$start_date, format="%m/%d/%Y")
outbreak_data$end_date <- as.Date(outbreak_data$end_date, format="%m/%d/%Y")

# Extract month and season from dates
outbreak_data$month <- format(outbreak_data$start_date, "%m")
outbreak_data$season <- ifelse(outbreak_data$month %in% c("12", "01", "02"), "Winter",
                               ifelse(outbreak_data$month %in% c("03", "04", "05"), "Spring",
                                      ifelse(outbreak_data$month %in% c("06", "07", "08"), "Summer",
                                             "Fall")))

# # Calculate the total precipitation for each location
# precipitation_data <- precipitation_data %>%
#   mutate(total_precipitation = rowSums(dplyr::select(., starts_with('precipitation')), na.rm = TRUE))

# Calculate the total moisture for each location
moisture_data <- moisture_data %>%
  mutate(total_moisture = rowSums(dplyr::select(., starts_with('soilMois')), na.rm = TRUE))

# Calculate the total temperature for each location
temperature_data <- temperature_data %>%
  mutate(total_temperature = rowSums(dplyr::select(., starts_with('soilTemp')), na.rm = TRUE))

# Load country borders
world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- world[world$continent == "Africa", ]

# Define the longitude and latitude range for Africa
xlim_africa <- c(-25, 60)  # Approximate longitude range for Africa
ylim_africa <- c(-35, 40)  # Approximate latitude range for Africa

# Spatial plot for Cholera Outbreaks
ggplot(data = outbreak_data) +
  geom_sf(data = world, fill = NA, color = "gray50") +  # Plot country borders
  geom_sf(data = africa, fill = "gray95", color = "gray50") +  # Highlight the African continent
  geom_point(aes(x = longitude, y = latitude, size = total_suspected_cases, fill = total_suspected_cases), pch = 21, alpha = 0.7) +
  scale_size(name = "Total Suspected Cases", range = c(1, 10)) +
  scale_fill_viridis_c(name = "Total Suspected Cases", option = "C", begin = 0.3, end = 0.9, direction = 1) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +  # Combine legends
  coord_sf(xlim = xlim_africa, ylim = ylim_africa, expand = FALSE) +  # Zoom into African region
  labs(
    title = "Spatial Distribution of Cholera Cases in Africa",
    subtitle = "Size and color of points represent the number of suspected cases",
    x = "Longitude",
    y = "Latitude"
  ) +
  Rohan_theme +
  theme(
    legend.position = "right",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# # Create the precipitation spatial plot
# ggplot(data = precipitation_data) +
#   geom_sf(data = world, fill = NA, color = "gray50") +  # Plot world borders
#   geom_sf(data = africa, fill = "gray95", color = "gray50") +  # Highlight the African continent
#   geom_point(aes(x = longitude, y = latitude, size = total_precipitation, fill = total_precipitation), pch = 21, alpha = 0.7) +
#   scale_size(name = "Total Precipitation", range = c(1, 10)) +
#   scale_fill_viridis_c(name = "Total Precipitation", option = "C", begin = 0.3, end = 0.9, direction = 1) +
#   guides(fill = guide_legend(override.aes = list(size = 6))) +  # Use override.aes to match size in legend
#   coord_sf(xlim = xlim_africa, ylim = ylim_africa, expand = FALSE) +  # Zoom into the African region
#   labs(
#     title = "Spatial Distribution of Precipitation in Africa",
#     subtitle = "Size and fill of points represent total precipitation",
#     x = "Longitude",
#     y = "Latitude"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     text = element_text(size = 12),
#     plot.title = element_text(size = 14, face = "bold"),
#     plot.subtitle = element_text(size = 12)
#   )

# Create the soil moisture spatial plot
ggplot(data = moisture_data) +
  geom_sf(data = world, fill = NA, color = "gray50") +  # Plot world borders
  geom_sf(data = africa, fill = "gray95", color = "gray50") +  # Highlight the African continent
  geom_point(aes(x = longitude, y = latitude, size = total_moisture/120, fill = total_moisture/120), pch = 21, alpha = 0.7) +
  scale_size(name = "Average Soil Moisture (kg m-2)", range = c(1, 10)) +
  scale_fill_viridis_c(name = "Average Soil Moisture (kg m-2)", option = "C", begin = 0.3, end = 0.9, direction = 1) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +  # Use override.aes to match size in legend
  coord_sf(xlim = xlim_africa, ylim = ylim_africa, expand = FALSE) +  # Zoom into the African region
  labs(
    title = "Spatial Distribution of Soil Moisture in Africa",
    subtitle = "Size and fill of points represent average soil moisture",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Create the soil temperature spatial plot
ggplot(data = temperature_data) +
  geom_sf(data = world, fill = NA, color = "gray50") +  # Plot world borders
  geom_sf(data = africa, fill = "gray95", color = "gray50") +  # Highlight the African continent
  geom_point(aes(x = longitude, y = latitude, size = total_temperature/120, fill = total_temperature/120), pch = 21, alpha = 0.7) +
  scale_size(name = "Average Soil Temperature (K)", range = c(1, 10)) +
  scale_fill_viridis_c(name = "Average Soil Temperature (K)", option = "C", begin = 0.3, end = 0.9, direction = 1) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +  # Use override.aes to match size in legend
  coord_sf(xlim = xlim_africa, ylim = ylim_africa, expand = FALSE) +  # Zoom into the African region
  labs(
    title = "Spatial Distribution of Soil Temperature in Africa",
    subtitle = "Size and fill of points represent average soil temperature",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

merged_data_moisture <- merge(outbreak_data, moisture_data, by = "location_period_id")
merged_data_temperature <- merge(outbreak_data, temperature_data, by = "location_period_id")

# # Merge the datasets
# merged_data <- merge(outbreak_data, precipitation_data, by = "location_period_id")
#
# # Convert date columns to Date type if not already
# merged_data$start_date <- as.Date(merged_data$start_date, format="%m/%d/%Y")
# merged_data$end_date <- as.Date(merged_data$start_date, format="%m/%d/%Y")

merged_data_moisture$start_date <- as.Date(merged_data_moisture$start_date, format="%m/%d/%Y")
merged_data_moisture$end_date <- as.Date(merged_data_moisture$start_date, format="%m/%d/%Y")

merged_data_temperature$start_date <- as.Date(merged_data_temperature$start_date, format="%m/%d/%Y")
merged_data_temperature$end_date <- as.Date(merged_data_temperature$start_date, format="%m/%d/%Y")



# # Aggregate data by month/year and calculate total precipitation and cholera cases
# monthly_data <- merged_data %>%
#   mutate(month = floor_date(start_date, "month")) %>%
#   group_by(month) %>%
#   summarize(total_cases = sum(total_suspected_cases, na.rm = TRUE),
#             total_precipitation = sum(`precipitation.2010_1`, na.rm = TRUE))

# Aggregate data by month/year and calculate total moisture and cholera cases
monthly_data_moisture <- merged_data_moisture %>%
  mutate(month = floor_date(start_date, "month")) %>%
  group_by(month) %>%
  summarize(total_cases = sum(total_suspected_cases, na.rm = TRUE),
            total_moisture = sum(`soilMois.2010_01`, na.rm = TRUE))

# Aggregate data by month/year and calculate total temperature and cholera cases
monthly_data_temperature <- merged_data_temperature %>%
  mutate(month = floor_date(start_date, "month")) %>%
  group_by(month) %>%
  summarize(total_cases = sum(total_suspected_cases, na.rm = TRUE),
            total_temperature = sum(`soilTemp.2010_01`, na.rm = TRUE))

# # Used to scale the secondary axis below
# factor <- max(monthly_data$total_cases) / max(monthly_data$total_precipitation)

factor_moisture <- max(monthly_data_moisture$total_cases) / max(monthly_data_moisture$total_moisture)
factor_temperature <- max(monthly_data_temperature$total_cases) / max(monthly_data_temperature$total_temperature)

# # Create the time-series plot with a secondary axis
# ggplot(monthly_data, aes(x = month)) +
#   geom_line(aes(y = total_cases, colour = "Cholera Cases")) +
#   geom_line(aes(y = total_precipitation * factor, colour = "Precipitation")) +
#   scale_y_continuous(
#     name = "Total Cholera Cases",
#     limits = c(0, 80000), # Set limit for the primary axis (left side)
#     sec.axis = sec_axis(~./factor, name="Total Precipitation (mm/day)",
#                         labels = scales::comma) # Adjust the secondary axis (right side)
#   ) +
#   labs(title = "Cholera Cases and Precipitation Over Time",
#        x = "Time") +
#   scale_colour_manual(
#     "",
#     breaks = c("Cholera Cases", "Precipitation"),
#     values = c("blue", "green")
#   ) +
#   theme_minimal() +
#   theme(legend.position="bottom")

# Create the time-series plot with a secondary axis
ggplot(monthly_data_moisture, aes(x = month)) +
  geom_line(aes(y = total_cases, colour = "Cholera Cases")) +
  geom_line(aes(y = total_moisture * factor_moisture, colour = "Moisture")) +
  scale_y_continuous(
    name = "Total Cholera Cases",
    limits = c(0, 80000), # Set limit for the primary axis (left side)
    sec.axis = sec_axis(~./factor_moisture, name="Total Moisture (kg m-2)",
                        labels = scales::comma) # Adjust the secondary axis (right side)
  ) +
  labs(title = "Cholera Cases and Moisture Over Time",
       x = "Time") +
  scale_colour_manual(
    "",
    breaks = c("Cholera Cases", "Moisture"),
    values = c("blue", "green")
  ) +
  theme_minimal() +
  theme(legend.position="bottom")

# Create the time-series plot with a secondary axis
ggplot(monthly_data_temperature, aes(x = month)) +
  geom_line(aes(y = total_cases, colour = "Cholera Cases")) +
  geom_line(aes(y = total_temperature * factor_temperature, colour = "Temperature")) +
  scale_y_continuous(
    name = "Total Cholera Cases",
    limits = c(0, 80000), # Set limit for the primary axis (left side)
    sec.axis = sec_axis(~./factor_temperature, name="Total Temperature (K)",
                        labels = scales::comma) # Adjust the secondary axis (right side)
  ) +
  labs(title = "Cholera Cases and Temperature Over Time",
       x = "Time") +
  scale_colour_manual(
    "",
    breaks = c("Cholera Cases", "Temperature"),
    values = c("blue", "green")
  ) +
  theme_minimal() +
  theme(legend.position="bottom")

# merge all 3 datasets
monthly_data_all <- merge(monthly_data_moisture, monthly_data_temperature, by = "month")

# plot all 3 datasets
ggplot(monthly_data_all, aes(x = month)) +
  geom_line(aes(y = total_cases.x, colour = "Cholera Cases")) +
  geom_line(aes(y = total_moisture * factor_moisture, colour = "Moisture")) +
  geom_line(aes(y = total_temperature * factor_temperature, colour = "Temperature")) +
  scale_y_continuous(
    name = "Total Cholera Cases",
    limits = c(0, 80000), # Set limit for the primary axis (left side)
    sec.axis = sec_axis(~./factor_moisture, name="Total Moisture (kg m-2)",
                        labels = scales::comma) # Adjust the secondary axis (right side)
  ) +
  labs(title = "Cholera Cases, Moisture and Temperature Over Time",
       x = "Time") +
  scale_colour_manual(
    "",
    breaks = c("Cholera Cases", "Moisture", "Temperature"),
    values = c("blue", "green", "red")
  ) +
  theme_minimal() +
  theme(legend.position="bottom")


print("test")
# # Shapiro-Wilk normality test for total_cases
# shapiro.test(monthly_data$total_cases)

shapiro.test(monthly_data_moisture$total_cases)
shapiro.test(monthly_data_temperature$total_cases)

# # Shapiro-Wilk normality test for total_precipitation
# shapiro.test(monthly_data$total_precipitation)

shapiro.test(monthly_data_moisture$total_moisture)
shapiro.test(monthly_data_temperature$total_temperature)

# # Spearman correlation test - Data not normal
# cor.test(monthly_data$total_cases, monthly_data$total_precipitation, method = "spearman")

cor.test(monthly_data_moisture$total_cases, monthly_data_moisture$total_moisture, method = "spearman")
cor.test(monthly_data_temperature$total_cases, monthly_data_temperature$total_temperature, method = "spearman")

# # Robust Linear Model - Data not normal
# lm_model <- rlm(total_cases ~ total_precipitation, data = monthly_data)

lm_model_moisture <- rlm(total_cases ~ total_moisture, data = monthly_data_moisture)
lm_model_temperature <- rlm(total_cases ~ total_temperature, data = monthly_data_temperature)

# # Use the model to predict cholera cases for a range of precipitation values
# future_precipitation_data <- data.frame(total_precipitation = seq(min(monthly_data$total_precipitation, na.rm = TRUE),
#                                                                   max(monthly_data$total_precipitation, na.rm = TRUE), length.out = 100))
# predictions <- predict(lm_model, newdata = future_precipitation_data)

future_moisture_data <- data.frame(total_moisture = seq(min(monthly_data_moisture$total_moisture, na.rm = TRUE),
                                                        max(monthly_data_moisture$total_moisture, na.rm = TRUE), length.out = 100))
predictions_moisture <- predict(lm_model_moisture, newdata = future_moisture_data)

future_temperature_data <- data.frame(total_temperature = seq(min(monthly_data_temperature$total_temperature, na.rm = TRUE),
                                                              max(monthly_data_temperature$total_temperature, na.rm = TRUE), length.out = 100))
predictions_temperature <- predict(lm_model_temperature, newdata = future_temperature_data)





# # Add predictions to the future precipitation data frame
# future_precipitation_data$predicted_cases <- predictions
future_moisture_data$predicted_cases <- predictions_moisture
future_temperature_data$predicted_cases <- predictions_temperature

# # Plot the predictions along with the original data

ggplot(monthly_data_moisture, aes(x = total_moisture, y = total_cases)) +
    geom_point() +
    geom_line(data = future_moisture_data, aes(x = total_moisture, y = predicted_cases), color = 'red') +
    # ylim(0, 80000) +
    # xlim(0, 150) +
    labs(title = "Predicted Cholera Cases Based on Moisture (rlm)",
         x = "Total Moisture (kg m-2)",
         y = "Predicted Total Cholera Cases") +
    Rohan_theme

ggplot(monthly_data_temperature, aes(x = total_temperature, y = total_cases)) +
    geom_point() +
    geom_line(data = future_temperature_data, aes(x = total_temperature, y = predicted_cases), color = 'red') +
    # ylim(0, 80000) +
    # xlim(0, 150) +
    labs(title = "Predicted Cholera Cases Based on Temperature (rlm)",
         x = "Total Temperature (K)",
         y = "Predicted Total Cholera Cases") +
    Rohan_theme

# # Output the summary of the linear model
summary(lm_model_moisture)
summary(lm_model_temperature)

# # Calculate the RMSE for the linear model
rmse_moisture <- sqrt(mean((monthly_data_moisture$total_cases - predict(lm_model_moisture))^2))
rmse_temperature <- sqrt(mean((monthly_data_temperature$total_cases - predict(lm_model_temperature))^2))

# # Output the RMSE for the linear model
rmse_moisture
rmse_temperature


# 3 more models:
# 1. Random Forest
# 2. Support Vector Machine
# 3. ???

# Random Forest Model

# split training and testing data
set.seed(123)
train_indices <- sample(1:nrow(monthly_data_moisture), 0.8 * nrow(monthly_data_moisture))
train_data_moisture <- monthly_data_moisture[train_indices, ]
test_data_moisture <- monthly_data_moisture[-train_indices, ]

train_indices <- sample(1:nrow(monthly_data_temperature), 0.8 * nrow(monthly_data_temperature))
train_data_temperature <- monthly_data_temperature[train_indices, ]
test_data_temperature <- monthly_data_temperature[-train_indices, ]

# Train the Random Forest Model
rf_model_moisture <- randomForest(total_cases ~ total_moisture, data = train_data_moisture, ntree = 500)
rf_model_temperature <- randomForest(total_cases ~ total_temperature, data = train_data_temperature, ntree = 500)

# Make predictions on the test data
test_data_moisture$predicted_cases_rf <- predict(rf_model_moisture, newdata = test_data_moisture)
test_data_temperature$predicted_cases_rf <- predict(rf_model_temperature, newdata = test_data_temperature)

# Calculate the RMSE for the Random Forest Model
rmse_rf_moisture <- sqrt(mean((test_data_moisture$total_cases - test_data_moisture$predicted_cases_rf)^2))
rmse_rf_temperature <- sqrt(mean((test_data_temperature$total_cases - test_data_temperature$predicted_cases_rf)^2))

# Output the RMSE for the Random Forest Model
rmse_rf_moisture
rmse_rf_temperature

# Make predictions for a range of moisture values
future_moisture_data$predicted_cases_rf <- predict(rf_model_moisture, newdata = future_moisture_data)
future_temperature_data$predicted_cases_rf <- predict(rf_model_temperature, newdata = future_temperature_data)




# Plot the predictions along with the original data
ggplot(monthly_data_moisture, aes(x = total_moisture, y = total_cases)) +
    geom_point() +
    geom_line(data = future_moisture_data, aes(x = total_moisture, y = predicted_cases_rf), color = 'red') +
    labs(title = "Predicted Cholera Cases Based on Moisture (Random Forest)",
         x = "Total Moisture (kg m-2)",
         y = "Predicted Total Cholera Cases") +
    Rohan_theme

ggplot(monthly_data_temperature, aes(x = total_temperature, y = total_cases)) +
    geom_point() +
    geom_line(data = future_temperature_data, aes(x = total_temperature, y = predicted_cases_rf), color = 'red') +
    labs(title = "Predicted Cholera Cases Based on Temperature (Random Forest)",
         x = "Total Temperature (K)",
         y = "Predicted Total Cholera Cases") +
    Rohan_theme

# plot the predictions compared to just the training data
ggplot(train_data_moisture, aes(x = total_moisture, y = total_cases)) +
  geom_point() +
  geom_line(data = future_moisture_data, aes(x = total_moisture, y = predicted_cases_rf), color = 'red') +
  labs(title = "Predicted Cholera Cases Based on Moisture (Random Forest)",
       x = "Total Moisture (kg m-2)",
       y = "Predicted Total Cholera Cases") +
  Rohan_theme


#
# # plot both models together
# ggplot(monthly_data_moisture, aes(x = total_moisture, y = total_cases)) +
#     geom_point() +
#     geom_line(data = future_moisture_data, aes(x = total_moisture, y = predicted_cases_rf), color = 'red') +
#     geom_line(data = future_moisture_data, aes(x = total_moisture, y = predicted_cases), color = 'blue') +
#     labs(title = "Predicted Cholera Cases Based on Moisture (Random Forest vs rlm)",
#          x = "Total Moisture (kg m-2)",
#          y = "Predicted Total Cholera Cases") +
#     Rohan_theme


# # store both predictions in a new dataframe by month and total_cases
# monthly_data <- merge(monthly_data_moisture, monthly_data_temperature, by = "month")
# # remove duplicate total_cases column
# monthly_data <- monthly_data[, -2]
# # rename old total_cases column
# colnames(monthly_data)[3] <- "total_cases"
# monthly_data$moisture_prediction <- future_moisture_data$predicted_cases_rf
# monthly_data$temperature_prediction <- future_temperature_data$predicted_cases_rf



# Support Vector Machine Model
library(e1071)

# Train the Support Vector Machine Model
svm_model_moisture <- svm(total_cases ~ total_moisture, data = train_data_moisture)
svm_model_temperature <- svm(total_cases ~ total_temperature, data = train_data_temperature)

# Make predictions on the test data
test_data_moisture$predicted_cases_svm <- predict(svm_model_moisture, newdata = test_data_moisture)
test_data_temperature$predicted_cases_svm <- predict(svm_model_temperature, newdata = test_data_temperature)

# Calculate the RMSE for the Support Vector Machine Model
rmse_svm_moisture <- sqrt(mean((test_data_moisture$total_cases - test_data_moisture$predicted_cases_svm)^2))
rmse_svm_temperature <- sqrt(mean((test_data_temperature$total_cases - test_data_temperature$predicted_cases_svm)^2))

# Output the RMSE for the Support Vector Machine Model
rmse_svm_moisture
rmse_svm_temperature

# Make predictions for a range of moisture values
  future_moisture_data$predicted_cases_svm <- predict(svm_model_moisture, newdata = future_moisture_data)
future_temperature_data$predicted_cases_svm <- predict(svm_model_temperature, newdata = future_temperature_data)

# Plot the predictions along with the original data
ggplot(monthly_data_moisture, aes(x = total_moisture, y = total_cases)) +
    geom_point() +
    geom_line(data = future_moisture_data, aes(x = total_moisture, y = predicted_cases_svm), color = 'red') +
    labs(title = "Predicted Cholera Cases Based on Moisture (SVM)",
         x = "Total Moisture (kg m-2)",
         y = "Predicted Total Cholera Cases") +
    Rohan_theme

ggplot(monthly_data_temperature, aes(x = total_temperature, y = total_cases)) +
    geom_point() +
    geom_line(data = future_temperature_data, aes(x = total_temperature, y = predicted_cases_svm), color = 'red') +
    labs(title = "Predicted Cholera Cases Based on Temperature (SVM)",
         x = "Total Temperature (K)",
         y = "Predicted Total Cholera Cases") +
    Rohan_theme


# svr
# Train the Support Vector Regression Model
