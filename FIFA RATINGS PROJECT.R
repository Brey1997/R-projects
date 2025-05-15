# PROJECT 5 
# Aubrey Chihota
# LAST VISIT - 13/10/2024
#using an alpha = 0.01

# Load libraries
library(dplyr)
library(ggplot2)
library(stargazer)
library(car)
library(ggplot2)
library(reshape2)
library(pwr)

# Load the FIFA22 data
data <- read.csv("CLEAN_FIFA22_official_data.csv")
# Subsetting data for the year 2021
fifa_data <- data %>% filter(Year_Joined == 2014)  # Exclude Year from IVs

# Check the number of rows
nrow(fifa_data)

# Select necessary columns for the model
fifa_subset <- fifa_data %>%
  select(Overall, Age, Potential, Value... , Preferred.Foot) %>%
  na.omit()  # Remove rows with missing values

list(is.na(fifa_subset)) #check how many missing values has been removed

# Divide Value by 1,000,000
fifa_subset$Value... <- fifa_subset$Value... / 1000000

# Check the structure of the dataset to confirm Year is not included
str(fifa_subset)

# Convert preferred_foot to factor and set the reference level
fifa_subset$Preferred.Foot <- as.factor(fifa_subset$Preferred.Foot)
fifa_subset$Preferred.Foot <- relevel(fifa_subset$Preferred.Foot, ref = "Right")

# Check the levels of preferred_foot
levels(fifa_subset$Preferred.Foot)

#SUMMARY FOR IVS AND DV
summarytools::freq(fifa_subset$Preferred.Foot)
summarytools::descr(fifa_subset$Overall)
summarytools::descr(fifa_subset$Age)
summarytools::descr(fifa_subset$Potential)
summarytools::descr(fifa_subset$Value...)

# Fit the multiple linear regression model
model <- lm(Overall ~ Age + Potential + Value... + Preferred.Foot, data = fifa_subset)

# Summary of the regression model
summary(model)

# Diagnostic plots
par(mfrow = c(2, 2))  # Set up the plotting area
plot(model)

# Check for multicollinearity (optional)
vif(model)

# Create a stargazer table of the regression results (summarised code from poe)
stargazer(model, type = "text", title = "Regression Results", digits = 2)

# Create the first plot (Overall vs Age and Potential with Preferred Foot as color)
plot1 <- ggplot(fifa_subset, aes(x = Age, y = Overall, color = Preferred.Foot)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  labs(title = "Plot 1. Overall Rating vs Age and Preferred Foot",
       x = "Age",
       y = "Overall Rating",
       color = "Preferred Foot") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 10))  # Reduce the font size of the title

# Create the second plot (Overall vs Value and Potential as color)
plot2 <- ggplot(fifa_subset, aes(x = Value..., y = Overall, color = Potential)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  labs(title = "Plot 2. Overall Rating vs Value and Potential",
       x = "Value (in millions)",
       y = "Overall Rating",
       color = "Pontential") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 10))  # Reduce the font size of the title

# Display the plots
plot1
plot2

# CALCULATING THE F-STAT
# Get the summary of the model
summary_model <- summary(model)

# Extract R^2
R2 <- summary_model$r.squared
R2

# Calculate f^2
f2 <- R2 / (1 - R2)

# Print the f^2 statistic
print(paste("f^2:", round(f2, 3)))

# Calculate power
power_analysis <- pwr.f2.test(u = 4,  # Number of predictors
                              v = 280,  # Degrees of freedom
                              f2 = 15.21,  # Convert effect size to f²
                              sig.level = 0.01)#


#print power analysis
print(power_analysis)



##################END OF CODE ##############################

