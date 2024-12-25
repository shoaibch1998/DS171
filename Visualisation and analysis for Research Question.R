library(ggplot2)
data <- read.csv("C:/Users/Nabeel Ahmed/Desktop/TR&DP/DS171/top_200_password_2020_by_country updated.csv")
filtered_data <- data[data$country %in% c("Australia", "Canada"), ]
# Categorize password strength based on our ranges
filtered_data$password_strength <- cut(filtered_data$Time_to_crack_in_seconds,
                                       breaks = c(-Inf, 60, 3600, 86400, Inf), # time intervals for Weak, Moderate, Strong, Very Strong
                                       labels = c("Weak", "Moderate", "Strong", "Very Strong"))
# Combine 'Strong' and 'Very Strong' into 'Strongest'
filtered_data$password_strength <- ifelse(filtered_data$password_strength %in% c("Strong", "Very Strong"),
                                          "Strongest", 
                                          as.character(filtered_data$password_strength))
# Set desired order of password strength categories
filtered_data$password_strength <- factor(filtered_data$password_strength, 
                                          levels = c("Weak", "Moderate", "Strongest"))
# Remove rows with missing country or Time_to_crack_in_seconds
filtered_data <- filtered_data %>%
  filter(!is.na(Time_to_crack_in_seconds) & !is.na(country))

install.packages("dplyr")

# Save the filtered and updated dataset to a new file
 write.csv(filtered_data, "filtered_dataset.csv", row.names = FALSE)

 # Filter the dataset for Australia and Canada
 filtered_data <- data[data$country %in% c("Australia", "Canada"), ]
 
 # Categorize password strength based on time to crack in seconds
 filtered_data$password_strength <- cut(
   filtered_data$Time_to_crack_in_seconds,
   breaks = c(-Inf, 60, 3600, 86400, Inf), # Define intervals
   labels = c("Weak", "Moderate", "Strong", "Very Strong") # Assign category labels
 )

 library(ggplot2)
 ggplot(proportions, aes(x = password_strength, y = proportion, fill = country)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(title = "Proportion of Password Strength by Country", 
        x = "Password Strength", 
        y = "Proportion") +
   theme_minimal()
 

# Enhanced bar plot with labels and custom colors
plot <- ggplot(proportions, aes(x = password_strength, y = proportion, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(proportion)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5) + # Add text labels for proportions above bars
  labs(title = "Proportion of Password Strength Categories in Australia and Canada",
       x = "Password Strength Category",
       y = "Proportion") +
  scale_fill_manual(values = c("Australia" = "blue", "Canada" = "red")) + # Custom colors
  theme_minimal()

# Display plot
print(plot)
# Create a contingency table for Chi-square test
contingency_table <- table(filtered_data$password_strength, filtered_data$country)
print(contingency_table)

# Create contingency table
contingency_table <- table(filtered_data$country, filtered_data$password_strength)

# Perform Chi-square test
chi_test <- chisq.test(contingency_table)

# Print the results
print(chi_test)

