library(ggplot2)
data <- read.csv("D:\Msc Cyber security\Semester A\Team Research & Project development\Git\DS171\top_200_password_2020_by_country updated.csv")
filtered_data <- data[data$country %in% c("Australia", "Canada"), ]
# Categorize password strength based on our ranges
filtered_data$password_strength <- cut(filtered_data$Time_to_crack_in_seconds,
                                       breaks = c(-Inf, 60, 3600, 86400, Inf), # time intervals for Weak, Moderate, Strong, Very Strong
                                       labels = c("Weak", "Moderate", "Strong", "Very Strong"))
# Remove rows with missing country or Time_to_crack_in_seconds
filtered_data <- filtered_data %>%
  filter(!is.na(Time_to_crack_in_seconds) & !is.na(country))


# Calculate proportions
library(dplyr)
proportions <- filtered_data %>%
  group_by(country, password_strength) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(country) %>%
  mutate(proportion = count / sum(count))

# Enhanced bar plot with labels and custom color
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

