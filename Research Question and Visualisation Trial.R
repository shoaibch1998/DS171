library(ggplot2)
data <- read.csv("D:/Team Research and Development Project/Assignment 1/top_200_password_2020_by_country.csv")
filtered_data <- data[data$country %in% c("Australia", "Canada"), ]
# Categorize password strength
filtered_data$password_strength <- cut(filtered_data$Time_to_crack_in_seconds,
                                       breaks = c(-Inf, 1, 60, Inf),
                                       labels = c("Weak", "Moderate", "Strong"))

# Calculate proportions
library(dplyr)
proportions <- filtered_data %>%
  group_by(country, password_strength) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(country) %>%
  mutate(proportion = count / sum(count))

# Create bar plot
plot <- ggplot(proportions, aes(x = password_strength, y = proportion, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Password Strength Categories in Australia and Canada",
       x = "Password Strength Category",
       y = "Proportion") +
  theme_minimal()
