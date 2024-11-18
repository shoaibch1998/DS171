data$password_strength <- cut(data$Time_to_crack_in_seconds,
                              breaks = c(-Inf, 1, 60, Inf),
                              labels = c("Weak", "Moderate", "Strong"))
