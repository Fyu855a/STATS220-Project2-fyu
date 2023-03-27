library(tidyverse)

# Read data from URL into data frame
learning_data <- read_csv("https://raw.githubusercontent.com/Fyu855a/STATS220-Project2-fyu/main/How%20do%20you%20learn%20about%20ChatGPT.csv")

# Rename columns
learning_data <- learning_data %>%
  rename(first_learn = `How did you first learn about ChatGPT?`,
         used_before = `Have you ever used ChatGPT before?`,
         used_for = `What do you use ChatGPT for? (Select all that apply)`,
         ease_of_use = `On a scale of 1-5, how easy is it to use ChatGPT?`,
         helpfulness = `On a scale of 1-5, how helpful do you find ChatGPT?`,
         issues = `Have you ever encountered any issues or problems while using ChatGPT? (Select all that apply)`)

# Calculate percentage for each category
learning_data_pct <- learning_data %>%
  group_by(first_learn) %>%
  summarize(n = n()) %>%
  mutate(percentage = n / sum(n))

# Generate bar chart of first learning source with rotated text on x-axis
ggplot(learning_data_pct, aes(x = first_learn, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "How People First Learned About ChatGPT",
       x = NULL,
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_y_continuous(labels = scales::percent)

# Calculate percentage for ChatGPT usage
learning_data_usage_pct <- learning_data %>%
  group_by(used_before) %>%
  summarize(n = n()) %>%
  mutate(percentage = n / sum(n))

# Generate bar chart of ChatGPT usage with rotated text on x-axis
ggplot(learning_data_usage_pct, aes(x = fct_recode(used_before, "Rarely" = "Yes, rarely", "Frequently" = "Yes, frequently"), y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "ChatGPT Usage Frequency",
       x = NULL,
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_y_continuous(labels = scales::percent)


# Extract numeric values from ease of use and helpfulness columns
learning_data$ease_of_use <- as.numeric(gsub("\\D", "", learning_data$ease_of_use))
learning_data$helpfulness <- as.numeric(gsub("\\D", "", learning_data$helpfulness))

mean_ease_of_use <- mean(learning_data$ease_of_use, na.rm = TRUE)
median_helpfulness <- median(learning_data$helpfulness)