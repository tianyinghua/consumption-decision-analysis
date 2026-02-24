df$location <- factor(df$location,
                      levels = c("Mainland China",
                                 "Other Countries",
                                 "United States"))
library(ggplot2)

p1 <- ggplot(df, aes(x = location, y = spend_num, fill = location)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  labs(title = "Monthly Spending Distribution by Location",
       x = "Location",
       y = "Monthly Spending (RMB)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"))

print(p1)
p1 <- ggplot(df, aes(x = location, y = spend_num, fill = location)) +
  geom_boxplot(alpha = 0.8, width = 0.6) +
  scale_fill_manual(values = c("#4C72B0", "#55A868", "#C44E52")) +
  labs(title = "Monthly Spending Distribution by Location",
       x = "Location",
       y = "Monthly Spending (RMB)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"))

print(p1)

ggsave("spending_boxplot.png", 
       plot = p1,
       width = 7, 
       height = 5, 
       dpi = 300)
library(dplyr)
summary_df <- df %>%
  group_by(location) %>%
  summarise(
    mean_spend = mean(spend_num),
    sd_spend = sd(spend_num),
    n = n(),
    se_spend = sd_spend / sqrt(n)
  )

p2 <- ggplot(summary_df, aes(x = location, y = mean_spend, fill = location)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = mean_spend - se_spend,
                    ymax = mean_spend + se_spend),
                width = 0.2, linewidth = 0.8) +
  scale_fill_manual(values = c("#4C72B0", "#55A868", "#C44E52")) +
  labs(title = "Average Monthly Spending by Location",
       x = "Location",
       y = "Average Spending (RMB)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"))

print(p2)

ggsave("spending_mean_error.png",
       plot = p2,
       width = 7,
       height = 5,
       dpi = 300)

