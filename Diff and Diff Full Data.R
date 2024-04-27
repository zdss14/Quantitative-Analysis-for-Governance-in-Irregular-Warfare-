library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
Badhaadhe_Afmadow_combined <- read_excel("Library/Mobile Documents/com~apple~CloudDocs/MMAS/Data Sets/Edited Data/Badhaadhe Afmadow combined.xlsx")

# Bound Dates
treatment_date <- as.Date("2022-07-09")
start_pre_period <- treatment_date %m-% months(12) 
end_post_period <- treatment_date %m+% months(12) 

#Filter
Badhaadhe_Afmadow_combined_processed <- Badhaadhe_Afmadow_combined %>%
  mutate(`Event Date` = as.Date(`Event Date`, format="%Y-%m-%d"),
         YearMonth = paste(year(`Event Date`), month(`Event Date`), sep = "-"),
         IsBattle = as.numeric(`Event Type` == "Battles"),
         IsAlShabaab = as.numeric(`Actor 1` == "Al Shabaab"),
         AfterTreatment = ifelse(`Event Date` >= treatment_date, 1, 0),
         Treated = ifelse(`admin 2` == "Afmadow", 1, 0)) %>%
  filter(IsBattle == 1, IsAlShabaab == 1,
         `Event Date` >= start_pre_period & `Event Date` <= end_post_period)

aggregate_data_monthly <- Badhaadhe_Afmadow_combined_processed %>%
  group_by(YearMonth, Treated, AfterTreatment) %>%
  summarise(BattleCount = n(), .groups = 'drop')

#DiD
didreg_monthly <- lm(BattleCount ~ Treated * AfterTreatment, data = aggregate_data_monthly)
summary(didreg_monthly)

#summary for the graph
summary_data <- aggregate_data_monthly %>%
  mutate(
    Period = ifelse(AfterTreatment == 0, "Before", "After"),
    Group = ifelse(Treated == 0, "Badhaadhe", "Afmadow (Dhobley)")
  ) %>%
  group_by(Period, Group) %>%
  summarise(AverageBattles = mean(BattleCount), .groups = 'drop')
summary_data$Period <- factor(summary_data$Period, levels = c("Before", "After"))

#Graph
ggplot(summary_data, aes(x = Group, y = AverageBattles, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  facet_wrap(~ Period, scales = "free_x", nrow = 1) +
  labs(title = "Comparison of Monthly Battles Involving Al Shabaab, 12 Months Before and After Hospital Donation",
       x = "",  
       y = "Average Number of Monthly Battles") +
  scale_fill_manual(values = c("Badhaadhe" = "skyblue", "Afmadow (Dhobley)" = "orange")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",  
        strip.background = element_rect(fill = "lightblue"),
        strip.text = element_text(face = "bold"))

# Tidy up the regression results
tidy_results <- tidy(didreg_monthly)

# Display the tidy results in the Viewer tab
htmlTable(tidy_results)