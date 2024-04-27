library(readxl)
library(sandwich)
library(dplyr)
library(lmtest)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(viridis)
Bilis_Qooqaani_Data <- read_excel("Library/Mobile Documents/com~apple~CloudDocs/MMAS/Data Sets/Bilis Qooqaani Data.xlsx")

# Filter
Bilis_Qooqaani_Data <- Bilis_Qooqaani_Data %>%
  mutate(`Event Date` = as.Date(`Event Date`, format="%Y-%m-%d"),
         IsBattle = as.numeric(`Event Type` == "Battles"),
         IsAlShabaab = as.numeric(`Actor 1` == "Al Shabaab"),
         MonthsSinceTreatment = floor(as.numeric(interval(treatment_date, `Event Date`)/months(1)))) %>%
  filter(IsBattle == 1, IsAlShabaab == 1, MonthsSinceTreatment >= -12, MonthsSinceTreatment <= 0)
monthly_battle_counts <- Bilis_Qooqaani_Data %>%
  group_by(MonthsSinceTreatment) %>%
  summarise(Count = n(), .groups = 'drop')

# Regression
model <- lm(Count ~ MonthsSinceTreatment, data=monthly_battle_counts)

# Robust
robust_summary <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
print(robust_summary)

# Graph
ggplot(monthly_battle_counts, aes(x = MonthsSinceTreatment, y = Count)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "purple") +
  labs(title = "Change in Number of Battles Initiated by Al Shabaab 12 Months Prior",
       x = "Months Before School Handover",
       y = "Number of Battles") +
  theme_clean() +  
  scale_color_viridis(discrete = TRUE)  

# coefficent table
tidy_results <- tidy(robust_summary)

# Display the tidy results in the Viewer tab
htmlTable(tidy_results)
