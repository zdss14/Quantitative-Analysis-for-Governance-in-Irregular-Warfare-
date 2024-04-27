library(readxl)
library(sandwich)
library(dplyr)
library(lmtest)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(viridis)
Jowhaar_full_Data <- read_excel("Library/Mobile Documents/com~apple~CloudDocs/MMAS/Data Sets/Jowhaar full Data.xlsx")

# treatment date
treatment_date <- as.Date("2022-03-27")

# Filter 
Jowhaar_full_Data <- Jowhaar_full_Data %>%
  filter(`Event Type` == "Battles", grepl("Al Shabaab", `Actor 1`)) %>%
  mutate(`Event Date` = as.Date(`Event Date`),
         # months since treatment
         MonthsSinceTreatment = as.integer(interval(treatment_date, `Event Date`)/months(1))) %>%
  filter(MonthsSinceTreatment >= 0, MonthsSinceTreatment <= 12)

# Bin by month and count battles
monthly_battle_counts <- Jowhaar_full_Data %>%
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
  labs(title = "Change in Number of Battles Initiated by Al Shabaab Over 12 Months",
       x = "Months Since Treatment",
       y = "Number of Battles") +
  theme_clean() +
  scale_color_viridis(discrete = TRUE)

# coefficent table
tidy_results <- tidy(robust_summary)

# Display the tidy results in the Viewer tab
htmlTable(tidy_results)