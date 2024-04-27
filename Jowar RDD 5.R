library(tidyverse)
library(lubridate)
library(broom)
library(readxl)

# Load the data
Jowhaar_full_Data <- read_excel("Library/Mobile Documents/com~apple~CloudDocs/MMAS/Data Sets/Jowhaar full Data.xlsx")

# Filter 
filtered_data <- Jowhaar_full_Data %>%
  filter(grepl("Al Shabaab", `Actor 1`), `Event Type` == "Battles") %>%
  mutate(Event_Date = as.Date(`Event Date`))

# Bin by month and count battles
monthly_battles <- filtered_data %>%
  mutate(YearMonth = floor_date(Event_Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Battles_Count = n())

# treatment date
treatment_date <- as.Date("2022-06-27")

# 12 months before and after the treatment
monthly_battles <- monthly_battles %>%
  mutate(Months_from_treatment = as.numeric(interval(treatment_date, YearMonth) / months(1)),
         Treatment = ifelse(YearMonth >= treatment_date, 1, 0)) %>%
  filter(abs(Months_from_treatment) <= 12)

# RDD
lm_rdd <- lm(Battles_Count ~ Months_from_treatment + Treatment, data = monthly_battles)

# Print 
cat("Regression Discontinuity Design Model Summary:\n")
print(summary(lm_rdd))

# Predict values for Graph
monthly_battles$Predicted <- predict(lm_rdd, newdata = monthly_battles)

# Labels
monthly_battles$Segment <- ifelse(monthly_battles$Treatment == 1, "Post-Handover", "Pre-Handover")

# Graph
ggplot(monthly_battles, aes(x = Months_from_treatment, y = Battles_Count, color = Segment)) +
  geom_point() +
  geom_line(aes(y = Predicted, group = Segment), linetype = "solid") +
  labs(title = "RDD of Jowar Police Station Handover", x = "Months from Police Station Handover", y = "Number of Battles", color = "Segment") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal()

# Tidy up the regression results
tidy_results <- tidy(lm_rdd)

# Display the tidy results in the Viewer tab
htmlTable(tidy_results)