library(tidyverse)

# Read data.  Do cleaning
tidy.train <- read.csv('data-raw/train.csv') %>%
  # First do base factor conversions
  mutate(across(where(is.character), as.factor),
  # Then order certain factors
    Education.Level = factor(Education.Level,
                             levels = c("High School", "Associate Degree",
                                        "Bachelor’s Degree", "Master’s Degree", "PhD"),
                             ordered = TRUE),
    Work.Life.Balance = factor(Work.Life.Balance,
                               levels = c("Poor", "Fair", "Good", "Excellent"),
                               ordered = TRUE),
    Job.Satisfaction = factor(Job.Satisfaction,
                              levels = c("Low", "Medium", "High", "Very High"),
                              ordered = TRUE),
    Job.Level = factor(Job.Level,
                       levels = c("Entry", "Mid", "Senior"),
                       ordered = TRUE),
    Company.Reputation = factor(Company.Reputation,
                                levels = c("Poor", "Fair", "Good", "Excellent"),
                                ordered = TRUE),
    Employee.Recognition = factor(Employee.Recognition,
                                  levels = c("Low", "Medium", "High", "Very High"),
                                  ordered = TRUE),
    Number.of.Promotions = factor(Number.of.Promotions,
                                  levels = 0:4,
                                  ordered = TRUE),
    Number.of.Dependents = factor(Number.of.Dependents,
                                  levels = 0:6,
                                  ordered = TRUE),
    # Ensure conversion of key categoricals
    Attrition = factor(Attrition, levels = c("Left", "Stayed")),
    Remote.Work = factor(Remote.Work)
  ) %>%
  # Remove ID upfront, not necessary
  select(-Employee.ID) %>%
  #Fix colnames
  rename_with(~gsub("\\.", " ", .))


# Save the data frame to the data/ directory as MaxTemp.rda
usethis::use_data(tidy.train, overwrite = TRUE)
