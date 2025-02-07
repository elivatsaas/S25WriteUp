library(tidyverse)

# Read in the data.  Do some cleaning/verification
tidy.test <- read.csv('data-raw/test.csv') %>%
  mutate(across(where(is.character), as.factor),
    # Handle ordinal variables
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
    # Convert other key categoricals
    Attrition = factor(Attrition, levels = c("Stayed", "Left")),
    Remote.Work = factor(Remote.Work)
  ) %>%
 select(-Employee.ID) %>%
  #Fix colnames
  rename_with(~gsub("\\.", " ", .))

# Save the data frame to the data/ directory as MaxTemp.rda
usethis::use_data(tidy.test, overwrite = TRUE)
