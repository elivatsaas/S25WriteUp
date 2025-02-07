---
title: "Spring 2025 Writing Sample"
author: "Eli Vatsaas"
date: "2025-02-05"
always_allow_html: true
output:
  pdf_document:
    fig_caption: true
    toc: true
    toc_depth: 3
    number_sections: true
    latex_engine: xelatex
  github_document:
  word_document:
    toc: true
    toc_depth: '3'
header-includes:
- \usepackage{booktabs}
- \usepackage{float}
- \usepackage{ragged2e}
- \usepackage{xcolor}
- \usepackage[utf8]{inputenc}
- \usepackage{textgreek}
- \setcounter{secnumdepth}{3}
---

```{r global-options, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  fig.pos = "H",
  out.width = "90%",
  cache = FALSE
)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(scales)
library(S25WorkSample)
library(knitr)
library(dplyr)
library(stringr)
library(gridExtra)  
library(rcompanion) 
library(patchwork)
library(kableExtra)
```

```{r data-load, echo=FALSE}
# Data preparation
data("tidy.train")
```
# Introduction
  I chose this data that looks at whether an employee stayed at or left a company based on a variety of factors. I chose this to broaden my portfolio while still doing work I find interesting, like looking at why people burnout. I hoped to learn what caused employees to leave companies. Although this dataset was synthetic, it was a worthwhile learning experience. This dataset was obtained from Kaggle, at www.kaggle.com/datasets/stealthtechnologies/employee-attrition-dataset/. 

# Data Cleaning

  Since the data was synthetic, there was minimal cleaning to be done. I turned my character variables to factors, and ordered the ones where ordering mattered. I removed the employee id as it was not needed for this work. I finally removed all periods separating words in columns while adding spaces. This will allow for better visualization. For modeling one may want to turn these factors into dummy variables, and standardize the numerical data. Some of the key variables in the data set are **Attrition:** Whether an employee stayed at or left their company. *Factor (Stayed/Left).* **Job Level:** An employee's position importance in their company. *Factor (Entry/Mid/Senior).* **Age:** Age of employee in years. *Integer (Range:*`r min(tidy.train$Age)` *to* `r max(tidy.train$Age)`*).* As seen in the table below, there are no missing values, confirming tidy data. 

\newpage
```{r summary-table, fig.show='hold', out.width='100%',  fig.align='center', fig.pos='H', echo=FALSE}
# Create summary dataframe
variable_summary <- data.frame(
  Variable = names(tidy.train),
  Type = sapply(tidy.train, function(x) if(length(class(x)) > 1) class(x)[2] else class(x)),
  Unique_Values <- sapply(tidy.train, function(x) {
    if(is.numeric(x)) {
      y <- range(x)
      paste(y[1], "to", y[2])
    } else {
      y <- levels(x)
      paste(y, collapse = ", ")
    }
  }),
 Distinct_Count = sapply(tidy.train, function(x) length(unique(x))),
 Missing_Count = sapply(tidy.train, function(x) sum(is.na(x)))
)

# Format the table using kable
kable(variable_summary, 
      col.names = c("Variable", "Type", "Unique Values/Range", "# Distinct", "# Missing"),
      align = c('l', 'l', 'l', 'r', 'r'),
      caption = "Summary of Variables in tidy.train Dataset",
      linesep = "\n",
      longtable = TRUE,
      booktabs = TRUE,
      escape = TRUE,
      row.names = FALSE) %>%
  kable_styling(
    latex_options = c("striped", "repeat_header"),
    font_size = 11
  ) %>%
  column_spec(1, width = "4cm") %>%
  column_spec(c(2,4,5), width = "1.5cm") %>%
  column_spec(3, width = "4.5cm") %>%
  footnote(
    general = paste(
    "This table shows us important information about the data. It shows the",
    "variable name followed by the data type that variable is. It then shows either the",
    "unique variables from that data (factor levels) or the range of the numerical variables.",
    "The second to last column shows the number of distinct values in the variable. The last",
    "column confirms the tidy data, showing no missing values. This table shows the data is",
    "ready for visualization without having to worry about handling NA's."),
   threeparttable = TRUE,
   general_title = ""
  )
```

\newpage
```{r cat-grab, echo=FALSE}
# Choose categorical variables
categorical_vars <- c("Marital Status", "Job Level", "Remote Work")

#Pivot data for easier visualizations, unorder factors to avoid errors
df_long <- tidy.train %>%  
  rename_with(~gsub("\\.", " ", .)) %>%
  # Convert all factor columns to character before pivoting
  mutate(across(all_of(categorical_vars), as.character)) %>%
  select(Attrition, all_of(categorical_vars)) %>%
  pivot_longer(
    cols = all_of(categorical_vars),
    names_to = "Variable",
    values_to = "Value"
  )
```
```{r chi_sq_test, echo = FALSE}
#Calculate and store Chi Square test results
chi_results <- list(
  marital = chisq.test(table(tidy.train$Attrition, tidy.train$`Marital Status`)),
  joblevel = chisq.test(table(tidy.train$Attrition, tidy.train$`Job Level`)),
  remote = chisq.test(table(tidy.train$Attrition, tidy.train$`Remote Work`))
)

# Save values for caption
chi2.mar <- round(chi_results$marital$statistic, 1)
chi2.job <- round(chi_results$joblevel$statistic, 1)
chi2.rw <- round(chi_results$remote$statistic, 1)
chip.mar <- format.pval(chi_results$marital$p.value, digits = 1)
chip.job <- format.pval(chi_results$joblevel$p.value, digits = 1)
chip.rw <- format.pval(chi_results$remote$p.value, digits = 1)
```
\newpage
```{r first-graph, echo=FALSE, fig.height=30, fig.width=22}
# Create a summary data frame for each variable
counts_label <- df_long %>%
  group_by(Variable, Value, Attrition) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Variable, Value) %>%
  summarise(
    Total = sum(n),
    Left = n[Attrition == "Left"],
    Stayed = n[Attrition == "Stayed"],
    .groups = "drop"
  ) %>%
  #add label for later
  mutate(Label = glue::glue(
    "n = {comma(Total)}<br>
    <span style='color:#66C2A5;'>{comma(Left)}</span> | 
    <span style='color:#FC8D62;'>{comma(Stayed)}</span> 
    <br><br>"
  ))

#Reorder factor for graph
df_long <- df_long %>%
  mutate(
    Value = if_else(
      Variable == "Marital Status",
      factor(Value, levels = c("Single", "Married", "Divorced")),
      factor(Value)
    )
  )

#prepare plot caption
plot_caption <- paste(
  "Statistical Significance:",
  sprintf("Job Level: X² = %.2f, p %s", chi2.job, chip.job),
  sprintf("Marital Status: X² = %.2f, p %s", chi2.mar, chip.mar),
  sprintf("Remote Work: X² = %.2f, p %s", chi2.rw, chip.rw),
  "Note: Chi-square tests indicate significant differences (critical value = 0.05)",
  sep = "\n"
)

ggplot(df_long, aes(x = Value, fill = Attrition)) +
  geom_bar(position = position_fill(reverse = TRUE), width = 0.85) +
  ggtext::geom_richtext(
    data = counts_label,
    aes(x = Value, y = -0.07, label = Label),
    fill = NA, 
    label.color = NA,
    hjust = 0.5, 
    angle = 45, 
    size = 9,
  ) +
  facet_wrap(~Variable, scales = "free_x", ncol = 3) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Attrition Patterns Across Key Employee Characteristics",
    subtitle = "Proportional distribution of employees who stayed vs. left their company",
    y = "Percentage of Employees",
    x = NULL,
    caption = plot_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 45, hjust = 0.5),
    plot.subtitle = element_text(size = 28, hjust = 0.5, margin = margin(b = 20)),
    
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1, 
      vjust = 0.4,
      size = 30,
      face = "bold"
    ),

    axis.title.y = element_text(margin = margin(r = 15), size = 30),
      axis.text.y = element_text(
      size = 30
    ),
    plot.caption = element_text(
      vjust = 0,
      hjust = 0.5, 
      size = 40, 
      lineheight = 1.5,
      margin = margin(t = 10)
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 30),
    legend.box.margin	= margin(t = 50),
    strip.text = element_text(
      face = "bold", 
      size = 25,
      margin = margin(b = 10)
    ),
    panel.spacing = unit(1.5, "lines")
  ) +
  scale_fill_manual(
    values = c("Stayed" = "#FC8D62", "Left" = "#66C2A5"),
    labels = c("Remained with Company", "Left Company")
  )
```
\newpage
```{r faceted_plot, echo=FALSE, fig.height=20, fig.width=12}
# Choose numeric variables
numeric_vars <- c("Age", "Distance from Home")

# Reshape data for faceting
df_long <- tidy.train %>%
  dplyr::select(all_of(numeric_vars), Attrition) %>%
  pivot_longer(cols = -Attrition,
               names_to = "Variable",
               values_to = "Value")

# Create a labeller function for custom x-axis titles
custom_x_labeller <- as_labeller(c(
  "Age" = "Age (Years)",
  "Distance from Home" = "Distance from Home (Miles)"
))
df_long$Attrition <- factor(df_long$Attrition, levels = c("Left", "Stayed"))
# Create faceted plot with custom x-axis titles
ggplot(df_long) +
  geom_density(aes(x = Value, fill = Attrition), alpha = 0.5, position = "identity") +
  facet_wrap(~ Variable, scales = "free", ncol = 1, labeller = custom_x_labeller) +  # Custom labeller
  labs(title = "Distribution of Numeric Variables by Attrition Status",
       x = "",
       y = "Density") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 20),  # Style for facet labels
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Set2",
                    labels = c( "Left Company","Remained with Company")
)
```
\newpage
# Data Discussion
  The first of the two graphs show the relationship between attrition and a few key categorical variables. Across the three variables, **Job Level, Marital Status, and Remote Work,** we see patterns that give us information about whether employees may stay. In **Job Role**, we see that *Entry* employees are likely to leave and *Senior* are likely to stay. In **Marital Status** we see that *Single* employees are likely to leave and *Married* employees may be likely to stay. In **Remote Work** we can infer remote employees are likely to stay and in office employees are more likely to leave. We can confirm these by locking at our Chi Square test standard residuals: 
```{r chi-table, echo=FALSE}
# Create a data frame of the residuals
residuals_df <- data.frame(
  Group = c(
    "Entry", "Mid", "Senior",
    "Divorced", "Married", "Single",
    "No", "Yes"
  ),
  Stayed = c(
    chi_results$joblevel$stdres["Stayed", c("Entry", "Mid", "Senior")],
    chi_results$marital$stdres["Stayed", c("Divorced", "Married", "Single")],
    chi_results$remote$stdres["Stayed", c("No", "Yes")]
  ),
  Left = c(
    chi_results$joblevel$stdres["Left", c("Entry", "Mid", "Senior")],
    chi_results$marital$stdres["Left", c("Divorced", "Married", "Single")],
    chi_results$remote$stdres["Left", c("No", "Yes")]
  )
)

# Format the table using kable
chi_kable <- kable(residuals_df,
      caption = "Standardized Residuals from Chi-Square Tests",
      col.names = c("Group", "Stayed", "Left"),
      digits = 2,
      row.names = FALSE, 
      align = c('l', 'l', 'r', 'r')) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE) %>%
  pack_rows("Job Level", 1, 3) %>%
  pack_rows("Marital Status", 4, 6) %>%
  pack_rows("Remote Work", 7, 8) %>%
  add_footnote("Note: Values > |1.96| indicate significant association at p < 0.05",
              notation = "none")
```
`r chi_kable`
  From these results, we confirm what was seen in the visualization. The weakeast assocation is *Mid* level employees at `r chi_results$joblevel$stdres[1,"Mid"]` and *Divorced* employees at `r chi_results$marital$stdres[1,"Divorced"]`. The rest of the grouping having strong associations with **Attrition**, the strongest being *Single* employees association with leaving. This is `r chi_results$marital$stdres[2,"Single"]`, confirming what was found in the visualizaiton. 
```{r age-anova} 
sum <- summary(aov(Age ~ Attrition, data = tidy.train))
f.test <- sum[[1]][["F value"]][1]
p.val <- sum[[1]][["Pr(>F)"]][1]
```
  The final two graphs show the pattern in attrition in two key numeric variables. The top graph shows the pattern in attrition in age, showing interesting patterns. This graph suggests that young employees are much more likely to leave their jobs, where around age 30 this switches to the opposite. When we run a simple ANOVA to test this, we seem to confirm this. After running an ANOVA on **Age** and **Attrition**, we see an F test statistic of `r f.test` and p value of `r p.val`. These show strong evidence for age as a factor of **Attrition.** With such a small p-value, we can believe age is a strong factor in attrition, although we can do some more checks. When we look at the mean and standard deviation of age across each attrition group, we see something else.
```{r age-stats}
age.mean <- aggregate(Age ~ Attrition, data = tidy.train, mean)
age.sd <- aggregate(Age ~ Attrition, data = tidy.train, sd)
```
  The mean age for those that left the company was `r age.mean[2,2]`, while the mean age of those that stayed was `r age.mean[1,2]`. The corresponding standard deviations are `r age.sd[2,2]`, `r age.sd[1,2]`. This shows that the difference in age across these groups is small, with the means being just 1.1828 apart. The standard deviations are also both high around 12. This suggests that while age may be a factor in attrition, it is not the driving force, and there are other factors. We can do the same tests for the other visualized variable, distance from home, and fit an ANOVA.
```{r dfh-info}
#Do some workarounds cause r likes causing problems. 
tidy.train <- tidy.train %>%
  rename_with(~gsub(" ", "\\.", .))
dist.aov <- summary(aov(Distance.from.Home ~ Attrition, data = tidy.train))
f.test <- dist.aov[[1]][["F value"]][1]
p.val <- dist.aov[[1]][["Pr(>F)"]][1]
dfh.mean <- aggregate(Distance.from.Home ~ Attrition, data = tidy.train, mean)
dfh.sd <- aggregate(Distance.from.Home ~ Attrition, data = tidy.train, sd)
tidy.train <- tidy.train %>% 
  rename_with(~gsub("\\.", " ", .))
```
  With the ANOVA on **Distance From Home** and **Attrition**, we see an F test statistic of `r f.test` and p value of `r p.val`. Once again, we have a low p-value, suggesting that the distance an employee lives from the office is an important factor to whether they stay at their work. when looking at their mean and standard deviation, we see the mean and standard devaition for those who stayed at a company are mean = `r dfh.mean[1,2]` and sd = `r dfh.sd [1,2]`. For those that left the corresponding stats are mean = `r dfh.mean[2,2]` and sd = `r dfh.sd [2,2]`. The means show some distance, with the difference being `r dfh.mean[1,2] - dfh.mean[2,2]`, and the standard deviations hovering around `r mean(dfh.sd[,2])` This suggests a lot of variablity in this data, but some evidence that distance to home is important to employees. 


# Conclusion
  With some more time, I believe there would be several valuable analyses that could further enhance our understanding of employee attrition. A logistic regression model to predict **Attrition** could give more insight into these whether employees leave. Additionally, I think it would be interesting to explore what would motivate employees to stay, providing value for companies. Being particularly useful in retention strategies. 

  In previous projects, no single modeling method stood out as significantly superior, with accuracy rates ranging from $72\%$ to $75\%$. However, key predictors such as senior job status, single marital status, and remote work status consistently emerged as strong indicators of attrition. These findings align with the patterns observed in the EDA above, reinforcing their importance in understanding employee turnover.

  Another area providing promise is the interaction effects between variables. 
  
  Does the combination of remote work and distance from home have a stronger impact on attrition than either factor alone? 
    
  Does job satisfaction interact with leadership opportunities to influence employee retention? 
    
  Investigating these interactions could provide a stronger understanding of the data, though there could be some challenges. One of these challenges may be multicollinearity, which could be handled with VIF or regularization methods. Overall this dataset provides a lot of possibility for exploration and learning. Showing possible deep hidden relationships to be found. 

# Link to Github 
elivatsaas/S25WriteUp
