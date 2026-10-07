suppressPackageStartupMessages({
library(MASS)
  library(tidyverse)
  library(readxl)
  library(shiny)
  library(knitr)
  library(ggsci)
  library(lme4)
  library(pROC)
  library(pscl)
  library(performance)
  library(rcompanion)
  library(kableExtra)
  library(cowplot)
  library(ggpubr)
  library(stargazer)
})
### Reading and cleaning the data
ipe_data <- read_xlsx("Full Data iPE.Standards Prioritization.UNH.xlsx")
ipe_data$School_Label[1470] <- 9 #fixing a typo in the original spreadsheet

theme_update(plot.title = element_text(hjust = 0.5), text = element_text(family = "serif"))

### Re-coding a bunch of variables
ipe_data <- ipe_data %>%
  mutate(
    poverty = factor(case_when(
      `School FRL` == 2 ~ "mid-high poverty",
      `School FRL` == 1 ~ "mid-low poverty",
      `School FRL` == 0 ~ "low poverty"
      ), 
      levels = c(
        "low poverty", "mid-low poverty", "mid-high poverty"
        )
      ),
    race_2 = case_when(
      Race == 4 ~ "white",
      Race == 3 ~ "other",
      Race == 2 ~ "black",
      Race == 1 ~ "other",
      Race == 0 ~ "other",
      is.na(Race) ~ "unknown"
    ),
    fitness = factor(case_when(
      Strength_Days >= 3 & `PA Minutes per day` == 3 ~ "high fitness",
      Strength_Days < 2 & `PA Minutes per day` == 0 ~ "low fitness",
      TRUE ~ "medium fitness"
     ), levels = c("high fitness", "medium fitness", "low fitness")
    ),
    weight_status2 = factor(
      case_when(
        Weight_Status == 0 ~ "very underweight",
        Weight_Status == 1 ~ "slightly underweight",
        Weight_Status == 2 ~ "about the right weight",
        Weight_Status == 3 ~ "slightly overweight",
        Weight_Status == 4 ~ "very overweight"
      ), 
      levels = c("very underweight", "slightly underweight", "about the right weight",
                 "slightly overweight", "very overweight")
    ),
    weight_action2 = factor(
      case_when(
        Weight_Action == 0 ~ "lose weight",
        Weight_Action == 1 ~ "gain weight",
        Weight_Action == 2 ~ "stay the same weight",
        Weight_Action == 3 ~ "no action"
      ),
      levels = c("lose weight", "gain weight", "stay the same weight", 
                 "no action")
    ),
    tv2 = factor(
      case_when(
        TV == 0 ~ "No TV",
        TV == 1 ~ "< 1hr",
        TV == 2 ~ "1 hour",
        TV == 3 ~ "2 hours",
        TV == 4 ~ "3 hours",
        TV == 5 ~ "4 hours",
        TV == 6 ~ "5+ hours"
      ),
      levels = c("No TV", "< 1hr", "1 hour", "2 hours", "3 hours", "4 hours", "5+ hours")
    ),
    gaming_hours = factor(
      case_when( 
        Video_Computer_Hours == 0 ~ "No gaming",
        Video_Computer_Hours == 1 ~ "< 1hr",
        Video_Computer_Hours == 2 ~ "1 hour",
        Video_Computer_Hours == 3 ~ "2 hours",
        Video_Computer_Hours == 4 ~ "3 hours",
        Video_Computer_Hours == 5 ~ "4 hours",
        Video_Computer_Hours == 6 ~ "5+ hours"
      ),
      levels = c("No gaming", "< 1hr", "1 hour", "2 hours", "3 hours", "4 hours", "5+ hours")
    ),
    active_daily = case_when(
      `PA Minutes per day` == "0" ~ 0,
      `PA Minutes per day` == "1" ~ 1,
      `PA Minutes per day` == "2" ~ 1,
      `PA Minutes per day` == "3" ~ 1,
      `PA Minutes per day` == "-" ~ NA,
    ),
    meets_standards = case_when(
      fitness == "high fitness" ~ 1,
      TRUE ~ 0
    ),
    `PA Minutes per day` = as.numeric(ifelse(
      `PA Minutes per day`=="-", NA, `PA Minutes per day`
      )
    )
  ) %>%
  relocate(poverty, race_2, .after = `School FRL`)

### Calculating scores by standard, then standardizing
ipe_data <- ipe_data %>%
  mutate(s2_total = rowSums(across(S2_PT_1:S2_PT_25)),
         s2_pct = s2_total/21,
         s3_total = rowSums(across(S3_PT_26:S3_PT_29)),
         s3_pct = s3_total/4,
         s4_total = rowSums(across(S4_PT_7:S4_PT_39)),
         s4_pct = s4_total/14
         ) %>%
  mutate(overall_pct_std = scale(PT_Percentage),
         s2_pct_std = scale(s2_pct),
         s3_pct_std = scale(s3_pct),
         s4_pct_std = scale(s4_pct))

### Changing some variable types
ipe_data$Teacher_Label <- as.character(ipe_data$Teacher_Label)
ipe_data$School_Label <- as.character(ipe_data$School_Label)
ipe_data$PA_Days <- as.numeric(ipe_data$PA_Days)

ipe_data$`PA Minutes per day` <- factor(
  ipe_data$`PA Minutes per day`,
  levels = c("-", "0", "1", "2", "3"),
  labels = c(NA, "0–20", "20–40", "40–60", "60+")
)
