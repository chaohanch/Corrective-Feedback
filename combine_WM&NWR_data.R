# correlation analysis

# library
library(tidyverse)
library(lmerTest)
library(emmeans)
library(car)

# get WM number of correct recall data
df_wm_recall <- read_csv("R input/WM recall coded.csv") %>%
  group_by(TrainingSession, ID) %>%
  # sum N correct recall
  dplyr::summarize(WM_n_correctRecall = sum(n_correctRecall)) %>%
  ungroup()

# get NWR number of correct click data
df_nwr_click <- read_csv("R output/NWR_accuracy.csv") %>%
  group_by(TrainingSession, ID) %>%
  # sum N correct clicks
  dplyr::summarize(NWR_n_correctRecall = sum(n_correct)) %>%
  ungroup()

# get WM accuracy data
df_wm_YN <- read_csv("R output/WM_YN_acc_summary.csv") %>%
  rename(WM_YN_acc = YN_acc)

# combine three datasets together
df_exp <- df_wm_recall %>%
  inner_join(df_nwr_click) %>%
  inner_join(df_wm_YN)

# export the experiment data
write_csv(df_exp, file = "R output/experiment_data.csv")
