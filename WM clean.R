# last updated on 9/1/2023

# clean the WM data, export YN accuracy and recall data for manual coding
# load in library
library(tidyverse)
library(readxl)

setwd("/Volumes/GoogleDrive/Shared drives/Corrective feedback/Experiment results/WM data/")

# get a list of files names for read in
file_list <- list.files(pattern = "*.csv", recursive = TRUE)

#  read in all files and save them in a list
myfiles <- lapply(file_list, read_csv)

# combine all files into one dataframe by row
df_wm <- as.data.frame(do.call(rbind, myfiles))

# check the number of levels matches the subject info sheet
# levels(as.factor(df_wm$participant)) # 52 subjects up to S14

# read in subject info
df_subj_info <- read_excel("../../subject info.xlsx") %>%
  select(c(TrainingSession:ID_NWR))

# check the number of levels: 
# levels(as.factor(df_subj_info$ID_WM)) # 52 subjects up to S14

# merge with WM data
df_wm_merged <- df_wm %>%
  inner_join(df_subj_info, by = c("participant" = "ID_WM"))

# check the number of levels: 
# levels(as.factor(df_wm_merged$participant)) # 52 subjects up to S14

#%%%%%%%%%%%%%%%%%%%%%%
#%% extract Y_N data %%
#%%%%%%%%%%%%%%%%%%%%%%
df_wm_yn <- df_wm_merged %>%
  filter(block_type == "Exp" & question_type == "Y_N") %>%
  select(c(expName, TrainingSession, Date, ID, keyY_N.corr))

# calculate the accuracy
df_wm_yn_summary <- df_wm_yn %>%
  group_by(ID) %>%
  summarize(YN_acc = mean(keyY_N.corr))


# export data
write_csv(df_wm_yn_summary, file = "../../Chao working folder/analysis/R output/WM_YN_acc_summary.csv")

#%%%%%%%%%%%%%%%%%%%%%%
#%% extract recall data
#%%%%%%%%%%%%%%%%%%%%%%
df_wm_recall <- df_wm_merged %>%
  filter(block_type == "Exp" & question_type == "recall") %>%
  select(c(expName, TrainingSession, Date, ID, textbox.text, total_sentence)) %>%
  # add item info
  group_by(ID, total_sentence) %>%
  mutate(item = paste0(total_sentence, ".", row_number()))

# read in experiment stimulus
df_stim_0 <- read_csv("../../Chao working folder/analysis/R input/WM STIMLIST.csv")
df_stim <- df_stim_0 %>%
  # extract correct answers
  mutate(strings = str_match(Sentence, "\\d([:alpha:].*?)\\.")[,2],
         Ngroup = as.character(Ngroup))

# group answers by sentence group
df_stim_summary <- df_stim %>%
  group_by(Ngroup) %>%
  summarize(correct_answers = paste(strings, collapse = ", "))

# merge the correct answers into the recall data
df_wm_recall_merged <- df_wm_recall %>%
  inner_join(df_stim_summary, by = c("item" = "Ngroup")) %>%
  # rearrange columns for manual coding
  relocate(textbox.text, .after = correct_answers) %>%
  # add 3 columns with empty values
  mutate(n_correctRecall = NA,
         note = NA,
         exclude = NA)

# export data (for manual coding)
write_csv(df_wm_recall_merged, file = "../../Chao working folder/analysis/R output/WM_recall_for_coding.csv")
