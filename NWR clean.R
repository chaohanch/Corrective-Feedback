# last updated on 9/1/2023

# set directory to the source file location

# load in library
library(tidyverse)
library(readxl)

setwd("/Volumes/GoogleDrive/Shared drives/Corrective feedback/Experiment results/NWR data/")

# get a list of files names for read in
file_list <- list.files(pattern = "*.csv", recursive = TRUE)
#  read in all files and save them in a list
myfiles <- lapply(file_list, read_csv)
# combine all files into one dataframe by row
df_nwr <- as.data.frame(do.call(rbind, myfiles))

# check the number of levels matches the subject info sheet
# levels(as.factor(df_nwr$participant)) # 48 subjects up to S14

# add subject info
df_subj_info <- read_excel("../../subject info.xlsx") %>%
  select(c(TrainingSession:ID_NWR))

# check the number of levels: 
# levels(as.factor(df_subj_info$ID_NWR)) # 48 subjects up to S14

# merge with NWR data
df <- df_nwr %>%
  inner_join(df_subj_info, by = c("participant" = "ID_NWR")) %>%
  # extract useful data
  filter(exp_types == "Test") %>%
  select(c(expName, TrainingSession, Date, ID, clicked_texts:correct_answer, stim_files))

# check the number of levels
# levels(as.factor(df$ID)) # 48 subjects up to S14

# extract matched groups from correct response
# Note:
# - string "\\\\\"(.*?)\\\\\"" matches to expression "\"(.*?)\""
# - the original df$correct_answer[2] is an expression already
correct_ans <- str_extract_all(df$correct_answer, "text\\d+")
# extract matched groups from clicked response
clicked <- str_extract_all(df$clicked_texts, "text\\d+")

# convert the lists to dataframe. Now one column corresponds to one row in the original dataset
correct_ans_df <- as.data.frame(sapply(correct_ans, function(x) x))
# x[1:8] makes sure filling in NA in case of fewer than 8 clicks
clicked_df <- as.data.frame(sapply(clicked, function(x) x[1:8]))
# combine the two dataframes into one
combined_df <- rbind(correct_ans_df, clicked_df)

# find the count of duplicated items in each column (which is the correct click)
n_correct <- apply(combined_df, 2, function(x) sum(duplicated(x)))

# add the count to the original dataset
df_ncorrect <- df %>%
  mutate(n_correct = n_correct) %>%
  # remove the redundant columns
  select(-c(clicked_texts, correct_answer, TrainingSession)) %>%
  mutate(trial = str_extract(stim_files, "\\d")) %>%
  # combine it with subject info
  inner_join(df_subj_info, by = c("Date", "ID")) %>%
  select(-c(stim_files, ID_NWR))

# export NWR data
write_csv(df_ncorrect, file = "../../Chao working folder/analysis/R output/NWR_accuracy.csv")
