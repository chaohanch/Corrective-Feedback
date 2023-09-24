# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### correlation analysis experiment ~ explicit knowledge ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# library
library(Hmisc)
library(tidyverse)
library(ggpubr)

# Set working directory to source file location
setwd("/Volumes/GoogleDrive/Shared drives/Corrective feedback/Chao working folder/analysis")

# source for dataset
source("prepare_data_for_modeling.R")
source("flattenCorrMatrix.R")
source("~/Documents/RData/ggplot/ggplot_theme_Publication.R")

# read in experiment data
df_exp <- read_csv("R output/experiment_data.csv")

# read in explicit knowledge
df_knowledge <- read_csv("R input/explicit knowledge.csv")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### put all data together ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# get pre score for each participant
df_subjpre <- df_diffmod %>%
  # get mean pre socre
  group_by(TrainingSession, ID) %>%
  dplyr::summarize(mean_pre = mean(pretest)) %>%
  ungroup()

# get mean diff score for each participant
df_subjdiff <- df_diffmod %>%
  # for recast and meta group only
  filter(Group %in% c("recast", "meta")) %>%
  droplevels() %>%
  # get mean diff socre
  group_by(TrainingSession, ID) %>%
  dplyr::summarize(mean_diff = mean(diff_score)) %>%
  ungroup()

# merge data
df_all4cor <- df_knowledge %>%
  select(c(TrainingSession, ID, labelTone_pre, pronounceTone_pre)) %>%
  # incorporate WM and NWR data
  full_join(df_exp) %>%
  # incorporate pre rating data
  full_join(df_subjpre) %>%
  # incorporate mean rating data
  full_join(df_subjdiff)

# identity outliers
out_thres = 3

df_all4cor_outlier <- df_all4cor %>%
  # identify WM outliers
  mutate(WM_up_thres = mean(WM_n_correctRecall, na.rm = TRUE) + out_thres*sd(WM_n_correctRecall, na.rm = TRUE),
         WM_low_thres = mean(WM_n_correctRecall, na.rm = TRUE) - out_thres*sd(WM_n_correctRecall, na.rm = TRUE),
         WM_outliers = WM_n_correctRecall > WM_up_thres | WM_n_correctRecall < WM_low_thres) %>%
  # identity NWR outliers
  mutate(NWR_up_thres = mean(NWR_n_correctRecall, na.rm = TRUE) + out_thres*sd(NWR_n_correctRecall, na.rm = TRUE),
         NWR_low_thres = mean(NWR_n_correctRecall, na.rm = TRUE) - out_thres*sd(NWR_n_correctRecall, na.rm = TRUE),
         NWR_outliers = NWR_n_correctRecall > NWR_up_thres | NWR_n_correctRecall < NWR_low_thres) %>%
  # identify mean_pre outliers
  mutate(ratepre_up_thres = mean(mean_pre, na.rm = TRUE) + out_thres*sd(mean_pre, na.rm = TRUE),
         ratepre_low_thres = mean(mean_pre, na.rm = TRUE) - out_thres*sd(mean_pre, na.rm = TRUE),
         ratepre_outliers = mean_pre > ratepre_up_thres | mean_pre < ratepre_low_thres) %>%
  # identify mean_diff outliers
  mutate(rating_diff_upThres = mean(mean_diff, na.rm = TRUE) + out_thres*sd(mean_diff, na.rm = TRUE),
         rating_diff_lowThres = mean(mean_diff, na.rm = TRUE) - out_thres*sd(mean_diff, na.rm = TRUE),
         rating_diff_outliers = mean_diff > rating_diff_upThres | mean_diff < rating_diff_lowThres) %>%
  # identity labelTone_pre outliers
  mutate(labelpre_up_thres = mean(labelTone_pre, na.rm = TRUE) + out_thres*sd(labelTone_pre, na.rm = TRUE),
         labelpre_low_thres = mean(labelTone_pre, na.rm = TRUE) - out_thres*sd(labelTone_pre, na.rm = TRUE),
         labelpre_outliers = labelTone_pre > labelpre_up_thres | labelTone_pre < labelpre_low_thres) %>%
  # identity pronounceTone_pre outliers
  mutate(pronouncepre_up_thres = mean(pronounceTone_pre, na.rm = TRUE) + out_thres*sd(pronounceTone_pre, na.rm = TRUE),
         pronouncepre_low_thres = mean(pronounceTone_pre, na.rm = TRUE) - out_thres*sd(pronounceTone_pre, na.rm = TRUE),
         pronouncepre_outliers = pronounceTone_pre > pronouncepre_up_thres | pronounceTone_pre < pronouncepre_low_thres)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### correlation between WM and NWR ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# correction before removing outliers
df_cor <- df_all4cor %>%
  select(TrainingSession:ID, WM_n_correctRecall, NWR_n_correctRecall)

cor.test(df_cor$WM_n_correctRecall, df_cor$NWR_n_correctRecall)
# t = 2.178, df = 46, p-value = 0.03457

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = NWR_n_correctRecall)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "NWR score") # y-axis label

# correction after removing outliers
df_cor <- df_all4cor_outlier %>%
  select(TrainingSession:ID, WM_n_correctRecall, NWR_n_correctRecall,
         WM_outliers, NWR_outliers) %>%
  # remove outlier
  filter(WM_outliers != TRUE & NWR_outliers != TRUE)

cor.test(df_cor$WM_n_correctRecall, df_cor$NWR_n_correctRecall)
# t = 1.5337, df = 44, p-value = 0.1323

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = NWR_n_correctRecall)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "NWR score") # y-axis label

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# correlation between pre-test score and WM ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# correction before removing outliers
df_cor <- df_all4cor %>%
  select(TrainingSession:ID, WM_n_correctRecall, mean_pre)

cor.test(df_cor$WM_n_correctRecall, df_cor$mean_pre)
# t = 2.3919, df = 35, p-value = 0.02226

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = mean_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "pretest rating") # y-axis label

# correction after removing outliers
df_cor <- df_all4cor_outlier %>%
  select(TrainingSession:ID, WM_n_correctRecall, mean_pre,
         WM_outliers, ratepre_outliers) %>%
  # remove outlier
  filter(WM_outliers != TRUE & ratepre_outliers != TRUE)

cor.test(df_cor$WM_n_correctRecall, df_cor$mean_pre)
# t = 1.7813, df = 33, p-value = 0.08407

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = mean_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "pretest rating") # y-axis label


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# correlation between pre-test score and NWR ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# correction before removing outliers
df_cor <- df_all4cor %>%
  select(TrainingSession:ID, NWR_n_correctRecall, mean_pre)

cor.test(df_cor$NWR_n_correctRecall, df_cor$mean_pre)
# t = 1.6809, df = 35, p-value = 0.1017

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = NWR_n_correctRecall, y = mean_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "NWR score", # x-axis label
       y = "pretest rating") # y-axis label

# correction after removing outliers
df_cor <- df_all4cor_outlier %>%
  select(TrainingSession:ID, NWR_n_correctRecall, mean_pre,
         NWR_outliers, ratepre_outliers) %>%
  # remove outlier
  filter(NWR_outliers != TRUE & ratepre_outliers != TRUE)

cor.test(df_cor$NWR_n_correctRecall, df_cor$mean_pre)
# t = 1.5929, df = 33, p-value = 0.1207

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = NWR_n_correctRecall, y = mean_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "NWR score", # x-axis label
       y = "pretest rating") # y-axis label

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# correlation between WM and pre-tone-labeling ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# correction before removing outliers
df_cor <- df_all4cor %>%
  select(TrainingSession:ID, WM_n_correctRecall, labelTone_pre)

cor.test(df_cor$WM_n_correctRecall, df_cor$labelTone_pre)
# t = 2.6407, df = 31, p-value = 0.01284

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = labelTone_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "pre-tone-labeling score score") # y-axis label


# correction after removing outliers
df_cor <- df_all4cor_outlier %>%
  select(TrainingSession:ID, WM_n_correctRecall, labelTone_pre,
         WM_outliers, labelpre_outliers) %>%
  # remove outlier
  filter(WM_outliers != TRUE & labelpre_outliers != TRUE)

cor.test(df_cor$WM_n_correctRecall, df_cor$labelTone_pre)
# t = 2.6407, df = 31, p-value = 0.01284

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = labelTone_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "pre-tone-labeling score score") # y-axis label

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# correlation between NWR and pre-tone-labeling ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# correction before removing outliers
df_cor <- df_all4cor %>%
  select(TrainingSession:ID, NWR_n_correctRecall, labelTone_pre)

cor.test(df_cor$NWR_n_correctRecall, df_cor$labelTone_pre)
# t = -0.00058541, df = 31, p-value = 0.9995

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = NWR_n_correctRecall, y = labelTone_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "NWR score", # x-axis label
       y = "pre-tone-labeling score score") # y-axis label


# correction after removing outliers
df_cor <- df_all4cor_outlier %>%
  select(TrainingSession:ID, NWR_n_correctRecall, labelTone_pre,
         NWR_outliers, labelpre_outliers) %>%
  # remove outlier
  filter(NWR_outliers != TRUE & labelpre_outliers != TRUE)

cor.test(df_cor$NWR_n_correctRecall, df_cor$labelTone_pre)
# t = 0.40074, df = 30, p-value = 0.6914

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = NWR_n_correctRecall, y = labelTone_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "NWR score", # x-axis label
       y = "pre-tone-labeling score score") # y-axis label

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# correlation between WM and pronounceTone_pre ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# correction before removing outliers
df_cor <- df_all4cor %>%
  select(TrainingSession:ID, WM_n_correctRecall, pronounceTone_pre)

cor.test(df_cor$WM_n_correctRecall, df_cor$pronounceTone_pre)
# t = -0.39391, df = 29, p-value = 0.6965

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = pronounceTone_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "pre-tone-production score score") # y-axis label


# correction after removing outliers
df_cor <- df_all4cor_outlier %>%
  select(TrainingSession:ID, WM_n_correctRecall, pronounceTone_pre,
         WM_outliers, pronouncepre_outliers) %>%
  # remove outlier
  filter(WM_outliers != TRUE & pronouncepre_outliers != TRUE)

cor.test(df_cor$WM_n_correctRecall, df_cor$pronounceTone_pre)
# t = -0.64435, df = 28, p-value = 0.5246

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = pronounceTone_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "pre-tone-production score score") # y-axis label

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# correlation between NWR and pronounceTone_pre ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# correction before removing outliers
df_cor <- df_all4cor %>%
  select(TrainingSession:ID, NWR_n_correctRecall, pronounceTone_pre)

cor.test(df_cor$NWR_n_correctRecall, df_cor$pronounceTone_pre)
# t = -1.4346, df = 29, p-value = 0.1621

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = NWR_n_correctRecall, y = pronounceTone_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "NWR score", # x-axis label
       y = "pre-tone-production score score") # y-axis label

# correction after removing outliers
df_cor <- df_all4cor_outlier %>%
  select(TrainingSession:ID, NWR_n_correctRecall, pronounceTone_pre,
         NWR_outliers, pronouncepre_outliers) %>%
  # remove outlier
  filter(NWR_outliers != TRUE & pronouncepre_outliers != TRUE)

cor.test(df_cor$NWR_n_correctRecall, df_cor$pronounceTone_pre)
# t = -0.58803, df = 27, p-value = 0.5614

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = NWR_n_correctRecall, y = pronounceTone_pre)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "NWR score", # x-axis label
       y = "pre-tone-production score score") # y-axis label

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# correlation between rating_diff score and WM ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# correction before removing outliers
df_cor <- df_all4cor %>%
  select(TrainingSession:ID, WM_n_correctRecall, mean_diff)

cor.test(df_cor$WM_n_correctRecall, df_cor$mean_diff)
# t = -0.0074456, df = 26, p-value = 0.9941

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = mean_diff)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "Improvement score (posttest rating - pretest rating)") # y-axis label

# correction after removing outliers
df_cor <- df_all4cor_outlier %>%
  select(TrainingSession:ID, WM_n_correctRecall, mean_diff,
         WM_outliers, rating_diff_outliers) %>%
  # remove outlier
  filter(WM_outliers != TRUE & rating_diff_outliers != TRUE)

cor.test(df_cor$WM_n_correctRecall, df_cor$mean_diff)
# t = -0.61592, df = 25, p-value = 0.5435

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = WM_n_correctRecall, y = mean_diff)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "WM score", # x-axis label
       y = "Improvement score (posttest rating - pretest rating)") # y-axis label

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# correlation between rating_diff score and NWR ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# correction before removing outliers
df_cor <- df_all4cor %>%
  select(TrainingSession:ID, NWR_n_correctRecall, mean_diff)

cor.test(df_cor$NWR_n_correctRecall, df_cor$mean_diff)
# t = -0.97173, df = 26, p-value = 0.3401

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = NWR_n_correctRecall, y = mean_diff)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "NWR score", # x-axis label
       y = "Improvement score (posttest rating - pretest rating)") # y-axis label


# correction after removing outliers
df_cor <- df_all4cor_outlier %>%
  select(TrainingSession:ID, NWR_n_correctRecall, mean_diff,
         NWR_outliers, rating_diff_outliers) %>%
  # remove outlier
  filter(NWR_outliers != TRUE & rating_diff_outliers != TRUE)

cor.test(df_cor$NWR_n_correctRecall, df_cor$mean_diff)
# t = -1.5739, df = 25, p-value = 0.1281

# plotting ####
ggplot(data = df_cor, 
       mapping = aes(x = NWR_n_correctRecall, y = mean_diff)) +
  geom_point() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  theme_Publication() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0.1, #adjust the label in x axis
           label.y.npc = 0.8, #adjust the label in y axis
           size = 4) +
  labs(x = "NWR score", # x-axis label
       y = "Improvement score (posttest rating - pretest rating)") # y-axis label


# NEED UPDATE CODE BELOW



# label ~ pronounce
res2 <- rcorr(as.matrix(df_cor[, 3:ncol(df_cor)]))
flattenCorrMatrix(res2$r, res2$P)
# > flattenCorrMatrix(res2$r, res2$P)
#                    row              column           cor          p
# 1        labelTone_pre   pronounceTone_pre  0.2781913199 0.07444375
# 2        labelTone_pre  WM_n_correctRecall  0.4285337998 0.01283991
# 3    pronounceTone_pre  WM_n_correctRecall -0.0729521443 0.69652985
# 4        labelTone_pre NWR_n_correctRecall -0.0001051428 0.99953666
# 5    pronounceTone_pre NWR_n_correctRecall -0.2574133495 0.16211052
# 6   WM_n_correctRecall NWR_n_correctRecall  0.3057487057 0.03457070
# 7        labelTone_pre            mean_pre  0.2733548838 0.15134317
# 8    pronounceTone_pre            mean_pre  0.0415452974 0.83056328
# 9   WM_n_correctRecall            mean_pre  0.3748347255 0.02226445
# 10 NWR_n_correctRecall            mean_pre  0.2733120290 0.10167969