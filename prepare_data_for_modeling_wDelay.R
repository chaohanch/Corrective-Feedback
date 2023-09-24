# last updated on 2023-09-01

# ========================= #
# Prepare data for modeling #
# ========================= #

# library
library(tidyverse)

# set working directory
setwd("/Volumes/GoogleDrive/Shared drives/Corrective feedback/Chao working folder/analysis")

# read in rating data
df_rating_0 <- read_csv("R input/rating for analysis.csv")
df_rating <- df_rating_0 %>%
  # exclude some data
  filter(!RatingCode %in% c("od746_没有_t23_27797",
                           "od658_没有_t23_13717")) %>% # discard for noise
  # select useful columns
  select(c(TrainingSession:RatingCode,
           Syllable1_RD:Syllable2_RD,
           Need3rdRater:Syllable2_3rd)) %>%
  # integrate 3rd rating
  mutate(Syl1 = if_else(is.na(Need3rdRater), Syllable1_RD, Syllable1_3rd),
         Syl2 = if_else(is.na(Need3rdRater), Syllable2_RD, Syllable2_3rd)) %>%
  # recode ratings to be binary
  mutate(Syl1_binary = if_else(Syl1 == 3, 1, 0),
         Syl2_binary = if_else(Syl2 == 3, 1, 0)) %>%
  # remove columns
  select(-c(Syllable1_RD:Syllable2_3rd))

# read in segmentation data
df_seg_0 <- read_csv("R input/segmentation for analysis.csv")
df_seg <- df_seg_0 %>%
  # remove not elicited rows
  filter(!is.na(MatchingCode)) %>%
  # select columns
  select(TrainingSession, ID:Token, RatingCode)

# merge segmentation data with rating data
df_production <- df_seg %>%
  # merge with rating data
  inner_join(df_rating, by = c("TrainingSession", "RatingCode")) %>%
  # correct for one coding error
  mutate(Test = dplyr::recode(Test, post = "pos")) %>%
  # leave binary ratings only
  select(-c(Syl1:Syl2)) %>%
  # making Syllable a variable
  pivot_longer(cols = c(Syl1_binary:Syl2_binary), names_to = "Syllable", values_to = "score") %>%
  # correct for the CF value of "今天" and "下午"
  mutate(CF = ifelse(Character %in% c("今天", "下午"), "n", CF)) %>%
  # correct for tones
  mutate(Tone = case_when(
    Character == "星期" ~ "t11",
    Character == "上午" ~ "t43",
    Character == "下午" ~ "t43",
    Character == "中午" ~ "t13",
    Character == "工作" ~ "t14",
    Character == "学校" ~ "t24",
    Character == "吃饭" ~ "t14",
    Character == "还要" ~ "t24",
    Character == "没有" ~ "t23",
    Character == "有空" ~ "t34",
    Character == "睡觉" ~ "t44",
    Character == "可以" ~ "t23",
    TRUE ~ Tone
  )) %>%
  droplevels()

# read in training strategy
df_trainingstrategy <- read_csv("R input/training_strategy.csv")


# get raw rating score (binary) for modeling/visualization
df_mod <- df_production %>%
  # factor coding
  mutate(Test = factor(Test, levels = c("pre", "pos", "del"), labels = c("pretest", "posttest", "delayedtest")),
         CF = factor(CF, levels = c("n", "y"), labels = c("noCF", "CF")),
         Type = factor(Type, levels = c("cont", "spon"), labels = c("controlled", "spontaneous")),
         Syllable = factor(Syllable, levels = c("Syl1_binary", "Syl2_binary"), labels = c("syl1", "syl2")),
         Tone = as.factor(Tone)) %>%
  unite(col = Item, Character, Syllable, remove = FALSE) %>%
  # integrate training strategy data
  inner_join(df_trainingstrategy) %>%
  mutate(Group = factor(Group, levels = c("recast", "meta", "placebo")))

# get difference score for modeling/visualization
df_diffmod <- df_mod %>%
  # exclude MWhitsell's data
  filter(!ID %in% c("MWhtsell",
                    "SCole")) %>%
  # mean score for each item (Character-Syllable combination)
  group_by(TrainingSession, ID, Type, CF, Group, Test, Tone, Item) %>%
  dplyr::summarize(score_item = mean(score)) %>%
  ungroup() %>%
  # mean score for each condition
  group_by(TrainingSession, ID, Type, CF, Group, Test) %>%
  dplyr::summarize(mean_score = mean(score_item)) %>%
  ungroup() %>%
  pivot_wider(names_from = Test, values_from = mean_score) %>%
  # get difference score (pos minus pre)
  mutate(pos_minus_pre = posttest - pretest) %>%
  # get difference score (del minus pre)
  mutate(del_minus_pre = delayedtest - pretest)

# remove redundant variables
rm(list = setdiff(ls(), c("df_mod", "df_diffmod")))
