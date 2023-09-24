# last updated on 9/1/2023

# library
library(lmerTest)
# library(tidyverse)
library(emmeans)
library(car)
# library(rstatix)

setwd("/Volumes/GoogleDrive/Shared drives/Corrective feedback/Chao working folder/analysis")

# source for dataset
source("prepare_data_for_modeling.R")
# source for plotting
source("~/Documents/RData/ggplot/ggplot_theme_Publication.R")


# %%%%%%%%%%%%%%%%%%%%%%%
# raw score analysis ####
# %%%%%%%%%%%%%%%%%%%%%%%

# Set orthogonal contrasts

# Test
pre_posANDdel <- c(-2/3, 1/3, 1/3)
pos_del <- c(0, -1/2, 1/2)
contrasts(df_mod$Test) <- cbind(pre_posANDdel, pos_del)

# CF
noCF_CF <- c(-1/2, 1/2)
contrasts(df_mod$CF) <- cbind(noCF_CF)

# Type
cont_spon <- c(-1/2, 1/2)
contrasts(df_mod$Type) <- cbind(cont_spon)

# Strategry
strategy_baseline <- c(-1/3, -1/3, 2/3)
recast_meta <- c(-1/2, 1/2, 0)
contrasts(df_mod$Group) <- cbind(strategy_baseline, recast_meta)

# build model for the effect of Test and Group, collapsing CF and Type
# other random structures don't work
mod <- glmer(score ~ Test * Group +
               (1 | ID) +
               (1 | Item),
             data = df_mod, family = "binomial")
summary(mod)
Anova(mod, type = "III")

# simple effect
emmeans(mod, pairwise ~ Test | Group)

# %%%%%%%%%%%%%%%%%%%%
# exclude placebo group and delayed test, testing effect of CF, production type, strategy
# %%%%%%%%%%%%%%%%%%%%

# get difference score for modeling/visualization
df_mod2 <- df_mod %>%
  # exclude placebo group
  filter(Group != "placebo") %>%
  # exclude delayed test
  filter(Test != "delayedtest") %>%
  droplevels()

# Set orthogonal contrasts

# Test
pre_pos <- c(-1/2, 1/2)
contrasts(df_mod2$Test) <- cbind(pre_pos)

# CF
noCF_CF <- c(-1/2, 1/2)
contrasts(df_mod2$CF) <- cbind(noCF_CF)

# Type
cont_spon <- c(-1/2, 1/2)
contrasts(df_mod2$Type) <- cbind(cont_spon)

# build model for the effect of Test and Group, collapsing CF and Type
# other random structures don't work
mod <- glmer(score ~ Test * CF * Type +
               (1 | ID) +
               (1 | Item),
             data = df_mod2, family = "binomial")
summary(mod)
Anova(mod, type = "III")

# simple effect
emmeans(mod, pairwise ~ Test | CF)
emmeans(mod, pairwise ~ Test | CF * Group)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# analysis on difference scores
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# get difference score for modeling/visualization
df_diffmod <- df_mod %>%
  # exclude placebo group
  filter(Group != "placebo") %>%
  # exclude MWhitsell's data
  filter(!ID %in% c("MWhtsell",
                    "SCole")) %>%
  droplevels() %>%
  # mean score for each item (Character-Syllable combination)
  group_by(TrainingSession, ID, Type, CF, Group, Test, Tone, Item) %>%
  dplyr::summarize(score_item = mean(score)) %>%
  ungroup() %>%
  # mean score for each condition
  group_by(TrainingSession, ID, Type, CF, Group, Test) %>%
  dplyr::summarize(mean_score = mean(score_item)) %>%
  ungroup() %>%
  pivot_wider(names_from = Test, values_from = mean_score) %>%
  # # get difference score (pos minus pre)
  # mutate(pos_minus_pre = posttest - pretest) %>%
  # # get difference score (del minus pre)
  # mutate(del_minus_pre = delayedtest - pretest) %>%
  # get difference score ((pos+del)/2 - pre)
  mutate(diff_score = (posttest+delayedtest)/2 - pretest)

# Set orthogonal contrasts

# CF
noCF_CF <- c(-1/2, 1/2)
contrasts(df_diffmod$CF) <- cbind(noCF_CF)

# Type
cont_spon <- c(-1/2, 1/2)
contrasts(df_diffmod$Type) <- cbind(cont_spon)

# Group
recast_meta <- c(-1/2, 1/2)
contrasts(df_diffmod$Group) <- cbind(recast_meta)

# build model
mod_diff <- lmer(diff_score ~ CF * Type * Group +
               (1 + CF + Type | ID),
             data = df_diffmod)
summary(mod_diff)
Anova(mod_diff, type = "III")

# simple effect (Strategy)
emmeans(mod_diff, pairwise ~ CF)
emmeans(mod_diff, pairwise ~ Type)
emmeans(mod_diff, pairwise ~ Group)

## line plot (difference rating)
ggplot(data = df_diffmod, mapping = aes(x = Group, y = diff_score, color = CF)) +
  facet_grid(. ~ Type) +
  geom_point(stat = "summary", fun = mean,
             position = position_dodge(.2)) +
  geom_line(stat = "summary", fun = mean, mapping = aes(group = CF),
            position = position_dodge(.2)) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2,
                position = position_dodge(.2)) +
  scale_colour_Publication() +
  theme_Publication()

# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Test for only recast and meta level
# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# df_diffmod_fb <- df_diffmod %>%
#   filter(Group != "placebo") %>%
#   droplevels()
# 
# # Set orthogonal contrasts
# 
# # CF
# noCF_CF <- c(-1/2, 1/2)
# contrasts(df_diffmod_fb$CF) <- cbind(noCF_CF)
# 
# # Type
# cont_spon <- c(-1/2, 1/2)
# contrasts(df_diffmod_fb$Type) <- cbind(cont_spon)
# 
# # Group
# recast_meta <- c(-1/2, 1/2)
# contrasts(df_diffmod_fb$Group) <- cbind(recast_meta)
# 
# # build model
# mod <- lmer(diff_score ~ CF * Type * Group +
#               (1 + CF + Type | ID),
#             data = df_diffmod_fb)
# summary(mod)
# Anova(mod, type = "III")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### control for the lexicon ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

df_preRating <- df_mod %>%
  filter(Test == "pre") %>%
  droplevels() %>%
  group_by(TrainingSession, ID, Type, CF, Strategy, Tone, Character) %>%
  dplyr::summarize(char_rating = mean(score)) %>%
  ungroup() %>%
  group_by(TrainingSession, ID, Type, CF, Strategy, Tone) %>%
  dplyr::summarize(mean_rating = mean(char_rating)) %>%
  ungroup() %>%
  mutate(Tone = as.factor(Tone))

contrasts(df_preRating$CF) <- contr.helmert(2)
contrasts(df_preRating$Tone) <- contr.helmert(10)
mod <- lmer(mean_rating ~ CF * Tone + (1 | ID), data = df_preRating)
summary(mod)
Anova(mod, type = "III")

emmeans(mod, pairwise ~ CF | Tone)

# Conclusion: remove t13, t14, t24, t32, t44

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### model (after remove t13, t14, t24, t32, t44) #####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

df_mod_t <- df_mod %>%
  # remove t13, t14, t24, t32, t44
  filter(!Tone %in% c("t13", "t14", "t24", "t32", "t44")) %>%
  droplevels()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# analysis on difference scores
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
df_diffmod_t <- df_mod_t %>%
  group_by(TrainingSession, ID, Type, CF, Strategy, Test) %>%
  dplyr::summarize(mean_score = mean(score)) %>%
  ungroup() %>%
  pivot_wider(names_from = Test, values_from = mean_score) %>%
  # exclude MWhitsell's data
  filter(ID != "MWhtsell") %>%
  # get difference score (pos minus pre)
  mutate(diff_score = pos - pre)

# Set orthogonal contrasts

# CF
contrasts(df_diffmod_t$CF) <- cbind(noCF_CF)

# Type
contrasts(df_diffmod_t$Type) <- cbind(cont_spon)

# Strategry
contrasts(df_diffmod_t$Strategy) <- cbind(strategy_baseline, recast_meta)

# build model
mod <- lmer(diff_score ~ CF * Type * Strategy +
              (1 + CF + Type | ID),
            data = df_diffmod_t)
summary(mod)
Anova(mod, type = "III")

# effect in each level of CF
df_diffmod_t_cf <- df_diffmod_t %>%
  filter(CF == "CF") %>%
  droplevels()
# build model
mod_cf <- lmer(diff_score ~ Type * Strategy +
              (1 | ID),
            data = df_diffmod_t_cf)
summary(mod_cf)
Anova(mod_cf, type = "III")
emmeans(mod_cf, pairwise ~ Strategy | Type)
emmeans(mod_cf, pairwise ~ Type | Strategy)

df_diffmod_t_ncf <- df_diffmod_t %>%
  filter(CF == "noCF") %>%
  droplevels()
# build model
mod_ncf <- lmer(diff_score ~ Type * Strategy +
              (1 | ID),
            data = df_diffmod_t_ncf)
summary(mod_ncf)
Anova(mod_ncf, type = "III")

# simple effect (Strategy)
emmeans(mod, pairwise ~ Strategy + Type | CF)
emmeans(mod, pairwise ~ Strategy | CF + Type)
emmeans(mod, pairwise ~ Type)
emmeans(mod, pairwise ~ CF)

## line plot (raw rating)
ggplot(data = df_mod_t, mapping = aes(x = Test, y = score, color = CF)) +
  # facet_grid(. ~ Type) +
  facet_grid(Type ~ Strategy) +
  geom_point(stat = "summary", fun = mean,
             position = position_dodge(.2)) +
  geom_line(stat = "summary", fun = mean, mapping = aes(group = CF),
            position = position_dodge(.2)) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2,
                position = position_dodge(.2)) +
  scale_fill_Publication() +
  theme_Publication()

## line plot (difference rating)
ggplot(data = df_diffmod_t, mapping = aes(x = Strategy, y = diff_score, color = CF)) +
  facet_grid(. ~ Type) +
  geom_point(stat = "summary", fun = mean,
             position = position_dodge(.2)) +
  geom_line(stat = "summary", fun = mean, mapping = aes(group = CF),
            position = position_dodge(.2)) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2,
                position = position_dodge(.2)) +
  scale_fill_Publication() +
  theme_Publication()

