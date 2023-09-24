##### Tone #####

# source for dataset
source("prepare_data_for_modeling.R")
# source for plotting
source("~/Documents/RData/ggplot/ggplot_theme_Publication.R")

library(lmerTest)
library(car)
library(emmeans)


# Analysis

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Does every tone receive significant improvement from pre- to pos- test?
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# subset data
df_tone <- df_mod %>%
  filter(Group %in% c("recast", "meta")) %>%
  droplevels()

# set orthogonal contrasts

# Test
pre_pos <- c(-1/2, 1/2)
contrasts(df_tone$Test) <- cbind(pre_pos)

# Tone
contrasts(df_tone$Tone) <- contr.helmert(10)

# build model
mod <- glmer(score ~ Test * Tone + 
               (1 + Test | ID), data = df_tone,
             family = "binomial")
summary(mod)
Anova(mod, type = "III")

emmeans(mod, pairwise ~ Test | Tone)

# %%%%%%
# plot #
# %%%%%%
df_plot <- df_mod %>%
  group_by(TrainingSession, ID, Type, CF, Group, Test, Tone, Item) %>%
  dplyr::summarize(score_item = mean(score)) %>%
  ungroup() %>%
  filter(Group %in% c("recast", "meta")) %>%
  droplevels()

ggplot(data = df_plot, mapping = aes(x = Tone, y = score_item, fill = Test)) +
  geom_bar(stat = "summary", fun = mean,
           position = position_dodge(0.9)) +
  geom_errorbar(stat = "summary", fun.data = mean_se,
                width = 0.2, position = position_dodge(0.9)) +
  scale_fill_Publication() +
  theme_Publication()
