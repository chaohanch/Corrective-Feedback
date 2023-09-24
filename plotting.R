# %%%%%%%%%%%%%
# visualization
# %%%%%%%%%%%%%

# last updated on 8/5/2023

# library
library(tidyverse)
library(rstatix)
library(RColorBrewer)

# source for dataset
source("prepare_data_for_modeling.R")
# source for plotting
source("~/Documents/RData/ggplot/ggplot_theme_Publication.R")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### plot for descriptive view ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create data for plotting
df_plot <- df_mod %>%
  group_by(TrainingSession, ID, Type, CF, Group, Test, Tone, Item) %>%
  dplyr::summarize(score_item = mean(score)) %>%
  ungroup()

# compare pre- vs post- mean rating for each group
ggplot(data = df_plot, mapping = aes(x = Test, y = score_item, color = Group)) +
  geom_point(stat = "summary", fun = mean,
             position = position_dodge(.2)) +
  geom_line(stat = "summary", fun = mean, mapping = aes(group = Group),
            position = position_dodge(.2)) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2,
                position = position_dodge(.2)) +
  scale_colour_Publication() +
  theme_Publication()
# 500 x 400

# a more detailed view:
# compare pre- vs post- mean rating for each group in each condition
ggplot(data = df_plot, mapping = aes(x = Test, y = score_item, color = Group)) +
  facet_grid(CF ~ Type) +
  geom_point(stat = "summary", fun = mean,
             position = position_dodge(.2)) +
  geom_line(stat = "summary", fun = mean, mapping = aes(group = Group),
            position = position_dodge(.2)) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2,
                position = position_dodge(.2)) +
  scale_colour_Publication() +
  theme_Publication()
# 800 x 500

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# compare pre- vs post- mean rating for each tone in each group
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# set color
cols <- brewer.pal(10, "Set3")

# line plot
ggplot(data = df_plot, mapping = aes(x = Test, y = score_item, color = Tone)) +
  facet_grid(. ~ Group) +
  geom_point(stat = "summary", fun = mean,
             position = position_dodge(.5)) +
  geom_line(stat = "summary", fun = mean, mapping = aes(group = Tone),
            position = position_dodge(.5)) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2,
                position = position_dodge(.5)) +
  scale_color_manual(values = c(cols[1],
                                cols[2],
                                cols[3],
                                cols[4],
                                cols[5],
                                cols[6],
                                cols[7],
                                cols[8],
                                cols[9],
                                cols[10])) +
  theme_dark_blue()
# 900x600

# bar plot
ggplot(data = df_plot, mapping = aes(x = Tone, y = score_item, fill = Test)) +
  facet_grid(Group ~ .) +
  geom_bar(stat = "summary", fun = mean,
           position = position_dodge(0.9)) +
  geom_errorbar(stat = "summary", fun.data = mean_se,
                width = 0.2, position = position_dodge(0.9)) +
  scale_fill_Publication() +
  theme_Publication()
# 900x600

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# a more detailed view: compare pre- vs post- mean rating 
# for each tone in each group
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# set color
cols <- brewer.pal(10, "Set3")

# recast line plot
ggplot(data = df_plot[df_plot$Group == "recast", ], mapping = aes(x = Test, y = score_item, color = Tone)) +
  facet_grid(CF ~ Type) +
  geom_point(stat = "summary", fun = mean,
             position = position_dodge(.5)) +
  geom_line(stat = "summary", fun = mean, mapping = aes(group = Tone),
            position = position_dodge(.5)) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2,
                position = position_dodge(.5)) +
  scale_color_manual(values = c(cols[1],
                                cols[2],
                                cols[3],
                                cols[4],
                                cols[5],
                                cols[6],
                                cols[7],
                                cols[8],
                                cols[9],
                                cols[10])) +
  theme_dark_blue() +
  labs(title = "recast group")
# 500x500

# recast bar plot
ggplot(data = df_plot[df_plot$Group == "recast", ], mapping = aes(x = Tone, y = score_item, fill = Test)) +
  facet_grid(CF * Type ~ .) +
  geom_bar(stat = "summary", fun = mean,
           position = position_dodge(0.9)) +
  geom_errorbar(stat = "summary", fun.data = mean_se,
                width = 0.2, position = position_dodge(0.9)) +
  scale_fill_Publication() +
  theme_Publication() +
  labs(title = "recast group") +
  theme(title=element_text(size=10))
# 800x600

# meta line plot
ggplot(data = df_plot[df_plot$Group == "meta", ], mapping = aes(x = Test, y = score_item, color = Tone)) +
  facet_grid(CF ~ Type) +
  geom_point(stat = "summary", fun = mean,
             position = position_dodge(.5)) +
  geom_line(stat = "summary", fun = mean, mapping = aes(group = Tone),
            position = position_dodge(.5)) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2,
                position = position_dodge(.5)) +
  scale_color_manual(values = c(cols[1],
                                cols[2],
                                cols[3],
                                cols[4],
                                cols[5],
                                cols[6],
                                cols[7],
                                cols[8],
                                cols[9],
                                cols[10])) +
  theme_dark_blue() +
  labs(title = "meta group")
# 500x500

# meta bar plot
ggplot(data = df_plot[df_plot$Group == "meta", ], mapping = aes(x = Tone, y = score_item, fill = Test)) +
  facet_grid(CF * Type ~ .) +
  geom_bar(stat = "summary", fun = mean,
           position = position_dodge(0.9)) +
  geom_errorbar(stat = "summary", fun.data = mean_se,
                width = 0.2, position = position_dodge(0.9)) +
  scale_fill_Publication() +
  theme_Publication() +
  labs(title = "meta group") +
  theme(title=element_text(size=10))
# 800x600