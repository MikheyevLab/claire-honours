# Cycle 2 survival data March 1st edit using file 'SurvivalDataCycle2R"
# Load packages
library(readr)
library(tidyverse)
library(emmeans)
library(dplyr)
library(Rmisc)
# Import data
C2R <- read_csv("data/SurvivalDataCycle2R.csv")
View(C2R)
# Visualize data steps
# 1 - Initial plot, each treatment with cage coloured
ggplot(C2R, aes(x = day, y = percent_survived, colour = factor(cage)))+
  geom_line(aes(group=cage))+
  facet_wrap(~treatment)+
  theme(legend.position = "top")
# Summary statistics using Rmisc via Vienna recommendation as quick way to get SE
StatsC2R <- summarySE(C2R, measurevar="percent_survived", groupvars=c("treatment","day"))
view(StatsC2R)
# First plot
gg1C2 <- ggplot(data=StatsC2R, aes(x=day, y=percent_survived, group = treatment, colour = treatment)) + geom_line() + geom_point()
print(gg1C2)
# Plot with SE
gg2C2 <- ggplot(data=StatsC2R, aes(x=day, y=percent_survived, group = treatment, colour = treatment)) +
  geom_errorbar(aes(ymin=percent_survived-se, ymax=percent_survived+se)) +
  geom_errorbar(aes(ymin=percent_survived-sd, ymax=percent_survived+sd), width=.3, size=1.1) +
  geom_line()
print(gg2C2)
# Plot with point
gg3C2 <- ggplot(data=StatsC2R, aes(x=day, y=percent_survived, group = treatment, colour = treatment)) +
  geom_errorbar(aes(ymin=percent_survived-se, ymax=percent_survived+se)) +
  geom_line() + geom_point()
print(gg3C2)
# Plot by treatment/cage
gg4C2 <- ggplot(StatsC2R, aes(x = day, y = percent_survived, colour = treatment))+
  facet_wrap(~treatment)+
  theme(legend.position = "top")+
  geom_errorbar(aes(ymin=percent_survived-se, ymax=percent_survived+se), width=0.5, size=0.5) +
  geom_point(size=3) + geom_line(size=1)
print(gg4C2)
