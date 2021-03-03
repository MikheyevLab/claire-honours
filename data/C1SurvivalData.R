# Cycle 1 survival data March 1st edit using file 'SurvivalDataCycle1R"
# Load packages
library(readr)
library(tidyverse)
library(emmeans)
library(dplyr)
library(Rmisc)
library(glmmTMB)
library(car)
# Import data
C1R <- read_csv("data/SurvivalDataCycle1R.csv")
View(C1R)
# Visualize data steps
# 1 - Initial plot, each treatment with cage coloured
ggplot(C1R, aes(x = day, y = percent_survived, colour = factor(cage)))+
  geom_line(aes(group=cage))+
  facet_wrap(~treatment)+
  theme(legend.position = "top")
# Summary statistics using Rmisc via Vienna recommendation as quick way to get SE
StatsC1R <- summarySE(C1R, measurevar="percent_survived", groupvars=c("treatment","day"))
view(StatsC1R)
# First plot
gg1C1 <- ggplot(data=StatsC1R, aes(x=day, y=percent_survived, group = treatment, colour = treatment)) + geom_line() + geom_point()
print(gg1C1)
# Plot with SE
gg2C1 <- ggplot(data=StatsC1R, aes(x=day, y=percent_survived, group = treatment, colour = treatment)) +
  geom_errorbar(aes(ymin=percent_survived-se, ymax=percent_survived+se)) +
  geom_errorbar(aes(ymin=percent_survived-sd, ymax=percent_survived+sd), width=.3, size=1.1) +
  geom_line()
print(gg2C1)
# Plot with point
gg3C1 <- ggplot(data=StatsC1R, aes(x=day, y=percent_survived, group = treatment, colour = treatment)) +
  geom_errorbar(aes(ymin=percent_survived-se, ymax=percent_survived+se)) +
  geom_line() + geom_point()
print(gg3C1)
# Plot by treatment/cage
gg4C1 <- ggplot(StatsC1R, aes(x = day, y = percent_survived, colour = treatment))+
  facet_wrap(~treatment)+
  theme(legend.position = "top")+
  geom_errorbar(aes(ymin=percent_survived-se, ymax=percent_survived+se), width=0.5, size=0.5) +
  geom_point(size=3) + geom_line(size=1)
print(gg4C1)
# Model
mod1<-glmmTMB(cbind(alive,dead)~treatment*day + (1|cage:treatment), data = C1R, family = binomial(link = "logit"))
Anova(mod1)
