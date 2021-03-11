#Power Analysis of Experiment 1 Feeding Behaviour
#Import data

#view(pollen)
library(pwr)
library(tidyverse)
library(readxl)
library(lmerTest)
library(emmeans)
library(performance)
pwr.anova.test(k=4, f=0.25, sig.level=0.05, power=0.8)
# One-way anlaysis of cariance power suggest we need 44.5 samples
# in each treatment to acheive a power of 80% with a significance of 5%.

# Power test solve for power
pwr.t.test(n=10, d=0.5, sig.level=0.5,power=NULL, alternative="two.sided", type="two.sample")
# Power= 0.70

# Power test solve for sample size
pwr.t.test(n=NULL, d=0.5, sig.level=0.5,power=.8, alternative="two.sided", type="two.sample")
# N=17.144