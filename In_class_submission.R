recordings <- read.csv("recordings.csv")
sensorinfo <- read.csv("sensorinfo.csv")

library(brms)
library(tidyverse)
library(MASS)
library(marginaleffects)
library(modelr)
library(bayesplot)

# data cleaning
combined_dat <- merge(recordings, sensorinfo, by = "sensorid", all = TRUE)

# SONG COUNT ~ BOAT ACTIVITY
mod1 <- brm(totsongs ~ boatactivity + distshore + waterdepth + 1|sensorid + 1|dayid,
            data = combined_dat, 
            family = negbinomial())
summary(mod1)
pp_check(mod1)
mcmc_plot(mod1)

# TOTAL SONGS ~ BOAT NOISE
mod2 <- brm(totsongs ~ boatnoise + watertemp + 1|sensorid + 1|dayid,
            data = combined_dat, 
            family = negbinomial())
summary(mod2)
pp_check(mod2)
mcmc_plot(mod2)

# SONG LENGTH ~ BOAT ACTIVITY

mod3 <- brm(songlength ~ boatactivity + distshore + waterdepth + (1|sensorid) + (1|dayid),
            data = combined_dat,
            family = "Gamma" (link = log))
summary(mod3)
pp_check(mod3)
mcmc_plot(mod3)

# SONG LENGTH ~ BOAT NOISE

mod4 <- brm(songlength ~ boatnoise + watertemp + 1|sensorid + 1|dayid,,
            data = combined_dat,
            family = "Gamma"(link = log))

summary(mod4)
pp_check(mod4)
mcmc_plot(mod4)

