require(dplyr)
require(lme4)
require(lmerTest)

# calculating P2 means for reporting in manuscript

dat = read.delim("./5 P2/AllSubs_acceptedTrials_long_nobe_nobs.txt")


# Gender Task -------------------------------------------------------------


gendat = filter(dat, Task == "Gender")

# Effect of Target Race
select(gendat, TarRace, value) %>% 
  group_by(TarRace) %>% 
  summarise_all(funs(mean), na.rm=T)
  
# Effect of Target Gender
select(gendat, TarGender, value) %>% 
  group_by(TarGender) %>% 
  summarise_all(funs(mean), na.rm=T)

# Effect of Fixation
select(gendat, Fix, value) %>% 
  group_by(Fix) %>% 
  summarise_all(funs(mean), na.rm=T)

# Effect of ParRace
select(gendat, ParRace, value) %>% 
  group_by(ParRace) %>% 
  summarise_all(funs(mean), na.rm=T)


# TarRace x Fix x ParRace interaction
select(gendat, ParRace, TarRace, Fix, value) %>% 
  group_by(ParRace, TarRace, Fix) %>% 
  summarise_all(funs(mean), na.rm=T)


# Race Task ---------------------------------------------------------------

racedat = filter(dat, Task == "Race")

# Effect of Target Race
select(racedat, TarRace, value) %>% 
  group_by(TarRace) %>% 
  summarise_all(funs(mean), na.rm=T)

# Effect of Fixation
select(racedat, Fix, value) %>% 
  group_by(Fix) %>% 
  summarise_all(funs(mean), na.rm=T)

# Effect of ParRace
select(racedat, ParRace, value) %>% 
  group_by(ParRace) %>% 
  summarise_all(funs(mean), na.rm=T)





# Probe interactions

racedat$TarRace.e = -1
racedat$TarRace.e[racedat$TarRace == "White"] = 1

racedat$TarGender.e = -1
racedat$TarGender.e[racedat$TarGender == "male"] = 1

racedat$ParRace.e = -1
racedat$ParRace.e[racedat$ParRace == "White"] = 1

racedat$Fix.e = -1
racedat$Fix.e[racedat$Fix == "forehead"] = 1

# TarRace x TarGen interaction
lmer(value ~ TarRace.e + (TarRace.e|Subject) + (1|Electrode), data = filter(racedat, TarGender == "female")) %>% 
  summary()

lmer(value ~ TarRace.e + (TarRace.e|Subject) + (1|Electrode), data = filter(racedat, TarGender == "male")) %>% 
  summary()

lmer(value ~ TarGender.e + (TarGender.e|Subject) + (1|Electrode), data = filter(racedat, TarRace == "Black")) %>% 
  summary()

lmer(value ~ TarGender.e + (TarGender.e|Subject) + (1|Electrode), data = filter(racedat, TarRace == "White")) %>% 
  summary()

# TarRace x ParRace for eyes/forehead separately

lmer(value ~ TarRace.e*ParRace.e + (TarRace.e|Subject) + (1|Electrode), data = filter(racedat, Fix == "eyes")) %>% 
  summary()

lmer(value ~ TarRace.e*ParRace.e + (TarRace.e|Subject) + (1|Electrode), data = filter(racedat, Fix == "forehead")) %>% 
  summary()

# TarRace x Fix for W/B Ps separately

lmer(value ~ TarRace.e*Fix.e + (TarRace.e|Subject) + (1|Electrode), data = filter(racedat, ParRace == "Black")) %>% 
  summary()

lmer(value ~ TarRace.e*Fix.e + (TarRace.e|Subject) + (1|Electrode), data = filter(racedat, ParRace == "White")) %>% 
  summary()



# Comparison of tasks -----------------------------------------------------

# separately for eye and forehead fix trials

# Add predictors, effect coding
dat$TarRace.e = -1
dat$TarRace.e[dat$TarRace == "White"] = 1

dat$Task.e = -1
dat$Task.e[dat$Task == "Race"] = 1

dat$TarGender.e = -1
dat$TarGender.e[dat$TarGender == "male"] = 1

lmer(value ~ TarGender.e*Task.e + TarRace.e*Task.e + (TarGender.e+TarRace.e + Task.e|Subject) + (1|Electrode), data = filter(dat, Fix == "eyes")) %>% 
  summary()

lmer(value ~ TarGender.e*Task.e + TarRace.e*Task.e + (TarGender.e+TarRace.e + Task.e|Subject) + (1|Electrode), data = filter(dat, Fix == "forehead")) %>% 
  summary()

