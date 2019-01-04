require(lme4)
require(lmerTest)
require(dplyr)

parRace = read.delim("ParticipantRace.txt", stringsAsFactors = F)

# Gender ------------------------------------------------------------------

# VT-1 --------------------------------------------------------------------

VT1 = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF1_longDatForMLM.txt", sep=""))


# Effect code
VT1$Race.e = -1
VT1$Race.e[VT1$Race == "White"] = 1

VT1$Gen.e = -1
VT1$Gen.e[VT1$Gender == "male"] = 1

VT1$Fix.e = -1
VT1$Fix.e[VT1$Fix == "fore"] = 1

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT1)
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT1)
summary(m2)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Gen_VF1_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# with interaction
m3 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = VT1)
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Interactions within task/Gen_VF1_RaceGenInteraction.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

# estimate effects separately for White and Black participants
# add participant race

for (i in unique(VT1$Subject)) {
  VT1$ParRace[VT1$Subject == i] = parRace$ParRace[parRace$Subject == i]
}
VT1$ParRace = factor(VT1$ParRace)

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT1, ParRace == "White"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Gen_VF1_RaceGenFix_WhitePs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT1, ParRace == "Black"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Gen_VF1_RaceGenFix_BlackPs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

# VT-2 --------------------------------------------------------------------

VT2 = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF2_longDatForMLM.txt", sep=""))

# Effect code
VT2$Race.e = -1
VT2$Race.e[VT2$Race == "White"] = 1

VT2$Gen.e = -1
VT2$Gen.e[VT2$Gender == "male"] = 1

VT2$Fix.e = -1
VT2$Fix.e[VT2$Fix == "fore"] = 1

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT2)
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT2)
summary(m2)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Gen_VF2_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# with interaction
m3 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = VT2)
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Interactions within task/Gen_VF2_RaceGenInteraction.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()


# estimate effects separately for White and Black participants
# add participant race

for (i in unique(VT2$Subject)) {
  VT2$ParRace[VT2$Subject == i] = parRace$ParRace[parRace$Subject == i]
}
VT2$ParRace = factor(VT2$ParRace)

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT2, ParRace == "White"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Gen_VF2_RaceGenFix_WhitePs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT2, ParRace == "Black"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Gen_VF2_RaceGenFix_BlackPs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()


# VT-3 --------------------------------------------------------------------

VT3 = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF3_longDatForMLM.txt", sep=""))

# Effect code
VT3$Race.e = -1
VT3$Race.e[VT3$Race == "White"] = 1

VT3$Gen.e = -1
VT3$Gen.e[VT3$Gender == "male"] = 1

VT3$Fix.e = -1
VT3$Fix.e[VT3$Fix == "fore"] = 1

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT3)
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT3)
summary(m2)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Gen_VF3_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# with interaction
m3 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = VT3)
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Interactions within task/Gen_VF3_RaceGenInteraction.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
"________________________________________________________________________________________________"
select(VT3, meanAmp_factor, Race, Gender) %>% 
  group_by(Race, Gender) %>% 
  summarise_all(mean) %>% 
  as.data.frame()
sink()


# estimate effects separately for White and Black participants
# add participant race

for (i in unique(VT3$Subject)) {
  VT3$ParRace[VT3$Subject == i] = parRace$ParRace[parRace$Subject == i]
}
VT3$ParRace = factor(VT3$ParRace)

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT3, ParRace == "White"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Gen_VF3_RaceGenFix_WhitePs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT3, ParRace == "Black"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Gen_VF3_RaceGenFix_BlackPs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()




# Race ------------------------------------------------------------------

# VT-1 --------------------------------------------------------------------

VT1 = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF1_longDatForMLM.txt", sep=""))

# Effect code
VT1$Race.e = -1
VT1$Race.e[VT1$Race == "White"] = 1

VT1$Gen.e = -1
VT1$Gen.e[VT1$Gender == "male"] = 1

VT1$Fix.e = -1
VT1$Fix.e[VT1$Fix == "fore"] = 1

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT1)
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT1)
summary(m2)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Race_VF1_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# with interaction
m3 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = VT1)
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Interactions within task/Race_VF1_RaceGenInteraction.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
select(VT1, meanAmp_factor, Race, Gender) %>% 
  group_by(Race, Gender) %>% 
  summarise_all(mean) %>% 
  as.data.frame()
sink()


# estimate effects separately for White and Black participants
# add participant race

for (i in unique(VT1$Subject)) {
  VT1$ParRace[VT1$Subject == i] = parRace$ParRace[parRace$Subject == i]
}
VT1$ParRace = factor(VT1$ParRace)

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT1, ParRace == "White"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Race_VF1_RaceGenFix_WhitePs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT1, ParRace == "Black"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Race_VF1_RaceGenFix_BlackPs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()


# VT-2 --------------------------------------------------------------------

VT2 = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF2_longDatForMLM.txt", sep=""))

# Effect code
VT2$Race.e = -1
VT2$Race.e[VT2$Race == "White"] = 1

VT2$Gen.e = -1
VT2$Gen.e[VT2$Gender == "male"] = 1

VT2$Fix.e = -1
VT2$Fix.e[VT2$Fix == "fore"] = 1

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT2)
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT2)
summary(m2)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Race_VF2_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# with interaction
m3 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = VT2)
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Interactions within task/Race_VF2_RaceGenInteraction.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
select(VT2, meanAmp_factor, Race, Gender) %>% 
  group_by(Race, Gender) %>% 
  summarise_all(mean) %>% 
  as.data.frame()
sink()

# estimate effects separately for White and Black participants
# add participant race

for (i in unique(VT2$Subject)) {
  VT2$ParRace[VT2$Subject == i] = parRace$ParRace[parRace$Subject == i]
}
VT2$ParRace = factor(VT2$ParRace)

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT2, ParRace == "White"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Race_VF2_RaceGenFix_WhitePs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT2, ParRace == "Black"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Race_VF2_RaceGenFix_BlackPs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()


# VT-3 --------------------------------------------------------------------

VT3 = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF3_longDatForMLM.txt", sep=""))

# Effect code
VT3$Race.e = -1
VT3$Race.e[VT3$Race == "White"] = 1

VT3$Gen.e = -1
VT3$Gen.e[VT3$Gender == "male"] = 1

VT3$Fix.e = -1
VT3$Fix.e[VT3$Fix == "fore"] = 1

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT3)
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = VT3)
summary(m2)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Race_VF3_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# with interaction
m3 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = VT3)
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Interactions within task/Race_VF3_RaceGenInteraction.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
select(VT3, meanAmp_factor, Race, Gender) %>% 
  group_by(Race, Gender) %>% 
  summarise_all(mean) %>% 
  as.data.frame()
sink()


# estimate effects separately for White and Black participants
# add participant race

for (i in unique(VT3$Subject)) {
  VT3$ParRace[VT3$Subject == i] = parRace$ParRace[parRace$Subject == i]
}
VT3$ParRace = factor(VT3$ParRace)

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT3, ParRace == "White"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Race_VF3_RaceGenFix_WhitePs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

m3 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Gen.e+Fix.e|Subject) + (1|Electrode), data = filter(VT3, ParRace == "Black"))
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Separately for white and black Ps/Race_VF3_RaceGenFix_BlackPs.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()









# Look at effect of participant race --------------------------------------

# Add ParRace
parRace = read.delim("5 P2/AllSubs_indavgs_long_nobe_nobs.txt")

# VT1
## Gen
VT1 = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF1_longDatForMLM.txt", sep=""))
VT1$ParRace = NULL
for (i in unique(VT1$Subject)){
  VT1$ParRace[VT1$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}
VT1$ParRace = as.factor(VT1$ParRace)
# Effect code
VT1$Race.e = -1
VT1$Race.e[VT1$Race == "White"] = 1

VT1$Gen.e = -1
VT1$Gen.e[VT1$Gender == "male"] = 1

VT1$Fix.e = -1
VT1$Fix.e[VT1$Fix == "fore"] = 1

sum1 = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = VT1) %>% 
  summary()
sum2 = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = VT1) %>% 
  summary()
sum3 = lmer(meanAmp_factor ~ Fix.e*ParRace + (Fix.e|Subject) + (1|Electrode), data = VT1) %>% 
  summary()

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/ParRace_Gen_VF1.txt", sep=""))
sum1
"________________________________________________________________________________________________"
sum2
"________________________________________________________________________________________________"
sum3
sink()

select(VT1, meanAmp_factor, Gen.e, ParRace) %>% 
  group_by(Gen.e, ParRace) %>% 
  summarise_each(funs(mean))

## Race
VT1 = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF1_longDatForMLM.txt", sep=""))
VT1$ParRace = NULL
for (i in unique(VT1$Subject)){
  VT1$ParRace[VT1$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}
VT1$ParRace = as.factor(VT1$ParRace)
# Effect code
VT1$Race.e = -1
VT1$Race.e[VT1$Race == "White"] = 1

VT1$Gen.e = -1
VT1$Gen.e[VT1$Gender == "male"] = 1

VT1$Fix.e = -1
VT1$Fix.e[VT1$Fix == "fore"] = 1

sum1 = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = VT1) %>% 
  summary()
sum2 = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = VT1) %>% 
  summary()
sum3 = lmer(meanAmp_factor ~ Fix.e*ParRace + (Fix.e|Subject) + (1|Electrode), data = VT1) %>% 
  summary()

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/ParRace_Race_VF1.txt", sep=""))
sum1
"________________________________________________________________________________________________"
sum2
"________________________________________________________________________________________________"
sum3
sink()

# VT2
## Gen
VT2 = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF2_longDatForMLM.txt", sep=""))
VT2$ParRace = NULL
for (i in unique(VT2$Subject)){
  VT2$ParRace[VT2$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}
# Effect code
VT2$Race.e = -1
VT2$Race.e[VT2$Race == "White"] = 1

VT2$Gen.e = -1
VT2$Gen.e[VT2$Gender == "male"] = 1

VT2$Fix.e = -1
VT2$Fix.e[VT2$Fix == "fore"] = 1
sum1 = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = VT2) %>% 
  summary()
sum2 = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = VT2) %>% 
  summary()
sum3 = lmer(meanAmp_factor ~ Fix.e*ParRace + (Fix.e|Subject) + (1|Electrode), data = VT2) %>% 
  summary()

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/ParRace_VF2_Gen.txt", sep=""))
sum1
"________________________________________________________________________________________________"
sum2
"________________________________________________________________________________________________"
sum3
sink()

## Race
VT2 = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF2_longDatForMLM.txt", sep=""))
VT2$ParRace = NULL
for (i in unique(VT2$Subject)){
  VT2$ParRace[VT2$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}
# Effect code
VT2$Race.e = -1
VT2$Race.e[VT2$Race == "White"] = 1

VT2$Gen.e = -1
VT2$Gen.e[VT2$Gender == "male"] = 1

VT2$Fix.e = -1
VT2$Fix.e[VT2$Fix == "fore"] = 1
sum1 = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = VT2) %>% 
  summary()
sum2 = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = VT2) %>% 
  summary()
sum3 = lmer(meanAmp_factor ~ Fix.e*ParRace + (Fix.e|Subject) + (1|Electrode), data = VT2) %>% 
  summary()

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/ParRace_VF2_Race.txt", sep=""))
sum1
"________________________________________________________________________________________________"
sum2
"________________________________________________________________________________________________"
sum3
sink()


# VT3
## Gen
VT3 = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF3_longDatForMLM.txt", sep=""))

VT3$ParRace = NULL
for (i in unique(VT3$Subject)){
  VT3$ParRace[VT3$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}
# Effect code
VT3$Race.e = -1
VT3$Race.e[VT3$Race == "White"] = 1

VT3$Gen.e = -1
VT3$Gen.e[VT3$Gender == "male"] = 1

VT3$Fix.e = -1
VT3$Fix.e[VT3$Fix == "fore"] = 1
sum1 = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = VT3) %>% 
  summary()
sum2 = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = VT3) %>% 
  summary()
sum3 = lmer(meanAmp_factor ~ Fix.e*ParRace + (Fix.e|Subject) + (1|Electrode), data = VT3) %>% 
  summary()

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/ParRace_VF3_Gen.txt", sep=""))
sum1
"________________________________________________________________________________________________"
sum2
"________________________________________________________________________________________________"
sum3
sink()


## Race
VT3 = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF3_longDatForMLM.txt", sep=""))

VT3$ParRace = NULL
for (i in unique(VT3$Subject)){
  VT3$ParRace[VT3$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}
# Effect code
VT3$Race.e = -1
VT3$Race.e[VT3$Race == "White"] = 1

VT3$Gen.e = -1
VT3$Gen.e[VT3$Gender == "male"] = 1

VT3$Fix.e = -1
VT3$Fix.e[VT3$Fix == "fore"] = 1
sum1 = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = VT3) %>% 
  summary()
sum2 = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = VT3) %>% 
  summary()
sum3 = lmer(meanAmp_factor ~ Fix.e*ParRace + (Fix.e|Subject) + (1|Electrode), data = VT3) %>% 
  summary()

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/ParRace_VF3_Race.txt", sep=""))
sum1
"________________________________________________________________________________________________"
sum2
"________________________________________________________________________________________________"
sum3
sink()



# Look at effect of task --------------------------------------------------

# VT-1 --------------------------------------------------------------------

gen = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF1_longDatForMLM.txt", sep=""))
race = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF1_longDatForMLM.txt", sep=""))

gen$Task = "Gender"
race$Task = "Race"

VF1 = rbind(gen, race)

# Effect code
VF1$Race.e = -1
VF1$Race.e[VF1$Race == "White"] = 1

VF1$Gen.e = -1
VF1$Gen.e[VF1$Gender == "male"] = 1

VF1$Fix.e = -1
VF1$Fix.e[VF1$Fix == "fore"] = 1

VF1$Task.e = -1
VF1$Task.e[VF1$Task == "Race"] = 1

# Task x Race
m2 = lmer(meanAmp_factor ~ Race.e*Task.e + (Race.e+Task.e|Subject) + (1|Electrode), data = VF1)
summary(m2)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF1_TaskxRace.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# Task x Gender
m3 = lmer(meanAmp_factor ~ Gen.e*Task.e + (Gen.e+Task.e|Subject) + (1|Electrode), data = VF1)
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF1_TaskxGen.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

# Task x Fix
m4 = lmer(meanAmp_factor ~ Fix.e*Task.e + (Fix.e+Task.e|Subject) + (1|Electrode), data = VF1)
summary(m4)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF1_TaskxFix.txt", sep=""))
summary(m4)
"________________________________________________________________________________________________"
coef(m4)
sink()

# Task x Race x Gen
m5 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = VF1)
summary(m5)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF1_TaskxRacexGen.txt", sep=""))
summary(m5)
"________________________________________________________________________________________________"
coef(m5)
sink()

# Task x Race x Gen x Fix
m6 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e*Fix.e + (Race.e+Gen.e+Task.e+Fix.e|Subject) + (1|Electrode), data = VF1)
summary(m6)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF1_TaskxRacexGenxFix.txt", sep=""))
summary(m6)
"________________________________________________________________________________________________"
coef(m6)
sink()

# Task x Race x Gen in eye trials
m7 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = filter(VF1, Fix == "eyes"))
summary(m7)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Just eye trials/VF1_TaskxRacexGen.txt", sep=""))
summary(m7)
"________________________________________________________________________________________________"
coef(m7)
sink()

# Task x Race x Gen in forehead trials
m8 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = filter(VF1, Fix == "fore"))
summary(m8)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Just forehead trials/VF1_TaskxRacexGen.txt", sep=""))
summary(m8)
"________________________________________________________________________________________________"
coef(m8)
sink()

# VT-2 --------------------------------------------------------------------

gen = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF2_longDatForMLM.txt", sep=""))
race = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF2_longDatForMLM.txt", sep=""))

gen$Task = "Gender"
race$Task = "Race"

VF2 = rbind(gen, race)

# Effect code
VF2$Race.e = -1
VF2$Race.e[VF2$Race == "White"] = 1

VF2$Gen.e = -1
VF2$Gen.e[VF2$Gender == "male"] = 1

VF2$Fix.e = -1
VF2$Fix.e[VF2$Fix == "fore"] = 1

VF2$Task.e = -1
VF2$Task.e[VF2$Task == "Race"] = 1

# Task x Race
m2 = lmer(meanAmp_factor ~ Race.e*Task.e + (Race.e+Task.e|Subject) + (1|Electrode), data = VF2)
summary(m2)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF2_TaskxRace.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# Task x Gender
m3 = lmer(meanAmp_factor ~ Gen.e*Task.e + (Gen.e+Task.e|Subject) + (1|Electrode), data = VF2)
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF2_TaskxGen.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

# Task x Fix
m4 = lmer(meanAmp_factor ~ Fix.e*Task.e + (Fix.e+Task.e|Subject) + (1|Electrode), data = VF2)
summary(m4)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF2_TaskxFix.txt", sep=""))
summary(m4)
"________________________________________________________________________________________________"
coef(m4)
sink()

# Task x Race x Gen
m5 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = VF2)
summary(m5)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF2_TaskxRacexGen.txt", sep=""))
summary(m5)
"________________________________________________________________________________________________"
coef(m5)
sink()

# Task x Race x Gen x Fix
m6 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e*Fix.e + (Race.e+Gen.e+Task.e+Fix.e|Subject) + (1|Electrode), data = VF2)
summary(m6)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF2_TaskxRacexGenxFix.txt", sep=""))
summary(m6)
"________________________________________________________________________________________________"
coef(m6)
sink()

# Task x Race x Gen in eye trials
m7 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = filter(VF2, Fix == "eyes"))
summary(m7)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Just eye trials/VF2_TaskxRacexGen.txt", sep=""))
summary(m7)
"________________________________________________________________________________________________"
coef(m7)
sink()

# Task x Race x Gen in forehead trials
m8 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = filter(VF2, Fix == "fore"))
summary(m8)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Just forehead trials/VF2_TaskxRacexGen.txt", sep=""))
summary(m8)
"________________________________________________________________________________________________"
coef(m8)
sink()

# Race x Gen interaction in all four Task x Fix conditions
m9 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = filter(VF2, Fix == "fore" & Task == "Race"))

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Sep by Task x Fix/VF2_RacexGen_RaceForeTrials.txt", sep=""))
summary(m9)
"________________________________________________________________________________________________"
coef(m9)
sink()

m10 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = filter(VF2, Fix == "fore" & Task == "Gender"))

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Sep by Task x Fix/VF2_RacexGen_GenForeTrials.txt", sep=""))
summary(m10)
"________________________________________________________________________________________________"
coef(m10)
sink()

m11 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = filter(VF2, Fix == "eyes" & Task == "Race"))

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Sep by Task x Fix/VF2_RacexGen_RaceEyesTrials.txt", sep=""))
summary(m11)
"________________________________________________________________________________________________"
coef(m11)
sink()

m12 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = filter(VF2, Fix == "eyes" & Task == "Gender"))

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Sep by Task x Fix/VF2_RacexGen_GenEyesTrials.txt", sep=""))
summary(m12)
"________________________________________________________________________________________________"
coef(m12)
sink()


# VT-3 --------------------------------------------------------------------

gen = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF3_longDatForMLM.txt", sep=""))
race = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF3_longDatForMLM.txt", sep=""))

gen$Task = "Gender"
race$Task = "Race"

VF3 = rbind(gen, race)

# Effect code
VF3$Race.e = -1
VF3$Race.e[VF3$Race == "White"] = 1

VF3$Gen.e = -1
VF3$Gen.e[VF3$Gender == "male"] = 1

VF3$Fix.e = -1
VF3$Fix.e[VF3$Fix == "fore"] = 1

VF3$Task.e = -1
VF3$Task.e[VF3$Task == "Race"] = 1

# Task x Race
m2 = lmer(meanAmp_factor ~ Race.e*Task.e + (Race.e+Task.e|Subject) + (1|Electrode), data = VF3)
summary(m2)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF3_TaskxRace.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# Task x Gender
m3 = lmer(meanAmp_factor ~ Gen.e*Task.e + (Gen.e+Task.e|Subject) + (1|Electrode), data = VF3)
summary(m3)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF3_TaskxGen.txt", sep=""))
summary(m3)
"________________________________________________________________________________________________"
coef(m3)
sink()

# Task x Fix
m4 = lmer(meanAmp_factor ~ Fix.e*Task.e + (Fix.e+Task.e|Subject) + (1|Electrode), data = VF3)
summary(m4)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF3_TaskxFix.txt", sep=""))
summary(m4)
"________________________________________________________________________________________________"
coef(m4)
sink()


# Task x Race x Gen
m5 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = VF3)
summary(m5)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF3_TaskxRacexGen.txt", sep=""))
summary(m5)
"________________________________________________________________________________________________"
coef(m5)
sink()


# Task x Race x Gen x Fix
m6 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e*Fix.e + (Race.e+Gen.e+Task.e+Fix.e|Subject) + (1|Electrode), data = VF3)
summary(m6)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/VF3_TaskxRacexGenxFix.txt", sep=""))
summary(m6)
"________________________________________________________________________________________________"
coef(m6)
sink()

# Task x Race x Gen in eye trials
m7 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = filter(VF3, Fix == "eyes"))
summary(m7)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Just eye trials/VF3_TaskxRacexGen.txt", sep=""))
summary(m7)
"________________________________________________________________________________________________"
coef(m7)
sink()

# Task x Race x Gen in eye trials
m8 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = filter(VF3, Fix == "fore"))
summary(m8)

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Just forehead trials/VF3_TaskxRacexGen.txt", sep=""))
summary(m8)
"________________________________________________________________________________________________"
coef(m8)
sink()


# Race x Gen interaction in all four Task x Fix conditions
m9 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = filter(VF3, Fix == "fore" & Task == "Race"))

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Sep by Task x Fix/VF3_RacexGen_RaceForeTrials.txt", sep=""))
summary(m9)
"________________________________________________________________________________________________"
coef(m9)
sink()

m10 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = filter(VF3, Fix == "fore" & Task == "Gender"))

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Sep by Task x Fix/VF3_RacexGen_GenForeTrials.txt", sep=""))
summary(m10)
"________________________________________________________________________________________________"
coef(m10)
sink()

m11 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = filter(VF3, Fix == "eyes" & Task == "Race"))

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Sep by Task x Fix/VF3_RacexGen_RaceEyesTrials.txt", sep=""))
summary(m11)
"________________________________________________________________________________________________"
coef(m11)
sink()

m12 = lmer(meanAmp_factor ~ Race.e*Gen.e + (Race.e+Gen.e|Subject) + (1|Electrode), data = filter(VF3, Fix == "eyes" & Task == "Gender"))

sink(file = paste("6 PCA/5 For MLM analysis/Model outputs/Effect of Task/Sep by Task x Fix/VF3_RacexGen_GenEyesTrials.txt", sep=""))
summary(m12)
"________________________________________________________________________________________________"
coef(m12)
sink()


# Plots of mean amp of all PCA factors separated by stim condition --------


VF1gen = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF1_longDatForMLM.txt", sep=""))
VF1race = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF1_longDatForMLM.txt", sep=""))

VF2gen = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF2_longDatForMLM.txt", sep=""))
VF2race = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF2_longDatForMLM.txt", sep=""))

VF3gen = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF3_longDatForMLM.txt", sep=""))
VF3race = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF3_longDatForMLM.txt", sep=""))

VF1gen$Task = "Gender"
VF2gen$Task = "Gender"
VF3gen$Task = "Gender"
VF1race$Task = "Race"
VF2race$Task = "Race"
VF3race$Task = "Race"

VF1gen$Component = "VF1"
VF1race$Component = "VF1"
VF2gen$Component = "VF2"
VF2race$Component = "VF2"
VF3gen$Component = "VF3"
VF3race$Component = "VF3"

alldat = rbind(VF1gen, VF1race, VF2gen, VF2race, VF3gen, VF3race)


#facet_labels = c(Black = "Black participants", White = "White participants")
ggplot(filter(alldat, Component == "VF1"), aes(Race, meanAmp_factor, fill = Gender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #facet_wrap(~ParRace, labeller=labeller(ParRace = facet_labels)) + 
  facet_wrap(~Fix*Task) +
  #  ggtitle("Total number of errors") +
  labs(y = "VF1 amplitude", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Target Gender")) +
  theme_bw() +
#  ggtitle("Gender task") +
#  coord_cartesian(ylim = c(400, 500)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))

ggsave("6 PCA/4 Figures/PCA amps/VF1.jpg")

#facet_labels = c(Black = "Black participants", White = "White participants")
ggplot(filter(alldat, Component == "VF2"), aes(Race, meanAmp_factor, fill = Gender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #facet_wrap(~ParRace, labeller=labeller(ParRace = facet_labels)) + 
  facet_wrap(~Fix*Task) +
  #  ggtitle("Total number of errors") +
  labs(y = "VF2 amplitude", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Target Gender")) +
  theme_bw() +
  #  ggtitle("Gender task") +
  #  coord_cartesian(ylim = c(400, 500)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))

ggsave("6 PCA/4 Figures/PCA amps/VF2.jpg")

#facet_labels = c(Black = "Black participants", White = "White participants")
ggplot(filter(alldat, Component == "VF3"), aes(Race, meanAmp_factor, fill = Gender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #facet_wrap(~ParRace, labeller=labeller(ParRace = facet_labels)) + 
  facet_wrap(~Fix*Task) +
  #  ggtitle("Total number of errors") +
  labs(y = "VF3 amplitude", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Target Gender")) +
  theme_bw() +
  #  ggtitle("Gender task") +
  #  coord_cartesian(ylim = c(400, 500)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))

ggsave("6 PCA/4 Figures/PCA amps/VF3.jpg")
