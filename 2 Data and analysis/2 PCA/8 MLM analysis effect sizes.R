require(lme4)
require(lmerTest)
require(dplyr)
require(MuMIn)

# Cohen's f^2 (Aiken & West)
# f^2 = (R^2[full] - R^2[reduced])/(1 - R^2[full])

# VT-2 --------------------------------------------------------------------


# Gender Task -------------------------------------------------------------

VT2 = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF2_longDatForMLM.txt", sep=""))

VT2$Race.e = -1
VT2$Race.e[VT2$Race == "White"] = 1

VT2$Gen.e = -1
VT2$Gen.e[VT2$Gender == "male"] = 1

VT2$Fix.e = -1
VT2$Fix.e[VT2$Fix == "fore"] = 1

full = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (1|Subject) + (1|Electrode), data = VT2)
summary(full)
f = r.squaredGLMM(full)

geneffect = lmer(meanAmp_factor ~ Race.e+Fix.e + (1|Subject) + (1|Electrode), data = VT2)
summary(geneffect)
g = r.squaredGLMM(geneffect)

raceeffect = lmer(meanAmp_factor ~ Gen.e+Fix.e + (1|Subject) + (1|Electrode), data = VT2)
summary(raceeffect)
r = r.squaredGLMM(raceeffect)

# Effect of gender
(f[2] - g[2])/(1-f[2])

# Effect of race
(f[2] - r[2])/(1-f[2])


# Race Task ---------------------------------------------------------------



VT2 = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF2_longDatForMLM.txt", sep=""))

VT2$Race.e = -1
VT2$Race.e[VT2$Race == "White"] = 1

VT2$Gen.e = -1
VT2$Gen.e[VT2$Gender == "male"] = 1

VT2$Fix.e = -1
VT2$Fix.e[VT2$Fix == "fore"] = 1

full = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (1|Subject) + (1|Electrode), data = VT2)
summary(full)
f = r.squaredGLMM(full)

geneffect = lmer(meanAmp_factor ~ Race.e+Fix.e + (1|Subject) + (1|Electrode), data = VT2)
summary(geneffect)
g = r.squaredGLMM(geneffect)

raceeffect = lmer(meanAmp_factor ~ Gen.e+Fix.e + (1|Subject) + (1|Electrode), data = VT2)
summary(raceeffect)
r = r.squaredGLMM(raceeffect)

# Effect of gender
(f[2] - g[2])/(1-f[2])

# Effect of race
(f[2] - r[2])/(1-f[2])



# VT-3 --------------------------------------------------------------------


# Gender Task -------------------------------------------------------------

VT3 = read.delim(paste("6 PCA/5 For MLM analysis/Gen_VF3_longDatForMLM.txt", sep=""))

VT3$Race.e = -1
VT3$Race.e[VT3$Race == "White"] = 1

VT3$Gen.e = -1
VT3$Gen.e[VT3$Gender == "male"] = 1

VT3$Fix.e = -1
VT3$Fix.e[VT3$Fix == "fore"] = 1

full = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (1|Subject) + (1|Electrode), data = VT3)
summary(full)
f = r.squaredGLMM(full)

geneffect = lmer(meanAmp_factor ~ Race.e+Fix.e + (1|Subject) + (1|Electrode), data = VT3)
summary(geneffect)
g = r.squaredGLMM(geneffect)

raceeffect = lmer(meanAmp_factor ~ Gen.e+Fix.e + (1|Subject) + (1|Electrode), data = VT3)
summary(raceeffect)
r = r.squaredGLMM(raceeffect)

# Effect of gender
(f[2] - g[2])/(1-f[2])

# Effect of race
(f[2] - r[2])/(1-f[2])


# Race Task ---------------------------------------------------------------



VT3 = read.delim(paste("6 PCA/5 For MLM analysis/Race_VF3_longDatForMLM.txt", sep=""))

VT3$Race.e = -1
VT3$Race.e[VT3$Race == "White"] = 1

VT3$Gen.e = -1
VT3$Gen.e[VT3$Gender == "male"] = 1

VT3$Fix.e = -1
VT3$Fix.e[VT3$Fix == "fore"] = 1

full = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (1|Subject) + (1|Electrode), data = VT3)
summary(full)
f = r.squaredGLMM(full)

geneffect = lmer(meanAmp_factor ~ Race.e+Fix.e + (1|Subject) + (1|Electrode), data = VT3)
summary(geneffect)
g = r.squaredGLMM(geneffect)

raceeffect = lmer(meanAmp_factor ~ Gen.e+Fix.e + (1|Subject) + (1|Electrode), data = VT3)
summary(raceeffect)
r = r.squaredGLMM(raceeffect)

# Effect of gender
(f[2] - g[2])/(1-f[2])

# Effect of race
(f[2] - r[2])/(1-f[2])





