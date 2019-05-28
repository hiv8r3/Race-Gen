require(lme4)
require(lmerTest)
require(dplyr)

# This script contains:
# 1. MLM comparing amplitude of PC-2 and PC-3 in each task.
# 2. MLMs testing effects of target race, target gender, and fixation separately on each PC and separate for each task (6 models total)
# 3. MLMs examining effect of Task
# 4. Effect size calculations for Task x Target Race and Task x Target Gender interactions
# 5. Bar plots of mean amp of PCs separated by condition
# 6. MLMs examining effect of Participant race
# 7. Gender x Task x PC and Race x Task x PC interactions (requested by reviewer 2)
# 8. Within-subject plots (requested by reviewer 1)


# 1. MLM comparing amplitude of PC-2 and PC-3 in each task. -------------

VT2 = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/4 For MLM analysis/BothTasks_VF2_longDatForMLM.txt", sep=""))
VT2$PC = "VT2"

VT3 = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/4 For MLM analysis/BothTasks_VF3_longDatForMLM.txt", sep=""))
VT3$PC = "VT3"

racetask = rbind(VT2[VT2$Task == "Race",],
                 VT3[VT3$Task == "Race",])

racetask$PC.e = -1
racetask$PC.e[racetask$PC == "VT3"] = 1

m1 = lmer(scale(meanAmp_factor) ~ PC.e + (PC.e|Subject) + (1|Electrode), data = racetask)
summary(m1)
# electrode doesn't account for any variance because electrodes are perfectly correlated. Confirmed with Joe Dien that this is expected
tapply(racetask$meanAmp_factor, racetask$PC, mean)

gentask = rbind(filter(VT2, Task == "Gender"),
                filter(VT3, Task == "Gender"))
gentask$PC.e = -1
gentask$PC.e[gentask$PC == "VT3"] = 1

m2 = lmer(scale(meanAmp_factor) ~ PC.e + (PC.e|Subject) + (1|Electrode), data = gentask)
summary(m2)

tapply(gentask$meanAmp_factor, gentask$PC, mean)


# 2. MLMs testing effects of target race, target gender, and fix --------

# VT-1 --------------------------------------------------------------------

VT1 = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/4 For MLM analysis/BothTasks_VF1_longDatForMLM.txt", sep=""))

# Effect code
VT1$Race.e = -1
VT1$Race.e[VT1$Race == "White"] = 1

VT1$Gen.e = -1
VT1$Gen.e[VT1$Gender == "male"] = 1

VT1$Fix.e = -1
VT1$Fix.e[VT1$Fix == "fore"] = 1


# Gender ------------------------------------------------------------------

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT1, Task == "Gender"))
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT1, Task == "Gender"))
summary(m2)

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/Gen_VF1_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# Race ------------------------------------------------------------------

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT1, Task == "Race"))
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT1, Task == "Race"))
summary(m2)

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/Race_VF1_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()


# VT-2 --------------------------------------------------------------------

VT2 = read.delim(paste("6 PCA/6 Tasks together (Revision 1)/4 For MLM analysis/BothTasks_VF2_longDatForMLM.txt", sep=""))

# Effect code
VT2$Race.e = -1
VT2$Race.e[VT2$Race == "White"] = 1

VT2$Gen.e = -1
VT2$Gen.e[VT2$Gender == "male"] = 1

VT2$Fix.e = -1
VT2$Fix.e[VT2$Fix == "fore"] = 1


# Gender ------------------------------------------------------------------

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Gender"))
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Gender"))
summary(m2)

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/Gen_VF2_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# Race ------------------------------------------------------------------

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Race"))
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Race"))
summary(m2)

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/Race_VF2_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()



# VT-3 --------------------------------------------------------------------

VT3 = read.delim(paste("6 PCA/6 Tasks together (Revision 1)/4 For MLM analysis/BothTasks_VF3_longDatForMLM.txt", sep=""))

# Effect code
VT3$Race.e = -1
VT3$Race.e[VT3$Race == "White"] = 1

VT3$Gen.e = -1
VT3$Gen.e[VT3$Gender == "male"] = 1

VT3$Fix.e = -1
VT3$Fix.e[VT3$Fix == "fore"] = 1


# Gender ------------------------------------------------------------------

# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Gender"))
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Gender"))
summary(m2)

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/Gen_VF3_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# Race ------------------------------------------------------------------


# Intercept only
m1 = lmer(scale(meanAmp_factor) ~ 1 + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Race"))
summary(m1)

# Full model
m2 = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Race"))
summary(m2)

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/Race_VF3_EffectCodedOutput.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()


# 3. MLMs examining effect of Task --------------------------------------------------

# VT-1 --------------------------------------------------------------------

# Effect code
VT1$Task.e = -1
VT1$Task.e[VT1$Task == "Race"] = 1

# Task x Race x Gen
m5 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = VT1)
summary(m5)

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/BothTasks_VF1_TaskxRacexGen.txt", sep=""))
summary(m5)
"________________________________________________________________________________________________"
coef(m5)
sink()


# VT-2 --------------------------------------------------------------------

# Effect code
VT2$Task.e = -1
VT2$Task.e[VT2$Task == "Race"] = 1

# Task x Race x Gen
m5 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = VT2)
summary(m5)

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/BothTasks_VF2_TaskxRacexGen.txt", sep=""))
summary(m5)
"________________________________________________________________________________________________"
coef(m5)
sink()

# VT-3 --------------------------------------------------------------------

# Effect code
VT3$Task.e = -1
VT3$Task.e[VT3$Task == "Race"] = 1

# Task x Race x Gen
m5 = lmer(meanAmp_factor ~ Race.e*Gen.e*Task.e + (Race.e+Gen.e+Task.e|Subject) + (1|Electrode), data = VT3)
summary(m5)

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/BothTasks_VF3_TaskxRacexGen.txt", sep=""))
summary(m5)
"________________________________________________________________________________________________"
coef(m5)
sink()


# 4. Effect size calculations for Task x Race and Task x Gen interactions --------

require(MuMIn)

# Cohen's f^2 (Aiken & West)
# f^2 = (R^2[full] - R^2[reduced])/(1 - R^2[full])

# VT-2 --------------------------------------------------------------------

# Gender Task -------------------------------------------------------------

race.gt = lmer(meanAmp_factor ~ Race.e + (1|Subject) + (1|Electrode), data = filter(VT2, Task == "Gender"))
summary(race.gt)
r.gt = r.squaredGLMM(race.gt)

gen.gt = lmer(meanAmp_factor ~ Gen.e + (1|Subject) + (1|Electrode), data = filter(VT2, Task == "Gender"))
summary(gen.gt)
g.gt = r.squaredGLMM(gen.gt)

empty.gt = lmer(meanAmp_factor ~ 1 + (1|Subject) + (1|Electrode), data = filter(VT2, Task == "Gender"))
summary(empty.gt)
e.gt = r.squaredGLMM(empty.gt)

# Effect of gender
(g.gt[2] - e.gt[2])/(1-g.gt[2])

# Effect of race
(r.gt[2] - e.gt[2])/(1-r.gt[2])


# Race Task ---------------------------------------------------------------

race.rt = lmer(meanAmp_factor ~ Race.e + (1|Subject) + (1|Electrode), data = filter(VT2, Task == "Race"))
summary(race.rt)
r.rt = r.squaredGLMM(race.rt)

gen.rt = lmer(meanAmp_factor ~ Gen.e + (1|Subject) + (1|Electrode), data = filter(VT2, Task == "Race"))
summary(gen.rt)
g.rt = r.squaredGLMM(gen.rt)

empty.rt = lmer(meanAmp_factor ~ 1 + (1|Subject) + (1|Electrode), data = filter(VT2, Task == "Race"))
summary(empty.rt)
e.rt = r.squaredGLMM(empty.rt)

# Effect of gender
(g.rt[2] - e.rt[2])/(1-g.rt[2])

# Effect of race
(r.rt[2] - e.rt[2])/(1-r.rt[2])


# VT-3 --------------------------------------------------------------------

# Gender Task -------------------------------------------------------------

race.gt = lmer(meanAmp_factor ~ Race.e + (1|Subject) + (1|Electrode), data = filter(VT3, Task == "Gender"))
summary(race.gt)
r.gt = r.squaredGLMM(race.gt)

gen.gt = lmer(meanAmp_factor ~ Gen.e + (1|Subject) + (1|Electrode), data = filter(VT3, Task == "Gender"))
summary(gen.gt)
g.gt = r.squaredGLMM(gen.gt)

empty.gt = lmer(meanAmp_factor ~ 1 + (1|Subject) + (1|Electrode), data = filter(VT3, Task == "Gender"))
summary(empty.gt)
e.gt = r.squaredGLMM(empty.gt)

# Effect of gender
(g.gt[2] - e.gt[2])/(1-g.gt[2])

# Effect of race
(r.gt[2] - e.gt[2])/(1-r.gt[2])



# Race Task ---------------------------------------------------------------

race.rt = lmer(meanAmp_factor ~ Race.e + (1|Subject) + (1|Electrode), data = filter(VT3, Task == "Race"))
summary(race.rt)
r.rt = r.squaredGLMM(race.rt)

gen.rt = lmer(meanAmp_factor ~ Gen.e + (1|Subject) + (1|Electrode), data = filter(VT3, Task == "Race"))
summary(gen.rt)
g.rt = r.squaredGLMM(gen.rt)

empty.rt = lmer(meanAmp_factor ~ 1 + (1|Subject) + (1|Electrode), data = filter(VT3, Task == "Race"))
summary(empty.rt)
e.rt = r.squaredGLMM(empty.rt)

# Effect of gender
(g.rt[2] - e.rt[2])/(1-g.rt[2])

# Effect of race
(r.rt[2] - e.rt[2])/(1-r.rt[2])





# 5. Plots of mean amp of all PCA factors separated by stim condition --------

# separated by fixation

#facet_labels = c(Black = "Black participants", White = "White participants")
ggplot(VT1, aes(Race, meanAmp_factor, fill = Gender)) +
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

ggsave("6 PCA/6 Tasks together (Revision 1)/3 Figures/Mean PCA amplitude/VF1_separatedbyfix.jpg")

#facet_labels = c(Black = "Black participants", White = "White participants")
ggplot(VT2, aes(Race, meanAmp_factor, fill = Gender)) +
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

ggsave("6 PCA/6 Tasks together (Revision 1)/3 Figures/Mean PCA amplitude/VF2_separatedbyfix.jpg")

#facet_labels = c(Black = "Black participants", White = "White participants")
ggplot(VT3, aes(Race, meanAmp_factor, fill = Gender)) +
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

ggsave("6 PCA/6 Tasks together (Revision 1)/3 Figures/Mean PCA amplitude/VF3_separatedbyfix.jpg")


# collapsed across fixation

#facet_labels = c(Black = "Black participants", White = "White participants")
ggplot(VT1, aes(Race, meanAmp_factor, fill = Gender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #facet_wrap(~ParRace, labeller=labeller(ParRace = facet_labels)) + 
  facet_wrap(~Task) +
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

ggsave("6 PCA/6 Tasks together (Revision 1)/3 Figures/Mean PCA amplitude/VF1.jpg")

#facet_labels = c(Black = "Black participants", White = "White participants")
ggplot(VT2, aes(Race, meanAmp_factor, fill = Gender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #facet_wrap(~ParRace, labeller=labeller(ParRace = facet_labels)) + 
  facet_wrap(~Task) +
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

ggsave("6 PCA/6 Tasks together (Revision 1)/3 Figures/Mean PCA amplitude/VF2.jpg")

#facet_labels = c(Black = "Black participants", White = "White participants")
ggplot(VT3, aes(Race, meanAmp_factor, fill = Gender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #facet_wrap(~ParRace, labeller=labeller(ParRace = facet_labels)) + 
  facet_wrap(~Task) +
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

ggsave("6 PCA/6 Tasks together (Revision 1)/3 Figures/Mean PCA amplitude/VF3.jpg")



# 6. Look at effect of participant race --------------------------------------

# Add ParRace
parRace = read.delim("5 P2/AllSubs_indavgs_long_nobe_nobs.txt")

# VT2
VT2$ParRace = NULL
for (i in unique(VT2$Subject)){
  VT2$ParRace[VT2$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}

race.gt = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Gender")) %>% 
  summary()
race.rt = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Race")) %>% 
  summary()
gen.gt = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Gender")) %>% 
  summary()
gen.rt = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Race")) %>% 
  summary()

# VT3
VT3$ParRace = NULL
for (i in unique(VT3$Subject)){
  VT3$ParRace[VT3$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}

race.gt = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Gender")) %>% 
  summary()
race.rt = lmer(meanAmp_factor ~ Race.e*ParRace + (Race.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Race")) %>% 
  summary()
gen.gt = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Gender")) %>% 
  summary()
gen.rt = lmer(meanAmp_factor ~ Gen.e*ParRace + (Gen.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Race")) %>% 
  summary()


# 7. Combine PC2 and PC3 --------------------------------------------------

VT2$Component = "PC2"
VT2$Component.e = -1

VT3$Component = "PC3"
VT3$Component.e = 1

combine = rbind(VT2, VT3)

# Model selection
lmer(meanAmp_factor ~ 1 + (Race.e*Task.e*Component.e|Subject) + (1|Electrode), data = combine) %>% 
  summary()

lmer(meanAmp_factor ~ 1 + (Race.e+Task.e+Component.e|Subject) + (1|Electrode), data = combine) %>% 
  summary() #use this

# Race x Task x Component
m6 = lmer(meanAmp_factor ~ Race.e*Task.e*Component.e + (Race.e+Task.e+Component.e|Subject) + (1|Electrode), data = combine) %>% 
  summary()

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/Requested/RacexTaskxPC.txt", sep=""))
summary(m6)
"________________________________________________________________________________________________"
coef(m6)
sink()


# Gender x Task x Component
m7 = lmer(meanAmp_factor ~ Gen.e*Task.e*Component.e + (Gen.e+Task.e+Component.e|Subject) + (1|Electrode), data = combine) %>% 
  summary()

sink(file = paste("6 PCA/6 Tasks together (Revision 1)/5 MLM output/Requested/GenderxTaskxPC.txt", sep=""))
summary(m7)
"________________________________________________________________________________________________"
coef(m7)
sink()



# 8. Within subjects plots of effect of task ------------------------------

# Gender Task
m2.gen = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Gender")) 

m3.gen = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Gender")) 

# Race Task
m2.race = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT2, Task == "Race")) 

m3.race = lmer(meanAmp_factor ~ Race.e+Gen.e+Fix.e + (Race.e+Fix.e+Gen.e|Subject) + (1|Electrode), data = filter(VT3, Task == "Race")) 

# Get BLUPs (Effect of Race)

PC2.gen = data.frame(Subject = row.names(coef(m2.gen)$Subject[2]),
                     Race.Effect = coef(m2.gen)$Subject[2],
                     Component = "PC2",
                     Task = "Gender")

PC3.gen = data.frame(Subject = row.names(coef(m3.gen)$Subject[2]),
                     Race.Effect = coef(m3.gen)$Subject[2],
                     Component = "PC3",
                     Task = "Gender")

PC2.race = data.frame(Subject = row.names(coef(m2.race)$Subject[2]),
                     Race.Effect = coef(m2.race)$Subject[2],
                     Component = "PC2",
                     Task = "Race")

PC3.race = data.frame(Subject = row.names(coef(m3.race)$Subject[2]),
                     Race.Effect = coef(m3.race)$Subject[2],
                     Component = "PC3",
                     Task = "Race")

# Combine into data set
RaceEffect = rbind(PC2.gen, PC3.gen, PC2.race, PC3.race)

# add ParRace
parRace = read.delim("5 P2/AllSubs_indavgs_long_nobe_nobs.txt")
for (i in unique(RaceEffect$Subject)){
  RaceEffect$ParRace[RaceEffect$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}


# Get BLUPs (Effect of Race)

PC2.gen = data.frame(Subject = row.names(coef(m2.gen)$Subject[3]),
                     Race.Effect = coef(m2.gen)$Subject[3],
                     Component = "PC2",
                     Task = "Gender")

PC3.gen = data.frame(Subject = row.names(coef(m3.gen)$Subject[3]),
                     Race.Effect = coef(m3.gen)$Subject[3],
                     Component = "PC3",
                     Task = "Gender")

PC2.race = data.frame(Subject = row.names(coef(m2.race)$Subject[3]),
                      Race.Effect = coef(m2.race)$Subject[3],
                      Component = "PC2",
                      Task = "Race")

PC3.race = data.frame(Subject = row.names(coef(m3.race)$Subject[3]),
                      Race.Effect = coef(m3.race)$Subject[3],
                      Component = "PC3",
                      Task = "Race")

# Combine into data set
GenEffect = rbind(PC2.gen, PC3.gen, PC2.race, PC3.race)

# add parRace
for (i in unique(GenEffect$Subject)){
  GenEffect$ParRace[GenEffect$Subject == i] = as.character(unique(parRace$ParRace[parRace$Subject == i]))
}

# plot 
require(ggplot2)

ggplot(filter(RaceEffect, Component == "PC2"), aes(Task, Race.e, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  ylab("Effect of Race") +
  ggtitle("PC-2") +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 22),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  coord_cartesian(ylim =c(-1.72, .85))

ggsave("./6 PCA/4 Figures/Within SS comparison/PC2_EffectofRace.jpg")

ggplot(filter(RaceEffect, Component == "PC2"), aes(Task, Race.e, group = Subject, color = ParRace)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  ylab("Effect of Race") +
  ggtitle("PC-2")  +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 22),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  coord_cartesian(ylim =c(-1.72, .85))

ggsave("./6 PCA/4 Figures/Within SS comparison/PC2_EffectofRace_ParRace.jpg")

ggplot(filter(RaceEffect, Component == "PC3"), aes(Task, Race.e, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  ylab("Effect of Race") +
  ggtitle("PC-3") +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 22),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  coord_cartesian(ylim =c(-1.72, .85))

ggsave("./6 PCA/4 Figures/Within SS comparison/PC3_EffectofRace.jpg")

ggplot(filter(GenEffect, Component == "PC2"), aes(Task, Gen.e, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  ylab("Effect of Gender") +
  ggtitle("PC-2") +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 22),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  coord_cartesian(ylim =c(-1.2, .85))

ggsave("./6 PCA/4 Figures/Within SS comparison/PC2_EffectofGender.jpg")

ggplot(filter(GenEffect, Component == "PC3"), aes(Task, Gen.e, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  ylab("Effect of Gender") +
  ggtitle("PC-3") +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 22),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  coord_cartesian(ylim =c(-1.2, .85))

ggsave("./6 PCA/4 Figures/Within SS comparison/PC3_EffectofGender.jpg")


