#EXAMPLE DATA

#load package
require(simr)
require(dplyr)

dat = read.delim("./5 P2/AllSubs_acceptedTrials_long_nobe_nobs.txt")

dat$TarRace.e = -1
dat$TarRace.e[dat$TarRace == "White"] = 1

dat$Task.e = -1
dat$Task.e[dat$Task == "Race"] = 1

dat$TarGender.e = -1
dat$TarGender.e[dat$TarGender == "male"] = 1



#specify model
m1 = lmer(value ~ TarRace.e*Task.e + (1|Subject) + (1|Electrode), data = dat)

fixef(m1)["TarRace.e:Task.e"]
fixef(m1)["TarRace.e:Task.e"]<- -0.1

#run simulation (for more than one predictor)
ps1 = powerSim(m1, test=fixed("TarRace.e:Task.e", method = "z"), nsim=1000)
pc1 = powerCurve(m1, test=fixed("TarRace.e:Task.e", method = "z"), along="Subject", nsim=1000)

# again, with smaller effect size
m2 = lmer(value ~ TarRace.e*Task.e + (1|Subject) + (1|Electrode), data = dat)

fixef(m2)["TarRace.e:Task.e"]
fixef(m2)["TarRace.e:Task.e"]<- -0.01

#run simulation (for more than one predictor)
ps2 = powerSim(m2, test=fixed("TarRace.e:Task.e", method = "z"), nsim=1000)
pc2 = powerCurve(m2, test=fixed("TarRace.e:Task.e", method = "z"), along="Subject", nsim=1000)

# again, with smaller effect size
m3 = lmer(value ~ TarRace.e*Task.e + (1|Subject) + (1|Electrode), data = dat)

fixef(m3)["TarRace.e:Task.e"]
fixef(m3)["TarRace.e:Task.e"]<- -0.05

#run simulation (for more than one predictor)
ps3 = powerSim(m3, test=fixed("TarRace.e:Task.e", method = "z"), nsim=1000)
pc3 = powerCurve(m3, test=fixed("TarRace.e:Task.e", method = "z"), along="Subject", nsim=1000)

plotdat = summary(pc3) %>% as.data.frame()
plotdat$mean_cl_normal = (plotdat$upper - plotdat$lower)/2

require(ggplot2)
ggplot(plotdat, aes(nlevels, mean)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "line") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=1,
                position=position_dodge(.9)) +
  scale_x_continuous("Sample size",
                     breaks=c(0:14)*5) +
  scale_y_continuous("Power",
                     breaks=c(0:100)*.1) +
  ggtitle("b = -0.05") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))



# From Kelsey -------------------------------------------------------------

#EXAMPLE DATA

#load package
library(simr)

#load data
setwd("/Users/Kelsey/Documents/Grad School/Research/Projects/Thesis/Working Directory")
View(simdata)

#specify model
model1 <- glmer(z~x+(1|g), family="poisson", data=simdata)

#determine fixed effect
summary(model1)
fixef(model1)["x"]
fixef(model1)["x"]<- -0.05

#run simulation
powerSim(model1, nsim=10)

#see what adding more participants would do
model2<-extend(model1, along="x", n=20)
powerSim(model2, nsim=10)

#view power curve to determine min sample size needed
pc2 <- powerCurve(model2, nsim=10)
print(pc2)
plot(pc2)

#with multiple predictors
#specify model
model1 <- glmer(z~x*y+(1|g), family="poisson", data=simdata)

#determine fixed effect
summary(model1)
fixef(model1)["x"]
fixef(model1)["x"]<- 0.05
fixef(model1)["y"]
fixef(model1)["y"]<- 0.2
fixef(model1)["x:y"]
fixef(model1)["x:y"]<- -0.01

#run simulation
powerSim(model1, test=fixed("x:y", method = "z"), nsim=10)



#MY DATA

#load package
library(simr)

#load data and subset
setwd("/Users/Kelsey/Documents/Grad School/Research/Projects/Thesis/Working Directory")
Data <- read.csv("Data.csv")
View(Data)
Data <- subset(Data, Task=="2" & Type=="Difference" & Trials=="All")
Data <- subset(Data, Group=="1")
View(Data)
Data <- Data[-c(2:4, 7:62, 64)]
Data<-na.omit(Data)
View(Data)

#specify model
m1 = lmer(RewP.mean.amp ~ Savor.Success + (1|Subject) + (1|Channel), data = Data, REML = TRUE)

#determine fixed effect
summary(m1)
fixef(m1)["Savor.Success"]

#run simulation
powerSim(m1, nsim=10)

#power curve
pc <- powerCurve(m1, nsim=10)
print(pc)
plot(pc)
#






