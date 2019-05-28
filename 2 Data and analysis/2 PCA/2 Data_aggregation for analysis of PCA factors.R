require(dplyr)
require(tidyr)

# This script contains:
# 1. Quantification of PCA waveform for each condition for each participant in each task.

# for electrode names
ced = read.delim("./6 PCA/6 Tasks together (Revision 1)/CED_RaceGen_Lab2.txt", header = T) # ced used in EP toolkit (excludes EOG and nose but includes mastoid references)
factorelec = as.character(ced$labels)
factorelec = factorelec[c(1:33,35:36)]

condList = c("RBME", "RBMF", "RWME", "RWMF", "RBFE", "RBFF", "RWFE", "RWFF",
             "GBME", "GBMF", "GWME", "GWMF", "GBFE", "GBFF", "GWFE", "GWFF")

subList = c("01", "02", "03", "04", "05", "06", "07", "08", "09", 10:66)

badSs = read.delim("./6 PCA/6 Tasks together (Revision 1)/badSubs_both.txt")
# make sure all are 2 digits
badSs = c("04", "06", badSs$Subject[2:14])

subList = subList[!(subList %in% badSs)]

# VT-1 --------------------------------------------------------------------

# read in data for temporal component TF07_SF1
VT1dat = NULL
for (i in subList) { # for each subject
  for (j in condList) { # for each condition
    temp = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/2 Files from Matlab (Try #4)/BothTasks_121_unmark2_e_TS____________", i, "_____", j, "_TF07SF1.txt", sep=""), header = F)
    names(temp) = factorelec
    temp$Time = (-50:500*2)
    temp$Subject = i
    temp$Condition = j
    VT1dat = rbind(VT1dat, temp)
  }
}
VT1dat$Subject = as.integer(VT1dat$Subject)

# look at what it looks like for different subjects
ggplot(data=VT1dat, aes(Time, CPZ, group = Condition)) + 
  geom_line(lwd=1.1,linetype="solid",aes(color = Condition)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0, linetype="dashed") + # adds line for stim onset +
  geom_vline(xintercept=80) +
  geom_vline(xintercept=140) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5))   # flips y axis


# select electrodes of interest, time of interest (80-140ms)
VT1wide = select(VT1dat, PZ, CPZ, POZ, CP1, CP2, PO3, PO4, Time, Subject, Condition)
VT1wide = VT1wide[VT1wide$Time >= 80 & VT1wide$Time <= 140,]
VT1wide$Condition = factor(VT1wide$Condition)

dat = VT1wide %>% 
  group_by(Subject, Condition) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  as.data.frame()

VT1long = gather(dat[,1:9], "Electrode", "meanAmp_factor", 3:9)
# add condition information
VT1long$Race = rep(c("Black", "White"), each=4)
VT1long$Gender = rep(c("female", "male"), each=2)
VT1long$Fix = rep(c("eyes", "fore"))

VT1long$Task = NA
VT1long$Task[grep("R", VT1long$Condition)] = "Race"
VT1long$Task[grep("G", VT1long$Condition)] = "Gender"

write.table(VT1long, "./6 PCA/6 Tasks together (Revision 1)/4 For MLM analysis/BothTasks_VF1_longDatForMLM.txt", row.names = F, sep = "\t")


# VT-2 --------------------------------------------------------------------

# read in data for temporal component TF08_SF1
VT2dat = NULL
for (i in subList) { # for each subject
  for (j in condList) { # for each condition
    temp = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/2 Files from Matlab (Try #4)/BothTasks_121_unmark2_e_TS____________", i, "_____", j, "_TF08SF1.txt", sep=""), header = F)
    names(temp) = factorelec
    temp$Time = (-50:500*2)
    temp$Subject = i
    temp$Condition = j
    VT2dat = rbind(VT2dat, temp)
  }
}
VT2dat$Subject = as.integer(VT2dat$Subject)

# look at what it looks like for different subjects
ggplot(data=VT2dat, aes(Time, CPZ, group = Condition)) + 
  geom_line(lwd=1.1,linetype="solid",aes(color = Condition)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0, linetype="dashed") + # adds line for stim onset +
  geom_vline(xintercept=115) +
  geom_vline(xintercept=180) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5))   # flips y axis


# select electrodes of interest, time of interest (115-180ms)
VT2wide = select(VT2dat, CPZ, CP1, CP2, PZ, CZ, C1, C2, Time, Subject, Condition)
VT2wide = VT2wide[VT2wide$Time >= 115 & VT2wide$Time <= 180,]
VT2wide$Condition = factor(VT2wide$Condition)

dat = VT2wide %>% 
  group_by(Subject, Condition) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  as.data.frame()

VT2long = gather(dat[,1:9], "Electrode", "meanAmp_factor", 3:9)
# add condition information
VT2long$Race = rep(c("Black", "White"), each=4)
VT2long$Gender = rep(c("female", "male"), each=2)
VT2long$Fix = rep(c("eyes", "fore"))

VT2long$Task = NA
VT2long$Task[grep("R", VT2long$Condition)] = "Race"
VT2long$Task[grep("G", VT2long$Condition)] = "Gender"


write.table(VT2long, "./6 PCA/6 Tasks together (Revision 1)/4 For MLM analysis/BothTasks_VF2_longDatForMLM.txt", row.names = F, sep = "\t")


# VT-3 --------------------------------------------------------------------

# read in data for temporal component TF06_SF1
VT3dat = NULL
for (i in subList) { # for each subject
  for (j in condList) { # for each condition
    temp = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/2 Files from Matlab (Try #4)/BothTasks_121_unmark2_e_TS____________", i, "_____", j, "_TF06SF1.txt", sep=""), header = F)
    names(temp) = factorelec
    temp$Time = (-50:500*2)
    temp$Subject = i
    temp$Condition = j
    VT3dat = rbind(VT3dat, temp)
  }
}
VT3dat$Subject = as.integer(VT3dat$Subject)

# look at what it looks like for different subjects
ggplot(data=VT3dat, aes(Time, CPZ, group = Condition)) + 
  geom_line(lwd=1.1,linetype="solid",aes(color = Condition)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0, linetype="dashed") + # adds line for stim onset +
  geom_vline(xintercept=145) +
  geom_vline(xintercept=230) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  scale_y_reverse("Amplitude (uV)", limits =c(14, -7.5))   # flips y axis


# select electrodes of interest, time of interest (115-180ms)
VT3wide = select(VT3dat, CZ, C1, C2, CPZ, PZ, CP1, CP2, Time, Subject, Condition)
VT3wide = VT3wide[VT3wide$Time >= 145 & VT3wide$Time <= 230,]
VT3wide$Condition = factor(VT3wide$Condition)

dat = VT3wide %>% 
  group_by(Subject, Condition) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  as.data.frame()

VT3long = gather(dat[,1:9], "Electrode", "meanAmp_factor", 3:9)
# add condition information
VT3long$Race = rep(c("Black", "White"), each=4)
VT3long$Gender = rep(c("female", "male"), each=2)
VT3long$Fix = rep(c("eyes", "fore"))

VT3long$Task = NA
VT3long$Task[grep("R", VT3long$Condition)] = "Race"
VT3long$Task[grep("G", VT3long$Condition)] = "Gender"

write.table(VT3long, "./6 PCA/6 Tasks together (Revision 1)/4 For MLM analysis/BothTasks_VF3_longDatForMLM.txt", row.names = F, sep = "\t")
