require(ggplot2)
require(dplyr)

# This script contains:
# 1. Breakdown of race for participants included in PCA analysis
# 2. Creation of plots superimposing PCA waveforms on grand averaged ERPs for each task separately.
# 3. Determination of which electrodes to include in the analysis for each of the PCs

# 1. Determine number and composition of participants included in PCA --------

badSs = read.delim("./6 PCA/6 Tasks together (Revision 1)/badSubs_both.txt")

P.race = read.delim("ParticipantRace.txt")

Included = P.race[!(P.race$Subject %in% badSs$Subject),]

nrow(Included[Included$ParRace == "White",])
nrow(Included[Included$ParRace == "Black",])

# For reading in PCA waveforms --------------------------------------------

ced = read.delim("./6 PCA/6 Tasks together (Revision 1)/CED_RaceGen_Lab2.txt", header = T) # ced used in EP toolkit (excludes EOG and nose but includes mastoid references)
factorelec = as.character(ced$labels)
factorelec = factorelec[c(1:33,35:36)]

none = element_blank()


# 2. Plots of grand averages + PCA waveforms --------------------------------
# Race Task ---------------------------------------------------------------
# Create average waveforms for each task

PCAfact = c("TF06SF1", "TF07SF1", "TF08SF1")
PCAcond = c("RBFE", "RBME", "RWFE", "RWME", "RBFF", "RBMF", "RWFF", "RWMF")
PCArace = NULL
for (l in PCAfact) {
  for (m in PCAcond) {
    temp = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/2 Files from Matlab (Try #4)/BothTasks_121_unmark2_e_TS_grand average_____", m, "_", l, ".txt", sep=""), header = F)
    names(temp) = factorelec
    # add identifiers
    temp$Time = (-50:500*2)
    temp$Waveform = l
    temp$Condition = m
    PCArace = rbind(PCArace, temp)
  }
}

PCArace = group_by(PCArace, Time, Waveform) %>% 
  select(-Condition) %>% 
  summarise_all(mean) %>% 
  as.data.frame()

# Integrate with grand averages from race task 

grand = read.delim("./4 Grand averages/Files/Race_RaceXGenXFix_allsubjects.txt")

datCPZ = select(grand, CPZ, Time, Condition) %>%
  rename(Waveform = Condition, Electrode = CPZ) %>% 
  rbind(select(PCArace, CPZ, Time, Waveform) %>% # add PC-1 at CPZ
          rename(Electrode = CPZ) %>% 
          filter(Waveform == "TF07SF1")) %>% 
  rbind(select(PCArace, CPZ, Time, Waveform) %>% # add PC-2 at CPZ
          rename(Electrode = CPZ) %>% 
          filter(Waveform == "TF08SF1")) %>%
  rbind(select(PCArace, CPZ, Time, Waveform) %>% # add PC-3 at CPZ
          rename(Electrode = CPZ) %>% 
          filter(Waveform == "TF06SF1"))
datCPZ$Waveform = factor(datCPZ$Waveform)

# Plot Race Task grand averages + PCA waveforms at CPZ 

CPZcolors <- c("Black_female_eyes" = "black", 
               "Black_female_fore" = "black", 
               "White_female_eyes" = "black", 
               "White_female_fore" = "black",
               "Black_male_eyes" = "black", 
               "Black_male_fore" = "black", 
               "White_male_eyes" = "black", 
               "White_male_fore" = "black",
               "TF07SF1" = "darkgreen",   # PC-1
               "TF08SF1" = "dodgerblue2", # PC-2
               "TF06SF1" = "violetred2")  # PC-3

plot.CPZ =
  ggplot(data=datCPZ, aes(Time, Electrode, group = Waveform)) + 
  geom_line(           # adds lines for waveforms
            linetype="solid",
            aes(color = Waveform, size = Waveform)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0, linetype="dashed") + # adds line for stim onset
  #annotate("text", label = "CPZ", x = -30, y = -6.5, size = 8, colour = "black") + # add electrode label
  theme_bw() + 
  theme(panel.grid.major.x = none, 
        panel.grid.minor.x = none,
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.position="none") +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 400), 
                     expand=c(0,0)   # expand=c(0,0) removes extra space before & after data
                     #breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
                     ) +
  scale_y_continuous("Amplitude (uV)", limits =c(-4, 8), expand=c(0,0)) +  
  scale_color_manual(values=alpha(CPZcolors, c(.3, .3, .3, .3, .3, .3, .3, .3, 1, 1, 1))) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 2, 2, 2))

ggsave(filename="./6 PCA/6 Tasks together (Revision 1)/3 Figures/Grand averages + PCA (Try #4)/Race_CPZ_rawERPs_TSfactors.png", plot.CPZ, width=7, height=6, units = "in")


# Gender Task ---------------------------------------------------------------

# Read in PCA waveform data 
# Create average waveforms for each task

PCAfact = c("TF06SF1", "TF07SF1", "TF08SF1")
PCAcond = c("GBFE", "GBME", "GWFE", "GWME", "GBFF", "GBMF", "GWFF", "GWMF")
PCAgen = NULL
for (l in PCAfact) {
  for (m in PCAcond) {
    temp = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/2 Files from Matlab (Try #4)/BothTasks_121_unmark2_e_TS_grand average_____", m, "_", l, ".txt", sep=""), header = F)
    names(temp) = factorelec
    # add identifiers
    temp$Time = (-50:500*2)
    temp$Waveform = l
    temp$Condition = m
    PCAgen = rbind(PCAgen, temp)
  }
}

PCAgen = group_by(PCAgen, Time, Waveform) %>% 
  select(-Condition) %>% 
  summarise_all(mean) %>% 
  as.data.frame()

# Integrate with grand averages from gender task 

grand = read.delim("./4 Grand averages/Files/Gender_RaceXGenXFix_allsubjects.txt")

datCPZ = select(grand, CPZ, Time, Condition) %>%
  rename(Waveform = Condition, Electrode = CPZ) %>% 
  rbind(select(PCAgen, CPZ, Time, Waveform) %>% # add PC-1 at CPZ
          rename(Electrode = CPZ) %>% 
          filter(Waveform == "TF07SF1")) %>% 
  rbind(select(PCAgen, CPZ, Time, Waveform) %>% # add PC-2 at CPZ
          rename(Electrode = CPZ) %>% 
          filter(Waveform == "TF08SF1")) %>%
  rbind(select(PCAgen, CPZ, Time, Waveform) %>% # add PC-3 at CPZ
          rename(Electrode = CPZ) %>% 
          filter(Waveform == "TF06SF1"))
datCPZ$Waveform = factor(datCPZ$Waveform)

# Plot Gender Task grand averages + PCA waveforms at CPZ

CPZcolors <- c("Black_female_eyes" = "black", 
               "Black_female_fore" = "black", 
               "White_female_eyes" = "black", 
               "White_female_fore" = "black",
               "Black_male_eyes" = "black", 
               "Black_male_fore" = "black", 
               "White_male_eyes" = "black", 
               "White_male_fore" = "black",
               "TF07SF1" = "darkgreen",   # PC-1
               "TF08SF1" = "dodgerblue2", # PC-2
               "TF06SF1" = "violetred2")  # PC-3

plot.CPZ =
  ggplot(data=datCPZ, aes(Time, Electrode, group = Waveform)) + 
  geom_line(          # adds lines for waveforms
            linetype="solid",
            aes(color = Waveform, size = Waveform)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0, linetype="dashed") + # adds line for stim onset
  #annotate("text", label = "CPZ", x = -30, y = -6.5, size = 8, colour = "black") + # add electrode label
  theme_bw() + 
  theme(panel.grid.major.x = none, 
        panel.grid.minor.x = none,
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.position="none") +
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 400), 
                     expand=c(0,0)   # expand=c(0,0) removes extra space before & after data
                     #breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  ) +
  scale_y_continuous("Amplitude (uV)", limits =c(-4, 8), expand=c(0,0)) +  
  scale_color_manual(values=alpha(CPZcolors, c(.3, .3, .3, .3, .3, .3, .3, .3, 1, 1, 1))) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 2, 2, 2))

ggsave(filename="./6 PCA/6 Tasks together (Revision 1)/3 Figures/Grand averages + PCA (Try #4)/Gender_CPZ_rawERPs_TSfactors.png", plot.CPZ, width=7, height=6, units = "in")



# 3. Figure out which electrodes to use for each virtual waveform ------------

# Use same electrodes across tasks

## VF-1 (TF07_SF1)
VT1 = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/2 Files from Matlab (Try #4)/BothTasks_121_unmark2_e_TS_grand average_combined_TF07SF1.txt", sep=""), header = F)
names(VT1) = factorelec
VT1$Time = (-50:500*2)

# Look at 80-140 ms (negative going)
set1 = VT1[VT1$Time > 80 & VT1$Time < 140,] %>%
  summarise_all(min) %>%
  sort()
plot(as.character(set1[1:20]))
# Use 7 electrodes: PZ, CPZ, POZ, CP1, CP2, PO3, PO4

## VT-2 (TF08_SF1)
VT2 = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/2 Files from Matlab (Try #4)/BothTasks_121_unmark2_e_TS_grand average_combined_TF08SF1.txt", sep=""), header = F)
names(VT2) = factorelec
VT2$Time = (-50:500*2)

# Look at 115-180 ms (positive going)
set2 = VT2[VT2$Time > 115 & VT2$Time < 180,] %>%
  summarise_all(max) %>%
  sort()
plot(as.character(set2[1:20]))
# Use 7 electrodes: CPZ, CP1, CP2, PZ, CZ, C1, C2

## VT-3 (TF06_SF1)
VT3 = read.delim(paste("./6 PCA/6 Tasks together (Revision 1)/2 Files from Matlab (Try #4)/BothTasks_121_unmark2_e_TS_grand average_combined_TF06SF1.txt", sep=""), header = F)
names(VT3) = factorelec
VT3$Time = (-50:500*2)

# Look at 145-230 ms (positive going)
set3 = VT3[VT3$Time > 145 & VT3$Time < 230,] %>%
  summarise_all(max) %>%
  sort()
plot(as.character(set3[1:20]))
# Use 7 electrodes: CZ, C1, C2, CPZ, PZ, CP1, CP2 



