library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(gridExtra)
library(ez)
library(tibble)
library(sjstats)

####Working Directory different for Apple and Windows!!!#####
#############################################################

#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Motion follows function/99_Analyses")

glossary <- read.csv('Glossary.csv', sep=";")
dataset <- read.csv('Main_Stud_ORG (csv).csv', header=TRUE, sep=";")
names(dataset)

####DATASET WRANGLING####

    #remove random demographic stuff and initial Godspeed items
    dataset <- select(dataset,  -Authorise_Fotos, -W_Language, -i.G.I.1:-i.Comfort)
    #remove DWV perceived legibility 1 and 2: only mean value will be needed
    dataset <- select(dataset, -DWV.Pleg.1_NP:-DWV.G.I.5)
    #remove BR_see,hear,smell etc.
    dataset <- select(dataset, -BR_See:-BR_Sound)
    #further
    dataset <- select(dataset, -D.Time_i_Sex:-D.Time_f_Sex, -D.Perc_Raw_Leg.1:-D.PLeg.2, -D.Pleg.1_Norm:-D.G.I.5)
    dataset <- select(dataset, -S.Time_i_Sex:-S.Time_f_Sex, -S.PLeg.1:-S.PLeg.2, -S.G.V.1:-S.G.I.5)
    dataset <- select(dataset, -L.Time_i_Sex:-L.Time_f_Sex, -L.PLeg.1:-L.PLeg.2, -L.G.V.1:-L.G.I.5)
    dataset <- select(dataset, -E.Time_i_Sex:-E.Time_f_Sex, -E.PLeg.1:-E.PLeg.2, -E.G.V.1:-E.G.I.5)
    dataset <- select(dataset, -LT.Time_i_Sex, LT.Time_i_or_f, -LT.PLeg.1:-LT.PLeg.2, -LT.G.V.1:-LT.G.I.5)
    dataset <- select(dataset, -BR.G.I.1:-BR.Comfort)
    dataset <- select(dataset, -So_Be:-Li_Bl, -Oth)
    dataset <- select(dataset, -L.Seen)
    
    #rename certain columns
    names(dataset)[names(dataset) == "ERR"] <- "ExperienceWithRealRobots"
    names(dataset)[names(dataset) == "LT.Time_i_or_f"] <- "LT.Time_f_Dec"
  
    #new dataset movement cues
    dataset_D <- select(dataset, Participant_Code, D.FD:D.Variant)
    dataset_D <- add_column(dataset_D, MovementCue = 1, .after = "Participant_Code")
    dataset_S <- select(dataset, Participant_Code, S.FD:S.Variant)
    dataset_S <- add_column(dataset_S, MovementCue = 2, .after = "Participant_Code")
    dataset_L <- select(dataset, Participant_Code, L.FD:L.Variant)
    dataset_L <- add_column(dataset_L, MovementCue = 3, .after = "Participant_Code")
    dataset_E <- select(dataset, Participant_Code, E.FD:E.Variant)
    dataset_E <- add_column(dataset_E, MovementCue = 4, .after = "Participant_Code")
    dataset_LT <- select(dataset, Participant_Code, LT.FD:LT.Variant)
    dataset_LT <- add_column(dataset_LT, MovementCue = 5, .after = "Participant_Code")
    
    colnames(dataset_D) <- c("Participant", "MovementCue", "FD","initial_time", "final_time", "observed perceivability", "observed legibility", "perceived legibility", "GI_Anthropomorphism", "GIII_Likeability", "GV_Perceived_Safety", "adequacy", "chosen_variant")
    colnames(dataset_S) <- c("Participant", "MovementCue", "FD","initial_time", "final_time", "observed perceivability", "observed legibility", "perceived legibility", "GI_Anthropomorphism", "GIII_Likeability", "GV_Perceived_Safety", "adequacy", "chosen_variant")
    colnames(dataset_L) <- c("Participant", "MovementCue", "FD","initial_time", "final_time", "observed perceivability", "observed legibility", "perceived legibility", "GI_Anthropomorphism", "GIII_Likeability", "GV_Perceived_Safety", "adequacy", "chosen_variant")
    colnames(dataset_E) <- c("Participant", "MovementCue", "FD","initial_time", "final_time", "observed perceivability", "observed legibility", "perceived legibility", "GI_Anthropomorphism", "GIII_Likeability", "GV_Perceived_Safety", "adequacy", "chosen_variant")
    colnames(dataset_LT) <- c("Participant", "MovementCue", "FD","initial_time", "final_time", "observed perceivability", "observed legibility", "perceived legibility", "GI_Anthropomorphism", "GIII_Likeability", "GV_Perceived_Safety", "adequacy", "chosen_variant")
    
    dataset_MC_long <- rbind(dataset_D, dataset_S, dataset_L, dataset_E, dataset_LT)
    dataset_MC_long$MovementCue <- factor(dataset_MC_long$MovementCue, levels=c(1,2,3,4,5), labels = c("Dodge", "Stop", "Linear", "Evade", "LongTerm"))
    dataset_MC_long$chosen_variant <- factor(dataset_MC_long$chosen_variant, levels=c(0, 1, 2), labels = c("intial", "human_like", "machine_like")) 
    
####Histograms####


#initial Godspeed single dataset
i_G <- select(dataset, i.G.I:i.G.V.)

#intial Godspeed histograms
i_G_I_histo <- ggplot(data = i_G, aes(i_G$i.G.I)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Anthropomorphism", x="fake|machinelike|unconscious|artificial|moving rigidly [1] - natural|humanlike|conscious|lifelike|moving elegantly [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

i_G_II_histo <- ggplot(data = i_G, aes(i_G$i.G.II)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Animacy", x="dead|stagnant|mechanical|artificial|inert|apathetic [1] - alive|lively|organic|lifelike|interactive|responsive [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

i_G_III_histo <- ggplot(data = i_G, aes(i_G$i.G.III)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Likeability", x="dislike|unfriendly|unkind|unpleasant|awful [1] - like|friendly|kind|pleasant|nice [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

i_G_IV_histo <- ggplot(data = i_G, aes(i_G$i.G.IV)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Perceived Intelligence", x="incompetent|ignorant|irresponsible|unintelligent|foolish [1] - competent|knowledgable|responsible|intelligent|sensible [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

i_G_V_histo <- ggplot(data = i_G, aes(i_G$i.G.V.)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Perceived Safety*", x="anxious|agitated|uncomfortable [1] - relaxed|calm|comfortable [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#grid.arrange(i_G_I_histo, i_G_II_histo, i_G_III_histo, i_G_IV_histo, i_G_V_histo, ncol=1)

#BEAM Robot final evaluation _ Comparision to initial mindset on robots
BR_G <- select(dataset, BR.G.I:BR.G.V.)

#intial Godspeed histograms
BR_G_I_histo <- ggplot(data = BR_G, aes(BR_G$BR.G.I)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Anthropomorphism", x="fake|machinelike|unconscious|artificial|moving rigidly [1] - natural|humanlike|conscious|lifelike|moving elegantly [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

BR_G_II_histo <- ggplot(data = BR_G, aes(BR_G$BR.G.II)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Animacy", x="dead|stagnant|mechanical|artificial|inert|apathetic [1] - alive|lively|organic|lifelike|interactive|responsive [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

BR_G_III_histo <- ggplot(data = BR_G, aes(BR_G$BR.G.III)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Likeability", x="dislike|unfriendly|unkind|unpleasant|awful [1] - like|friendly|kind|pleasant|nice [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

BR_G_IV_histo <- ggplot(data = BR_G, aes(BR_G$BR.G.IV)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Perceived Intelligence", x="incompetent|ignorant|irresponsible|unintelligent|foolish [1] - competent|knowledgable|responsible|intelligent|sensible [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

BR_G_V_histo <- ggplot(data = BR_G, aes(BR_G$BR.G.V.)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Perceived Safety*", x="anxious|agitated|uncomfortable [1] - relaxed|calm|comfortable [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#grid.arrange(BR_G_I_histo, BR_G_II_histo, BR_G_III_histo, BR_G_IV_histo, BR_G_V_histo, ncol=1)


grid.arrange(i_G_I_histo, BR_G_I_histo, i_G_II_histo, BR_G_II_histo, i_G_III_histo, BR_G_III_histo, i_G_IV_histo, BR_G_IV_histo, i_G_V_histo, BR_G_V_histo, ncol=2)
#ggsave("Histogram_initial_vs_BEAM.pdf")

#####Movement Cues####

#Histograms of Godspeed I, III, V

D_G_I_histo <- ggplot(data = dataset, aes(dataset$D.G.I)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Dodge-Anthropomorphism", x="fake|machinelike|unconscious|artificial|moving rigidly [1] - natural|humanlike|conscious|lifelike|moving elegantly [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

S_G_I_histo <- ggplot(data = dataset, aes(dataset$S.G.I)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Stop-Anthropomorphism", x="fake|machinelike|unconscious|artificial|moving rigidly [1] - natural|humanlike|conscious|lifelike|moving elegantly [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

L_G_I_histo <- ggplot(data = dataset, aes(dataset$L.G.I)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Linear-Anthropomorphism", x="fake|machinelike|unconscious|artificial|moving rigidly [1] - natural|humanlike|conscious|lifelike|moving elegantly [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

E_G_I_histo <- ggplot(data = dataset, aes(dataset$E.G.I)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="Evade-Anthropomorphism", x="fake|machinelike|unconscious|artificial|moving rigidly [1] - natural|humanlike|conscious|lifelike|moving elegantly [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

LT_G_I_histo <- ggplot(data = dataset, aes(dataset$LT.G.I)) +
  geom_histogram(binwidth=0.2, aes(fill=..count..), color="white") +
  scale_x_continuous(limits = c(1, 5) ) +
  scale_fill_gradient("Count", low = "#c6cec9", high = "#191919") + 
  labs(title ="LongTerm-Anthropomorphism", x="fake|machinelike|unconscious|artificial|moving rigidly [1] - natural|humanlike|conscious|lifelike|moving elegantly [5]") + 
  guides(fill=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(D_G_I_histo, S_G_I_histo, L_G_I_histo, E_G_I_histo, LT_G_I_histo, ncol=1)



#by(dataset$variable1, dataset$variable2, summary)

    ####ANOVA Movement Cues####
    #Check if everything is balanced
    ezDesign(data=dataset_MC_long, x=MovementCue, y=Participant)

    ####Anthropomorphism
    dataset_MC_long_GI_naomit <- subset(dataset_MC_long, !is.na(GI_Anthropomorphism))
    ANOVA_Anthropomorphism <- aov(GI_Anthropomorphism ~ MovementCue, data=dataset_MC_long_GI_naomit)
    anova_stats(ANOVA_Anthropomorphism)
    eta_sq(ANOVA_Anthropomorphism, partial = TRUE, ci.lvl = .95)
    omega_sq(ANOVA_Anthropomorphism, partial = TRUE, ci.lvl = .95)
   

    ####Likability
    dataset_MC_long_GIII_naomit <- subset(dataset_MC_long, !is.na(GIII_Likeability))
    ANOVA_Likeability <- aov(GIII_Likeability ~ MovementCue, data=dataset_MC_long_GIII_naomit)
    anova_stats(ANOVA_Likeability)
    eta_sq(ANOVA_Likeability, partial = TRUE, ci.lvl = .95)
    omega_sq(ANOVA_Likeability, partial = TRUE, ci.lvl = .95)
    
    ####Perceived Safety
    dataset_MC_long_GV_naomit <- subset(dataset_MC_long, !is.na(GV_Perceived_Safety))
    ANOVA_GV_Perceived_Safety <- aov(GV_Perceived_Safety ~ MovementCue, data=dataset_MC_long_GV_naomit)
    anova_stats(ANOVA_GV_Perceived_Safety )
    eta_sq(ANOVA_GV_Perceived_Safety, partial = TRUE, ci.lvl = .95)
    omega_sq(ANOVA_GV_Perceived_Safety, partial = TRUE, ci.lvl = .95)
    
    