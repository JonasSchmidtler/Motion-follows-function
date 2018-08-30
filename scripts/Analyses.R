library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(gridExtra)
library(ez)

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
    
    #rename certain columns
    names(dataset)[names(dataset) == "ERR"] <- "ExperienceWithRealRobots"
    
    
    
    #new dataset movement cues
    dataset_MC <- select(dataset, Participant_Code, D.Worked:LT.Variant)
    
    


    #convert wide format in long format
    #dataset_long <- gather(dataset, "Measurement","Value", i.G.I:LT.Desired, factor_key=TRUE)



#qplot(x = MC_Order, data = dataset,
#      color = I('black'), fill = I('#000099'))

#intial_stop <- subset(dataset, MC_Order)
    
    #subsetting using dplyr
    #dataset_new <- dataset %>%
    #                 filter(group =="XXX") %>%
    #                 filter(XX<100)

#by(dataset$variable1, dataset$variable2, summary)


#qplot(x=price/carat, data = diamonds)+
#  facet_wrap(~cut)

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



