###Packaging and Installs###
#install.packages("GGally")
library(GGally)
#install.packages("easystats", repos = "https://easystats.r-universe.dev")
library(easystats)
#install.packagees ("dplyer")
library(dplyr)
library(patchwork)
#install.packages("agricolae")
library("agricolae")
#install.packages("ggpubr")
library("ggpubr")
#install.packages("cowplot")
library("cowplot")
#install.packages("corrplot")
library(corrplot)
#install.packages('ape')
library(ape)

###Dataframe Reading and Storage###
#Read and peek at data for calculated trait values
DeltaTraits <- read.csv(file = 'C:/Users/David/OneDrive - Knights - University of Central Florida/HUT/DeltaTraits.csv')
head(DeltaTraits)

#Read and peek at data for raw trait values
AbsoluteTraits <- read.csv(file='C:/Users/David/OneDrive - Knights - University of Central Florida/HUT/RawTraitData.csv')
head(AbsoluteTraits)


###Visualizing Absolute Traits###
#Group raw trait values by species
AbsoluteTraits$Species
#Visualize LMA by species via boxplot
ggplot(AbsoluteTraits, aes(x = Species, y = LMA_g.m.2)) +
  geom_boxplot()+
  coord_flip() 
#Visualize SLA by species via boxplot
ggplot(AbsoluteTraits, aes(x = Species, y = SLA_m.2.g)) +
  geom_boxplot()+
  coord_flip()
#Visualize LMF by species via boxplot
ggplot(AbsoluteTraits, aes(x = Species, y = LMF_g.g)) +
  geom_boxplot()+
  coord_flip()
#Visualize LAR by species via boxplot
ggplot(AbsoluteTraits, aes(x = Species, y = LAR_m.2.kg)) +
  geom_boxplot()+
  coord_flip()
#Visualize Chlorophyll by species via boxplot
ggplot(AbsoluteTraits, aes(x = Species, y = Chlorophyll_content)) +
  geom_boxplot()+
  coord_flip()
#Visualize Biomass by species via boxplot
ggplot(AbsoluteTraits, aes(x = Species, y = Biomass_g)) +
  geom_boxplot()+
  coord_flip()

#Remove R_sinu_1 
#AbsoluteTraits2 <- AbsoluteTraits[-249,]
#AbsoluteTraits2
###Visualizing Absolute Traits###

#Reroup raw trait values by species
#AbsoluteTraits2$Species

#Visualize LMA by species via boxplot
#ggplot(AbsoluteTraits2, aes(x = Species, y = LMA_g.m.2)) +
  #geom_boxplot()+
  #coord_flip() 
#Visualize SLA by species via boxplot
#ggplot(AbsoluteTraits2, aes(x = Species, y = SLA_m.2.g)) +
  #geom_boxplot()+
  #coord_flip()
#Visualize LMF by species via boxplot
#ggplot(AbsoluteTraits2, aes(x = Species, y = LMF_g.g)) +
  #geom_boxplot()+
  #coord_flip()
#Visualize LAR by species via boxplot
#ggplot(AbsoluteTraits2, aes(x = Species, y = LAR_m.2.kg)) +
 #geom_boxplot()+
  #coord_flip()
#Visualize Chlorophyll by species via boxplot
#ggplot(AbsoluteTraits2, aes(x = Species, y = Chlorophyll_content)) +
  #geom_boxplot()+
  #coord_flip()
#Visualize Biomass by species via boxplot
#ggplot(AbsoluteTraits2, aes(x = Species, y = Biomass_g)) +
  #geom_boxplot()+
  #coord_flip()

#Correlation Matrix
#Select Traits of Interest by dataframe columns
RGRTraits <- AbsoluteTraits[, c(9,12,14,16,18)]
#Visualize selected trait dataframe
head(RGRTraits)
#Create correlation matrix of selected traits
M = cor(RGRTraits)
corrplot(M, method = 'number', type = 'lower', diag = FALSE)

#Correlation Matrix
#Select Traits of Interest by dataframe columns
#RGRTraits2 <- AbsoluteTraits2[, c(9,12,14,16,18)]
#Visualize selected trait dataframe
#head(RGRTraits2)
#Create correlation matrix of selected traits
#M2 = cor(RGRTraits2)
#corrplot(M2, method = 'number', type = 'lower', diag = FALSE)

#Create scatterplots for RGR Traits
#Compute mean SLA and mean Biomass for each species
SLA_Biomass_species_means <- 
  AbsoluteTraits %>% 
  group_by(Species, Family)%>%
  summarise(MeanSLA = mean(SLA_m.2.g), MeanBiomass = mean(Biomass_g))
SLA_Biomass_species_means
SLA_Abs <- ggplot(SLA_Biomass_species_means, aes(x = MeanSLA, y = MeanBiomass)) +
  geom_point(aes(color = factor(Family)))+
  stat_smooth(method = "lm",
              color ="black",
              se = FALSE,
              size = 1)
SLA_Cor <- SLA_Abs + stat_cor(method = "pearson")
SLA_Cor

LMF_Biomass_species_means <- 
  AbsoluteTraits %>% 
  group_by(Species, Family)%>%
  summarise(MeanLMF = mean(LMF_g.g), MeanBiomass = mean(Biomass_g))
LMF_Biomass_species_means
LMF_Abs <- ggplot(LMF_Biomass_species_means, aes(x = MeanLMF, y = MeanBiomass)) +
  geom_point(aes(color = factor(Family)))+
  stat_smooth(method = "lm",
            color ="black",
            se = FALSE,
            size = 1)
LMF_Cor <- LMF_Abs + stat_cor(method = "pearson")
LMF_Cor

LAR_Biomass_species_means <- 
  AbsoluteTraits %>% 
  group_by(Species, Family)%>%
  summarise(MeanLAR = mean(LAR_m.2.kg), MeanBiomass = mean(Biomass_g))
LAR_Biomass_species_means
LAR_Abs <- ggplot(LAR_Biomass_species_means, aes(x = MeanLAR, y = MeanBiomass)) +
  geom_point(aes(color = factor(Family)))+
  stat_smooth(method = "lm",
              color ="black",
              se = FALSE,
              size = 1)
LAR_Cor <- LAR_Abs + stat_cor(method = "pearson")
LAR_Cor

Chloro_Biomass_species_means <- 
  AbsoluteTraits %>% 
  group_by(Species, Family)%>%
  summarise(MeanChloro = mean(Chlorophyll_content), MeanBiomass = mean(Biomass_g))
Chloro_Biomass_species_means
Chloro_Abs <- ggplot(Chloro_Biomass_species_means, aes(x = MeanChloro, y = MeanBiomass)) +
  geom_point(aes(color = factor(Family)))+
  stat_smooth(method = "lm",
              color ="black",
              se = FALSE,
              size = 1)
Chloro_Cor <- Chloro_Abs + stat_cor(method = "pearson")
Chloro_Cor

#Insert correlation plots into single panel figure
multi_plot<- ggarrange(SLA_Cor,LMF_Cor,LAR_Cor,Chloro_Cor, #plots that are going to be included in this multipanel figure
                       labels = c("A", "B", "C","D"), #labels given each panel 
                       ncol = 2, nrow = 2, #adjust plot space 
                       common.legend = T) #does the plot have a common legend
multi_plot


#Forest Plot Visualization of Absolute Traits
#Visualize Biomass for each Species as forest-like plot 
Biomass_species_stats <- 
  AbsoluteTraits %>% 
  group_by(Species, Family)%>%
  summarise(MeanBiomass = mean(Biomass_g), SE = sd(Biomass_g)/sqrt(n()))
Biomass_species_stats
p1 <- ggplot(data = Biomass_species_stats, 
       aes(x = Species, y = MeanBiomass, ymin = MeanBiomass-SE, ymax = MeanBiomass+SE)) + 
  # this adds the means
  geom_point(aes(color = factor(Family)))+ 
  # this adds the error bars
  geom_errorbar(aes(color = factor(Family)))+ 
  # controlling the appearance
  scale_y_continuous(limits = c()) + 
  # use sensible labels
  xlab("Species") + ylab("Biomass (g)") +
  # flip x and y axes
  coord_flip() 
p1

SLA_species_stats <- 
  AbsoluteTraits %>% 
  group_by(Species, Family)%>%
  summarise(MeanSLA = mean(SLA_m.2.g), SE = sd(SLA_m.2.g)/sqrt(n()))
SLA_species_stats
p2 <- ggplot(data = SLA_species_stats, 
       aes(x = Species, y = MeanSLA, ymin = MeanSLA-SE, ymax = MeanSLA+SE)) + 
  # this adds the means
  geom_point(aes(color = factor(Family)))+ 
  # this adds the error bars
  geom_errorbar(aes(color = factor(Family)))+ 
  # controlling the appearance
  scale_y_continuous(limits = c()) + 
  # use sensible labels
  xlab(" ") + ylab("SLA (m^2/g)") +
  # flip x and y axes
  coord_flip()
p2

LMF_species_stats <- 
  AbsoluteTraits %>% 
  group_by(Species, Family)%>%
  summarise(MeanLMF = mean(LMF_g.g), SE = sd(LMF_g.g)/sqrt(n()))
LMF_species_stats
p3 <- ggplot(data = LMF_species_stats, 
       aes(x = Species, y = MeanLMF, ymin = MeanLMF-SE, ymax = MeanLMF+SE)) + 
  # this adds the means
  geom_point(aes(color = factor(Family)))+ 
  # this adds the error bars
  geom_errorbar(aes(color = factor(Family)))+ 
  # controlling the appearance
  scale_y_continuous(limits = c()) + 
  # use sensible labels
  xlab(" ") + ylab("LMF (g/g)") +
  # flip x and y axes
  coord_flip()
p3

LAR_species_stats <- 
  AbsoluteTraits %>% 
  group_by(Species, Family)%>%
  summarise(MeanLAR = mean(LAR_m.2.kg), SE = sd(LAR_m.2.kg)/sqrt(n()))
LAR_species_stats
p4<-ggplot(data = LAR_species_stats, 
       aes(x = Species, y = MeanLAR, ymin = MeanLAR-SE, ymax = MeanLAR+SE)) + 
  # this adds the means
  geom_point(aes(color = factor(Family)))+ 
  # this adds the error bars
  geom_errorbar(aes(color = factor(Family)))+ 
  # controlling the appearance
  scale_y_continuous(limits = c()) + 
  # use sensible labels
  xlab(" ") + ylab("LAR (m^2/kg)") +
  # flip x and y axes
  coord_flip()
p4

Chloro_species_stats <- 
  AbsoluteTraits %>% 
  group_by(Species, Family)%>%
  summarise(MeanChloro= mean(Chlorophyll_content), SE = sd(Chlorophyll_content)/sqrt(n()))
Chloro_species_stats
p5<-ggplot(data = Chloro_species_stats, 
       aes(x = Species, y = MeanChloro, ymin = MeanChloro-SE, ymax = MeanChloro+SE)) + 
  # this adds the means
  geom_point(aes(color = factor(Family)))+ 
  # this adds the error bars
  geom_errorbar(aes(color = factor(Family)))+ 
  # controlling the appearance
  scale_y_continuous(limits = c()) + 
  # use sensible labels
  xlab(" ") + ylab("Chlorophyll Content") +
  # flip x and y axes
  coord_flip()
p5

multi_plot<- ggarrange(p1,p2,p3,p4,p5, #plots that are going to be included in this multipanel figure
                       labels = c("E", "F", "G","H", "I"), #labels given each panel 
                       ncol = 5, nrow = 1, #adjust plot space 
                       common.legend = T) #does the plot have a common legend
multi_plot

###Visualizing Delta Trait Values

#Correlation Matrix
#Select Traits of Interest by dataframe columns
RGR_DeltaTraits <- DeltaTraits[, c(7,9,11,12,13)]
#Visualize selected trait dataframe
head(RGR_DeltaTraits)
#Create correlation matrix of selected traits
M_Delta = cor(RGR_DeltaTraits)
corrplot(M_Delta, method = 'number', type = 'lower', diag = FALSE)

SLA_Delta <- ggplot(DeltaTraits, aes(x = Delta_SLA, y = MGR)) +
  geom_point(aes(color = factor(Family)))+
  stat_smooth(method = "lm",
              color ="black",
              se = FALSE,
              size = 1)
SLA_Delta_Cor <- SLA_Delta + stat_cor(method = "pearson")
SLA_Delta_Cor

LMF_Delta <- ggplot(DeltaTraits, aes(x = Delta_LMF, y = MGR)) +
  geom_point(aes(color = factor(Family)))+
  stat_smooth(method = "lm",
              color ="black",
              se = FALSE,
              size = 1)
LMF_Delta_Cor <- LMF_Delta + stat_cor(method = "pearson")
LMF_Delta_Cor

LAR_Delta <- ggplot(DeltaTraits, aes(x = Delta_LAR, y = MGR)) +
  geom_point(aes(color = factor(Family)))+
  stat_smooth(method = "lm",
              color ="black",
              se = FALSE,
              size = 1)
LAR_Delta_Cor <- LAR_Delta + stat_cor(method = "pearson")
LAR_Delta_Cor

Chloro_Delta <- ggplot(DeltaTraits, aes(x = Delta_Chlorophyll_Content, y = MGR)) +
  geom_point(aes(color = factor(Family)))+
  stat_smooth(method = "lm",
              color ="black",
              se = FALSE,
              size = 1)
Chloro_Delta_Cor <- Chloro_Delta + stat_cor(method = "pearson")
Chloro_Delta_Cor

#Insert correlation plots into single panel figure
delta_multi_plot<- ggarrange(SLA_Delta_Cor,LMF_Delta_Cor,LAR_Delta_Cor,Chloro_Delta_Cor, #plots that are going to be included in this multipanel figure
                       labels = c("A", "B", "C","D"), #labels given each panel 
                       ncol = 2, nrow = 2, #adjust plot space 
                       common.legend = T) #does the plot have a common legend
delta_multi_plot

###Prune Develaux Tree###

filename <- "C:/Users/David/OneDrive - Knights - University of Central Florida/HUT/V10.root.LSUDAT-raxml-1000BS-GTRGAMMA-bootstrap-tree.newick"
tree <- phytools::read.newick(filename)
tip <- c("X52322.1_Arabidopsis_thaliana",
         "MT832198_Acaulospora_colombiana",
         "MT832206_Acaulospora_spinosa",
         "MT832211_Acaulospora_morrowiae",
         "MT832166_Ambispora_leptoticha",
         "MT832167_Ambispora_gerdemannii",
         "MT832162_Archaeospora_trappei",
         "MT832165_Archaeospora_schencki",
         "MT832158_Paraglomus_brasilianum",
         "MT832157_Paraglomus_occultum",
         "FR750062_Claroideoglomus_claroideum",
         "MT832170_Entrophospora_infrequens",
         "MT832171_Claroideoglomus_etunicatum",
         "MT832173_Claroideoglomus_claroideum",
         "MT832175_Septoglomus_constrictum",
         "MT832179_Septoglomus_viscosum",
         "FN547496_Funneliformis_caledonius",
         "MT832182_Funneliformis_mosseae",
         "MT832194_Rhizophagus_irregularis",
         "MT832189_Rhizophagus_intraradices",
         "MT832191_Rhizophagus_clarus",
         "MT832220_Scutellospora_calospora",
         "MT832221_Scutellospora_dipurpurascens",
         "MT832222_Cetraspora_pellucida",
         "MT832224_Racocetra_fulgida",
         "MT832227_Dentiscutata_erythropus",
         "MT832228_Dentiscutata_heterogama",
         "MT832231_Gigaspora_margarita",
         "MT832233_Gigaspora_rosea",
         "MT832236_Gigaspora_albida",
         "MT832237_Gigaspora_gigantea",
         "FN547573_Gigaspora_rosea",
         "MT832218_Diversispora_spurca",
         "EF067888_Diversispora_eburnea",
         "MT832217_Diversispora_epigaea")
plot(keep.tip(tree, tip))




