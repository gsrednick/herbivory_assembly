### Moorea Asynchrony and Herbivory study
# Anaylsis and figures script

# Written by G. Srednick
# 2/10/2022



# packages

library(tidyverse)
library(vegan)
library(ggvegan)
library(patchwork)
library(pairwiseAdonis)
library(codyn)
library(reshape2)
#library(ggforce)


setwd("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea")


# data





######################################  Exploratory  ########################################################

# change names for heterogeneity treatmetns
summarized_controls_v2 <- summarized_controls %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                           Clean = "Low heterogeneity",
                           Turbinaria = "High heterogeneity"))




### Initial effect tests ####
# Cage effects: cover, richness, biomass, and community structure 
# Block effects: cover, richness, biomass, and community structure 

## cover
# cage
summarized_controls_time<-summarized_controls_v2 %>% filter(!Day =="0",
                                                            !Cage.treatment %in% c("Control"))

cage_effects_lm <-aov(algal_cover ~ Cage.treatment, data = summarized_controls_time) 
summary(cage_effects_lm) 
TukeyHSD(cage_effects_lm) # no difference between open and cage control




boxplot(algal_cover ~ Block, data = summarized_controls_time, col = "red")

# block
full_test_cover<-aov(algal_cover ~ Treatment*as.factor(Day)*Block*Bommie_treat, data = summarized_controls_time)
summary(full_test_cover)

# no effect of block alone
# block x cage
# block x day
# cage x treatment x block
# cage x day x block
# cage x treatment x day x block

# so block does matter for cover?

# reduced
cage_effects_time_lm <-aov(log_algalcover ~Treatment*as.factor(Day)*Cage.treatment, data = summarized_controls_time)
summary(cage_effects_time_lm)
TukeyHSD(cage_effects_time_lm)






## richness

# cage effects
cage_effects_rich_lm <-aov(rich ~ Cage.treatment, data = exp_data_rich) 
summary(cage_effects_rich_lm) # no
TukeyHSD(cage_effects_rich_lm) # no 


cage_effects_rich_time_lm <-aov(rich ~ Cage.treatment * Treatment*as.factor(Day), data = exp_data_rich)
summary(cage_effects_rich_time_lm)
TukeyHSD(cage_effects_rich_time_lm)

# block effects
full_test_rich<-aov(rich ~ Cage.treatment * Treatment*as.factor(Day)*Block*Bommie_treat, data = exp_data_rich)
summary(full_test_rich)

boxplot(rich ~ Block, data = exp_data_rich, col = "red")

hist(subset(exp_data_rich, Treatment == "Simple")$rich, col = "green")


full_test_rich<-aov(rich ~ Cage.treatment * Treatment*as.factor(Day)*Block*Bommie_treat, data = exp_data_rich)
summary(full_test_rich)
# block effects exist



# reduced
exp_data_rich_reduced<-exp_data_rich %>% filter(!Day =="0",
                                                            !Cage.treatment %in% c("Control"))

rich_cage_effects_time_lm <-aov(rich ~Treatment*Bommie_treat*Cage.treatment, data = exp_data_rich_reduced)
summary(rich_cage_effects_time_lm)
TukeyHSD(rich_cage_effects_time_lm)




### biomass

# cage effects
cage_effects_biomass_lm <-aov(log_chng_biomass ~ Cage.treatment, data = change_biomass) 
summary(cage_effects_biomass_lm) # no
TukeyHSD(cage_effects_biomass_lm) # no 


# block effects
full_test_biomass<-aov(log_chng_biomass ~ Cage.treatment * Treatment*Block*Bommie_treat, data = change_biomass)
summary(full_test_biomass)

# no block effect

# Drop controls!
# Proceed to find if cages matter!!



# reduced
change_biomass_reduced<-change_biomass %>% filter(!Cage.treatment %in% c("Control"))
test_biomass_reduced<-aov(log_chng_biomass ~ Cage.treatment * Treatment*Bommie_treat, data = change_biomass_reduced)
summary(test_biomass_reduced)








#### Cage effect on herbivory ####
cover_short<-summarized_controls_v2 %>% filter(!Time_point == "1") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity", 
                               Turbinaria = "High heterogeneity"))

  

## cover
cage_cover_time_lm <-aov(log_algalcover ~ Cage.treatment * Treatment*as.factor(Day)*Bommie_treat, data = cover_short)
summary(cage_cover_time_lm)
TukeyHSD(cage_cover_time_lm)


cage_cover_lm <-aov(log_algalcover ~ Cage.treatment * Treatment*Bommie_treat, data = cover_short)
summary(cage_cover_lm)
TukeyHSD(cage_cover_lm)

cage_effect<-ggplot(cover_short, aes(x=Cage.treatment, y = algal_cover, fill = Cage.treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  stat_summary(aes(fill = Cage.treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  stat_summary(geom = "line", 
               aes(fill = Cage.treatment), 
               fun = "mean") +
  labs(x = "Cage treatment", y = "% of algal cover to bare space") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=15)) +
  ylim(0,105) +
  facet_grid(Bommie_treat~Treatment)

cage_effect_int_cover<-ggplot(cover_short, aes(x=as.numeric(Time_point), y = algal_cover, fill = Cage.treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  stat_summary(aes(fill = Cage.treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  stat_summary(geom = "line", 
               aes(fill = Cage.treatment), 
               fun = "mean") +
  labs(x = "Time point", y = "Total algal cover") +
  theme_bw() +
  theme(text = element_text(size=15)) +
  #ylim(0,15) +
  facet_grid(Bommie_treat~Treatment)


# consistent effect of cage --> proceed




## richness
rich_short<-exp_data_rich %>% filter(!Time_point == "1") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                             Clean = "Low heterogeneity",
                             Turbinaria = "High heterogeneity"))


bartlett.test(rich ~ Bommie_treat, data = rich_short) # Yes !
bartlett.test(rich ~ Treatment, data = rich_short) # Nope. Transform


cage_rich_time_lm <-aov(rich ~ Cage.treatment * Treatment*as.factor(Day)*Bommie_treat, data = rich_short)
summary(cage_rich_time_lm)
TukeyHSD(cage_rich_time_lm)


cage_rich_lm <-aov(rich ~ Cage.treatment * Treatment*Bommie_treat, data = rich_short)
summary(cage_rich_lm)
TukeyHSD(cage_rich_lm)

cage_effect_rich<-ggplot(rich_short, aes(x=Cage.treatment, y = rich, fill = Cage.treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  stat_summary(aes(fill = Cage.treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  stat_summary(geom = "line", 
               aes(fill = Cage.treatment), 
               fun = "mean") +
  labs(x = "Cage treatment", y = "Species richness") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=15)) +
  #ylim(0,15) +
  facet_grid(Bommie_treat~Treatment)


cage_effect_int_rich<-ggplot(rich_short, aes(x=as.numeric(Time_point), y = rich, fill = Cage.treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01) +
  stat_summary(aes(fill = Cage.treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6) +
  stat_summary(geom = "line", 
               aes(fill = Cage.treatment), 
               fun = "mean") +
  labs(x = "Time point", y = "Richness") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=15)) +
  #ylim(0,15) +
  facet_grid(Bommie_treat~Treatment)
# interaction between richness and treatment --> richness higher on exposed complex plates 
# have to rework this a wee bit. Something is off.




## biomass
biomass_short<-change_biomass %>% 
 filter(!Cage.treatment == "Control") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"))

cage_biomass_lm <-aov(log_chng_biomass ~ Cage.treatment, data = biomass_short)

cage_biomass_lm <-aov(log_chng_biomass ~ Cage.treatment * Treatment*Bommie_treat*Block, data = biomass_short)
summary(cage_biomass_lm)
TukeyHSD(cage_biomass_lm)

cage_effect_biomass<-ggplot(biomass_short, aes(x=Cage.treatment, y = log_chng_biomass, fill = Cage.treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  stat_summary(aes(fill = Cage.treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  stat_summary(geom = "line", 
               aes(fill = Cage.treatment), 
               fun = "mean") +
  labs(x = "Cage treatment", y = "Accumulated biomass") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=15)) +
  #ylim(0,15) +
  facet_grid(Bommie_treat~Treatment)




cage_effect_int_biomass<-ggplot(biomass_short, aes(x=Treatment, y = log_chng_biomass, fill = Cage.treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  stat_summary(aes(fill = Cage.treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  stat_summary(geom = "line", 
               aes(fill = Cage.treatment), 
               fun = "mean") +
  labs(x = "Treatment", y = "Accumulated biomass") +
  theme_bw() +
  theme( text = element_text(size=15))# +
  #ylim(0,15) +
 # facet_grid(~Treatment)
# no interaction ---> proceed.





cage_effect_plot<-cage_effect+ cage_effect_rich+cage_effect_biomass + plot_layout(ncol = 3, guides = "collect")

ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Figures/Manuscript/Prelim/cage_effect.pdf",
       plot=cage_effect_plot,width=15,height=6)



cage_effect_plot_time<-cage_effect_int_cover+ cage_effect_int_rich+cage_effect_int_biomass + plot_layout(ncol = 3, guides = "collect")

ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Figures/Manuscript/Prelim/cage_effect.pdf",
       plot=cage_effect_plot_time,width=18,height=6)











######################################  Main tests  ########################################################

# Hypotheses/Contents:
# (1) Differences among bommie treatments
# (2) Differences among tile treatments
# (3) Interaction between two treatments
# (4) Differences in community structure
# (5) Community asynchrony enhanced by complexity and heterogeneity
# (5a) Synchrony tests with Loreau at community and metacommunity
# (5b) Synchrony tests with Bray Curtis dissimilarity 





### Hypothesis 1-3 ####

## Cover
summarized_nocage_v2<- summarized_nocage %>%
  #filter(!Time_point == "1") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"),
         Day = recode(Time_point,
                      "1" = "0",
                      "2" = "18",
                      "3" = "33",
                      "4" = "67",
                      "5" = "92"))

cover_main<-ggplot(summarized_nocage_v2, aes(x=as.numeric(as.character(Day)), y = algal_cover, fill = Treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.03) +
  stat_summary(aes(fill = Treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6) +
  stat_summary(geom = "line", 
               aes(fill = Treatment), 
               fun = "mean") +
  theme(text = element_text(size=15)) +
  labs(x = "Day", y = "% of algal cover to bare space",  fill = "Habitat complexity") +
  #  guides(fill = FALSE) +
  theme_bw() +
  coord_cartesian(y = c(0,100)) +
  scale_fill_manual(values=c("Complex" = "red","Simple" = "blue"))+
  facet_wrap(~Bommie_treat,ncol = 1)


main_cover <-aov(log_algalcover ~ Treatment*Bommie_treat*Time_point, data = summarized_nocage_v2)
summary(main_cover)
TukeyHSD(cage_biomass_lm)


# means

summarized_nocage_v2 %>% group_by(Day) %>% summarize_at(vars(algal_cover),c(mean,sd)) %>% mutate_if(is.numeric,format,2)






## richness
rich_reduced<-exp_data_rich %>%
  filter(!Cage.treatment %in% c("Cage","Control")
         ,
         !Time_point == "1"
         ) %>% 
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"),
         Day = recode(Time_point,
                      "1" = "0",
                      "2" = "18",
                      "3" = "33",
                      "4" = "67",
                      "5" = "92"))
  


richness_main<-rich_reduced %>%
  filter(!Cage.treatment %in% c("Cage","Control")) %>%
  ggplot(aes(x=as.numeric(as.character(Day)), y = rich, fill = Treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01) +
  stat_summary(aes(fill = Treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6) +
  stat_summary(geom = "line", 
               aes(fill = Treatment), 
               fun = "mean") +
  theme(text = element_text(size=15)) +
  labs(x = "Day",  fill = "Habitat complexity") +
  ylab(bquote('No. species plate'^-1)) +
  #  guides(fill = FALSE) +
  theme_bw() +
  coord_cartesian(ylim=c(0, 8)) +
  scale_fill_manual(values=c("Complex" = "red","Simple" = "blue"))+
  facet_wrap(~Bommie_treat,ncol = 1)


# anova
main_richness <-aov(rich ~ Treatment*as.factor(Day)*Bommie_treat, data = rich_reduced)
summary(main_richness)
TukeyHSD(main_richness)




# means

rich_reduced %>% group_by(Treatment,Day) %>% summarize_at(vars(rich),c(mean,sd)) %>% mutate_if(is.numeric,format,2)





# time effect
# treatment effect
# Treatment by time effect

# proceed


## biomass
biomass_reduced<-biomass_short %>%
  filter(!Cage.treatment %in% c("Cage","Control"))

biomass_main<-ggplot(biomass_reduced, aes(x=Treatment, y = change_biomass, fill = Treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  stat_summary(aes(fill = Treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  stat_summary(geom = "line", 
               aes(fill = Treatment), 
               fun = "mean") +
  labs(x = "Habitat complexity", fill = "Habitat complexity") +
  ylab(bquote('Accumulated biomass plate'^-1*'(g)')) +
  theme(legend.position = "none",text = element_text(size=15)) +
  scale_fill_manual(values=c("Complex" = "red","Simple" = "blue"))+
  theme_bw() +
  coord_cartesian(y=c(0,35)) +
  facet_wrap(~Bommie_treat,ncol = 1)


biomass_main_test <-aov(log_chng_biomass ~ Treatment * Bommie_treat, data = biomass_reduced)
summary(biomass_main_test)
TukeyHSD(cage_biomass_lm)

# treatment effect; nothing else ---> proceed


# means

biomass_reduced %>% group_by(Treatment) %>% summarize_at(vars(change_biomass),c(mean,sd)) %>% mutate_if(is.numeric,format,2)




## join these into one figure
H1_plot<-cover_main + richness_main + biomass_main + plot_layout(ncol=3,guides = "collect")

ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Figures/Manuscript/Prelim/algal_parameters.pdf",
       plot=H1_plot,width=11,height=6)















### Hypothesis 4 ####

# Use PERMANOVA and MDS to detect differences in community structure across treatments

community<-exp_data_corrected_nocontrol %>% 
  filter(!Cage.treatment == "Cage",
         !Time_point == "1") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"))

community<-community[, colSums(community != 0) > 0]

com_summarized <- community[c(12:28)]

env_summarized <- community[c(1:11)]

com_summarized_mds <- metaMDS(comm = com_summarized, distance = "bray", 
                              trace = FALSE, autotransform = TRUE, na.rm = FALSE)
com_summarized_mds$stress # 0.08933897
summary(com_summarized_mds)
com_summarized_mds$species
com_summarized_mds_points<-data.frame(com_summarized_mds$points)
mds<-merge(env_summarized,com_summarized_mds_points, by="row.names", all.x=TRUE)




## plotting
# with envfit over the top
ef_sum <- envfit(com_summarized_mds, com_summarized, permu = 999)


ef_sum.scrs <- as.data.frame(scores(ef_sum, display = "vectors")) # save species intrinsic values into dataframe
ef_sum.scrs <- cbind(ef_sum.scrs, species = rownames(ef_sum.scrs)) # add species names to dataframe
ef_sum.scrs <- cbind(ef_sum.scrs, pval = ef_sum$vectors$pvals) # add pvalues to dataframe so you can select species which are significant

vectors_sum <- subset(ef_sum.scrs, pval<=0.05)


se_fn <- function(x) sd(x) / sqrt(length(x)) # Create own function

summarized_mds <- mds %>% 
  group_by(Treatment,Time_point,Bommie_treat) %>%
  summarize(mean_MDS1 = mean(MDS1),
               mean_MDS2 = mean(MDS2),
               se_MDS1 = se_fn(MDS1),
               se_MDS2 = se_fn(MDS2))
  
summarized_mds$Time_point<-as.numeric(summarized_mds$Time_point)

community_MDS<-ggplot(summarized_mds[order(summarized_mds$Time_point),], aes(x=mean_MDS1,y=mean_MDS2)) + 
  geom_segment(data=vectors_sum,aes(x=0,xend=NMDS1/1.5,y=0,yend=NMDS2/1.5), arrow = arrow(length = unit(0.1,"cm")), colour="grey") + 
  geom_point(aes(color = Treatment),size=5.5, color = "black") + 
  geom_point(aes(color = Treatment),size=5) + 
  geom_errorbarh(aes(xmax = mean_MDS1 + se_MDS1, xmin = mean_MDS1 - se_MDS1)) +
  geom_errorbar(aes(ymax = mean_MDS2 + se_MDS2, ymin = mean_MDS2 - se_MDS2)) +
  geom_path(aes(color=Treatment)) +
  geom_text(data=vectors_sum, aes(x=NMDS1/1.5,y=NMDS2/1.5,label=species),size=5) +
  theme_bw() +
  removeGrid() +
  theme(text = element_text(size = 14, color = "black"),
        axis.text.x = element_text(color="black"), 
        axis.text.y = element_text(color="black")) +
  labs(color = "Tile treatment", x = "MDS1",y="MDS2") +
  scale_colour_manual(values=c("Complex" = "red","Simple" = "blue"))+
  facet_grid(~Bommie_treat) +
  coord_cartesian(x=c(-1.5,1), y=c(-0.5,0.5))


ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Figures/Manuscript/Prelim/community_MDS.pdf",
       plot=community_MDS,width=12,height=6)

ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Figures/Manuscript/Prelim/community_MDS.jpeg",
       plot=community_MDS,width=12,height=6)



# PERMANOVA

com_perm<-adonis(com_summarized ~ Tile_arrangement * Bommie_treat*Time_point,
           data = env_summarized, 
           method = "bray", 
           permutations = 999)



# cages 
library(pairwiseAdonis)

community_nocage<-exp_data_corrected[, colSums(exp_data_corrected != 0) > 0]

com_nocage <- exp_data_corrected[c(12:28)]

env_nocage <- exp_data_corrected[c(1:11)]

com_cage_veg<-vegdist(com_nocage)

pairwise.adonis(com_cage_veg,env_nocage[,"Cage.treatment"])



# cage perm
come_cage_perm<-adonis(com_nocage ~ Treatment * Cage.treatment*Bommie_treat*Time_point,
                 data = env_nocage, 
                 method = "bray", 
                 permutations = 999)



# block perm
block_perm<-adonis(com_nocage ~ Tile_arrangement*Bommie_treat*Block*Cage.treatment,
                 data = env_nocage, 
                 method = "bray", 
                 permutations = 999)


# do averages and SE here -- horizontal and vertical error bars for each object perhaps, connected by lines for time




##### Hypothesis 5 ####



##### Hypothesis 5A ####


## Community synchrony ##
long_data<-exp_data_corrected_sums_nocontrol_v3 %>%
  pivot_longer(!c(Time_point,Date,Cage.treatment,Block,Treatment,Tile_no,Tile_arrangement,Bommie_treat,Bommie_no), names_to = "species", values_to = "cover")


com_dataframe<-long_data
com_dataframe<-com_dataframe %>% filter(!species == "BARE",
                                        !Time_point == "1") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                                 Clean = "Low heterogeneity",
                                 Turbinaria = "High heterogeneity"))

com_dataframe_full<-long_data %>% filter(!species == "BARE") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"))


com_dataframe$Time_point<-as.numeric(com_dataframe$Time_point)


# synchrony 
synchrony_com_loreau<-synchrony(com_dataframe,
                           time.var = "Time_point",
                           species.var = "species",
                           abundance.var = "cover",
                           replicate.var = "Tile_no",
                           metric = "Loreau")



# table for community
com_table <- meta_table %>% 
  group_by(Tile_no,Treatment,Bommie_treatment,Cage.treatment,Tile_arrangement) %>%
  mutate(Bommie_treatment = recode(Bommie_treatment, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity")) %>%
  #filter(!Cage.treatment %in% c("Control","Cage")) %>%
  summarize_all(mean)

loreau_com_complete<-merge(com_table,synchrony_com_loreau)



loreau_com_complete<-loreau_com_complete %>% filter(!Cage.treatment == "Cage")




# com plot 


loreau_com_averaged<-loreau_com_complete %>% 
  group_by(Tile_no,Treatment) %>%
  summarize_at(vars(synchrony),mean)

loreau_com_complete %>% 
  group_by(Treatment) %>%
  summarize_at(vars(synchrony),c(mean,sd))






loreau_comm<-ggplot(loreau_com_complete, aes(x=Bommie_treatment, y = as.numeric(synchrony), fill = Treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  stat_summary(aes(fill = Treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  labs(x = "Habitat heterogeneity ", y = "Community synchrony", fill = "Habitat complexity") +
  theme_bw() +
  theme(text = element_text(size=15),
        legend.position = "none") +
  scale_fill_manual(values=c("Complex" = "red","Simple" = "blue"))+
  ylim(0,1)
 # facet_zoom(ylim = c(0, 0.25))

loreau_comm_mean<-ggplot(loreau_com_complete, aes(x=Treatment, y = as.numeric(synchrony), fill = Treatment)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  stat_summary(aes(fill = Treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  theme_bw() +
  labs(x = "Habitat complexity ", fill = "Habitat complexity") +
  scale_fill_manual(values=c("Complex" = "red","Simple" = "blue"))+
  theme(text = element_text(size=15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  coord_cartesian(y=c(0,.2))



loreau_comm_both<-loreau_comm + loreau_comm_mean + plot_layout(guides = "collect",widths = c(2, 1)) & theme(text= element_text(size = 12)) 


com_sync_aov<-aov(synchrony~Treatment*Bommie_treatment,loreau_com_complete)
summary(com_sync_aov)
TukeyHSD(com_aov)


# effect of habitat complexity

# show averages and SD 
loreau_com_complete %>% summarize_at(vars(synchrony),c(mean,sd))




## Metacommunity synchrony ##

# summing to metacom
metacom_dataframe<-com_dataframe %>% 
  filter(Cage.treatment == "Open") %>%
  group_by(Bommie_no,Bommie_treat,Tile_arrangement,Time_point,species) %>%
  summarize_at(vars(cover),sum)

metacom_dataframe_full<-com_dataframe_full %>% 
  filter(Cage.treatment == "Open") %>%
  group_by(Bommie_no,Bommie_treat,Tile_arrangement,Time_point,species) %>%
  summarize_at(vars(cover),sum)


# calculations
# synchrony
synchrony_metacom_loreau<-synchrony(metacom_dataframe,
                                time.var = "Time_point",
                                species.var = "species",
                                abundance.var = "cover",
                                replicate.var = "Bommie_no",
                                metric = "Loreau")


# evenness


meta_even<-community_structure(metacom_dataframe_full,
  time.var = "Time_point",
  abundance.var = "cover",
  replicate.var = "Bommie_no",
  metric = "Evar")




# merge with metadata

metacom_table <- meta_table %>% 
  filter(Cage.treatment =="Open") %>%
  group_by(Bommie_treatment,Bommie_no,Tile_arrangement) %>%
  mutate(Bommie_treatment = recode(Bommie_treatment, 
                                 Clean = "Low heterogeneity",
                                 Turbinaria = "High heterogeneity")) %>%
  #filter(!Cage.treatment %in% c("Control","Cage")) %>%
  summarize_all(mean)


loreau_metacom_complete<-merge(synchrony_metacom_loreau,metacom_table)




loreau_metacom<-ggplot(loreau_metacom_complete, aes(x=Bommie_treatment, y = as.numeric(synchrony), fill = Tile_arrangement)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  stat_summary(aes(fill = Tile_arrangement), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  theme_bw() +
  labs(x = "Habitat heterogeneity ", y = "Metacommunity synchrony", fill = "Habitat complexity") +
  theme(text = element_text(size=15),
        legend.position = c(0.8,0.8)) +
  scale_fill_manual(values=c("Complex" = "red","Mix" = "white","Simple" = "blue"))+
  coord_cartesian(ylim = c(0,0.5))
  #facet_zoom(ylim = c(0, 0.25))



# Anova
metacom_loreau_aov<-aov(synchrony~as.factor(Tile_arrangement)*as.factor(Bommie_treatment), data = loreau_metacom_complete)
summary(metacom_loreau_aov)

# no effects 
loreau_metacom_complete %>% 
 # group_by(Treatment) %>%
  summarize_at(vars(synchrony),c(mean,sd))


# join plots


synchrony_plot<-loreau_comm_both / loreau_metacom + plot_layout(guides = "keep", ncol = 1) + plot_annotation(tag_levels = "A") & theme(text= element_text(size = 12)) 


ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Figures/Manuscript/Prelim/synchrony_plot.pdf",
       plot = synchrony_plot, width = 6,height = 9)






###### Hypothesis 5B #####



# Time series of similarity
# does that similarity change over time based on treatment?
# are metacommunities of different plates more dissimilar than those with same plates?
# this would reduce the aggregating effect that occurs when summing
# doesnt get at stability though.....is that okay?


community_diss<-exp_data_corrected_nocontrol %>% 
  filter(!Cage.treatment == "Cage")



dissim_df<- community_diss %>% 
  mutate(Tile_time = paste0(Tile_no,sep = "_",Time_point))

com_bray <- dissim_df[c(13:30)]
rownames(com_bray)<-dissim_df$Tile_time


env_bray <- dissim_df[c(1:12,31)]

com_sim <- vegdist(com_bray, distance = "bray", trace = FALSE, autotransform = FALSE)

distmat<-as.matrix(com_sim,labels=TRUE)

dist_df <- melt(as.matrix(distmat), varnames = c("TILE_1", "TILE_2"))
names(dist_df[3])<-"distance"


# now I add both to a dataframe with factor labels in the same way I did in the "correlation" df from the MPA paper

dist_df$Tile_1 <- sub("_[^_]*", "", dist_df$TILE_1)
dist_df$Time_point_1 <- sub("[^_]*_", "", dist_df$TILE_1)

dist_df$Tile_2 <- sub("_[^_]*", "", dist_df$TILE_2)
dist_df$Time_point_2 <- sub("[^_]*_", "", dist_df$TILE_2)


dist_df<-dist_df %>% tibble::rowid_to_column("ID")



# New separate dfs 
Tile1_df<-dist_df[c(1,4,5,6)]
names(Tile1_df)<-c("ID","value","Tile_no","Time_point")

Tile2_df<-dist_df[c(1,4,7,8)]
names(Tile2_df)<-c("ID","value","Tile_no","Time_point")

# Merge

Tile_1_merged<-merge(env_bray[-c(1,8,10,12)],Tile1_df)

Tile_2_merged<-merge(env_bray[-c(1,8,10,12)],Tile2_df)

names(Tile_1_merged)
# Rename 
names(Tile_1_merged)<-c("Tile_1","Time_point_1","Treatment_1",
                        "Tile_arrangement_1","Cage.treatment_1","Bommie_treat_1",
                        "Bommie_no_1","Day_1","Tile_time_1","ID","value")

names(Tile_2_merged)<-c("Tile_2","Time_point_2","Treatment_2",
                        "Tile_arrangement_2","Cage.treatment_2","Bommie_treat_2",
                        "Bommie_no_2","Day_2","Tile_time_2","ID","value")

dist_merged<-merge(Tile_1_merged,Tile_2_merged, by=c("ID","value"))



# make "both" groups

dist_merged_grouped<-dist_merged %>% 
  group_by(ID) %>%
  mutate(Tile_arrangement_merged = case_when(
    Tile_arrangement_1 == "Complex" & Tile_arrangement_2 == "Complex" ~ "Complex",
    Tile_arrangement_1 == "Complex" & Tile_arrangement_2 == "Simple" ~ "NULL",
    Tile_arrangement_1 == "Simple" & Tile_arrangement_2 == "Complex" ~ "NULL",
    Tile_arrangement_1 == "Simple" & Tile_arrangement_2 == "Simple" ~ "Simple",
    Tile_arrangement_1 == "Mix" & Tile_arrangement_2 == "Simple" ~ "NULL",
    Tile_arrangement_1 == "Simple" & Tile_arrangement_2 == "Mix" ~ "NULL",
    Tile_arrangement_1 == "Complex" & Tile_arrangement_2 == "Mix" ~ "NULL",
    Tile_arrangement_1 == "Mix" & Tile_arrangement_2 == "Complex" ~ "NULL",
    Tile_arrangement_1 == "Mix" & Tile_arrangement_2 == "Mix" ~ "Mix"),
    Bommie_treat_merged = case_when(
      Bommie_treat_1 == "Clean" & Bommie_treat_2 == "Turbinaria" ~ "NULL",
      Bommie_treat_1 == "Turbinaria" & Bommie_treat_2 == "Clean" ~ "NULL",
      Bommie_treat_1 == "Turbinaria" & Bommie_treat_2 == "Turbinaria" ~ "Turbinaria",
      Bommie_treat_1 == "Clean" & Bommie_treat_2 == "Clean" ~ "Clean"),
    Time_merged)


dist_merged_grouped_2<-dist_merged %>% 
  filter(Tile_arrangement_1==Tile_arrangement_2,
         Bommie_treat_1 ==Bommie_treat_2,
         Time_point_1==Time_point_2,
         Bommie_no_1==Bommie_no_2,
         !Tile_1 == Tile_2,
         !Day_1 == !Day_2)

dist_merged_grouped_3<-dist_merged_grouped_2 %>% distinct(Bommie_no_1,Time_point_1, .keep_all = TRUE) %>%
  mutate(Bommie_treat_1 = recode(Bommie_treat_1, 
                                 Clean = "Low heterogeneity",
                                 Turbinaria = "High heterogeneity"))



# change levels of time to date
dist_merged_grouped_3$Time_point_1<-revalue(dist_merged_grouped_3$Time_point_1, c("1" = "0",
                                                                                  "2"="18", 
                                                                                  "3"="33",
                                                                                  "4"="67",
                                                                                  "5"="92"))


# Replace NaN with zero -- this is from 0 dissimilarity for first time point
dist_merged_grouped_3$value[is.nan(dist_merged_grouped_3$value)] <- 0



# Plot this with tidyverse
bray_fig<-dist_merged_grouped_3 %>% 
  ggplot(aes(x=as.numeric(as.character(Day_1)),y=value,group=Tile_arrangement_1,fill=Tile_arrangement_1)) +
  stat_summary(aes(fill=Tile_arrangement_1),
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01) +
  stat_summary(aes(fill = Tile_arrangement_1), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6) +
  stat_summary(aes(fill = Tile_arrangement_1),
               geom = "line",
               fun = "mean",
               size = 1) +
  theme_bw() +
  theme(text = element_text(size=15),
        legend.position = c(0.75,0.85)) +
  scale_fill_manual(values=c("Complex" = "red","Mix" = "white","Simple" = "blue"))+
  labs(x = "Day", y = "Bray-Curtis distance", fill = "Habitat complexity") +
  facet_wrap(~Bommie_treat_1) +
  coord_cartesian(ylim = c(0,1))



ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Figures/Manuscript/Prelim/bray_figure.pdf",
       plot=bray_fig,width=10,height=6)



# test

dist_test_df<-dist_merged_grouped_3 %>% filter(!Day_1 == "0")

bray_aov<-lm(value~Tile_arrangement_1*Time_point_1*Bommie_treat_1,data=dist_test_df)
anova(bray_aov)




dist_merged_grouped_3 %>% 
   group_by(Tile_arrangement_1,Day_1) %>%
  summarize_at(vars(value),c(mean,sd))






## Try this plot again but with evenness 
dist_merged_grouped_3$Tile_no<-dist_merged_grouped_3$Tile_1
dist_merged_grouped_3$Time_point<-dist_merged_grouped_3$Time_point_2
dist_merged_grouped_3$Bommie_no<-dist_merged_grouped_3$Bommie_no_1

dist_even_df<-merge(meta_even,dist_merged_grouped_3)

dist_summarize<-dist_even_df %>%
  group_by(Tile_arrangement_1,Day_1,Bommie_treat_1) %>%
  summarize_at(vars(Evar,value,richness),c(mean = mean,se = se_fn))


dist_even_short<-dist_even_df %>% filter(!Day_1 == "0")

dist_even_model<-lm(richness~value*Tile_arrangement_1,dist_even_short)
summary(dist_even_model)

dist_even_model$coefficients

bray_fig_even<-dist_summarize %>% 
  ggplot(aes(x=as.numeric(as.character(Day_1)),y=value_mean,fill=Tile_arrangement_1)) +
  geom_point(aes(fill = Tile_arrangement_1,size=richness_mean+richness_se), pch=21) +
  geom_point(aes(fill = Tile_arrangement_1,size=richness_mean),pch=21) +
  #geom_point(aes(fill = Tile_arrangement_1,size=Evar_mean),pch=21) +
  geom_errorbar(aes(ymin=value_mean-value_se, ymax=value_mean+value_se), width=.1) +
  geom_path() +
  theme_bw() +
  theme(text = element_text(size=15),
        #legend.position = c(0.75,0.85)
        ) +
  scale_fill_manual(values=c("Complex" = "red","Mix" = "white","Simple" = "blue"))+
  labs(x = "Day", y = "Bray-Curtis distance", fill = "Habitat complexity", size = "Species richness") +
  facet_wrap(~Bommie_treat_1) +
  coord_cartesian(ylim = c(0,1))

ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Figures/Manuscript/Prelim/bray_rich_figure.pdf",
       plot=bray_fig_even,width=11,height=6)




ggplot(dist_even_df,aes(x=richness,y=value, fill = Tile_arrangement_1))+
  geom_point(aes(color=Tile_arrangement_1)) +
  geom_smooth(method = "lm")



#### Extra stats #####

## Regression between species richness and bray curtis distance -- for appendix
## Also....place species evenness as a second axis (or bubble size) for bray curtis plot
# "Does diversity drive pattern on mixed tiles; relative abundance driving pattern on simple tiles"



## Species table 

spp_mean_sd<-community %>% 
  filter(!Time_point == "1") %>%
  summarize_at(vars(BARE:Turf), c(mean,sd)) 
