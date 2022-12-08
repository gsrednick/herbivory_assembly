### Moorea Asynchrony and Herbivory study ###
# Analysis and figures script

# Accepted as Article in Ecology
# Srednick et al. 2022

# DOI: 


# Code written by G. Srednick
# Oct 2 2022



# packages
devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library(tidyverse)
library(vegan)
#library(ggvegan)
library(patchwork)
library(pairwiseAdonis)
#library(codyn)
library(reshape2)
library(ggExtra)
library(plyr)



### NOTES ###
# All tests are linear mixed models -- these are performed in "LMER_script.R"
# Summary statistics information is in "summary_stats.R"


######################################  Exploratory  ########################################################

## Cage artifacts plot ##
## (A) Cover
cage_artifact_int_cover<- exp_data_actual_time %>% 
  filter(!Bommie_treat == "High heterogeneity") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               "Low heterogeneity" = "High detectability",
                               "High heterogeneity" = "Low detectability"),
         Treatment = recode(Treatment,
                            "Complex" = "Low access",
                            "Simple" = "High access")) %>% 
  ggplot(aes(x=as.numeric(as.character(Day)), y = algal_cover, fill = Cage.treatment)) +
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
  labs(x = "Time point", y = "Total algal cover", fill = "Cage treatment") +
  scale_fill_manual(values=c("Cage" = "black","Open" = "white", "Control" = "grey"))+
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=15)) +
  #ylim(0,15) +
  facet_grid(~Treatment) +
  scale_x_continuous(labels = c(0,25,50,75,100), limits = c(0,100)) +
  removeGrid()


# consistent effect of cage --> proceed




## (B) richness
cage_artifact_int_rich<-exp_data_actual_time %>% 
  filter(!Bommie_treat == "High heterogeneity") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               "Low heterogeneity" = "High detectability",
                               "High heterogeneity" = "Low detectability"),
         Treatment = recode(Treatment,
                            "Complex" = "Low access",
                            "Simple" = "High access")) %>% 
  ggplot(aes(x=as.numeric(as.character(Day)), y = rich, fill = Cage.treatment)) +
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
  labs(x = "Day", y = "Richness", fill = "Cage treatment") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=15)) +
  scale_fill_manual(values=c("Cage" = "black","Open" = "white", "Control" = "grey"))+
  facet_grid(~Treatment) +
  scale_x_continuous(labels = c(0,25,50,75,100), limits = c(0,100)) +
  removeGrid()

# interaction between richness and treatment --> richness higher on exposed complex plates 




## (C) biomass
biomass_short<-change_biomass_actual %>% 
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"),
         Treatment = recode(Treatment,
                            "Complex" = "Low",
                            "Simple" = "High"),
         Cage.treatment = recode(Cage.treatment, 
                                 Cage = "Cage",
                                 Open = "Open",
                                 Control = "cage control")) 


cage_artifact_int_biomass<-biomass_short %>% filter(!Bommie_treat == "High heterogeneity") %>%
  ggplot(aes(x=Treatment, y = log_chng_biomass, fill = Cage.treatment)) +
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
  labs(x = "Accessibility", y = "Accumulated biomass", fill = "Cage treatment") +
  theme_bw() +
  scale_fill_manual(values=c("Cage" = "black","Open" = "white", "Control" = "grey"))+
  theme(text = element_text(size=15)) +
  removeGrid()
  


# no interaction ---> proceed.


# bring these together for plotting

cage_artifacts_plot_time<-cage_artifact_int_cover + 
  cage_artifact_int_rich + 
  cage_artifact_int_biomass + 
  plot_layout(ncol = 3, guides = "collect",widths = c(5, 5, 2.5)) +
  plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')


ggsave("./Figures/Manuscript/Final/Figure_S1.pdf",
       plot=cage_artifacts_plot_time,width=12,height=4)





## Cage effects plot ##
## (A) Cover
cage_effect_int_cover<-exp_data_actual_time %>% filter(!Cage.treatment == "Control") %>% 
  ggplot(aes(x=as.numeric(as.character(Day)), y = algal_cover, fill = Cage.treatment)) +
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
  labs(x = "Time point", y = "Total algal cover", fill = "Cage treatment") +
  scale_fill_manual(values=c("Cage" = "black","Open" = "white"))+
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=15)) +
  #ylim(0,15) +
  facet_grid(Bommie_treat~Treatment) +
  scale_x_continuous(breaks = c(0,25,50,75,100)) +
  removeGrid()



# consistent effect of cage --> proceed




## (B) richness
cage_effect_int_rich<-exp_data_actual_time %>% filter(!Cage.treatment == "Control") %>% 
  ggplot(aes(x=as.numeric(as.character(Day)), y = rich, fill = Cage.treatment)) +
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
  labs(x = "Time point", y = "Richness", fill = "Cage treatment") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=15)) +
  scale_fill_manual(values=c("Cage" = "black","Open" = "white"))+
  facet_grid(Bommie_treat~Treatment) +
  scale_x_continuous(breaks = c(0,25,50,75,100)) +
  removeGrid()

# interaction between richness and treatment --> richness higher on exposed complex plates 




## (C) biomass
biomass_short<-change_biomass_actual %>% 
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity")) %>%
  mutate(Cage.treatment = recode(Cage.treatment, 
                               Cage = "Cage",
                               Open = "Open",
                               Control = "cage control"))


cage_effect_int_biomass<-biomass_short %>% filter(!Cage.treatment == "cage control") %>% 
  ggplot(aes(x=Treatment, y = log_chng_biomass, fill = Cage.treatment)) +
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
  labs(x = "Treatment", y = "Accumulated biomass", fill = "Cage treatment") +
  theme_bw() +
  scale_fill_manual(values=c("Cage" = "black","Open" = "white"))+
  theme(text = element_text(size=15)) +
  facet_grid(Bommie_treat~.) +
  removeGrid()


# no interaction ---> proceed.


# bring these together for plotting

cage_effect_plot_time<-cage_effect_int_cover + 
  cage_effect_int_rich + 
  cage_effect_int_biomass + 
  plot_layout(ncol = 3, guides = "collect",widths = c(5, 5, 2.5)) +
  plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')


ggsave("./Figures/Manuscript/Final/Figure_S2.pdf",
       plot=cage_effect_plot_time,width=14,height=6)











######################################  Main tests  ########################################################

# Hypotheses/Contents:
# (1) Differences among bommie treatments
# (2) Differences among tile treatments
# (3) Interaction between two treatments
# (4) Differences in community structure
# (5) Variation in trajectories with Bray Curtis dissimilarity 





## Hypothesis 1-3 ####

#exp_data_plot<-exp_data_actual %>% filter(Cage.treatment== "Open")
exp_data_plot<-exp_data_actual %>% 
  filter(!Cage.treatment == "Control") %>% 
  mutate(Bommie_treat = recode(Bommie_treat, 
                               "Low heterogeneity" = "High detectability",
                               "High heterogeneity" = "Low detectability"),
         Treatment = recode(Treatment,
                            "Complex" = "Low access",
                            "Simple" = "High access"))

## Cover

cover_main<-ggplot(exp_data_plot, aes(x=as.numeric(as.character(Day)), y = algal_cover, color = Treatment, shape = Cage.treatment)) +
  stat_summary(geom = "point",
               #pch=21,
               fun = "mean", 
               size = 5) +
  stat_summary(geom = "line", 
               fun = "mean") +
  stat_summary(fun.data = mean_se,
               aes(group = interaction(Treatment, Cage.treatment)),
               geom = "errorbar", 
               width = 0.03,
               size = 0.8,
               color = "black") +
  theme(text = element_text(size=15)) +
  labs(x = "Day", y = "% of algal cover to bare space",  color = "Accessibility", shape = "Cage treatment") +
  #  guides(fill = FALSE) +
  theme_bw() +
  coord_cartesian(y = c(0,100),xlim = c(0,100)) +
  removeGrid() +
  scale_color_manual(values=c("Low access" = "red","High access" = "blue"))+
  facet_wrap(~Bommie_treat,ncol = 1)


#main_cover <-aov(log_algalcover ~ Treatment*Bommie_treat*Time_point, data = summarized_nocage_v2)
#summary(main_cover)
#TukeyHSD(cage_biomass_lm)


# means

exp_data_plot %>% group_by(Day) %>% summarize_at(vars(algal_cover),c(mean,sd)) %>% mutate_if(is.numeric,format,2)






## richness



richness_main<-ggplot(exp_data_plot,aes(x=as.numeric(as.character(Day)), y = rich, color = Treatment, shape = Cage.treatment)) +
  stat_summary(geom = "point",
               #pch=21,
               fun = "mean", 
               size = 5) +
  stat_summary(geom = "line", 
               fun = "mean") +
  stat_summary(fun.data = mean_se,
               aes(group = interaction(Treatment, Cage.treatment)),
               geom = "errorbar", 
               width = 0.03,
               size = 0.8,
               color = "black") +
  theme(text = element_text(size=15)) +
  labs(x = "Day",  color = "Accessibility", shape = "Cage treatment") +
  ylab(bquote('No. species plate'^-1)) +
  #  guides(fill = FALSE) +
  theme_bw() +
  coord_cartesian(ylim=c(0, 8),xlim = c(0,100)) +
  removeGrid()+
  scale_color_manual(values=c("Low access" = "red","High access" = "blue"))+
  facet_wrap(~Bommie_treat,ncol = 1)


# anova
#main_richness <-aov(rich ~ Treatment*as.factor(Day)*Bommie_treat, data = rich_reduced)
#summary(main_richness)
#TukeyHSD(main_richness)




# means

exp_data_plot %>% group_by(Treatment,Day) %>% summarize_at(vars(rich),c(mean,sd)) %>% mutate_if(is.numeric,format,2)





# time effect
# treatment effect
# Treatment by time effect

# proceed


## biomass
# use change_biomass_actual_nocontrol instead of change_biomass_actual_open
change_biomass_actual_nocontrol_plot<-change_biomass_actual_nocontrol %>% 
  mutate(Bommie_treat = recode(Bommie_treat, 
                               "Low heterogeneity" = "High detectability",
                               "High heterogeneity" = "Low detectability"),
         Treatment = recode(Treatment,
                            "Complex" = "Low access",
                            "Simple" = "High access"))

biomass_main<-ggplot(change_biomass_actual_nocontrol_plot, aes(x=Treatment, y = change_biomass, color = Treatment, shape = Cage.treatment)) +
  stat_summary(geom = "point",
               #pch=21,
               fun = "mean", 
               size = 5,
               position=position_dodge(0.6)) +
  stat_summary(fun.data = mean_se, 
               aes(group = interaction(Treatment, Cage.treatment)),
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6),
               color = "black") +
  labs(x = "Habitat complexity", color = "Accessibility", shape = "Cage treatment") +
  ylab(bquote('Accumulated biomass plate'^-1*'(g)')) +
  theme(legend.position = "none",text = element_text(size=15)) +
  scale_color_manual(values=c("Low access" = "red","High access" = "blue"))+
  theme_bw() +
  guides(color = "none") +
  removeGrid() +
  facet_wrap(~Bommie_treat,ncol = 1)


#biomass_main_test <-aov(log_chng_biomass ~ Treatment * Bommie_treat, data = biomass_reduced)
#summary(biomass_main_test)
#TukeyHSD(cage_biomass_lm)

# treatment effect; nothing else ---> proceed


# means

change_biomass_actual_open %>% group_by(Treatment) %>% summarize_at(vars(change_biomass),c(mean,sd)) %>% mutate_if(is.numeric,format,2)




## join these into one figure
H1_plot<-cover_main + richness_main + biomass_main + plot_layout(ncol=3,guides = "collect") & plot_annotation(tag_levels = "A")

ggsave("./Figures/Manuscript/Final/Figure_2.pdf",
       plot=H1_plot,width=11,height=5.8)















## Hypothesis 4 ####

# Use PERMANOVA and MDS to detect differences in community structure across treatments

community<-exp_data_actual %>% 
  filter(Cage.treatment == "Open") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"))


com_summarized <- community[c(12:30)]

env_summarized <- community[c(1:11)]

com_summarized_mds <- metaMDS(comm = com_summarized, distance = "bray", 
                              trace = FALSE, autotransform = TRUE, na.rm = FALSE)
com_summarized_mds$stress # 0.08934329
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
  dplyr::group_by(Treatment,Time_point,Bommie_treat) %>%
  dplyr::summarize(mean_MDS1 = mean(MDS1),
               mean_MDS2 = mean(MDS2),
               se_MDS1 = se_fn(MDS1),
               se_MDS2 = se_fn(MDS2))
  
summarized_mds$Time_point<-as.numeric(summarized_mds$Time_point)

  
community_MDS<-ggplot(summarized_mds[order(summarized_mds$Time_point),], aes(x=mean_MDS1,y=mean_MDS2)) + 
  geom_segment(data=vectors_sum,aes(x=0,xend=NMDS1/1.5,y=0,yend=NMDS2/1.5, color = species), size = 1,arrow = arrow(length = unit(0.3,"cm"))) + 
  #geom_point(aes(color = Treatment),size=5.5, color = "black") + 
  geom_point(aes(fill = Treatment),size=6, pch = 21) + 
  geom_errorbarh(aes(xmax = mean_MDS1 + se_MDS1, xmin = mean_MDS1 - se_MDS1)) +
  geom_errorbar(aes(ymax = mean_MDS2 + se_MDS2, ymin = mean_MDS2 - se_MDS2)) +
  geom_path(aes(fill=Treatment)) +
  #geom_text(data=vectors_sum, aes(x=NMDS1/1.5,y=NMDS2/1.5,label=species, color = species),size=5) +
  theme_bw() +
  removeGrid() +
  theme(text = element_text(size = 14, color = "black"),
        axis.text.x = element_text(color="black"), 
        axis.text.y = element_text(color="black")) +
  labs(color = "Species",fill = "Tile treatment", x = "MDS1",y="MDS2") +
  scale_fill_manual(values=c("Low access" = "red","High access" = "blue"))+
  facet_grid(~Bommie_treat) +
  coord_cartesian(x=c(-1.5,1), y=c(-0.5,0.5))


ggsave("./Figures/Manuscript/Final/Figure_3.pdf",
       plot=community_MDS,width=12,height=6)




### With Cage #
community_cage<-exp_data_actual %>% 
  filter(!Cage.treatment == "Control") %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"))


com_wcage_summarized <- community_cage[c(12:30)]

env_wcage_summarized <- community_cage[c(1:11)]

com_summarized_wcage_mds <- metaMDS(comm = com_wcage_summarized, distance = "bray", 
                              trace = FALSE, autotransform = TRUE, na.rm = FALSE)
com_summarized_wcage_mds$stress # 0.08934329
#summary(com_summarized_wcage_mds)


#com_summarized_mds$species
com_summarized_wcage_mds_points<-data.frame(com_summarized_wcage_mds$points)
mds_wcage<-merge(env_wcage_summarized,com_summarized_wcage_mds_points, by="row.names", all.x=TRUE)




## plotting
# with envfit over the top
ef_sum_wcage <- envfit(com_summarized_wcage_mds, com_wcage_summarized, permu = 999)


ef_sum.scrs_wcage <- as.data.frame(scores(ef_sum_wcage, display = "vectors")) # save species intrinsic values into dataframe
ef_sum.scrs_wcage <- cbind(ef_sum.scrs_wcage, species = rownames(ef_sum.scrs_wcage)) # add species names to dataframe
ef_sum.scrs_wcage <- cbind(ef_sum.scrs_wcage, pval = ef_sum_wcage$vectors$pvals) # add pvalues to dataframe so you can select species which are significant

vectors_wcage_sum <- subset(ef_sum.scrs_wcage, pval<=0.05)


se_fn <- function(x) sd(x) / sqrt(length(x)) # Create own function

summarized_mds_wcage <- mds_wcage %>% 
  dplyr::group_by(Treatment,Time_point,Bommie_treat,Cage.treatment) %>%
  dplyr::summarize(mean_MDS1 = mean(MDS1),
                   mean_MDS2 = mean(MDS2),
                   se_MDS1 = se_fn(MDS1),
                   se_MDS2 = se_fn(MDS2))

summarized_mds_wcage$Time_point<-as.numeric(summarized_mds_wcage$Time_point)

summarized_mds_wcage_plot<-summarized_mds_wcage %>% 
  mutate(Bommie_treat = recode(Bommie_treat, 
                             "Low heterogeneity" = "High detectability",
                             "High heterogeneity" = "Low detectability"),
       Treatment = recode(Treatment,
                          "Complex" = "Low access",
                          "Simple" = "High access"))

community_wcage_MDS<-ggplot(summarized_mds_wcage_plot[order(summarized_mds_wcage_plot$Time_point),], aes(x=mean_MDS1,y=mean_MDS2)) + 
  geom_segment(data=vectors_wcage_sum,aes(x=0,xend=NMDS1/1,y=0,yend=NMDS2/1, color = species), size = 1,arrow = arrow(length = unit(0.3,"cm"))) + 
  #geom_point(aes(color = Treatment),size=5.5, color = "black") + 
  geom_point(aes(fill = Treatment), alpha = 0.8,size=6, pch = 21) + 
  geom_point(data = . %>% filter(Time_point == "1"), 
             size=6, fill="grey", pch =21) +
  geom_errorbarh(aes(xmax = mean_MDS1 + se_MDS1, xmin = mean_MDS1 - se_MDS1)) +
  geom_errorbar(aes(ymax = mean_MDS2 + se_MDS2, ymin = mean_MDS2 - se_MDS2)) +
  geom_path(aes(fill=Treatment)) +
  theme_bw() +
  removeGrid() +
  theme(text = element_text(size = 14, color = "black"),
        axis.text.x = element_text(color="black"), 
        axis.text.y = element_text(color="black")) +
  labs(color = "Species",fill = "Accessibility", x = "MDS1",y="MDS2") +
  scale_fill_manual(values=c("Low access" = "red","High access" = "blue")) +
  #scale_color_manual(values=c("Complex" = "red","Simple" = "blue"))+
  facet_grid(Cage.treatment~Bommie_treat) #+
  coord_cartesian(x=c(-1.5,1.5), y=c(-0.5,0.5))
  #coord_cartesian(x=c(-1.5,1), y=c(-0.5,0.5))

  ggsave("./Figures/Manuscript/Final/Figure_3.pdf",
         plot=community_wcage_MDS,width=10,height=7)
  

#### PERMANOVAs ###

## (1) Cage effects test 
comm_control_data_summarized<-exp_control_data %>% 
  dplyr::filter(!Block == "1",
                !Tile_arrangement == "Mix") %>%
  dplyr::group_by(Day,Cage.treatment,Treatment,Bommie_treat,Bommie_no,Block) %>%
  summarise_all(mean, na.rm = TRUE)


com_control <- as.data.frame(comm_control_data_summarized[c(12:30)])
env_control <- as.data.frame(comm_control_data_summarized[c(1:11)])

env_control <- env_control %>% mutate_all(as.factor)


# Permanova
com_control_perm<-adonis2(com_control ~ Treatment * 
                           Cage.treatment * 
                           Bommie_treat*
                           Time_point*
                           Block,
                 data = env_control,
                 strata = env_control$Bommie_no,
                 method = "bray", 
                 permutations = 999) 
com_control_perm

# pairwise differences 
com_control_veg<-vegdist(com_control)
pairwise.adonis(com_control_veg,env_control[,"Cage.treatment"])

## Results
# Pairwise differences in community structure between open and controls
# A bunch of interactions 





## (2) Do cages reduce herbivory?
# df from LMER script
com_cage <- as.data.frame(exp_data_actual_time_nocontrol[c(12:30)])

env_cage <- as.data.frame(exp_data_actual_time_nocontrol[c(1:11)])

env_cage <- env_cage %>% mutate_all(as.factor)

# Permanova
com_cage_perm<-adonis2(com_cage ~ Treatment* 
                       Cage.treatment* 
                       Bommie_treat*
                       Time_point*
                       Block,
                      data = env_cage,
                      strata = env_cage$Bommie_no,
                      method = "bray", 
                      permutations = 999) 
com_cage_perm

# pairwise differences 
com_cage_veg<-vegdist(com_cage)
pairwise.adonis(com_cage_veg,env_cage[,"Cage.treatment"])


com_caged_mds <- metaMDS(comm = com_cage, distance = "bray", 
                              trace = FALSE, autotransform = F, na.rm = T)

com_caged_mds$species
com_caged_mds_df<-data.frame(com_caged_mds$points)
cage_mds<-merge(env_cage,com_caged_mds_df, by="row.names", all.x=TRUE)


cage_mds_plot<-cage_mds %>% 
  mutate(Bommie_treat = recode(Bommie_treat, 
                               "Low heterogeneity" = "High detectability",
                               "High heterogeneity" = "Low detectability"),
         Treatment = recode(Treatment,
                            "Complex" = "Low access",
                            "Simple" = "High access"))


cage_community_MDS<-ggplot(cage_mds_plot, aes(x=MDS1,y=MDS2)) + 
  geom_point(aes(shape = Treatment, color = Block),size=5) + 
  theme_bw() +
  removeGrid() +
  theme(text = element_text(size = 14, color = "black"),
        axis.text.x = element_text(color="black"), 
        axis.text.y = element_text(color="black")) +
  labs(color = "Block",shape = "Accessibility",linetype = "Accessibility", 
       x = "MDS1",y="MDS2") +
  facet_grid(Cage.treatment~Bommie_treat) +
  stat_ellipse(aes(color = Block),level = 0.95) +
  stat_ellipse(aes(linetype = Treatment),level = 0.95)

ggsave("./Figures/Manuscript/Final/Figure_S3.pdf",
       plot=cage_community_MDS,width=10,height=7)


## Results
# Pairwise differences in community structure between open and cages -- > good
# A bunch of interactions 


## (3) Reduced model 
com_reduced <- as.data.frame(exp_data_actual_open[c(12:30)])

env_reduced <- as.data.frame(exp_data_actual_open[c(1:11)])

env_reduced <- env_reduced %>% mutate_all(as.factor)


com_perm<-adonis(com_reduced ~ Treatment * 
                   Bommie_treat * 
                   Time_point,
                 data = env_reduced, 
                 strata = env_reduced$Bommie_no,
                 method = "bray", 
                 permutations = 999)

com_perm

## Results
# interaction between complexity arrangement and heterogeneity
# interaction between complexity arrangement and time



## Hypothesis 5 #####

# remember that time_point "1" gets added later
community_diss<-exp_data_actual_nocontrol %>% 
  filter(!Cage.treatment == "Cage")



dissim_df<- community_diss %>% 
  mutate(Tile_time = paste0(Tile_no,sep = "_",Time_point)) %>% 
  filter(!Time_point == "1") # remove T1 for analysis

com_bray <- dissim_df[c(13:30)] # BARE should not be part of the community
rownames(com_bray)<-dissim_df$Tile_time


env_bray <- dissim_df[c(1:10,31)]

com_sim <- vegdist(com_bray, distance = "bray", trace = FALSE, autotransform = FALSE)
### IGNORE WARNING ###

distmat<-as.matrix(com_sim,labels=TRUE)

dist_df <- melt(as.matrix(distmat), varnames = c("TILE_1", "TILE_2"))
names(dist_df[3])<-"distance"

dist_df_2<-dist_df %>% filter(!TILE_1 == TILE_2)

# now I add both to a dataframe with factor labels in the same way I did in the "correlation" df from the MPA paper

dist_df_2$Tile_1 <- sub("_[^_]*", "", dist_df_2$TILE_1)
dist_df_2$Time_point_1 <- sub("[^_]*_", "", dist_df_2$TILE_1)

dist_df_2$Tile_2 <- sub("_[^_]*", "", dist_df_2$TILE_2)
dist_df_2$Time_point_2 <- sub("[^_]*_", "", dist_df_2$TILE_2)


dist_df_2<-dist_df_2 %>% tibble::rowid_to_column("ID")



# New separate dfs 
Tile1_df<-dist_df_2[c(1,4,5,6)]
names(Tile1_df)<-c("ID","value","Tile_no","Time_point")

Tile2_df<-dist_df_2[c(1,4,7,8)]
names(Tile2_df)<-c("ID","value","Tile_no","Time_point")

# Merge

Tile_1_merged<-merge(env_bray[-c(1,5,10,11)],Tile1_df) # col 12 remains -- this was changed

Tile_2_merged<-merge(env_bray[-c(1,5,10,11)],Tile2_df) # col 12 remains -- this was changed

names(Tile_1_merged)
# Rename 
names(Tile_1_merged)<-c("Tile_1","Time_point_1","Treatment_1",
                        "Tile_arrangement_1","Bommie_treat_1",
                        "Bommie_no_1","Block_1","ID","value")

names(Tile_2_merged)<-c("Tile_2","Time_point_2","Treatment_2",
                        "Tile_arrangement_2","Bommie_treat_2",
                        "Bommie_no_2","Block_2","ID","value")

dist_merged<-merge(Tile_1_merged,Tile_2_merged, by=c("ID","value"))



# make "both" groups


dist_merged_grouped_2<-dist_merged %>% 
  filter(Tile_arrangement_1==Tile_arrangement_2,
         Bommie_treat_1 ==Bommie_treat_2,
         Time_point_1==Time_point_2,
         Bommie_no_1==Bommie_no_2,
         !Tile_1 == Tile_2) #          !Day_1 == !Day_2)

dist_merged_grouped_3<-dist_merged_grouped_2 %>% 
  distinct(Bommie_no_1,Time_point_1, .keep_all = TRUE) %>%
  mutate(Bommie_treat_1 = recode(Bommie_treat_1, 
                                 Clean = "Low heterogeneity",
                                 Turbinaria = "High heterogeneity"))


# Bring initial time point back in
dist_initial<-dist_merged_grouped_3 %>% 
  filter(Time_point_1 == "2") %>% # take one time point; change time point to "0"; make all "values" "0" for intitial time point
  mutate(value = 0,
         Time_point_1 = case_when(Time_point_1 == 2 ~ 1),
         Time_point_2 = case_when(Time_point_2 == 2 ~ 1))

dist_merged_complete<-rbind(dist_initial,dist_merged_grouped_3)

# change levels of time to date
dist_merged_complete$Time_point_1<-as.factor(dist_merged_complete$Time_point_1)
dist_merged_complete$Time_point_1<-revalue(dist_merged_complete$Time_point_1, c("1" = "0",
                                                                                  "2"="18", 
                                                                                  "3"="33",
                                                                                  "4"="67",
                                                                                  "5"="92"))


# Replace NaN with zero -- this is from 0 dissimilarity for first time point
#dist_merged_complete$value[is.nan(dist_merged_complete$value)] <- 0

#write.csv(dist_merged_grouped_3,"/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Data/Synchrony/bray_values.csv",row.names = F)


# Plot this with tidyverse
bray_fig<-dist_merged_complete %>% 
  ggplot(aes(x=as.numeric(as.character(Time_point_1)),y=value,group=Tile_arrangement_1,fill=Tile_arrangement_1)) +
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



#ggsave("./Figures/Manuscript/Final/Figure_5.pdf", plot=bray_fig,width=10,height=6)



# test

dist_test_df<-dist_merged_grouped_3 %>% filter(!Day_1 == "0")

# bring block back in 
block_df_2<-read.csv("./Data/block_data.csv")

block_df_2$Tile_no_1<-as.factor(block_df_2$Tile_no)
block_df_2$Bommie_no_1<-as.factor(block_df_2$Bommie_no)

dist_test_df$Tile_no_1<-as.factor(dist_test_df$Tile_1)
dist_test_df$Bommie_no_1<-as.factor(dist_test_df$Bommie_no_1)


dist_test_df<-merge(dist_test_df,block_df_2)

bray_aov<-lm(value~Tile_arrangement_1*Time_point_1*Bommie_treat_1,data=dist_test_df)
anova(bray_aov)



# summary check
dist_merged_grouped_3 %>% group_by(Tile_arrangement_1,Day_1) %>% summarize_at(vars(value),c(mean,sd))



### Post-review -- dissim with cages ###

# remember that time_point "1" gets added later
community_diss_wcage<-exp_data_actual_nocontrol %>% filter(Cage.treatment == "Cage")


dissim_cage_df<- community_diss_wcage %>% 
  mutate(Tile_time = paste0(Tile_no,sep = "_",Time_point)) %>% 
  filter(!Time_point == 1)

com_bray_cage <- dissim_cage_df[c(13:29)] # BARE should not be in here
rownames(com_bray_cage)<-dissim_cage_df$Tile_time


env_cage_bray <- dissim_cage_df[c(1:10,31)]
env_cage_bray_reduced<-env_cage_bray %>% 
  group_by(Tile_no,Bommie_no,Treatment,Tile_arrangement,Bommie_treat,Cage.treatment,Block) %>%
  summarize_all(mean) %>%
  select(1:7)

com_sim_cage <- vegdist(com_bray_cage, distance = "bray", trace = FALSE, autotransform = FALSE)
### IGNORE WARNING ###

distmat_cage<-as.matrix(com_sim_cage,labels=TRUE)

dist_cage_df <- melt(as.matrix(distmat_cage), varnames = c("TILE_1", "TILE_2"))
names(dist_cage_df[3])<-"distance"

dist_cage_df_2<-dist_cage_df %>% filter(!TILE_1 == TILE_2)

# now I add both to a dataframe with factor labels in the same way I did in the "correlation" df from the MPA paper

dist_cage_df_2$Tile_1 <- sub("_[^_]*", "", dist_cage_df_2$TILE_1)
dist_cage_df_2$Time_point_1 <- sub("[^_]*_", "", dist_cage_df_2$TILE_1)

dist_cage_df_2$Tile_2 <- sub("_[^_]*", "", dist_cage_df_2$TILE_2)
dist_cage_df_2$Time_point_2 <- sub("[^_]*_", "", dist_cage_df_2$TILE_2)


dist_cage_df_2<-dist_cage_df_2 %>% tibble::rowid_to_column("ID")



# New separate dfs 
Tile1_cage_df<-dist_cage_df_2[c(1,4,5,6)]
names(Tile1_cage_df)<-c("ID","value","Tile_no","Time_point")

Tile2_cage_df<-dist_cage_df_2[c(1,4,7,8)]
names(Tile2_cage_df)<-c("ID","value","Tile_no","Time_point")

# Merge
Tile_1_cage_merged<-merge(env_cage_bray_reduced,Tile1_cage_df) # col 12 remains -- this was changed
dim(Tile_1_cage_merged)

Tile_2_cage_merged<-merge(env_cage_bray_reduced,Tile2_cage_df) # col 12 remains -- this was changed
dim(Tile_2_cage_merged)

#names(Tile_1_cage_merged)

# Rename 
names(Tile_1_cage_merged)<-c("Tile_1","Bommie_no_1","Treatment_1",
                        "Tile_arrangement_1","Bommie_treat_1",
                        "Cage.treatment_1","Block_1","ID","value","Time_point_1")

names(Tile_2_cage_merged)<-c("Tile_2","Bommie_no_2","Treatment_2",
                             "Tile_arrangement_2","Bommie_treat_2",
                             "Cage.treatment_2","Block_2","ID","value","Time_point_2")

dist_merged_cage<-merge(Tile_1_cage_merged,Tile_2_cage_merged, by=c("ID","value"))

dim(dist_merged_cage)

# make "both" groups

dist_merged_cage_grouped<-dist_merged_cage %>% 
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
      Bommie_treat_1 == "Low heterogeneity" & Bommie_treat_2 == "High heterogeneity" ~ "NULL",
      Bommie_treat_1 == "High heterogeneity" & Bommie_treat_2 == "Low heterogeneity" ~ "NULL",
      Bommie_treat_1 == "High heterogeneity" & Bommie_treat_2 == "High heterogeneity" ~ "High heterogeneity",
      Bommie_treat_1 == "Low heterogeneity" & Bommie_treat_2 == "Low heterogeneity" ~ "Low heterogeneity"),
    Cage_treat_merged = case_when(
      Cage.treatment_1 == "Open" & Cage.treatment_2 == "Open" ~ "Open",
      Cage.treatment_1 == "Cage" & Cage.treatment_2 == "Open" ~ "Cage_Open",
      Cage.treatment_1 == "Open" & Cage.treatment_2 == "Cage" ~ "Cage_Open",
      Cage.treatment_1 == "Cage" & Cage.treatment_2 == "Cage" ~ "Cage"))


dist_merged_cage_grouped_2<-dist_merged_cage %>% 
  filter(Tile_arrangement_1==Tile_arrangement_2,
         Bommie_treat_1 ==Bommie_treat_2,
         Time_point_1==Time_point_2,
         Bommie_no_1==Bommie_no_2,
         Cage.treatment_1==Cage.treatment_2, # could also compare uncaged to caged....but wo
         !Tile_1 == Tile_2)

dist_merged_cage_grouped_3<-dist_merged_cage_grouped_2 %>% distinct(Bommie_no_1,Time_point_1, .keep_all = TRUE) %>%
  mutate(Bommie_treat_1 = recode(Bommie_treat_1, 
                                 Clean = "Low heterogeneity",
                                 Turbinaria = "High heterogeneity"))

# bring initial back in 
dist_cage_initial<-dist_merged_cage_grouped_3 %>% 
  filter(Time_point_1 == "2") %>% # take one time point; change time point to "0"; make all "values" "0" for intitial time point
  mutate(value = 0,
         Time_point_1 = case_when(Time_point_1 == 2 ~ 1),
         Time_point_2 = case_when(Time_point_2 == 2 ~ 1))

dist_merged_cage_complete<-rbind(dist_cage_initial,dist_merged_cage_grouped_3)


# change levels of time to date
dist_merged_cage_complete$Time_point_1<-revalue(dist_merged_cage_complete$Time_point_1, c("1" = "0",
                                                                                  "2"="18", 
                                                                                  "3"="33",
                                                                                  "4"="67",
                                                                                  "5"="92"))


# Replace NaN with zero -- this is from 0 dissimilarity for first time point
#dist_merged_cage_complete$value[is.nan(dist_merged_cage_complete$value)] <- 0

#write.csv(dist_merged_grouped_3,"/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Data/Synchrony/bray_values.csv",row.names = F)

# Plot this with tidyverse
bray_cage_fig<-dist_merged_cage_complete %>% 
  ggplot(aes(x=as.numeric(as.character(Time_point_1)),y=value,group=Tile_arrangement_1,fill=Tile_arrangement_1)) +
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
               linewidth = 1) +
  theme_bw() +
  theme(text = element_text(size=15),
        legend.position = c(0.75,0.85)) +
  scale_fill_manual(values=c("Complex" = "red","Mix" = "white","Simple" = "blue"))+
  labs(x = "Day", y = "Bray-Curtis distance", fill = "Habitat complexity") +
  facet_grid(~Bommie_treat_1) +
  coord_cartesian(ylim = c(0,1), xlim = c(0,100))



# ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Final/Figure_5.pdf",
#        plot=bray_fig,width=10,height=6)



#test
dist_test_cage_df<-dist_merged_cage_complete %>% filter(!Time_point_1 == "0")

# bring block back in 
#block_df_2<-read.csv("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Data/block_data.csv")

block_df_2$Tile_no_1<-as.factor(block_df_2$Tile_no)
block_df_2$Bommie_no_1<-as.factor(block_df_2$Bommie_no)

dist_test_df$Tile_no_1<-as.factor(dist_test_df$Tile_1)
dist_test_df$Bommie_no_1<-as.factor(dist_test_df$Bommie_no_1)


dist_test_df<-merge(dist_test_df,block_df_2)

bray_cage_aov<-lm(value~Tile_arrangement_1*Time_point_1*Bommie_treat_1,data=dist_test_cage_df)
anova(bray_aov)

## Interpretation
# in the absence of herbivory (cages) communities are more dissimilar over time in simple and mixed metacommunities
# differs among heterogeneity treatments
# generally herbivory leads to greater dissimilarity in community structure


# Bring open and caged together for combined figure
dist_merged_grouped_formerge<-dist_merged_complete
dist_merged_grouped_formerge$Cage.treatment_1 <- "Open"
dist_merged_grouped_formerge$Cage.treatment_2 <- "Open"

dist_merged_all_list<-list(dist_merged_cage_complete,dist_merged_grouped_formerge)


dist_merged_all<-ldply(dist_merged_all_list, data.frame)
dist_merged_all$Tile_time_1<-NULL
dist_merged_all$Tile_time_2<-NULL
dist_merged_all$Day_1<-NULL
dist_merged_all$Day_2<-NULL


# plot together

dist_merged_all_plot<-dist_merged_all %>% 
  mutate(Bommie_treat_1 = recode(Bommie_treat_1, 
                               "Low heterogeneity" = "High detectability",
                               "High heterogeneity" = "Low detectability"),
         Tile_arrangement_1 = recode(Tile_arrangement_1,
                            "Complex" = "Low access",
                            "Mix" = "Mixed access",
                            "Simple" = "High access"))

bray_all_fig<-dist_merged_all_plot %>% 
  ggplot(aes(x=as.numeric(as.character(Time_point_1)),y=value,group=Tile_arrangement_1,fill=Tile_arrangement_1)) +
  stat_summary(aes(fill = Tile_arrangement_1),
               geom = "line",
               fun = "mean",
               linewidth = 1) +
  stat_summary(aes(fill = Tile_arrangement_1), 
               geom = "point",
               pch=21,
               alpha = 0.8,
               fun = "mean", 
               size = 6) +
  stat_summary(data = . %>% filter(Time_point_1 == "0"),  # make the initial time point grey for viewing clarity
               geom = "point",
               fun = "mean", 
               size=6,
               fill="grey", 
               pch =21) +
  stat_summary(aes(fill=Tile_arrangement_1),
               fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01) +
  theme_bw() +
  theme(text = element_text(size=15),
        legend.position = c(0.7,0.9),
        legend.background = element_blank()) +
  scale_fill_manual(values=c("Low access" = "red","Mixed access" = "white","High access" = "blue"))+
  labs(x = "Day", y = "Bray-Curtis distance", fill = "Accessibility") +
  facet_grid(Cage.treatment_1~Bommie_treat_1) +
  removeGrid() +
  coord_cartesian(ylim = c(0,1), xlim = c(0,100))


ggsave("./Figures/Manuscript/Final/Figure_4.pdf",
       plot=bray_all_fig,width=10,height=7)








################# END #################
