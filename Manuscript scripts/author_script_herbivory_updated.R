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
library(ggExtra)
library(plyr)




######################################  Exploratory  ########################################################

## Cage artifacts plot ##
## (A) Cover
cage_artifact_int_cover<- exp_data_actual_time %>% filter(!Bommie_treat == "High heterogeneity") %>%
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
  scale_x_continuous(breaks = c(0,25,50,75,90)) +
  removeGrid()


# consistent effect of cage --> proceed




## (B) richness
cage_artifact_int_rich<-exp_data_actual_time %>% filter(!Bommie_treat == "High heterogeneity") %>%
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
  scale_x_continuous(breaks = c(0,25,50,75,90)) +
  removeGrid()

# interaction between richness and treatment --> richness higher on exposed complex plates 
# have to rework this a wee bit. Something is off.




## (C) biomass
biomass_short<-change_biomass_actual %>% 
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity")) %>%
  mutate(Cage.treatment = recode(Cage.treatment, 
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
  labs(x = "Treatment", y = "Accumulated biomass", fill = "Cage treatment") +
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


ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Final/Figure_S1.pdf",
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
  scale_x_continuous(breaks = c(0,25,50,75,90)) +
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
  scale_x_continuous(breaks = c(0,25,50,75,90)) +
  removeGrid()

# interaction between richness and treatment --> richness higher on exposed complex plates 
# have to rework this a wee bit. Something is off.




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


ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Final/Figure_S2.pdf",
       plot=cage_effect_plot_time,width=14,height=6)











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
exp_data_plot<-exp_data_actual %>% filter(Cage.treatment== "Open")

## Cover

cover_main<-ggplot(exp_data_plot, aes(x=as.numeric(as.character(Day)), y = algal_cover, fill = Treatment)) +
  stat_summary(aes(fill = Treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6) +
  stat_summary(geom = "line", 
               aes(fill = Treatment), 
               fun = "mean") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.03) +
  theme(text = element_text(size=15)) +
  labs(x = "Day", y = "% of algal cover to bare space",  fill = "Habitat complexity") +
  #  guides(fill = FALSE) +
  theme_bw() +
  coord_cartesian(y = c(0,100)) +
  scale_fill_manual(values=c("Complex" = "red","Simple" = "blue"))+
  facet_wrap(~Bommie_treat,ncol = 1)


#main_cover <-aov(log_algalcover ~ Treatment*Bommie_treat*Time_point, data = summarized_nocage_v2)
#summary(main_cover)
#TukeyHSD(cage_biomass_lm)


# means

exp_data_plot %>% group_by(Day) %>% summarize_at(vars(algal_cover),c(mean,sd)) %>% mutate_if(is.numeric,format,2)






## richness



richness_main<-ggplot(exp_data_plot,aes(x=as.numeric(as.character(Day)), y = rich, fill = Treatment)) +

  stat_summary(aes(fill = Treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6) +
  stat_summary(geom = "line", 
               aes(fill = Treatment), 
               fun = "mean") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01) +
  theme(text = element_text(size=15)) +
  labs(x = "Day",  fill = "Habitat complexity") +
  ylab(bquote('No. species plate'^-1)) +
  #  guides(fill = FALSE) +
  theme_bw() +
  coord_cartesian(ylim=c(0, 8)) +
  scale_fill_manual(values=c("Complex" = "red","Simple" = "blue"))+
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

biomass_main<-ggplot(change_biomass_actual_open, aes(x=Treatment, y = change_biomass, fill = Treatment)) +
  stat_summary(aes(fill = Treatment), 
               geom = "point",
               pch=21,
               fun = "mean", 
               size = 6,
               position=position_dodge(0.6)) +
  stat_summary(geom = "line", 
               aes(fill = Treatment), 
               fun = "mean") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.01,
               position=position_dodge(0.6)) +
  labs(x = "Habitat complexity", fill = "Habitat complexity") +
  ylab(bquote('Accumulated biomass plate'^-1*'(g)')) +
  theme(legend.position = "none",text = element_text(size=15)) +
  scale_fill_manual(values=c("Complex" = "red","Simple" = "blue"))+
  theme_bw() +
  coord_cartesian(y=c(0,35)) +
  facet_wrap(~Bommie_treat,ncol = 1)


#biomass_main_test <-aov(log_chng_biomass ~ Treatment * Bommie_treat, data = biomass_reduced)
#summary(biomass_main_test)
#TukeyHSD(cage_biomass_lm)

# treatment effect; nothing else ---> proceed


# means

change_biomass_actual_open %>% group_by(Treatment) %>% summarize_at(vars(change_biomass),c(mean,sd)) %>% mutate_if(is.numeric,format,2)




## join these into one figure
H1_plot<-cover_main + richness_main + biomass_main + plot_layout(ncol=3,guides = "collect")

ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Final/Figure_2.pdf",
       plot=H1_plot,width=11,height=6)















### Hypothesis 4 ####

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
  scale_fill_manual(values=c("Complex" = "red","Simple" = "blue"))+
  facet_grid(~Bommie_treat) +
  coord_cartesian(x=c(-1.5,1), y=c(-0.5,0.5))


ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Final/Figure_3.pdf",
       plot=community_MDS,width=12,height=6)





#### PERMANOVAs ####

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
com_control_perm<-adonis(com_control ~ Treatment * 
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
com_cage_perm<-adonis(com_cage ~ Treatment* 
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


cage_community_MDS<-ggplot(cage_mds, aes(x=MDS1,y=MDS2)) + 
  geom_point(aes(shape = Treatment, color = Block),size=5) + 
  theme_bw() +
  removeGrid() +
  theme(text = element_text(size = 14, color = "black"),
        axis.text.x = element_text(color="black"), 
        axis.text.y = element_text(color="black")) +
  labs(color = "Block",shape = "Complexity treatment",linetype = "Complexity treatment", 
       x = "MDS1",y="MDS2") +
  facet_grid(Cage.treatment~Bommie_treat) +
  stat_ellipse(aes(color = Block),level = 0.95) +
  stat_ellipse(aes(linetype = Treatment),level = 0.95)

ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Final/Figure_S3.pdf",
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









##### Hypothesis 5A - synchrony ####
exp_data_actual_nocontrol<-exp_data_actual %>% filter(!Cage.treatment == "Control")
exp_data_actual_nocontrol<-exp_data_actual_nocontrol[, colSums(exp_data_actual_nocontrol != 0) > 0]

exp_data_actual_nocontrol_V2<-exp_data_actual_nocontrol
exp_data_actual_nocontrol_V2$Name <- NULL
exp_data_actual_nocontrol_V2$algal_cover <- NULL
exp_data_actual_nocontrol_V2$log_algalcover <- NULL


## Community synchrony ##
long_data<-exp_data_actual_nocontrol_V2 %>%
  pivot_longer(!c(Time_point,Date,Cage.treatment,Block,Treatment,Tile_no,Tile_arrangement,Bommie_treat,Bommie_no,Day), names_to = "species", values_to = "cover")


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
  group_by(Tile_no,Treatment,Bommie_treat,Cage.treatment,Tile_arrangement) %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
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






loreau_comm<-ggplot(loreau_com_complete, aes(x=Bommie_treat, y = as.numeric(synchrony), fill = Treatment)) +
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


com_sync_aov<-aov(synchrony~Treatment*Bommie_treat,loreau_com_complete)
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



# merge with metadata

metacom_table <- meta_table %>% 
  filter(Cage.treatment =="Open") %>%
  group_by(Bommie_treat,Bommie_no,Tile_arrangement) %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                                 Clean = "Low heterogeneity",
                                 Turbinaria = "High heterogeneity")) %>%
  #filter(!Cage.treatment %in% c("Control","Cage")) %>%
  summarize_all(mean)


loreau_metacom_complete<-merge(synchrony_metacom_loreau,metacom_table)




loreau_metacom<-ggplot(loreau_metacom_complete, aes(x=Bommie_treat, y = as.numeric(synchrony), fill = Tile_arrangement)) +
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


ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Final/Figure_4.pdf",
       plot = synchrony_plot, width = 6,height = 9)






###### Hypothesis 5B #####

# remember that time_point "1" gets added later
community_diss<-exp_data_actual_nocontrol %>% 
  filter(!Cage.treatment == "Cage")



dissim_df<- community_diss %>% 
  mutate(Tile_time = paste0(Tile_no,sep = "_",Time_point))

com_bray <- dissim_df[c(12:29)]
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

Tile_1_merged<-merge(env_bray[-c(4,7,9)],Tile1_df) # col 12 remains -- this was changed

Tile_2_merged<-merge(env_bray[-c(4,7,9)],Tile2_df) # col 12 remains -- this was changed

names(Tile_1_merged)
# Rename 
names(Tile_1_merged)<-c("Tile_1","Time_point_1","Treatment_1",
                        "Tile_arrangement_1","Bommie_treat_1",
                        "Bommie_no_1","Day_1","Tile_time_1","ID","value")

names(Tile_2_merged)<-c("Tile_2","Time_point_2","Treatment_2",
                        "Tile_arrangement_2","Bommie_treat_2",
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
      Bommie_treat_1 == "Clean" & Bommie_treat_2 == "Clean" ~ "Clean"))


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

#write.csv(dist_merged_grouped_3,"/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Data/Synchrony/bray_values.csv",row.names = F)


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



ggsave("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Final/Figure_5.pdf",
       plot=bray_fig,width=10,height=6)



# test

dist_test_df<-dist_merged_grouped_3 %>% filter(!Day_1 == "0")

# bring block back in 
block_df_2<-read.csv("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Data/block_data.csv")

block_df_2$Tile_no_1<-as.factor(block_df_2$Tile_no)
block_df_2$Bommie_no_1<-as.factor(block_df_2$Bommie_no)

dist_test_df$Tile_no_1<-as.factor(dist_test_df$Tile_1)
dist_test_df$Bommie_no_1<-as.factor(dist_test_df$Bommie_no_1)


dist_test_df<-merge(dist_test_df,block_df_2)

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


bray_v_richness<-aov(value~richness*Tile_arrangement_1*Bommie_treat_1,dist_even_df)
summary(bray_v_richness)

bray_v_richness<-lm(value~richness*Tile_arrangement_1*Bommie_treat_1,dist_even_df)
summary(bray_v_richness)

library("lsmeans")

richness_treat_slopes <- lstrends(bray_v_richness, ~ Tile_arrangement_1, var = "richness")



#### Extra stats #####

## Regression between species richness and bray curtis distance -- for appendix
## Also....place species evenness as a second axis (or bubble size) for bray curtis plot
# "Does diversity drive pattern on mixed tiles; relative abundance driving pattern on simple tiles"



## Species table 

spp_mean_sd<-community %>% 
  filter(!Time_point == "1") %>%
  summarize_at(vars(BARE:Turf), c(mean,sd)) 
