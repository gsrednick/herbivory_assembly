# LMER script #

# probably not necessary as this isnt really a mixed model


## try with LMER 
library(lme4)
library(lmerTest)      # for p-values
library(performance)
library(tidyverse)
library(effects)
library(ggeffects)
library(purrr)
library(broom)
library(emmeans)


# for this report a table with: parameter, estimate, SE, and t
# run against null model; check and see if the alternative model better predicts variation (with AIC, Chi, and P)


### a note --- not sure this is correct for dealing with crossed random effects. it might be something more like this
### another note. these tables are likely fine.....just put them in the appendix. Make them similar to Baumann et al. 2021

# (1 + Treatment*Bommie_treat|Bommie_no)

## Cover ####
exp_data_actual_time<-exp_data_actual %>% filter(!Time_point == "1")

exp_data_actual_time$Day<-as.factor(exp_data_actual_time$Day)

exp_data_actual_time$log_rich<-log(exp_data_actual_time$rich+1)


# Crossed-nested linear mixed Model - algal species richness - Cage v. control effects
# no bommie_treatment here -- controls dont have this treatment 

cover_lmer_m0<-lmer(log_algalcover ~
                     Day * 
                     Treatment *
                      Block*
#                     Bommie_treat * # rationale == no controls for bommie treat
                     Cage.treatment *
                     (1|Bommie_no/Tile_no) +
                   data = exp_data_actual_time,
                   REML = F)
summary(cover_lmer_m0)
ranova(cover_lmer_m0)
anova(cover_lmer_m0)
summ(cover_lmer_m0, digits = 4)



cover_m0_plot <- ggpredict(cover_lmer_m0, terms = c("Day","Treatment", "Cage.treatment"))
plot(cover_m0_plot)

m0_pairwise<-emmeans(cover_lmer_m0, list(pairwise ~ Cage.treatment), adjust = "tukey")



# need to do another just for mixed tiles 

# i think we have to give bommie numbers to Controls with NA or else it wont run, but theres no rep maybe just put them on the same bommie?

# New attempt -- look at only bommies with the same tile types, average them. Exclude block XX.
block_df<-read.csv("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Data/block_data.csv")
block_df$Tile_no<-as.factor(block_df$Tile_no)
block_df$Bommie_no<-as.factor(block_df$Bommie_no)

exp_control_data<- left_join(exp_data_actual_time,block_df, by = c("Tile_no","Block")) %>% 
  mutate(Bommie_no = ifelse(is.na(Bommie_no.x), Bommie_no.y, Bommie_no.x)) %>%
  select(-Bommie_no.y, -Bommie_no.x)


exp_control_data_summarized<-exp_control_data %>% 
  dplyr::filter(!Block == "1",
                !Tile_arrangement == "Mix") %>%
  dplyr::group_by(Day,Cage.treatment,Treatment,Bommie_treat,Bommie_no,Block) %>%
  summarise_at(c("rich","algal_cover","log_rich","log_algalcover"), mean, na.rm = TRUE)

exp_control_data_summarized$Block<-as.factor(exp_control_data_summarized$Block)
exp_control_data_summarized$Bommie_no<-as.factor(exp_control_data_summarized$Bommie_no)

#count(unique(as.factor(exp_control_data$Bommie_no)))

cover_lmer_mix_m0<-lmer(log_algalcover ~
                      Day * 
                      Treatment *
                      #Bommie_treat *
                      Cage.treatment +
                      (1|Block/Bommie_no),
                    data = exp_control_data_summarized,
                    REML = F)

summary(cover_lmer_mix_m0)
cover_m0_rand<-ranova(cover_lmer_mix_m0)
cover_m0_aov<-anova(cover_lmer_mix_m0)
summ(cover_lmer_mix_m0, digits = 4)



# we would expect there to be a difference here -- although....this isnt random. 
# Wouldnt i want this as a fixed effect and to test this? Probably fine...


# Plotting
dat <- ggpredict(cover_lmer_mix_m0, terms = c("Day","Treatment", "Cage.treatment"))
plot(dat, connect.lines = T)

m0_pairwise<-emmeans(cover_lmer_mix_m0, list(pairwise ~ Cage.treatment), adjust = "tukey")

# print table 
write.csv(as.matrix(cover_m0_aov), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/cover_m0.csv", na = "")
write.csv(as.matrix(cover_m0_rand), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/cover_0_rand.csv", na = "")





## Results 
# Pattern with controls is the same as open plots -- no interactions of concern -- move ahead
# there are no controls for mixes so no worries. We just want to know that they are different, which they are.


# Crossed-nested linear mixed Model - algal species richness - Cage v. open effects


exp_data_actual_time_nocontrol<-exp_data_actual_time %>% filter(!Cage.treatment == "Control")

cover_lmer_m1<-lmer(log_algalcover ~ 
                     Day* 
                     Treatment *
                     Bommie_treat *
                     Cage.treatment +
                    (1|Bommie_no/Tile_no) + 
                    (1|Block),
                   data = exp_data_actual_time_nocontrol,
                   REML = F)


# this is for checking
check_collinearity(cover_lmer_m1) # there is collinearality and heterogeneity of variances. -- log transform helps
#performance::check_model(cover_lmer_m1) 
#check_outliers(cover_lmer_m1)
summary(cover_lmer_m1)
cover_m1_aov<-anova(cover_lmer_m1)
summ(cover_lmer_m1, digits = 4)
# export_summs(cover_lmer_m1, digits = 4, to.file = "pdf",file.name = "cover_lmer_m1.pdf")
cover_m1_rand<-ranova(cover_lmer_m1)

# plotted
dat <- ggpredict(cover_lmer_m1, terms = c("Day","Treatment", "Cage.treatment","Bommie_treat"))
plot(dat,connect.lines = T)

m1_cover_pairwise<-emmeans(cover_lmer_m1, list(pairwise ~ Cage.treatment), adjust = "tukey")



# print table 
write.csv(as.matrix(cover_m1_aov), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/cover_m1.csv", na = "")
write.csv(as.matrix(cover_m1_rand), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/cover_m1_rand.csv", na = "")


## Results
# Cover is lower on open plates -- no interactions of importance -- proceed and reduce to open








## Reduced test model
exp_data_actual_open<-exp_data_actual_time_nocontrol %>% filter(!Cage.treatment == "Cage")



# model
cover_lmer_m2<-lmer(log_algalcover ~
                     Day * 
                     Treatment *
                     Bommie_treat +
                     (1|Bommie_no/Tile_no) +
                     (1|Block),
                   data = exp_data_actual_open,
                   REML = F)

summary(cover_lmer_m2)
anova(cover_lmer_m2)


cover_lmer_m2.5<-lmer(log_algalcover ~
                      Day * 
                      Treatment *
                      Bommie_treat +
                      (1|Bommie_no/Tile_no) +
                      (1|Block),
                    data = exp_data_actual_open,
                    REML = T)

summary(cover_lmer_m2.5)
cover_m2_aov<-anova(cover_lmer_m2)
cover_m2_rand<-ranova(cover_lmer_m2)

summ(cover_lmer_m2, digits = 4)





# plotted
dat <- ggpredict(cover_lmer_m2.5, terms = c("Day","Treatment","Bommie_treat"))
plot(dat,connect.lines = T)

#plot(allEffects(cover_lmer_m2.5))


# print table 
write.csv(as.matrix(cover_m2_aov), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/cover_m2.csv", na = "")
write.csv(as.matrix(cover_m2_rand), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/cover_m2_rand.csv", na = "")


## Results
# effect of treatment, no effect over time 











## Richness ####
exp_control_data_summarized
# Crossed-nested linear mixed Model - algal species richness - Cage v, control effects

rich_lmer_m0<-lmer(log_rich ~
                     Day * 
                     Treatment *
                    #Bommie_treat * # rationale == no controls for bommie treat
                     Cage.treatment + 
                     (1|Block/Bommie_no),
                   data = exp_control_data_summarized,
                   REML = F)
summary(rich_lmer_m0)

rich_m0_aov<-anova(rich_lmer_m0)
rich_m0_rand<-ranova(rich_lmer_m0)



# plotted
dat <- ggpredict(rich_lmer_m0, terms = c("Day","Treatment","Cage.treatment"))
plot(dat,connect.lines = T)

m0_rich_pairwise<-emmeans(rich_lmer_m0, list(pairwise ~ Cage.treatment), adjust = "tukey")



# print table 
write.csv(as.matrix(rich_m0_aov), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/rich_m0.csv", na = "")
write.csv(as.matrix(rich_m0_rand), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/rich_m0_rand.csv", na = "")






# Crossed-nested linear mixed Model - algal species richness - Cage v. open effects
rich_lmer_m1<-lmer(log_rich ~ 
                  Treatment *
                  Bommie_treat *
                  Cage.treatment *
                  Day +
                  (1|Bommie_no/Tile_no) +
                  (1|Block),
                data = exp_data_actual_time_nocontrol,
                REML = F)

dat <- ggpredict(rich_lmer_m1, terms = c("Day","Treatment","Cage.treatment","Bommie_treat"))
plot(dat,connect.lines = T)



summary(rich_lmer_m1)
summ(rich_lmer_m1, digits = 4)

rich_m1_aov<-anova(rich_lmer_m1)
rich_m1_rand<-ranova(rich_lmer_m1)

# print table 
write.csv(as.matrix(rich_m1_aov), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/rich_m1.csv", na = "")
write.csv(as.matrix(rich_m1_rand), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/rich_m1_rand.csv", na = "")



## Reduced test model
rich_lmer_m2<-lmer(log_rich ~ 
                     Day * 
                     Treatment *
                     Bommie_treat *
                     (1|Bommie_no/Tile_no) +
                     (1|Block),
                   data = exp_data_actual_open,
                   REML = F)

summary(rich_lmer_m2)
summ(rich_lmer_m2, digits = 4)

rich_m2_aov<-anova(rich_lmer_m2)
rich_m2_rand<-ranova(rich_lmer_m2)


# plotted
dat <- ggpredict(rich_lmer_m2, terms = c("Day","Treatment","Bommie_treat"))
plot(dat,connect.lines = T)


# print table 
write.csv(as.matrix(rich_m2_aov), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/rich_m2.csv", na = "")
write.csv(as.matrix(rich_m2_rand), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/rich_m2_rand.csv", na = "")






## Biomass ####

# Crossed-nested linear mixed Model - algal species richness - Cage v, control effects
change_biomass_actual$Tile_no<-as.factor(change_biomass_actual$Tile_no)
change_biomass_actual$Bommie_no<-as.factor(change_biomass_actual$Bommie_no)

biomass_control_data<- left_join(change_biomass_actual,block_df, by = c("Tile_no","Block")) %>% 
  mutate(Bommie_no = ifelse(is.na(Bommie_no.x), Bommie_no.y, Bommie_no.x)) %>%
  select(-Bommie_no.y, -Bommie_no.x)


biomass_control_data_summarized<-biomass_control_data %>% 
  dplyr::filter(!Block == "1",
                !Tile_arrangement == "Mix") %>%
  dplyr::group_by(Cage.treatment,Treatment,Bommie_treat,Bommie_no,Block) %>%
  summarise_at(c("change_biomass","log_chng_biomass"), mean, na.rm = TRUE)



biomass_lmer_m0<-lmer(log_chng_biomass ~
                     Treatment *
                     #Bommie_treat *
                     Cage.treatment +
                     (1|Block) +
                     (1|Bommie_no),
                   data = biomass_control_data_summarized,
                   REML = F)
summary(biomass_lmer_m0)

biomass_m0_aov<-anova(biomass_lmer_m0)
biomass_m0_rand<-ranova(biomass_lmer_m0)

# plotted
dat <- ggpredict(biomass_lmer_m0, terms = c("Treatment","Cage.treatment"))
plot(dat,connect.lines = F)


m0_biomass_pairwise<-emmeans(biomass_lmer_m0, list(pairwise ~ Cage.treatment), adjust = "tukey")


# print table 
write.csv(as.matrix(biomass_m0_aov), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/biomass_m0.csv", na = "")
write.csv(as.matrix(biomass_m0_rand), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/biomass_m0_rand.csv", na = "")






## Results
# Open and control the same -- no interactions of importance

# Crossed-nested linear mixed Model - algal species richness - Cage v. open effects
change_biomass_actual_nocontrol<-change_biomass_actual %>% filter(!Cage.treatment == "Control")

biomass_lmer_m1<-lmer(log_chng_biomass ~ 
                     Treatment *
                     Bommie_treat *
                     Cage.treatment +
                     (1|Block/Bommie_no),
                   data = change_biomass_actual_nocontrol,
                   REML = F)


summary(biomass_lmer_m1)
summ(biomass_lmer_m1, digits = 4)

biomass_m1_aov<-anova(biomass_lmer_m1)
biomass_m1_rand<-ranova(biomass_lmer_m1)

# plotted
dat <- ggpredict(biomass_lmer_m1, terms = c("Treatment","Cage.treatment","Bommie_treat"))
plot(dat,connect.lines = F)


# print table 
write.csv(as.matrix(biomass_m1_aov), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/biomass_m1.csv", na = "")
write.csv(as.matrix(biomass_m1_rand), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/biomass_m1_rand.csv", na = "")




## Results
# 
# Random effect residual higher than variance of random effect

## Reduced test model
change_biomass_actual_open<-change_biomass_actual_nocontrol %>% filter(!Cage.treatment == "Cage")

biomass_lmer_m2<-lmer(log_chng_biomass ~ 
                     Treatment *
                     Bommie_treat *
                      (1|Block/Bommie_no),
                     data = change_biomass_actual_open,
                   REML = F)

summary(biomass_lmer_m2)
summ(biomass_lmer_m2, digits = 4)

biomass_m2_aov<-anova(biomass_lmer_m2)
biomass_m2_rand<-ranova(biomass_lmer_m2)

# plotted
dat <- ggpredict(biomass_lmer_m2, terms = c("Treatment","Bommie_treat"))
plot(dat,connect.lines = F)

# print table 
write.csv(as.matrix(biomass_m2_aov), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/biomass_m2.csv", na = "")
write.csv(as.matrix(biomass_m2_rand), file = "/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony/Figures/Manuscript/Prelim/Tables/biomass_m2_rand.csv", na = "")



## Results
# Random effect residual higher than variance of random effect
# Effect of complexity; no effect of heterogeneity; no interaction



m2_table<-tab_model(cover_lmer_m2,rich_lmer_m2,biomass_lmer_m2)
summary(cover_lmer_m2)
plot_model(cover_lmer_m2)

m2_table %>%
  kable() %>%
  kable_styling() %>%
  save_kable("test.pdf")

html2pdf(filename = "~/herbivory_asynchrony/Figures/Manuscript/Tables/m2_table.html", 
         table_width = 13, 
         silent = TRUE, 
         style = TRUE, 
         build_pdf = TRUE, 
         clean = TRUE)




### Bray-Curtis LMER ####

bray_aov<-lm(value~Tile_arrangement_1*Time_point_1*Bommie_treat_1,data=dist_test_df)
anova(bray_aov)



bray_lmer<-lmer(value ~ 
                  Time_point_1 * 
                  Tile_arrangement_1 *
                  Bommie_treat_1 +
                  (1|Block/Bommie_no_1),
                  data = dist_test_df,
                  REML = T)

summary(bray_lmer)
anova(bray_lmer)
ranova(bray_lmer)

dat <- ggpredict(bray_lmer, terms = c("Time_point_1","Tile_arrangement_1","Bommie_treat_1"))
plot(dat,connect.lines = T)

## END ##




#### Extra stuff ####


# this is for checking
check_collinearity(rich_lmer_m1) # there is collinearality and heterogeneity of variances. -- log transform helps
#performance::check_model(rich_lmer_m1) 
#check_outliers(rich_lmer_m1)


