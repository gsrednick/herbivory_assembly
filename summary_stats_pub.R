### Summary stats for Herbivory paper ###

# Title: 
# DOI: 
# Accepted as Article in Ecology
# Srednick et al. 2022


## Univariate tests ####

## Cover ###

## Cage artifacts
exp_control_data_summarized %>% 
  group_by(Cage.treatment) %>%
  summarize_at(vars(algal_cover),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)



## Cage effects
exp_data_actual_time_nocontrol %>% 
  group_by(Cage.treatment) %>%
  summarize_at(vars(algal_cover),funs(mean,sd)) %>% 
  mutate_if(is.numeric, format, 1)



## Actual test
abund_summary<-exp_data_actual_open %>% 
  group_by(Treatment) %>%
  summarize_at(vars(algal_cover),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)


abund_summary_all<-exp_data_actual_open %>% 
  summarize_at(vars(algal_cover),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)


means <- pull(abund_summary, mean)

both_sd<-as.vector(abund_summary_all[1,2])

abund_ES<-(as.numeric(means[1])-as.numeric(means[2])/as.numeric(both_sd))




## Richness ###

## Cage artifacts
exp_control_data_summarized %>%
  group_by(Cage.treatment) %>%
  summarize_at(vars(rich),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)


## Cage effects
exp_data_actual_time_nocontrol%>% 
  group_by(Cage.treatment) %>%
  summarize_at(vars(rich),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)


## Actual test
rich_summary<-exp_data_actual_open %>% 
  group_by(Treatment) %>%
  summarize_at(vars(rich),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)

# for power analysis
rich_summary_all<-exp_data_actual_open %>% 
  summarize_at(vars(rich),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)


rich_means <- pull(rich_summary, mean)

rich_both_sd<-as.vector(rich_summary_all[1,2])

rich_ES<-(as.numeric(rich_means[1])-as.numeric(rich_means[2])/as.numeric(rich_both_sd))



## Biomass ###

## Cage artifacts
biomass_control_data_summarized %>%
  group_by(Cage.treatment,Treatment) %>%
  summarize_at(vars(change_biomass),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)



## Cage effects
change_biomass_actual_nocontrol %>% 
  group_by(Cage.treatment) %>%
  summarize_at(vars(change_biomass),funs(mean,sd)) %>% 
  mutate_if(is.numeric, format, 1)



## Actual test
change_biomass_actual_open %>% 
  group_by(Treatment) %>%
  summarize_at(vars(change_biomass),funs(mean,sd)) %>% 
  mutate_if(is.numeric, format, 1)







## Bray Curtis ####
bray_meta_summary<-dist_test_df %>%
  group_by(Tile_arrangement_1) %>%
  summarize_at(vars(value),funs(mean,sd)) %>% 
  mutate_if(is.numeric, format, 1)

# for power analysis
bray_m_means <- pull(bray_meta_summary, mean)


bray_meta_summary_time<-dist_test_df %>%
  group_by(Tile_arrangement_1,Time_point_1) %>%
  summarize_at(vars(value),funs(mean,sd)) %>% 
  mutate_if(is.numeric, format, 1)


bray_m_means_time <- pull(bray_meta_summary_time, mean)


## Extra stats #####

## Regression between species richness and bray curtis distance -- for appendix
## Also....place species evenness as a second axis (or bubble size) for bray curtis plot
# "Does diversity drive pattern on mixed tiles; relative abundance driving pattern on simple tiles"



## Species table 

spp_mean_sd<-community %>% 
  filter(!Time_point == "1") %>%
  summarize_at(vars(BARE:Turf), c(mean,sd)) 






## Power analysis for controls ####
# test that there is enough power to test controls vs. open vs. caged
# look at temporal means
library(pwr)
library(simr)
library(lsr)

power_df<-exp_data_actual_time %>% 
  group_by(Tile_no,Treatment,Cage.treatment) %>% 
  summarize_all(mean,na.rm=T) %>% 
  select(Tile_no,Treatment,Cage.treatment,rich,algal_cover,log_algalcover,log_rich)

caged<-power_df %>% filter(Cage.treatment == "Cage") %>% select(algal_cover)
open<-power_df %>% filter(Cage.treatment == "Open") %>% select(algal_cover)
control<-power_df %>% filter(Cage.treatment == "Control") %>% select(algal_cover)


# assuming an effect size of XXX; how many replicates are appropriate to see a difference?

comp_1<-cohensD(open$algal_cover, control$algal_cover)
comp_2<-cohensD(open$algal_cover, caged$algal_cover)
comp_3<-cohensD(control$algal_cover, caged$algal_cover)


pwr::pwr.t.test(power = 0.8, d = comp_1, sig.level = 0.05)
pwr::pwr.t.test(power = 0.8, d = comp_2, sig.level = 0.05)
pwr::pwr.t.test(power = 0.8, d = comp_3, sig.level = 0.05)





# END #