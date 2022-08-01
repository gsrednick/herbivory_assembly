### Summary stats for Herbivory paper ####


# Univariate tests ####

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
exp_data_actual_open %>% 
  group_by(Treatment) %>%
  summarize_at(vars(algal_cover),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)





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
exp_data_actual_open %>% 
  group_by(Treatment,Day) %>%
  summarize_at(vars(rich),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)






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





## Synchrony ####

## Community ##
loreau_com_complete %>%
  group_by(Treatment) %>%
  summarize_at(vars(synchrony),funs(mean,sd)) %>%
  mutate_if(is.numeric, format, 1)


## Metacommunity ##
loreau_metacom_complete %>% 
  group_by(Tile_arrangement) %>%
  summarize_at(vars(synchrony),funs(mean,sd)) %>% 
  mutate_if(is.numeric, format, 1)



## Bray Curtis ##
dist_test_df %>%
  group_by(Tile_arrangement_1) %>%
  summarize_at(vars(value),funs(mean,sd)) %>% 
  mutate_if(is.numeric, format, 1)






#### Extra stats #####

## Regression between species richness and bray curtis distance -- for appendix
## Also....place species evenness as a second axis (or bubble size) for bray curtis plot
# "Does diversity drive pattern on mixed tiles; relative abundance driving pattern on simple tiles"



## Species table 

spp_mean_sd<-community %>% 
  filter(!Time_point == "1") %>%
  summarize_at(vars(BARE:Turf), c(mean,sd)) 


# END #