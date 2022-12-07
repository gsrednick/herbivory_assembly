#### Data curation --- Herbivory and asynchrony 

# Title: 
# DOI: 
# Accepted as Article in Ecology
# Srednick et al. 2022


# Written by G.Srednick

# Packages 
library(tidyverse)





setwd("~/Documents/Software/R/Tidy Workshop/Git/herbivory_asynchrony")

# original data
#exp_data<-read.csv("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Data/experiment_data.csv")

# reanalyzed with final datapoint
exp_data<-read.csv("./Data/experiment_data_V2.csv")
treat_table<-read.csv("./Data/treatments_table.csv")
change_biomass<-read.csv("./Data/biomass_change_DATA.csv")
#meta_table<-read.csv("./Data/meta_table.csv")

meta_table<-exp_data %>% 
  select(2:8) %>%
  group_by(Tile_no,Treatment,Tile_arrangement,Cage.treatment,Bommie_treat,Bommie_no,Block) %>% 
  summarize_all(mean)


# Fix dates
exp_data$Date <- as.POSIXct(exp_data$Date, format = "%m/%d/%y")






# Recalculate with "OFF" removed
exp_data_V2<-exp_data

exp_data_V2$Off<-NULL
exp_data_V3<-exp_data_V2[c(12:30)]

#Calculate row sums (without "Off" catagory)
New100<-rowSums(exp_data_V3)

Fixer<-(exp_data_V2[,12:30]/New100)*100
exp_data_corrected<-data.frame(exp_data_V2[,c(1:11)],Fixer)




######################################  Exploratory  ########################################################

#### To do: merge richness and cover into same DF; simplify the approach


# Hypotheses:
# (1) Differences among bommie treatments
# (2) Differences among tile treatments
# (3) Interaction between two treatments
# (4) Cage effects?
# (5) Community asynchrony promotes community stability

## Univariate ##

# filter to first timepoint
exp_data_corrected$Time_point<-as.factor(exp_data_corrected$Time_point)


# time
exp_data_corrected_first<-exp_data_corrected %>% filter(Date == "2021-06-28")
exp_data_corrected_second<-exp_data_corrected %>% filter(Date == "2021-07-13")



# no control
exp_data_corrected_nocontrol<-exp_data_corrected %>% 
  filter(!Bommie_treat == "Control")



# Sum up algae vs. bare vs. cca vs. recruits

exp_data_corrected_sums<-exp_data_corrected %>%
  mutate(algal_cover = select(., AMPH:Turf) %>% rowSums(na.rm = TRUE)) 


# transformation

exp_data_corrected_sums$log_algalcover<-log(exp_data_corrected_sums$algal_cover +1)



exp_data_corrected_sums_nocontrol<-exp_data_corrected_sums %>% 
  filter(!Cage.treatment == "Control")




summarized <-exp_data_corrected_sums_nocontrol[c(1:11,31,32)]
summarized_controls <-exp_data_corrected_sums[c(1:11,31,32)]

summarized_nocage<- summarized %>% filter(!Cage.treatment == "Cage")

summarized_controls_v2 <- summarized_controls %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"))


# summarized_long <-exp_data_corrected_sums %>% gather(species, cover,-c(1:8))


# wide to long

exp_long<-exp_data_corrected_sums[-c(31,32)] %>% gather(species, cover,-c(1:11))


# Lets check normality and heterogeneity of variances

qqnorm(exp_data_corrected_sums$algal_cover) # def not normal

bartlett.test(algal_cover ~ Cage.treatment, data = exp_data_corrected_sums) # nope -- transform
bartlett.test(algal_cover ~ Treatment, data = exp_data_corrected_sums) # Yes!
bartlett.test(algal_cover ~ Tile_arrangement, data = exp_data_corrected_sums) # Yes!
bartlett.test(algal_cover ~ Bommie_treat, data = exp_data_corrected_sums) # Yes !





#### Richness calculation and testing ####
exp_data_rich <- exp_data_corrected
exp_data_rich$rich<-rowSums(exp_data_corrected[c(13:30)] > 0)

exp_data_rich_time<-exp_data_rich %>% 
  filter(!Time_point %in% c("1"))




## merge richness and cover into the same df
dim(exp_data_corrected_sums)

exp_data_actual<-merge(exp_data_rich,exp_data_corrected_sums)
dim(exp_data_actual)



exp_data_actual <- exp_data_actual %>%
  mutate(Bommie_treat = recode(Bommie_treat, 
                               Clean = "Low heterogeneity",
                               Turbinaria = "High heterogeneity"))



exp_data_actual$Tile_no <-as.factor(exp_data_actual$Tile_no)
exp_data_actual$Bommie_no <-as.factor(exp_data_actual$Bommie_no)



#### Biomass ####

qqnorm(log(change_biomass$change_biomass+1)) # def not normal

bartlett.test(change_biomass ~ Cage.treatment, data = change_biomass) # nope
bartlett.test(change_biomass ~ Bommie_treat, data = change_biomass) # nope
bartlett.test(change_biomass ~ Tile_arrangement, data = change_biomass) # nope

# will have to transform -- log(x+1)

change_biomass$log_chng_biomass<-log(change_biomass$change_biomass)


change_biomass_actual<-merge(change_biomass,meta_table)

change_biomass_actual<- change_biomass_actual %>% mutate(Bommie_treat = recode(Bommie_treat, 
                                                      Clean = "Low heterogeneity",
                                                      Turbinaria = "High heterogeneity"))



# Now run author_script_herbivory_updated.R
# Then run LMER_script.R


### END ###