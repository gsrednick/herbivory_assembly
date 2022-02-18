#### Data curation --- Herbivory and asynchrony 
# Written by G.Srednick

# Packages 
library(tidyverse)



# original data
#exp_data<-read.csv("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Data/experiment_data.csv")

# reanalyzed with final datapoint
exp_data<-read.csv("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Data/experiment_data_V2.csv")

treat_table<-read.csv("/Users/icarus2/Documents/Software/R/Tidy Workshop/Git/asynchrony_experiment_moorea/Data/treatments_table.csv")


# Fix dates
exp_data$Date <- as.POSIXct(exp_data$Date, format = "%m/%d/%y")






# Recalculate with "OFF" removed
exp_data_V2<-exp_data

exp_data_V2$Off<-NULL
exp_data_V3<-exp_data_V2[c(11:29)]

#Calculate row sums (without "Off" catagory)
New100<-rowSums(exp_data_V3)

Fixer<-(exp_data_V2[,11:29]/New100)*100
exp_data_corrected<-data.frame(exp_data_V2[,c(1:10)],Fixer)








######################################  Exploratory  ########################################################

# Hypotheses:
# (1) Differences among bommie treatments
# (2) Differences among tile treatments
# (3) Interaction between two treatments
# (4) Cage effects?
# (5) Community asynchrony promotes community stability





# Lets check normality and heterogeneity of variances

qqnorm(exp_data_corrected_sums$algal_cover) # def not normal

bartlett.test(algal_cover ~ Cage.treatment, data = exp_data_corrected_sums) # nope -- transform
bartlett.test(algal_cover ~ Treatment, data = exp_data_corrected_sums) # Yes!
bartlett.test(algal_cover ~ Tile_arrangement, data = exp_data_corrected_sums) # Yes!
bartlett.test(algal_cover ~ Bommie_treat, data = exp_data_corrected_sums) # Yes !

# answer.... Transform


#qqnorm((exp_data_corrected_sums$algal_cover)) # def not normal
#hist(exp_data_corrected_sums$algal_cover)


# ugh......super heteregeneous




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




summarized <-exp_data_corrected_sums_nocontrol[c(1:10,30,31)]
summarized_controls <-exp_data_corrected_sums[c(1:10,30,31)]

# summarized_long <-exp_data_corrected_sums %>% gather(species, cover,-c(1:8))


# wide to long

exp_long<-exp_data_corrected_sums[-c(30,31)] %>% gather(species, cover,-c(1:8))








#### Richness calculation and testing ####
exp_data_rich <- exp_data_corrected
exp_data_rich$rich<-rowSums(exp_data_corrected[c(13:30)] > 0)

exp_data_rich_time<-exp_data_rich %>% 
  filter(!Time_point %in% c("1"))
