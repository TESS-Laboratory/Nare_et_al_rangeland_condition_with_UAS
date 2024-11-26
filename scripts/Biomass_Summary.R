# Load necessary libraries
library(tidyverse)

# Read the csv file----

Biomass <- read_csv("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/Biosum.csv")

summary(Biomass) #gives summary data
head(Biomass)  #Display first few rows
tail(Biomass)  #Display last rows
dim(Biomass)  #Diplay number of columns and rows

# Calculate sum of dry weight for each plot----
ID <- Biomass %>%
  group_by(`Plot ID`) %>%
  summarize(total_AGB = sum(`Dry weight [g]`)) 

# Join with original data to filter for rows corresponding to maximum biomass----
Biomass_Summary <- Biomass %>%
  left_join(ID,by="Plot ID") %>%
  group_by(`Plot ID`) %>%
  filter(`Dry weight [g]` == max(`Dry weight [g]`)) %>%
  select(`Plot ID`, Species=`Partition`,total_AGB)

### Save as csv
write.csv(Biomass_Summary, file = "Biomass_summary.csv", row.names = FALSE)


#### Playing around


summary(Biomass_summary)
plot(Biomass_summary)
ggplot(Biomass_summary, aes(AOI,total_AGB))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(Biomass_summary, aes(AOI,total_AGB,color=AOI))+
  geom_violin()+
  geom_point()+
  theme_classic()+
  labs(title = "Biomass summary")
unique(Biomass_summary$Species)

ggplot(Biomass_summary, aes(x=total_AGB))+
  geom_histogram()


