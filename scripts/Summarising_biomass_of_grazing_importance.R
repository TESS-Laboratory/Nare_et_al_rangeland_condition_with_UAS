# Load necessary libraries
library(tidyverse)

####Load data
gv <- read_csv("data/Grazing_value.csv")
head(gv)
view(gv)

# Calculate grazing value for each plot----

ID <- gv %>% 
  mutate(synthetic_grazing_value= AGB_g_sq_m*Grazing_value)
view(ID)
ID <- ID %>%
  group_by(Harvest_plot_ID) %>%
  summarise(synthetic_grazing_value_sum = sum(synthetic_grazing_value)) 
view(ID)


### Save as csv
write.csv(ID, file = "data/Synthetic_grazing_value.csv", row.names = FALSE)
