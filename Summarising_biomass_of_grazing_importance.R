# Load necessary libraries
library(tidyverse)

####Load data
gv <- read_csv("data/Grazing_value.csv")
head(gv)

# Calculate grazing value for each plot----
ID <- gv %>%
  group_by(Harvest_plot_ID) %>%
  summarise(grazing_value = sum(Synthetic_score)) 
ID

# Join with original data to filter for rows corresponding to maximum biomass----
Grazing_value_summary <- gv %>%
  left_join(ID,by="Harvest_plot_ID") %>%
  group_by(`Harvest_plot_ID`) %>%
  filter(Synthetic_score == max(Synthetic_score))%>%
  select(`Harvest_plot_ID`, Species, Dry_weight_g, Ecological_class,Weight,grazing_value) 
Grazing_value_summary  
view(Grazing_value_summary)

### Save as csv
write.csv(Grazing_value_summary, file = "data/Synthetic_grazing_value.csv", row.names = FALSE)
