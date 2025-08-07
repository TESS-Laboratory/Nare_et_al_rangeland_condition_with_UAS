# Title: This script implements an Analysis of Covariance (ANCOVA) framework to 
#examine how grazing intensity (AOI) modifies the relationships between 
#UAV-derived vegetation metrics (NDVI and canopy height) and AGB. 

# Author: Alan Nare and Andrew Cunliffe

# Load necessary libraries
library(tidyverse)
library(performance)
library(sjPlot)
library(effects)
library(patchwork)
library(pracma)
library(report)
library(olsrr)
library(robustbase)
library(car)
library(emmeans)

## Create Plotting theme
theme_beautiful <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly

# Read the data
ND <- read_csv("data/Data.csv")
head(ND)
# 


ND$AOI <- as.factor(ND$AOI)

# NDVI-AGB relationship with grazing interaction
m_ndvi <- robustbase::lmrob(AGB_g_m_2 ~ NDVI * AOI, data = ND)
ndvi_anova <- Anova(m_ndvi, type = "III")  # Test interaction significance
ndvi_anova

##plot
ggplot(ND, aes(x = NDVI, y = AGB_g_m_2, color = AOI)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "NDVI-AGB Relationship by Grazing Intensity (AOI)")+
  theme_beautiful()




# Canopy height-AGB relationship with grazing interaction
m_height <- robustbase::lmrob(AGB_g_m_2 ~ Mean_Canopy_Height_m * AOI, data = ND)
Anova(m_height, type = "III")

#### plot
ggplot(ND, aes(x = Mean_Canopy_Height_m, y = AGB_g_m_2, color = AOI)) +
  # geom_point() +
  geom_smooth(method = "lmrob", se = FALSE) +
  labs(x = "Mean Canopy Height (m)", title = "Canopy Height-AGB Relationship by Grazing Intensity (AOI)")+
  theme_beautiful()



# 
# ## NDVI post hoc
# # Robust model (your main model for effect estimation)
# m_ndvi <- lmrob(AGB_g_m_2 ~ NDVI * AOI, data = ND)
# 
# # Standard lm model for post hoc ONLY
# m_ndvi_lm <- lm(AGB_g_m_2 ~ NDVI * AOI, data = ND)
# 
# # Post hoc comparisons using emmeans
# library(emmeans)
# 
# # Marginal means
# ndvi_emmeans <- emmeans(m_ndvi_lm, ~ NDVI * AOI)
# 
# # Pairwise comparisons at each NDVI level
# ndvi_pairs <- pairs(ndvi_emmeans, by = "NDVI", adjust = "tukey")
# 
# # Slopes (emtrends): how NDVI effects differ across AOIs
# ndvi_slopes <- emtrends(m_ndvi_lm, ~ AOI, var = "NDVI")
# ndvi_slope_pairs <- pairs(ndvi_slopes, adjust = "tukey")
# 
# # View results
# summary(ndvi_pairs)
# summary(ndvi_slope_pairs)

