# Load Packages -----------------------------------------------------------
library(sportyR)

# Load Data ---------------------------------------------------------------
# If necessary, load goalies_22.csv and SoG_22.csv
goalies_22 <- read.csv("goalies_22.csv")
SoG_22 <- read.csv("SoG_22")

# Clean up the SoG data, keeping only variables interesting for analysis
keep_col <- c("shotID", "goalieNameForShot", "xCordAdjusted", "yCordAdjusted", "save", "xSave", "shotGeneratedRebound", "xRebound", "reboundSave", "xReboSave")
# Change NA to 0 - this only impacts the NA's assigned to the saveAfterReboundShot column
SoG_22[is.na(SoG_22)] <- 0

# Select saves only and transform data set longer 
# This was not used but prepares a data set for different types of manipulations 
# Easier for plotting with sportyR
SoG_22_gg <- SoG_22 %>% select(keep_col) %>% filter(save == 1) %>%
  mutate_at(c("save", "xSave", "shotGeneratedRebound", "xRebound", "reboundSave", "xReboSave"), factor) %>%
  pivot_longer(cols = c("save", "xSave", "shotGeneratedRebound", "xRebound", "reboundSave", "xReboSave"),
               names_to = "type",
               values_to = "result")

# Shot Plots 2022--------------------------------------------------------------
# Create 2022 saves dataset
saves_22 <- SoG_22 %>% filter(save == 1)

# Plot density map of all 2022 Saves 
gg_saves_22 <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = saves_22, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Saves for the 2021-22 NHL Season") 

# Run plot
gg_saves_22

# Create 2022 xSaves dataset
xSaves_22 <- SoG_22 %>% filter(xSave == 1)

# Plot density map of all 2022 xSaves 
gg_xSaves_22 <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = xSaves_22, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "xSaves for the 2021-22 NHL Season")

# Run the plot
gg_xSaves_22

# Create 2022 Rebounds data
rebound_22 <- SoG_22 %>% filter(shotGeneratedRebound == 1)

# Create density map of all 2022 Shots that generated rebounds 
gg_rebound_22 <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = rebound_22, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Shots that Generated Rebounds for the 2021-22 NHL Season")

# Run the plot
gg_rebound_22

# Create 2022 xRebounds data
xRebound_22 <- SoG_22 %>% filter(xRebound == 1)

# Plot density map of all 2022 xRebounds
gg_xRebound_22 <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = xRebound_22, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Shots that were Expected to Generate Rebounds for the 2021-22 NHL Season")

# Run the plot
gg_xRebound_22

# Create 2022 Rebound Saves data
reboundSave_22 <- SoG_22 %>% filter(reboundSave == 1)

# Plot density map of all 2022 Rebound Saves 
gg_reboundSave_22 <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = reboundSave_22, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Rebound Saves for the 2021-22 NHL Season")

# Run the plot
gg_reboundSave_22

# Create 2022 xReboundSaves data
xReboundSave_22 <- SoG_22 %>% filter(xReboSave == 1)

# Plot density map of all 2022 xReboundSaves 
gg_xReboundSave_22 <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = xReboundSave_22, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "xRebound Saves for the 2021-22 NHL Season")

# Run Plot
gg_xReboundSave_22

# Goalie Stats 2022 ------------------------------------------------------------

gg_xSVP <- goalies_22 %>% filter(Shots > 1000) %>%
  ggplot(aes(goalieNameForShot, xSVP)) +
  geom_col(fill="gray25", colour = "gray30", width = 1, alpha = 0.25) +
  geom_col(aes(y = SVP, fill = SVPvXSVP), colour = "black", width = 0.6, position = "stack", show.legend = T) +
  scale_fill_distiller(palette = "RdGy", direction = -1) +
  geom_hline(colour = "black", linetype = "longdash", yintercept = mean(goalies_22$SVP)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = "10", colour = "black", hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = "10", colour = "black"),
        legend.position = "right") +
  coord_cartesian(ylim = c(85,95)) +
  labs(x = "Goaltender", 
       y = "Save%", 
       fill = "Act V Exp",
       title = "Save % vs xSave % for the 2021-22 NHL Season")

gg_xSVP

gg_xREB <- goalies_22 %>% filter(Shots > 1000) %>%
  ggplot(aes(goalieNameForShot, xReboundPerc)) +
  geom_col(fill="gray25", colour = "gray30", width = 1, alpha = 0.25) +
  geom_col(aes(y = ReboundPerc, fill = REBvXREB), colour = "black", width = 0.6, position = "stack", show.legend = T) +
  scale_fill_distiller(palette = "RdGy", direction = -1) +
  geom_hline(colour = "black", linetype = "longdash", yintercept = mean(goalies_22$ReboundPerc)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = "10", colour = "black", hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = "10", colour = "black"),
        legend.position = "right") +
  coord_cartesian(ylim = c(2.5,10)) + 
  labs(x = "Goaltender", 
       y = "Rebound%", 
       fill = "Act V Exp",
       title = "Rebound % vs xRebound % for the 2021-22 NHL Season")

gg_xREB

gg_xRS <- goalies_22 %>% filter(Shots > 1000) %>%
  ggplot(aes(goalieNameForShot, xReboSVP)) +
  geom_col(fill="gray25", colour = "gray30", width = 1, alpha = 0.25) +
  geom_col(aes(y = ReboSVP, fill = ReboSVPvXReboSVP), colour = "black", width = 0.6, position = "stack", show.legend = T) +
  scale_fill_distiller(palette = "RdGy", direction = -1, values = c(-.66,1)) +
  geom_hline(colour = "black", linetype = "longdash", yintercept = mean(goalies_22$ReboSVP, na.rm = T)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = "10", colour = "black", hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = "10", colour = "black"),
        legend.position = "right") +
  coord_cartesian(ylim = c(50,100)) +
  labs(x = "Goaltender", 
       y = "Rebound SV%", 
       fill = "Act V Exp",
       title = "Rebound Save % vs xRebound Save % for the 2021-22 NHL Season")

gg_xRS

gg_xSxRS_1000 <- goalies_22 %>% filter(Shots > 1000) %>%
  ggplot(aes(goalieNameForShot, xSxRS)) +
  geom_col(aes(fill = REBvXREB), colour = "black", size = .75, width = 0.85, show.legend = T) +
  geom_col(aes(y = SVPvXSVP), alpha = 0, colour = "black", size = .75, width = 0.85, show.legend = T) +
  scale_fill_distiller(palette = "RdBu", direction = -1, values = c(0,1)) +
  geom_hline(colour = "black", linetype = "longdash", yintercept = mean(goalies_22$xSxRS, na.rm = T, show.legend = T)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = "10", colour = "black", hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = "10", colour = "black"),
        legend.position = "right") +
  coord_cartesian(ylim = c(0,2.5)) +
  labs(x = "Goaltender", 
       y = "Actual vs. Expected Ratio", 
       fill = "xReb Ratio", 
       title = "x/SV% + x/Rebound SV% with x/Rebound% for the 2021-22 NHL Season")

gg_xSxRS_1000

gg_xSxRS_500 <- goalies_22 %>% filter(Shots > 500 & Shots < 1000) %>% 
  ggplot(aes(goalieNameForShot, xSxRS)) +
  geom_col(aes(fill = REBvXREB), colour = "black", size = .75, width = 0.85, show.legend = T) +
  geom_col(aes(y = SVPvXSVP), alpha = 0, colour = "black", size = .75, width = 0.85, show.legend = T) +
  scale_fill_distiller(palette = "RdBu", direction = -1, values = c(0,1)) +
  geom_hline(colour = "black", linetype = "longdash", yintercept = mean(goalies_22$xSxRS, na.rm = T, show.legend = T)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = "10", colour = "black", hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = "10", colour = "black"),
        legend.position = "right") +
  coord_cartesian(ylim = c(0,2.5)) +
  labs(x = "Goaltender", 
       y = "Actual vs. Expected Ratio", 
       fill = "xReb Ratio", 
       title = "x/SV% + x/Rebound SV% with x/Rebound% for the 2021-22 NHL Season")
gg_xSxRS_500

# Individual Goalies ------------------------------------------------------
# Saros -------------------------------------------------------------------
#Saves
saros_saves <- SoG_22 %>% 
  filter(goalieNameForShot == "Juuse Saros", save == 1)
saros_rebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Juuse Saros", shotGeneratedRebound == 1)
saros_reboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Juuse Saros", reboundSave == 1)


saros_gg <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = saros_saves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = saros_rebounds, aes(xCordAdjusted, yCordAdjusted, size = shotRebound), alpha= 0.5, shape = 3, size = 3) +
  scale_shape_discrete(breaks = c("Reb", "RebSave")) +
  geom_point(data = saros_reboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Juuse Saros Saves for the 2021-22 NHL Season")

#xSaves
saros_xSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Juuse Saros", xSave == 1)
saros_xRebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Juuse Saros", xRebound == 1)
saros_xReboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Juuse Saros", xReboSave == 1)

saros_xgg <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = saros_xSaves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = saros_xRebounds, aes(xCordAdjusted, yCordAdjusted), alpha= 0.5, shape = 3, size = 3) +
  geom_point(data = saros_xReboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Juuse Saros xSaves for the 2021-22 NHL Season")
saros_gg
saros_xgg
# Kuemper -----------------------------------------------------------------
#Saves
kuemper_saves <- SoG_22 %>% 
  filter(goalieNameForShot == "Darcy Kuemper", save == 1)
kuemper_rebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Darcy Kuemper", shotGeneratedRebound == 1)
kuemper_reboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Darcy Kuemper", reboundSave == 1)

kuemper_gg <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = kuemper_saves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = kuemper_rebounds, aes(xCordAdjusted, yCordAdjusted, size = shotRebound), alpha= 0.5, shape = 3, size = 3) +
  scale_shape_discrete(breaks = c("Reb", "RebSave")) +
  geom_point(data = kuemper_reboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Darcy Kuemper Saves for the 2021-22 NHL Season")

#xSaves
kuemper_xSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Darcy Kuemper", xSave == 1)
kuemper_xRebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Darcy Kuemper", xRebound == 1)
kuemper_xReboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Darcy Kuemper", xReboSave == 1)

kuemper_xgg <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = kuemper_xSaves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = kuemper_xRebounds, aes(xCordAdjusted, yCordAdjusted), alpha= 0.5, shape = 3, size = 3) +
  geom_point(data = kuemper_xReboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Darcy Kuemper xSaves for the 2021-22 NHL Season")
kuemper_gg
kuemper_xgg

# Quick -------------------------------------------------------------------
#Saves
quick_saves <- SoG_22 %>% 
  filter(goalieNameForShot == "Jonathan Quick", save == 1)
quick_rebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Jonathan Quick", shotGeneratedRebound == 1)
quick_reboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Jonathan Quick", reboundSave == 1)

quick_gg <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = quick_saves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = quick_rebounds, aes(xCordAdjusted, yCordAdjusted, size = shotRebound), alpha= 0.5, shape = 3, size = 3) +
  scale_shape_discrete(breaks = c("Reb", "RebSave")) +
  geom_point(data = quick_reboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Jonathan Quick Saves for the 2021-22 NHL Season")

#xSaves
quick_xSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Jonathan Quick", xSave == 1)
quick_xRebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Jonathan Quick", xRebound == 1)
quick_xReboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Jonathan Quick", xReboSave == 1)

quick_xgg <- geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = quick_xSaves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = quick_xRebounds, aes(xCordAdjusted, yCordAdjusted), alpha= 0.5, shape = 3, size = 3) +
  geom_point(data = quick_xReboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Jonathan Quick xSaves for the 2021-22 NHL Season")

# Jarry -------------------------------------------------------------------
#Saves
jarry_saves <- SoG_22 %>% 
  filter(goalieNameForShot == "Tristan Jarry", save == 1)
jarry_rebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Tristan Jarry", shotGeneratedRebound == 1)
jarry_reboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Tristan Jarry", reboundSave == 1)

geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = jarry_saves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = jarry_rebounds, aes(xCordAdjusted, yCordAdjusted, size = shotRebound), alpha= 0.5, shape = 3, size = 3) +
  scale_shape_discrete(breaks = c("Reb", "RebSave")) +
  geom_point(data = jarry_reboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Tristan Jarry Saves for the 2021-22 NHL Season")

#xSaves
jarry_xSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Tristan Jarry", xSave == 1)
jarry_xRebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Tristan Jarry", xRebound == 1)
jarry_xReboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Tristan Jarry", xReboSave == 1)

geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = jarry_xSaves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = jarry_xRebounds, aes(xCordAdjusted, yCordAdjusted), alpha= 0.5, shape = 3, size = 3) +
  geom_point(data = jarry_xReboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Tristan Jarry xSaves for the 2021-22 NHL Season")


# Bobrovsky -------------------------------------------------------------------
#Saves
bobrovsky_saves <- SoG_22 %>% 
  filter(goalieNameForShot == "Sergei Bobrovsky", save == 1)
bobrovsky_rebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Sergei Bobrovsky", shotGeneratedRebound == 1)
bobrovsky_reboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Sergei Bobrovsky", reboundSave == 1)

geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = bobrovsky_saves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = bobrovsky_rebounds, aes(xCordAdjusted, yCordAdjusted, size = shotRebound), alpha= 0.5, shape = 3, size = 3) +
  scale_shape_discrete(breaks = c("Reb", "RebSave")) +
  geom_point(data = bobrovsky_reboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Sergei Bobrovsky Saves for the 2021-22 NHL Season")

#xSaves
bobrovsky_xSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Sergei Bobrovsky", xSave == 1)
bobrovsky_xRebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Sergei Bobrovsky", xRebound == 1)
bobrovsky_xReboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Sergei Bobrovsky", xReboSave == 1)

geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = bobrovsky_xSaves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = bobrovsky_xRebounds, aes(xCordAdjusted, yCordAdjusted), alpha= 0.5, shape = 3, size = 3) +
  geom_point(data = bobrovsky_xReboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Sergei Bobrovsky xSaves for the 2021-22 NHL Season")


# Mrazek -------------------------------------------------------------------
#Saves
mrazek_saves <- SoG_22 %>% 
  filter(goalieNameForShot == "Petr Mrazek", save == 1)
mrazek_rebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Petr Mrazek", shotGeneratedRebound == 1)
mrazek_reboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Petr Mrazek", reboundSave == 1)

geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = mrazek_saves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = mrazek_rebounds, aes(xCordAdjusted, yCordAdjusted, size = shotRebound), alpha= 0.5, shape = 3, size = 3) +
  geom_point(data = mrazek_reboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Petr Mrazek Saves for the 2021-22 NHL Season")

#xSaves
mrazek_xSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Petr Mrazek", xSave == 1)
mrazek_xRebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Petr Mrazek", xRebound == 1)
mrazek_xReboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Petr Mrazek", xReboSave == 1)

geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = mrazek_xSaves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = mrazek_xRebounds, aes(xCordAdjusted, yCordAdjusted), alpha= 0.5, shape = 3, size = 3) +
  geom_point(data = mrazek_xReboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Petr Mrazek xSaves for the 2021-22 NHL Season")

# Smith -------------------------------------------------------------------
#Saves
smith_saves <- SoG_22 %>% 
  filter(goalieNameForShot == "Mike Smith", save == 1)
smith_rebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Mike Smith", shotGeneratedRebound == 1)
smith_reboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Mike Smith", reboundSave == 1)

geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = smith_saves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = smith_rebounds, aes(xCordAdjusted, yCordAdjusted, size = shotRebound), alpha= 0.5, shape = 3, size = 3) +
  geom_point(data = smith_reboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Mike Smith Saves for the 2021-22 NHL Season")

#xSaves
smith_xSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Mike Smith", xSave == 1)
smith_xRebounds <- SoG_22 %>% 
  filter(goalieNameForShot == "Mike Smith", xRebound == 1)
smith_xReboundSaves <- SoG_22 %>% 
  filter(goalieNameForShot == "Mike Smith", xReboSave == 1)

geom_hockey(league = "NHL", full_surf = T, rotate = F ) +
  stat_density_2d(data = smith_xSaves, aes(xCordAdjusted, yCordAdjusted, fill = ..density.., alpha = ..density..), geom = "raster", contour = FALSE) + 
  scale_alpha_continuous(range = c(0.5,1)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_point(data = smith_xRebounds, aes(xCordAdjusted, yCordAdjusted), alpha= 0.5, shape = 3, size = 3) +
  geom_point(data = smith_xReboundSaves, aes(xCordAdjusted, yCordAdjusted), alpha = 0.5, shape = 1, size = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = "NONE") +
  xlim(0, 101) +
  labs(title = "Mike Smith xSaves for the 2021-22 NHL Season")

