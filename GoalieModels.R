# Packages ----------------------------------------------------------------
# Load packages
library(tidyverse)
library(caret)
library(pROC)
library(effects)

# Turn off scientific notation
options(scipen = 999)

# Data Loading ------------------------------------------------------------

# Load data all seasosn
shots_raw22 <- read.csv("shots_2022.csv")
shots_raw21 <- read.csv("shots_2021.csv")
shots_raw20 <- read.csv("shots_2020.csv")
shots_raw19 <- read.csv("shots_2019.csv")
shots_raw18 <- read.csv("shots_2018.csv")

# Merge data
allShots_raw <-
  bind_rows(shots_raw22,
            shots_raw21,
            shots_raw20,
            shots_raw19,
            shots_raw18)

# Remove individual season data
rm(shots_raw18,
   shots_raw19,
   shots_raw20,
   shots_raw21,
   shots_raw22)

# Data Cleaning -----------------------------------------------------------
# Remove shots on empty net
allShots_raw <- subset(allShots_raw, shotOnEmptyNet == 0)

# Create a binary "save" variable
allShots_raw$save <-
  ifelse(allShots_raw$shotWasOnGoal == 1 &
           allShots_raw$goal == 0, "1", "0")

# Create players on ice variables for shooting and defending teams
allShots_raw <-
  allShots_raw %>% mutate(
    shootingOnIce = shootingTeamDefencemenOnIce + shootingTeamForwardsOnIce,
    defendingTeamOnIce = defendingTeamDefencemenOnIce + defendingTeamForwardsOnIce,
    .keep = "unused"
  )

# Create reboundGoal and reboundSave variables
allShots_raw$reboundGoal <-
  ifelse(
    allShots_raw$shotRebound == 1 &
      allShots_raw$goal == 1 &
      allShots_raw$lastEventCategory == "SHOT" ,
    1,
    0
  )
allShots_raw$reboundSave <-
  ifelse(
    allShots_raw$shotRebound == 1 &
      allShots_raw$goal == 0 &
      allShots_raw$lastEventCategory == "SHOT" &
      allShots_raw$shotWasOnGoal == 1 ,
    1,
    0
  )

# Create shotReboundAfterSave variable
allShots_raw$shotReboundAfterSave <-
  ifelse(allShots_raw$shotRebound == 1 &
           allShots_raw$lastEventCategory == "SHOT" ,
         1,
         0)

# Create condition variable that describes game state (players on ice for each team)
allShots_raw$condition <-
  paste(allShots_raw$shootingOnIce,
        allShots_raw$defendingTeamOnIce,
        sep = "on")

# Keep shots on goal only
allSoG <- allShots_raw %>% filter(shotWasOnGoal == 1)

# Select the variables to keep
allSoG <- allSoG %>%
  select(
    shotID,
    arenaAdjustedShotDistance,
    xCordAdjusted,
    yCordAdjusted,
    distanceFromLastEvent,
    condition,
    shotGeneratedRebound,
    lastEventCategory,
    save,
    goal,
    goalieNameForShot,
    goalieIdForShot,
    lastEventShotAngle,
    lastEventShotDistance,
    shooterLeftRight,
    shotAngleAdjusted,
    shotType,
    speedFromLastEvent,
    shotRebound,
    shotReboundAfterSave,
    shotRush,
    shotWasOnGoal,
    timeSinceLastEvent,
    season,
    lastEventxCord,
    lastEventyCord,
    lastEventCategory,
    reboundGoal,
    reboundSave
  )

# Remove observations with NA
allSoG <- na.omit(allSoG)

# Mutate select variables to factors/binary
allSoG <- allSoG %>%
  mutate_at(
    c(
      "condition",
      "lastEventCategory",
      "shotRebound",
      "shotReboundAfterSave",
      "shotRush",
      "save",
      "shooterLeftRight",
      "shotType",
      "shotWasOnGoal",
      "season",
      "shotGeneratedRebound",
      "goalieNameForShot",
      "goal",
      "reboundGoal",
      "reboundSave",
      "lastEventCategory"
    ),
    factor
  )

# Create vector for conditions to keep
keepCond <-
  c("3on3",
    "4on3",
    "4on4",
    "4on5",
    "5on3",
    "5on4",
    "5on5",
    "6on4",
    "6on5")

# Create vector for events to remove
rmlastEvent <-
  c("EGT",
    "EISTR",
    "GOAL",
    "PEND",
    "ANTHEM",
    "PSTR",
    "CHL",
    "GEND",
    NA,
    "NA")

# Keep conditions based on vector
allSoG <- allSoG[allSoG$condition %in% keepCond, ]

# Remove any missing observations
allSoG <- droplevels(allSoG, exclude = "")

# Remove event levels based on vector
allSoG <- droplevels(allSoG, exclude = rmlastEvent)

# Remove any new NA values
allSoG <- na.omit(allSoG)

# xSaves Model Creation ---------------------------------------------------
##### xSaves model training
# Create vector of seasons to use in the training data
training_seasons <- c(2017, 2018, 2019)

# Keep seasons based on the training_seasons vector
model_training <- allSoG[allSoG$season %in% training_seasons, ]

# Downsample the model_training dataset to train xSaves model (caret package)
xSave_training <-
  downSample(x = model_training, y = model_training$save)

# Create a binary logistic regression model for xSaves
xSaveGLM <-
  glm(save ~  xCordAdjusted + yCordAdjusted + condition + lastEventCategory + shotAngleAdjusted +
      shotType + speedFromLastEvent + shotRebound + shotRush + shooterLeftRight,
    family = binomial,
    data = xSave_training
  )

# Perform stepwise AIC to minimize AIC
xSaveGLM <- step(xSaveGLM)

# VIF for multicolinearity
vif(xSaveGLM)

# Model Plots
plot(xSaveGLM)

# Summarize model results
summary(xSaveGLM) #or
xSavesGLM_summary <- broom::tidy(xSaveGLM, conf.int = TRUE, conf.level = 0.95, exponentiate = T) #and
broom::glance(xSaveGLM)

# Create ROC area under curve plot
roc(
  save ~ xSaveGLM$fitted.values,
  data = xSave_training,
  plot = T,
  main = "ROC CURVE",
  col = "blue"
)

# Plot the independent variable effects (effects package)
plot(allEffects(xSaveGLM))

# Plot rebound shots effect
plot(predictorEffect("shotRebound", xSaveGLM))

# Plot x Coordinates effect
plot(predictorEffect("xCordAdjusted", xSaveGLM))

# Check observations diagnostics
xSaveGLM.data <- augment(xSaveGLM) %>%
  mutate(index = 1:n())

# xSaves Predictions ------------------------------------------------------

# Run Model Prediction for down sampled training Data
xSave_training$predSave <- predict.glm(xSaveGLM, type = "response")

# Check the ratio of the dependent variable in training dataset
xSave_training %>% count(save) %>% summarize("%" = n / sum(n))

# Use summary function to find the median (50%)
summary(xSave_training$predSave)

# Assign xSave to predictions about the median value, xGoal to prediction below the median value
# Maintains original goals/saves ratio in predictions
xSave_training$xSave <-
  ifelse(xSave_training$predSave >= 0.49689, "1", "0")
xSave_training$xGoal <-
  ifelse(xSave_training$predSave < 0.49689, "1", "0")

# Check the percentage of saves and goals predicted correctly
mean(xSave_training$xSave == xSave_training$save)
mean(xSave_training$xSave == xSave_training$goal)

# Check distribution of predictions vs original
xSave_training %>%
  count(xSave, save) %>%
  mutate(distribution = n / sum(n) * 100)

##### Run Model Predictions for 2022 Season

# Subset 2022 season from all shots on goal data
SoG_22 <- subset(allSoG, season == "2021")

# Apply model predictions to 2022 season
SoG_22$predSave <-
  predict.glm(xSaveGLM, type = "response", newdata = SoG_22)

# Check the ratio of saves in training dataset
SoG_22 %>% count(save) %>% summarize("%" = n / sum(n)) # 91%

# Find the 9th quantile in the predictions
quantile(SoG_22$predSave, c(0.091))

# Assign xSave to predictions about 9th quantile, xGoal to predictions below
# Maintain the original goals/saves ratio in the predictions
SoG_22$xSave <-
  as.numeric(ifelse(SoG_22$predSave > quantile(SoG_22$predSave, c(0.091)), "1", "0"))
SoG_22$xGoal <-
  as.numeric(ifelse(SoG_22$predSave < quantile(SoG_22$predSave, c(0.091)), "1", "0"))

# Check the proportion of saves and goals predicted correctly
mean(SoG_22$xSave == SoG_22$save)
mean(SoG_22$xSave == SoG_22$goal)

# xSaves Stats ------------------------------------------------------------
# Create summary stats by goalie name using original data and xPredictions
xS_22_stats <- SoG_22 %>%
  group_by(goalieNameForShot) %>%
  summarise(
    Shots = n(),
    xGoals = sum(xGoal),
    xSaves = sum(xSave),
    xSVP = xSaves / Shots * 100,
    GA = sum(as.numeric(goal) - 1),
    Saves = Shots - GA,
    SVP = Saves / Shots * 100,
    SVPvXSVP = SVP / xSVP
  )

# Check the stats summary
summary(xS_22_stats)

# xRebound Model Creation -------------------------------------------------
# Downsample for generated rebounds from the allSoG dataset
xRebound_training <-
  downSample(x = model_training, y = model_training$shotGeneratedRebound)
# Remove problem variable
xRebound_training <- xRebound_training[-c(16712),]

# Create xRebounds binary logistic regression model structure
xReboundGLM <-
  glm(
    shotGeneratedRebound ~ xCordAdjusted + yCordAdjusted + condition +
      lastEventCategory + shotAngleAdjusted + shotType + speedFromLastEvent +
      shotRebound + shotRush + shooterLeftRight,
    family = "binomial",
    data = xRebound_training
  )

# Perform stepwise function to minimize AIC
xReboundGLM <- step(xReboundGLM)

# VIF for multicolinearity
vif(xReboundGLM)

# Plot the model results
plot(xReboundGLM)

# Summarize the model results
summary(xReboundGLM) # or
xReboundGLM_summary <- broom::tidy(xReboundGLM, conf.int = TRUE, conf.level = 0.95, exponentiate = T) #and
broom::glance(xReboundGLM)

# Plot ROC area under the curve
roc(
  shotGeneratedRebound ~ xReboundGLM$fitted.values,
  data = xRebound_training,
  plot = T,
  main = "ROC CURVE",
  col = "blue"
)

# Plot all independent predictor effects
plot(allEffects(xReboundGLM))

# xRebound Predictions ----------------------------------------------------

# Run Model Prediction for xRebound training data
xRebound_training$predRebound <-
  predict.glm(xReboundGLM, type = "response")

# Check ratio of shots that generate rebound vs shots that do not
xRebound_training %>% count(shotGeneratedRebound) %>% summarize("%" = n /
                                                                  sum(n))

# Summary to find the median value of predicted rebound
summary(xRebound_training$predRebound)

# Assign xRebound if predicted value is greater than the median
xRebound_training$xRebound <-
  ifelse(xRebound_training$predRebound > 0.50618, "1", "0")

# Check the percentage of correct predictions
mean(xRebound_training$xRebound == xRebound_training$shotGeneratedRebound)

# Check distrubutions of predictions vs actual
xRebound_training %>%
  count(xRebound, shotGeneratedRebound) %>%
  mutate(distribution = n / sum(n) * 100)

# Run Model Predictions for 2022 Season
SoG_22$predRebound <-
  predict.glm(xReboundGLM, type = "response", newdata = SoG_22)

# Check distribution of shots that generate rebounds vs. shots that do not
SoG_22 %>% count(shotGeneratedRebound) %>% summarize("%" = n / sum(n))

# Find the 93.7th percentile of the predicted values
quantile(SoG_22$predRebound, c(0.937))

# Assign 1 for xRebound if the prediction is above the 93.7 percentile, 0 if the value is below
SoG_22$xRebound <-
  as.numeric(ifelse(SoG_22$predRebound > quantile(SoG_22$predRebound, c(0.937)), "1", "0"))

# Check the percentage of correct predictions
mean(SoG_22$xRebound == SoG_22$shotGeneratedRebound)

# xRebound Stats ----------------------------------------------------------
# Create summary stats by goalie name using original data and xPredictions
xR_22_stats <- SoG_22 %>%
  group_by(goalieNameForShot) %>%
  summarise(
    Shots = n(),
    xRebounds = sum(xRebound),
    xReboundPerc = xRebounds / Shots * 100,
    Rebounds = sum(as.numeric(shotGeneratedRebound) - 1),
    ReboundPerc = Rebounds / Shots * 100,
    REBvXREB = xReboundPerc / ReboundPerc
  )

# Check summary of new statistics
summary(xR_22_stats)

# xReboSave Model Creation -----------------------------------------------------------

# Create training data by selecting on rebound shots on goal
xReboSave_training <-
  filter(model_training, shotReboundAfterSave == 1)

# Downsample the data so half is rebound saves and half are rebound goals
xReboSave_training <-
  downSample(x = xReboSave_training, y = xReboSave_training$reboundSave)

# Create xReboundSave model structure
xReboSaveGLM <-
  glm(
    reboundSave ~ lastEventShotDistance + lastEventShotAngle +
      xCordAdjusted + yCordAdjusted + condition + shooterLeftRight +
      shotRush + shotType + timeSinceLastEvent + shotAngleAdjusted,
    family = "binomial",
    data = xReboSave_training
  )

# Perform stepwise function to minimize AIC
xReboSaveGLM <- step(xReboSaveGLM)

# VIF for multicolinearity
vif(xReboSaveGLM)

# Plot the model
plot(xReboSaveGLM)

# Summarize the model results
summary(xReboSaveGLM) # or
xReboSaveGLM_summary <- broom::tidy(xReboSaveGLM, conf.int = TRUE, conf.level = 0.95, exponentiate = T) #and
broom::glance(xReboSaveGLM)

# Plot ROC area under the curve
roc(
  reboundSave ~ xReboSaveGLM$fitted.values,
  data = xReboSave_training,
  plot = T,
  main = "ROC CURVE",
  col = "blue"
)

# Plot all independent varaible effects
plot(allEffects(xReboSaveGLM))

# xReboSave Predictions ---------------------------------------------------
# Run predictions for xReboSave training data
xReboSave_training$predReboSave <-
  predict.glm(xReboSaveGLM, type = "response")

# Check proportions of rebound saves to rebound goals
xReboSave_training %>% count(reboundSave) %>% summarize("%" = n / sum(n))

# Summarize predictions to fing the median
summary(xReboSave_training$predReboSave)

# Assign xReboSave to predictions above the median value, xReboGoal to prediction below the median value
# Maintains original saves/goals ratio in predictions
xReboSave_training$xReboSave <-
  ifelse(xReboSave_training$predReboSave > 0.4994, "1", "0")
xReboSave_training$xReboGoal <-
  ifelse(xReboSave_training$predReboSave < 0.4994, "1", "0")

# Check the percentage of correct predictions
mean(xReboSave_training$xReboSave == xReboSave_training$reboundSave)

# Cehck the proprotions of actual vs predicted values
xReboSave_training %>%
  count(xReboSave, reboundSave) %>%
  mutate(distribution = n / sum(n) * 100)

# Apply predictions for 2022 season - if shot is not a rebound, assign an NA
SoG_22$predReboSave <- ifelse(
  SoG_22$shotReboundAfterSave == 1,
  predict.glm(xReboSaveGLM, type = "response", newdata = SoG_22),
  NA
)

# Check the proportions of rebound shots that are saves vs goals
SoG_22 %>% filter(shotReboundAfterSave == 1) %>% count(reboundSave)  %>% summarize("%" = n /
                                                                                     sum(n))

# Find the value of 22.1 percentile of predicted rebound saves
quantile(SoG_22$predReboSave, 0.221, na.rm = T)

# Assign xReboSave if the predicted value is above the 22.1 percentile, xReboGoal is below
SoG_22$xReboSave <-
  ifelse(SoG_22$predReboSave >= quantile(SoG_22$predReboSave, 0.221, na.rm =
                                           T),
         "1",
         "0")
SoG_22$xReboGoal <-
  ifelse(SoG_22$predReboSave < quantile(SoG_22$predReboSave, 0.221, na.rm =
                                          T),
         "1",
         "0")

# Check the percentage of correct predictions
mean(SoG_22$xReboSave == SoG_22$reboundSave, na.rm = T)

# Check the proportion of xReboSaves vs rebound saves
SoG_22 %>%
  count(xReboSave, reboundSave) %>%
  mutate(distribution = n / sum(n) * 100)

# xReboSave Stats --------------------------------------------------------
# Create summary stats by goalie name using original data and xPredictions
xRS_22_stats <- SoG_22 %>%
  group_by(goalieNameForShot) %>%
  summarise(
    ReboShots = sum(as.numeric(shotReboundAfterSave) - 1),
    ReboSaves = sum(as.numeric(reboundSave) - 1),
    ReboGoals = sum(as.numeric(reboundGoal) - 1),
    ReboSVP = ReboSaves / ReboShots * 100,
    xReboSaves = sum(as.numeric(xReboSave), na.rm = T),
    xReboGoals = sum(as.numeric(xReboGoal), na.rm = T),
    xReboSVP = xReboSaves / ReboShots * 100,
    ReboSVPvXReboSVP = ReboSVP / xReboSVP
  )

# Summarize the results
summary(xRS_22_stats)

# Merge all goalie stats
goalies_22 <- left_join(xS_22_stats, xR_22_stats)
goalies_22 <- left_join(goalies_22, xRS_22_stats)
goalies_22 <-
  goalies_22 %>% mutate(
    xSxRxRS = SVPvXSVP + REBvXREB + ReboSVPvXReboSVP,
    xRxRS = REBvXREB + ReboSVPvXReboSVP,
    xSxRS = SVPvXSVP + ReboSVPvXReboSVP
  )

# Write stats and shots on goal with predictions to a .csv
write.csv(goalies_22, file = "goalies_22.csv")
write.csv(SoG_22, file = "SoG_22.csv")

# Remove all variables other than SoG data and models - for plotting on the next script
#rm(list = setdiff(ls(), c("allSoG", "SoG_22", "xSaveGLM", "xReboundGLM", "xReboSaveGLM")))