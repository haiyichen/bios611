library(tidyverse)
library(lme4)
library(broom)
library(glmmTMB)
library(broom.mixed)

setwd("D:/study/UNC_Biostatistics/2024 fall/BIOS 611/project")
match <- read.csv("data/2021-2022.csv")
match <- match %>%
  select(which(names(.) %in% c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "HTHG", "HTAG", "HTR", "HS", "AS", "HST", "AST", "HC", "AC", "HF", "AF", "HY", "AY", "HR", "AR", "HBP", "ABP")))


# Create the home team dataset
home_data <- match %>%
  select(HomeTeam, FTHG, FTR, HST, HS, HC, HF, HY, HR) %>%
  rename(
    Goal = FTHG,
    Team = HomeTeam,
    Shots = HS,
    ShotsOnTarget = HST,
    Corners = HC,
    Fouls = HF,
    YellowCards = HY,
    RedCards = HR
  ) %>%
  mutate(Location = "Home")  # Add location indicator

# Create the away team dataset
away_data <- match %>%
  select(AwayTeam, FTAG, FTR, AST, AS, AC, AF, AY, AR) %>%
  rename(
    Goal = FTAG,
    Team = AwayTeam,
    Shots = AS,
    ShotsOnTarget = AST,
    Corners = AC,
    Fouls = AF,
    YellowCards = AY,
    RedCards = AR
  ) %>%
  mutate(Location = "Away")  # Add location indicator

# Combine home and away datasets
data <- bind_rows(home_data, away_data)

# Create an indicator variable for win or draw
data <- data %>%
  mutate(
    WinOrDraw = ifelse((FTR == "H" & Location == "Home") | 
                         (FTR == "D") | 
                         (FTR == "A" & Location == "Away"), 1, 0)
  )

# Remove unnecessary FTR column if not needed for the model
data <- data %>% select(-FTR)

# Question 1
teams <- unique(data$Team)


results <- teams %>%
  lapply(function(team) {
    # Subset data for the team
    team_data <- data %>% filter(Team == team) %>%
      mutate(
      Location = as.factor(Location),
      WinOrDraw = as.factor(WinOrDraw),
       )%>%select(-Team)
         # Fit logistic regression
         model <- glm(WinOrDraw ~ ., data = team_data, family = binomial)
         
         # Extract the p-value for Location effect
         tidy(model) %>%
           filter(term == "LocationHome") %>%
             mutate(Team = team)
      }) %>%
  bind_rows() 

results_adjusted <- results %>%
  mutate(p_adjusted = p.adjust(p.value, method = "fdr"))

write.csv(results, "winordraw_home.csv", row.names = FALSE)
write.csv(results_adjusted, "winordraw_home_adjusted.csv", row.names = FALSE)

# Question 2
data$Location <- as.factor(data$Location)
fixed_effects <- paste(
  setdiff(names(data), c("WinOrDraw", "Team", "RedCards", "YellowCards")),  # Exclude these
  collapse = " + "
)
fixed_effects_formula <- as.formula(paste("~", fixed_effects))

model_red_cards <- glmmTMB(
  as.formula(paste("RedCards ~", fixed_effects, "+ (1 | Team)")),  # Fixed effects + random intercept
  ziformula = ~1,  # Zero-inflation component
  family = poisson,
  data = data
)

# Zero-inflated Poisson for Yellow Cards
model_yellow_cards <- glmmTMB(
  as.formula(paste("YellowCards ~", fixed_effects, "+ (1| Team)")),  # Fixed effects + random intercept
  ziformula = ~1,  # Zero-inflation component
  family = poisson,
  data = data
)


model_yellow_cards <- glmmTMB(
  as.formula(paste("YellowCards ~", fixed_effects, "+ (1| Team)")),  # Fixed effects + random intercept
  ziformula = ~1,  # Zero-inflation component
  family = poisson,
  data = data
)

# Extract fixed effects from the model
fixed_effects_yellow <- tidy(model_red_cards, effects = "fixed")
fixed_effects_red <- tidy(model_red_cards, effects = "fixed")
write.csv(fixed_effects_yellow, "fixed_effects_yellow.csv", row.names = FALSE)
write.csv(fixed_effects_red, "fixed_effects_red.csv", row.names = FALSE)

# Question 3
fixed_effects <- paste(
  setdiff(names(data), c("WinOrDraw", "Team", "Goal")),  # Exclude these
  collapse = " + "
)
fixed_effects_formula <- as.formula(paste("~", fixed_effects))

model_goal <- glmmTMB(
  as.formula(paste("Goal ~", fixed_effects, "+ (1 | Team)")), 
  family = poisson,
  data = data
)

fixed_effects_goal <- tidy(model_goal, effects = "fixed")
write.csv(fixed_effects_goal, "fixed_effects_goal.csv", row.names = FALSE)
