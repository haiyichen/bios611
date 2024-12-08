library(tidyverse)

setwd("D:/study/UNC_Biostatistics/2024 fall/BIOS 611/project")
match <- read.csv("data/2021-2022.csv")
match <- match %>%
  select(which(names(.) %in% c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "HTHG", "HTAG", "HTR", "HS", "AS", "HST", "AST", "HC", "AC", "HF", "AF", "HY", "AY", "HR", "AR", "HBP", "ABP")))


# Add lables to the varaibles
variable_labels <- c(
  "FTHG"="Home Goal",
  "FTAG"="Away Goal",
  "HS" = "Home Shots",
  "AS" = "Away Shots",
  "HST" = "Home Shots on Target",
  "AST" = "Away Shots on Target",
  "HC" = "Home Corners",
  "AC" = "Away Corners",
  "HF" = "Home Fouls",
  "AF" = "Away Fouls",
  "HY" = "Home Yellow Cards",
  "AY" = "Away Yellow Cards",
  "HR" = "Home Red Cards",
  "AR" = "Away Red Cards"
)


numeric_vars <- match %>% select(where(is.numeric))
categorical_vars <- match %>% select(where(~ is.character(.) | is.factor(.)))

numeric_long <- numeric_vars %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

home_away_vars <- match %>%
  select(names(variable_labels)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

home_away_vars <- home_away_vars %>%
  mutate(
    Team = if_else(str_detect(Variable, "^H"), "Home", "Away"),
    DescriptiveLabel = variable_labels[Variable]
  )

plot1 <- ggplot(home_away_vars, aes(x = DescriptiveLabel, y = Value, fill = Team)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Home and Away Statistics",
    x = "Variable",
    y = "Value",
    fill = "Team"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot
ftr_counts <- as.data.frame(table(match$FTR))
colnames(ftr_counts) <- c("Result", "Count")

# Replace levels with descriptive labels
ftr_counts$Result <- factor(ftr_counts$Result, 
                            levels = c("H", "D", "A"),
                            labels = c("Home Win", "Draw", "Away Win"))

# Calculate percentages
ftr_counts$Percentage <- round((ftr_counts$Count / sum(ftr_counts$Count)) * 100, 1)

# Create a pie chart with percentages
plot2 <- ggplot(ftr_counts, aes(x = "", y = Count, fill = Result)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5), size = 5) +
  labs(
    title = "Full-Time Results",
    fill = "Result"
  ) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

ggsave("box.png", plot1, width =10,height =6,dpi = 200 )
ggsave("pie.png", plot2, width =8,height =5,dpi = 200 )
