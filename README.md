# BIOS 611 Project
This is the project for BIOS 611 course at UNC Chapel Hill

## Prerequisites
- R with the following libraries installed:
- tidyverse
- ggplot2
- randomForest
- glmmTMB
- broom
- broom.mixed
- A Dockerfile is included to simplify the environment setup.

## Generate Report with Docker
report.pdf can be generated by running
```bash
docker build -t image_name .
docker run --rm -v "$(pwd):/workspace" image_name make
```
## Background
The home Field advantage is believed to exists in many sports such as soccer, football, and basketball. It refers to the benefit that the home team is said to gain over the visiting team. Some of the factors that results in home field advantage are familiarity with the playing environment, reduced travel fatigue, referee bias, and also the presence of home supporters, whose vocal encouragement and psychological impact can motivate the home team and pressure the opposition. The English Premier League (EPL) is regarded as one of the most popular and competitive soccer leagues around the world. We are interested in verifying if the home field advantage significantly influence the results of matches in. In the meantime, we are also interested in the relationship between the standing and defending/passing/possession/shooting performance of teams. Thus, we are going to use datasets of season 2021-2022 of EPL to investigate the following questions:

1. Is the home team more likely to get a win or draw than the away team?

2. Is the home team less likely to get yellow orred cards than the away
team?

3. Is the home team more likely to have more goals than the away team?

4. If we cluster teams with their defending/passing/possession/shooting performance, are teams within a cluster close in the standing?

5. Which of the above performance is most important to predict the standing of a team?

