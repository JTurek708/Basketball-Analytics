### How Do the Bulls Stack Up Amongst Eastern Conference Foes
# First 1.5 months of the season
# Bulls sit at 6-7

## Load packages
library(tidyverse)
library(nbastatR)
warnings()
library(BasketballAnalyzeR)
library(jsonlite)
library(janitor)
library(extrafont)
library(scales)
library(teamcolors)
library(zoo)
library(future)
library(lubridate)
library(ggthemes)
library(ggrepel)

# Load in Offensive/Defensive Box Scores 22-23 season as of 11/10
# Set Eastern Conference teams to var eastern_conf
eastern_conf <- c('Milwaukee Bucks', 'Cleveland Cavaliers', 'Boston Celtics',
                  'Atlanta Hawks', 'Toronto Raptors', 'Washington Wizards',
                  'Chicago Bulls', 'Indiana Pacers', 'New York Knicks',
                  'Brooklyn Nets', 'Philadelphia 76ers', 'Miami Heat',
                  'Detroit Pistons', 'Orlando Magic', 'Charlotte Hornets')
Season_Off_Box_Scores
eastern_conf_obox <- Season_Off_Box_Scores %>%
  filter(Team %in% eastern_conf)
View(eastern_conf_obox)
eastern_conf_dbox <- Season_Def_Box_Scores %>%
  filter(Team %in% eastern_conf)
View(eastern_conf_dbox)

## Create Possesion, PACE, ORtng and DRtngs for EC
eastern_conf_obox <- eastern_conf_obox %>%
  mutate(POSS = (eastern_conf_obox$`2PA`+eastern_conf_obox$`3PA`)+0.44*FTA-ORB+TOV)
eastern_conf_obox <- eastern_conf_obox %>%
  mutate(PACE = 5*POSS/MP)
eastern_conf_obox <- eastern_conf_obox %>%
  mutate(Off_Rtng = (PTS/POSS)*100)

# Calculate defensive factors
eastern_conf_dbox <- eastern_conf_dbox %>%
  mutate(POSS = (eastern_conf_dbox$`2PA`+eastern_conf_dbox$`3PA`)+0.44*FTA-ORB+TOV)
eastern_conf_dbox <- eastern_conf_dbox %>%
  mutate(DEF_PACE = 5*POSS/MP)
eastern_conf_dbox <- eastern_conf_dbox %>%
  mutate(Def_Rtng = (PTS/POSS)*100)

# Need to calculate POSS for defensive side
ec_ratings <- eastern_conf_obox %>%
  mutate(Def_Rtng = (eastern_conf_dbox$Def_Rtng),
         Def_Pace = eastern_conf_dbox$DEF_PACE,
         Def_Poss = eastern_conf_dbox$POSS)
View(ec_ratings)
ec_ratings <- ec_ratings %>%
  select(Team, POSS, PACE, Off_Rtng, Def_Rtng, Def_Pace, Def_Poss)

# Round all numeric columns
ec_ratings <- ec_ratings %>%
  mutate_if(is.numeric, round,0)

# Graph off/def rtngs
ratings_plot <- ggplot(ec_ratings, aes(x=Off_Rtng, y=Def_Rtng))+
  geom_point(color='red') +
  labs(title = 'Offensive/Defensive Ratings, 2022-23 Season',
       subtitle = 'Eastern Conference',
       x = 'Offensive Rating',
       y = 'Defensive Rating') +
  theme_classic() +
  geom_label_repel(aes(label = Team))
ratings_plot

# Effective field goal percentage
# Off_eFG = (P2M + 1.5*P3M)/P2A + P3A
# Def_eFG = same as above for opponents
eastern_conf_obox <- eastern_conf_obox %>%
  mutate(Off_eFGPct = (`2P`+1.5*`3P`)/(`2PA`+`3PA`)*100)
eastern_conf_obox <- eastern_conf_obox %>%
  mutate(Def_eFGPct = (eastern_conf_dbox$`2P`+1.5*eastern_conf_dbox$`3P`)/(eastern_conf_dbox$`2PA`+eastern_conf_dbox$`3PA`)*100)

# Graph of off/def effective FGPct
eFG_Pct_plot <- ggplot(eastern_conf_obox, aes(Off_eFGPct, Def_eFGPct))+
  geom_point(color='red') +
  labs(title = 'Offensive/Defensive Effective Field Goal Percentage, 2022-23 Season',
       subtitle = 'Eastern Conference',
       x = 'Offensive Effective Field Goal %',
       y = 'Defensive Effective Field Goal %') +
  theme_classic() +
  geom_label_repel(aes(label=Team))
eFG_Pct_plot
