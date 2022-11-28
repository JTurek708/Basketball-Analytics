# Lauri Markkanen Analysis
library(tidyverse)
library(bbplot)
library(ggthemes)

# Create Graph of Adv Stats
lm_graph_per <- ggplot(Markannen_Adv_Stats, aes(x=Season, y=PER, fill=Tm)) +
  geom_bar(stat = 'identity',
           position = 'identity') +
  theme_fivethirtyeight() +
  labs(title='Lauri Markkanen - Player Effenciency Rating',
       fill = 'Team') +
  coord_flip()
lm_graph_per

# Graph USG%
lm_graph_usg <- ggplot(Markannen_Adv_Stats, aes(x=Season, y = `USG%`, fill = Tm)) +
  geom_bar(stat = 'identity', position = 'identity') +
  theme_fivethirtyeight() +
  labs(title = 'Lauri Markkanen - Usage Rate',
       fill = 'Team') +
  coord_flip()
lm_graph_usg
