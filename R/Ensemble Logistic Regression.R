player.data.df <- playerdata_statistics_2010_2017
rm(playerdata_statistics_2010_2017)

# Load Libraries
library(dplyr)
library(tidyr)

# Number of players per team that average double figures
player.data.df %>% 
  filter(gp > 10,
         Season == '2017') %>% 
  select(Player, Team, Season, pts, fouls, gp) %>% 
  mutate(ppg = pts / gp,
         fouls.pg = fouls / gp,
         ppg.risk = ppg - (ppg * (fouls.pg / 5))) %>% 
  group_by(Team) %>% 
  #group_by(Team, Player, ppg, fouls.pg, ppg.risk) %>% 
  filter(ppg.risk > 10) %>% 
  summarize(double.figures = n()) %>%
  arrange(desc(double.figures))

# Good Rebounders
player.data.df %>% 
  filter(gp > 10, 
         Season == '2017') %>% 
  select(Player, Team, Season, orebs, drebs, tot.reb, fouls, gp, Ht) %>% 
  mutate(orebs.pg = orebs / gp,
         drebs.pg = drebs / gp,
         tot.reb.pg = tot.reb / gp,
         fouls.pg = fouls / gp,
         tot.reb.risk = tot.reb.pg - (tot.reb.pg * (fouls.pg / 5))) %>% 
  group_by(Team) %>% 
  group_by(Team, Player, tot.reb.pg, tot.reb.risk, fouls.pg, Ht) %>% 
  filter(tot.reb.risk > 4,
         Team == 'Northern Ky.') %>% 
  summarize(good.orebs = n()) %>% 
  arrange(desc(good.orebs))

# Team depth
player.data.df %>% 
  filter(gp > 10,
         Season == '2017') %>% 
  select(Player, Team, Season, gp, fouls, mp) %>% 
  mutate(min.pg = mp / gp,
         fouls.pg = fouls / gp,
         min.pg.risk = min.pg - (min.pg * (fouls.pg / 5))) %>% 
  filter(min.pg.risk > 15, 
         Team == 'Villanova') %>% 
  #group_by(Team) %>% 
  group_by(Team, Player, min.pg, fouls.pg, min.pg.risk) %>% 
  summarize(team.depth = n()) %>% 
  arrange(desc(team.depth))

# Foul Risk Players
player.data.df %>% 
  filter(gp > 10,
         Season == '2017') %>% 
  select(Player, Team, Season, gp, fouls, pts, tot.reb) %>% 
  mutate(fouls.pg = fouls / gp,
         pts.pg = pts / gp,
         tot.reb.pg = tot.reb / gp) %>% 
  filter(fouls.pg > 2.87, 
         pts.pg > 10 | tot.reb.pg > 5) %>% 
  group_by(Team) %>% 
  summarize(foul.risk = n()) %>% 
  arrange(desc(foul.risk))

# Team FT Percent
player.data.df %>% 
  filter(Season == '2017') %>% 
  select(Team, fta, ft) %>% 
  group_by(Team) %>% 
  summarize(ft.percent = sum(ft, na.rm = TRUE) / sum(fta, na.rm = TRUE)) %>% 
  arrange(desc(ft.percent))
