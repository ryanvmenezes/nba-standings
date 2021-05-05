library(tidyverse)
library(googlesheets4)

gs4_auth(email = 'ryanvmenezes@gmail.com')
sheet.id = 'https://docs.google.com/spreadsheets/d/1CP5oyVJwl87rCVQAwzxufkUAtQstJKz69ZwVjtC4XtQ/edit#gid=0'

sims = read_rds('sims.rds')

sims

n.sims = nrow(sims) / n_distinct(sims$team)

seeds = sims %>%
  group_by(team, conf, seed) %>%
  count() %>%
  ungroup()

seeds

summary = sims %>%
  group_by(team, conf) %>%
  summarise(
    mean.wins = mean(w),
    mean.losses = mean(l),
    mean.seed = mean(seed),
    curr.wins = mean(curr.w),
    curr.losses = mean(curr.l),
    future.wins = mean(sim.w),
    future.losses = mean(sim.l),
  ) %>%
  arrange(conf, mean.seed) %>%
  left_join(
    seeds %>%
      mutate(seed = str_c('s', str_pad(seed, pad = '0', side = 'left', width = 2))) %>%
      arrange(seed) %>%
      mutate(n = n / n.sims) %>%
      pivot_wider(names_from = 'seed', values_from = 'n')
  )

summary

summary %>% write_csv('summary.csv', na = '')

summary %>% sheet_write(ss = sheet.id, sheet = 'summary')

matchups = expand_grid(
  slug1 = seeds %>% select(-n) %>% unite(team, conf, seed, col = 'slug1') %>% pull(),
  slug2 = seeds %>% select(-n) %>% unite(team, conf, seed, col = 'slug2') %>% pull(),
) %>%
  separate(slug1, sep = '_', into = c('team1', 'conf1', 'seed1')) %>%
  separate(slug2, sep = '_', into = c('team2', 'conf2', 'seed2')) %>%
  mutate(seed1 = as.numeric(seed1), seed2 = as.numeric(seed2)) %>%
  filter(
    team1 != team2,
    conf1 == conf2,
    (seed1 == 3 & seed2 == 6) | (seed1 == 4 & seed2 == 5) | (seed1 == 7 & seed2 == 8) | (seed1 == 9 & seed2 == 10)
  ) %>%
  left_join(seeds %>% rename_all(~str_c(.x, '1'))) %>%
  left_join(seeds %>% rename_all(~str_c(.x, '2'))) %>%
  transmute(
    conf = conf1,
    seed1, team1, seed2, team2,
    prob = (n1 / n.sims) * (n2 / n.sims)
  ) %>%
  arrange(desc(conf), seed1, -prob)

matchups

matchups %>% write_csv('matchups.csv', na = '')

matchups %>% sheet_write(ss = sheet.id, sheet = 'matchups')

proj = read_csv('nba_elo_latest.csv')

proj

update.summary = proj %>% 
  drop_na(score1) %>% 
  filter(date == max(date)) %>% 
  distinct(date) %>% 
  left_join(proj) %>% 
  summarise(
    last.updated.date = min(date),
    games.on.date = n(),
    games.completed = sum(!is.na(score1))
  )

update.summary

update.summary %>% sheet_write(ss = sheet.id, sheet = 'last-updated')
range_autofit(ss = sheet.id, sheet = 'last-updated')
