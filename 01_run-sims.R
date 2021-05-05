start = lubridate::now()

library(tidyverse)
library(furrr)

# plan(multisession)
plan(multicore)
options('future.fork.enable' = TRUE)

proj = read_csv('https://projects.fivethirtyeight.com/nba-model/nba_elo_latest.csv')

proj %>% write_csv('nba_elo_latest.csv', na = '')

proj

teams = read_csv('teams.csv')

teams

gen.mid = function(t1, t2) { str_c(sort(c(t1, t2)), collapse = '|') }

runsim = function () {
  break.multi = function(threeway) {
    div.leader = threeway %>%
      filter(div.rank == 1) %>%
      mutate(tiebreak = 0)

    others = threeway %>%
      filter(div.rank != 1)

    if (nrow(others) == 1) { others = others %>% mutate(tiebreak = 0) }
    if (nrow(others) == 2) { others = break.twoway(others) }
    if (nrow(others) > 2) {
      others = threeway %>%
        left_join(
          expand_grid(t1 = threeway$team, t2 = threeway$team) %>%
            filter(t1 != t2) %>%
            mutate(mid = map2_chr(t1, t2, gen.mid)) %>%
            pivot_longer(c(t1, t2), names_to = 'teamid', values_to = 'team') %>%
            select(-teamid) %>%
            distinct() %>%
            left_join(games, by = c('team', 'mid')) %>%
            group_by(team) %>%
            summarise(tiebreak = sum(win), .groups = 'drop') %>%
            mutate(tiebreak = rank(desc(tiebreak), ties.method = 'first')),
          by = 'team'
        )
    }
    bind_rows(div.leader, others)
  }

  break.twoway = function(twoway) {
    twoway %>%
      left_join(
        twoway %>%
          transmute(
            team,
            mid = gen.mid(twoway$team[1], twoway$team[2])
          ) %>%
          left_join(games, by = c('team', 'mid')) %>%
          group_by(team) %>%
          summarise(tiebreak = sum(win), .groups = 'drop') %>%
          mutate(tiebreak = rank(desc(tiebreak))),
        by = 'team'
      )
  }

  break.ties = function(grp) {
    if (nrow(grp) == 1) {
      return (grp %>% mutate(tiebreak = 0))
    } else if (nrow(grp) == 2) {
      return (break.twoway(grp))
    } else {
      return (break.multi(grp))
    }
  }

  games = proj %>%
    left_join(teams %>% rename_all(~str_c(.x, '1')), by = 'team1') %>%
    left_join(teams %>% rename_all(~str_c(.x, '2')), by = 'team2') %>%
    transmute(
      gmid = glue::glue('{team1}_{team2}_{date}'),
      mid = map2_chr(team1, team2, gen.mid),
      conf1, conf2,
      div1, div2,
      score1, score2,
      raptor_prob1, raptor_prob2,
      game.status = if_else(!is.na(score1), 'final', 'sim'),
      t1win = case_when(
        game.status == 'final' ~ score1 > score2,
        game.status == 'sim' ~ map_lgl(raptor_prob1, ~runif(1) < .x)
      ),
      conf = conf1 == conf2,
      div = div1 == div2,
      team1, team2,
    ) %>%
    pivot_longer(
      c(team1, team2),
      names_to = 'teamid',
      values_to = 'team'
    ) %>%
    mutate(win = (t1win & teamid == 'team1') | (!t1win & teamid == 'team2'))

  records = games %>%
    group_by(team) %>%
    summarise(
      w = sum(win),
      l = sum(!win),
      conf.w = sum(case_when(conf ~ win, TRUE ~ FALSE)),
      conf.l = sum(case_when(conf ~ !win, TRUE ~ FALSE)),
      div.w = sum(case_when(div ~ win, TRUE ~ FALSE)),
      div.l = sum(case_when(div ~ !win, TRUE ~ FALSE)),
      curr.w = sum(case_when(game.status == 'final' ~ win, TRUE ~ FALSE)),
      curr.l = sum(case_when(game.status == 'final' ~ !win, TRUE ~ FALSE)),
      sim.w = sum(case_when(game.status == 'sim' ~ win, TRUE ~ FALSE)),
      sim.l = sum(case_when(game.status == 'sim' ~ !win, TRUE ~ FALSE)),
      .groups = 'drop'
    ) %>%
    left_join(teams, by = 'team') %>%
    arrange(conf, -w) %>%
    group_by(div) %>%
    mutate(div.rank = rank(desc(w), ties.method = 'first')) %>%
    ungroup()

  records %>%
    group_by(conf, w, l) %>%
    nest() %>%
    mutate(data = map(data, break.ties)) %>%
    unnest(c(data)) %>%
    ungroup() %>%
    arrange(conf, -w, tiebreak) %>%
    distinct(conf, team, .keep_all = TRUE) %>%
    group_by(conf) %>%
    mutate(seed = row_number()) %>%
    ungroup()
}

# runsim() %>% view()

n.sims = 10000

sims = tibble(sim = 1:n.sims) %>%
  mutate(sim.result = future_map(sim, ~runsim(), .progress = TRUE)) %>%
  unnest(c(sim.result))

# beepr::beep()

sims

sims %>% write_rds('sims.rds', compress = 'gz')

end = lubridate::now()

print(end - start)
beepr::beep()
