usethis::use_git()
usethis::use_github()
library(distill)
distill::create_post("Home Court Advantage")
distill::create_post(
  "Adjusted Elo Rankings",
  author = "Colin Kohoutek",
  slug = "auto", # generates a website slug (URL)
  date_prefix = TRUE, # adds date for sorting
  draft = FALSE, 
  edit = interactive()
)

distill::create_post(
  "sandbox",
  author = "Colin Kohoutek",
  slug = "auto", # generates a website slug (URL)
  date_prefix = NULL, # adds date for sorting
  draft = TRUE, 
  edit = interactive()
)
rename_post_dir("_posts/2022-09-05-home-court-advantage", date_prefix = "9/05/2022") 


gitcreds::gitcreds_set()









library(tidyverse)
library(vroom)
library(fs)
library(gt)
library(gtExtras)
k_loop <- c(1, 5, 10, 15, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 35)

df_box_score_all <- tibble()
for (k in k_loop){
  for (year in 2009:2014){
    link <- paste0("C:/Users/ckoho/Documents/Inputs/NCAA/results_eoy_", k, "_", 
                   year, "_mbb_box_score.csv")
    df <- vroom(link)
    df$k <- k
    df$year <- year
    df <- df %>%
      mutate(rounded = round(team2_odds, 2))
    df_box_score_all <- df_box_score_all %>%
      bind_rows(df)
  }
}

df_summary_year <- df_box_score_all %>%
  group_by(k, year, rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n()) %>%
  filter(k == 27) %>%
  filter (n > 50)
df_summary_all <- df_box_score_all %>%
  group_by(k, rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n())

ggplot(df_summary_year, aes(x=mean_result, y=rounded)) + 
  geom_point(aes(color=as_factor(year), size = n)) + 
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1) +
  xlab("win_percentage") + ylab("predicted_win_percentage")
ggplot(df_summary_year, aes(x=mean_result, y=rounded)) + 
  geom_point(aes(color=as_factor(year))) + 
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1) +
  xlab("win_percentage") + ylab("predicted_win_percentage")
ggsave("prediction_accuracy_k27_sample_plot.png")
ggsave("prediction_accuracy_k27_plot.png")



ggsave("k_evaluation_logloss.png")

df_box_score_all <- tibble()
k_loop <- c(1, 5, 10, 15, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 35)
for (k in k_loop){
  for (year in 2009:2014){
    link <- paste0("C:/Users/ckoho/Documents/Inputs/NCAA/results_eoy_", k, "_", 
                   year, "_mbb_box_score.csv")
    df <- vroom(link)
    df$k <- k
    df$year <- year
    df <- df %>%
      mutate(rounded = round(team2_odds, 2))
    df_box_score_all <- df_box_score_all %>%
      bind_rows(df)
  }
}




for (k in k_loop){
  for (year in 2009:2014){
    df <- vroom(paste("results_eoy_", k, "_", year, "_mbb_box_score.csv", 
                      sep = ""))
    df$k <- k
    df$year <- year
    df <- df %>%
      mutate(rounded = round(team2_odds, 2))
    df_box_score_all <- df_box_score_all %>%
      bind_rows(df)
  }
}
df_summary_year <- df_box_score_all %>%
  group_by(k, year, rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n()) %>%
  filter(k == 27)



ggplot(df_summary_year, aes(x = mean, y = avg_logloss)) + 
  geom_point(aes(color = factor(year))) 
ggsave("k_linearity.png")


ggplot(df_summary_all, aes(x=rounded, y=mean_result)) + geom_point(aes(color=as.factor(k))) + 
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1)













##########################
###Log loss calculation
#########################
k_loop <- c(1, 5, 10, 15, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 35)
k_loop <- c(1, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30, 40)
k_loop <- c(1, 5, 10, 15, 20, 25, 30, 35)
df_box_score_all <- tibble()
for (k in k_loop){
  for (year in 2009:2021){
    link <- paste0("C:/Users/ckoho/Documents/Inputs/NCAA/results_eoy_", 
                   year, "_mbb_box_score.csv")
    df <- vroom(link)
    df$year <- year
    #print(k)
    #print(year)
    df <- df %>%
      mutate(eq1 = result * log(team2_odds),
             eq2 = (1 - result) * log(1-team2_odds),
             logloss = -(eq1 + eq2)
      )
    df <- df %>%
      mutate(rounded = round(team2_odds, 2))
    df_box_score_all <- df_box_score_all %>%
      bind_rows(df)
  }
}
df_summary_all <- df_box_score_all %>%
  group_by(k) %>%
  summarize(avg_logloss = mean(logloss),
            median_logloss = median(logloss),
            n = n())

df_summary_year <- df_box_score_all %>%
  group_by(year) %>%
  summarize(avg_logloss = mean(logloss),
            median_logloss = median(logloss),
            n = n()) %>%
  select(-"median_logloss") %>%
  rename("logloss" = "avg_logloss")

write_csv(df_summary_year, "all_year_logloss.csv")
df_summary_year %>%
  gt() %>%
  gt_color_rows(avg_logloss,palette = "ggsci::default_gsea")




####Home-Court-Advantage
library(tidyverse)
library(vroom)
library(fs)
library(gt)
library(gtExtras)

#Calculating each year win percentage
files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "torvik_box_score.*.csv")
df_torvik <- vroom(files) %>%
  filter(loc == "A")
df_torvik <- df_torvik %>%
  mutate(result = if_else(team2 == win, 1, 0),
         month = lubridate::month(df_torvik$date))
df_season_winper <- df_torvik %>%
  group_by(season) %>%
  summarize(win = round(mean(result),3)) %>%
  pivot_wider(names_from=season, values_from=win)
df_month_winper <- df_torvik %>%
  group_by(season, month) %>%
  summarize(win = mean(result),
            n = n()) %>%
  filter(n > 10)

ggplot(df_month_winper, aes(x = season, y = win)) + 
  geom_point(aes(color = month, size = n)) 
ggsave("_posts/2022-04-10-home-court-advantage/homecourt_month_winper.png")
df_season_winper %>%
  gt() %>%
  tab_header(
    title = "Home Win Percentage By Year")
df_month_winper <- vroom("_posts/2022-04-10-home-court-advantage/homecourt_month_winper.csv") %>%
  mutate(month = factor(month, 
                        levels = c("November", "December", "January", 
                                   "February", "March")))
write_csv(df_season_winper, 
          "_posts/2022-04-10-home-court-advantage/homecourt_season_winper.csv")

write_csv(df_month_winper, 
          "_posts/2022-04-10-home-court-advantage/homecourt_month_winper.csv")


#Conference Games

files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "torvik_box_score.*.csv")
df_torvik <- vroom(files) 
df <- df_torvik %>%
  filter(team1_conf == team2_conf,
         loc == "A")
df <- df %>%
  mutate(result = if_else(team2 == win, 1, 0),
         month = lubridate::month(df$date))


df_season_winper <- df %>%
  group_by(season) %>%
  summarize(win = round(mean(result),3)) %>%
  pivot_wider(names_from=season, values_from=win)
df_month_winper <- df %>%
  group_by(season, month) %>%
  summarize(win = mean(result),
            n = n()) %>%
  filter(n > 10)

df_month_winper <- df_month_winper %>%
  mutate(month = factor(month, 
                        levels = c("11", "12", "1", "2", "3")))
ggplot(df_month_winper, aes(x = season, y = win)) + 
  geom_point(aes(color = month, size = n)) 
df_conf_winper %>%
  gt() %>%
  tab_header(
    title = "Conference Home Win Percentage By Year")
write_csv(df_season_winper, 
          "_posts/2022-04-10-home-court-advantage/conference_season_winper.csv")


df_season_winper <- df %>%
  group_by(season) %>%
  summarize(win = round(mean(result),3))
ggplot(df_season_winper, aes(x = season, y = win)) + 
  geom_point()


#Home and Away
files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "torvik_box_score.*.csv")
df_torvik <- vroom(files) 
df <- df_torvik %>%
  filter(loc == "A")
df <- df %>%
  mutate(result = if_else(team2 == win, 1, 0),
         month = lubridate::month(df$date)) %>%
  select(team1, team2, date, month, result, season)
df1 <- df %>%
  mutate(month = factor(month, 
                        levels = c("11", "12", "1", 
                                   "2", "3")))
df_repeat <- df %>%
  inner_join(df, by = c("team1" = "team2", "team2" = "team1", 
                        "season" = "season"))
df_season_winper <- df_repeat %>%
  group_by(season) %>%
  summarize(win = round(mean(result.x),3)) %>%
  pivot_wider(names_from=season, values_from=win)
write_csv(df_season_winper, 
          "_posts/2022-09-05-home-court-advantage/repeat_season_winper.csv")
df_season_winper %>%
  gt() %>%
  tab_header(
    title = "Home Win Percentage By Year")

files <- fs::dir_ls(path = "_posts/2022-09-05-home-court-advantage/", 
                    regexp = "/[rc].*_season_winper.csv")
df_combined <- vroom(files) 
df_combined <- df_combined %>%
  add_column(Method = c("Conference", "Repeat"))
df_combined %>%
  gt(rowname_col = "Method") %>%
  tab_header(
    title = "Home Win Percentage By Year")
write_csv(df_combined, 
          "_posts/2022-09-05-home-court-advantage/combined_season_winper.csv")




###Home Court Stability
library(tidyverse)
library(vroom)
library(fs)
library(gt)
library(gtExtras)
files <- fs::dir_ls(path = "../../Inputs/NCAA/HCA/", 
                    regexp = "results.*.csv")
df <- vroom(files) 


df_avg <- df %>%
  filter(loc != "N") %>%
  mutate(delta = team2_odds - result ) %>%
  group_by(season) %>%
  mutate(cum_avg = cummean(delta),
         games = 1:n())
df_avg1 <- df_avg %>%
  filter(season == 2009 | season == 2010| season > 2018)
ggplot(df_avg1, aes(x=games, y=cum_avg)) + 
  geom_point(aes(color=as_factor(year))) + 
  ylim(-.4,.1)


df %>%
  filter(loc != "N") %>%
  mutate(delta = team2_odds - result ) %>%
  group_by(season) %>%
  summarise(cum_avg = mean(delta)) %>%
  arrange(cum_avg)

####################################
####Improving our Ratings
###################################

library(tidyverse)
library(vroom)
library(fs)
library(gt)
library(gtExtras)


# Home and home results.
files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "torvik_box_score.*.csv")
df_torvik <- vroom(files) 
df_torvik <- df_torvik %>%
  filter(loc == "A")
df_torvik <- df_torvik %>%
  mutate(result = if_else(team2 == win, 1, 0)) %>%
  select(team1, team2, result, season, team1_pts, team2_pts)
df_repeat <- df_torvik %>%
  inner_join(df_torvik, by = c("team1" = "team2", "team2" = "team1", 
                               "season" = "season")) %>%
  mutate(game1_diff = team2_pts.x - team1_pts.x,
         game2_diff = team1_pts.y - team2_pts.y) 

df_summary <- df_repeat %>%
  group_by(game1_diff) %>%
  summarise(game2_diff = mean(game2_diff),
            n = n(),
            result = mean(result.y)) %>%
  filter(n > 100)
#filter(n > 27)
df_summary <- df_summary %>%
  mutate(rating_delta = (400 * log ((1 - result)/ result))/log(10)/2)

#Plot all with fit
formula <- y ~ x
ggplot(df_repeat, aes(x=game1_diff, y=game2_diff)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  ggpmisc::stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        label.x.npc = "right", label.y.npc = 0.15,
                        formula = formula, parse = TRUE, size = 3)
ggsave("margin_of_victory_all_fit.png")

ggplot(df_summary, aes(x=game1_diff, y=game2_diff)) + 
  #geom_point(aes(size=n)) +
  geom_point() + 
  scale_x_continuous(breaks= seq(-30,30, by=5), limits = c(-30,30)) + 
  scale_y_continuous(breaks= seq(-10, 8, by=2), limits = c(-15,8)) + 
  geom_hline(yintercept=0)
ggsave("margin_of_victory_all_nosize.png")

ggplot(df_summary, aes(x=game1_diff, y=result)) + 
  geom_point(aes(size=n)) + geom_hline(yintercept=.5)
ggsave("mov_win_per.png")


formula <- y ~ x
ggplot(df_summary, aes(x=rating_delta, y=game2_diff)) + 
  geom_point(aes(size=n)) +
  geom_smooth(method = "lm", se = FALSE) + 
  ggpmisc::stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3) +
  geom_abline(aes(intercept = 0, slope = .0815))
ggsave("mov_rating_delta.png")
ggplot(df_summary, aes(x=rating_delta, y=game2_diff)) + 
  geom_point(aes(size=n)) +
  geom_abline(aes(intercept = 0, slope = .0815))




#############################
### Violin Plot           ###
#############################
df_ranking <- vroom("C:/Users/ckoho/Documents/Inputs/NCAA/mbb_elo_2021.csv", 
                    altrep = FALSE)
#Select just each end of year ranking.
df_ranking <- df_ranking %>%
  select(team,elo_2008, elo_2009, elo_2010, elo_2011, elo_2012, elo_2013, elo_2014, 
         elo_2015, elo_2016, elo_2017, elo_2018, elo_2019, elo_2020, elo_2021) %>%
  pivot_longer(!team, names_to = "year", values_to = "rating")

#Violin plot of each year ratings.
ggplot(df_ranking, aes(year, rating)) + geom_violin(adjust = .5) #+ geom_jitter()
ggsave("violin_plot.png")




########################################################################
###Log Loss calculation. Comparing autocor, default, line, and aelo. ###
########################################################################

#autocor
files <- fs::dir_ls(path = "../../Inputs/NCAA/auto_corr/", 
                    regexp = "results_eoy.*.csv")
df_autocor <- vroom(files, altrep = FALSE)
df_autocor <- df_autocor %>%
  mutate(eq1 = result * log(team2_odds),
         eq2 = (1 - result) * log(1-team2_odds),
         logloss = -(eq1 + eq2)
  )
df_ac_sum <- df_autocor %>%
  group_by(season) %>%
  summarize(ac = mean(logloss))

#default
files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "results_eoy.*.csv")
df <- vroom(files, altrep = FALSE)
df <- df %>%
  mutate(eq1 = result * log(team2_odds),
         eq2 = (1 - result) * log(1-team2_odds),
         logloss = -(eq1 + eq2)
  )
df_def_sum <- df %>%
  group_by(season) %>%
  summarize(normal = mean(logloss))

#aelo
files <- fs::dir_ls(path = "../../Inputs/NCAA/aelo/", 
                    regexp = "results_eoy.*.csv")
df <- vroom(files, altrep = FALSE)
df <- df %>%
  mutate(eq1 = result * log(team2_odds),
         eq2 = (1 - result) * log(1-team2_odds),
         logloss = -(eq1 + eq2)
  )
df_aelo_sum <- df %>%
  group_by(season) %>%
  summarize(aelo = mean(logloss))

#line
files <- fs::dir_ls(path = "../../Inputs/NCAA/line/", 
                    regexp = "results_eoy.*.csv")
df <- vroom(files, altrep = FALSE)
df <- df %>%
  mutate(eq1 = result * log(team2_odds),
         eq2 = (1 - result) * log(1-team2_odds),
         logloss = -(eq1 + eq2)
  )
df_line_sum <- df %>%
  group_by(season) %>%
  summarize(line = mean(logloss))

df1 <- df_def_sum %>%
  left_join(df_ac_sum) %>%
  #left_join(df_aelo_sum) %>%
  left_join(df_line_sum) %>%
  filter(season > 2008 ) %>%
  filter(season < 2017)
write_csv(df1, 
          "_posts/2022-11-12-improving-on-our-elo-ratings/methods_logloss.csv")
ggplot(df1, aes(x=season, y=logloss)) + geom_point(aes(color = group))


df <- read_csv("_posts/2022-11-12-improving-on-our-elo-ratings/methods_logloss.csv")
df_pivot <- data %>%
  pivot_longer(!season, names_to = "group", values_to = "logloss")
ggplot(df_pivot, aes(x=season, y=logloss)) + geom_point(aes(color = group)) + 
  labs(title = "Logloss Calculation", 
       subtitle = "The line based elo is significantly higher (worse) than the default and autocorrelation methods",
       x = "Season",
       y = "Log Loss",
       caption = "Colin Kohoutek, data from @sportsdataverse"
  ) + theme_bw() + theme(plot.caption = element_text(face = "italic",
                                                     size = 4,
                                                     color = "grey"),
                         plot.tag = element_text(face = "bold"),
                         plot.subtitle = element_text(size = 4))
ggsave("_posts/2022-11-12-improving-on-our-elo-ratings/logloss_options.png")

df_pivot <- data %>%
  pivot_longer(!season, names_to = "group", values_to = "logloss") %>%
  filter(group != "line")
ggplot(df_pivot, aes(x=season, y=logloss)) + geom_point(aes(color = group)) + 
  labs(title = "Logloss, Line Rating Removed", 
       subtitle = "Autcorrelation is slightly worse than the normal rating system.",
       x = "Season",
       y = "Log Loss",
       caption = "Colin Kohoutek, data from @sportsdataverse"
  ) + theme_bw() + theme(plot.caption = element_text(face = "italic",
                                                     size = 4,
                                                     color = "grey"),
                         plot.tag = element_text(face = "bold"),
                         plot.subtitle = element_text(size = 4))


ggsave("_posts/2022-11-12-improving-on-our-elo-ratings/logloss_options_noline.png")

######################################
# ggplot theme development
#######################################
ggplot(df1, aes(x=season, y=line)) + geom_point() + 
  labs(title = "Title Here", 
       subtitle = "Explain what the graph is showing",
       x = "X Label",
       y = "Y Label",
       caption = "Colin Kohoutek, data from @sportsdataverse"
  ) + theme_bw() + theme(plot.caption = element_text(face = "italic",
                                                     size = 8,
                                                     color = "grey"),
                         plot.tag = element_text(face = "bold"),
                         plot.subtitle = element_text(size = 8))
  


########################################################################
###Adjusted Elo Rankings Log Loss calculation.                       ###
###Comparing l values and to default.                                ###
########################################################################

df_aelo_sum <- NULL
l_loop <- c(.01, .1, .2, .3, .4, .05, .5, .15, .25, .75, 0)
for (l in l_loop){
  regexp_path <- paste0("results_eoy_", l, ".*csv")
  files <- fs::dir_ls(path = "../../Inputs/NCAA/aelo/", 
                      regexp = regexp_path)
  df_aelo <- vroom(files, altrep = FALSE)
  df_aelo <- df_aelo %>%
    mutate(eq1 = result * log(team2_odds),
           eq2 = (1 - result) * log(1-team2_odds),
           logloss = -(eq1 + eq2)
    )
  df_aelo <- df_aelo %>%
    group_by(season) %>%
    summarize(l = l,
              ac = mean(logloss))
  df_aelo_sum <- df_aelo_sum %>%
    bind_rows(df_aelo)
  
}
df_aelo_sum_filt <- df_aelo_sum %>%
  filter(season > 2008) %>%
  filter(season < 2019)
ggplot(df_aelo_sum_filt, aes(x=season, y=ac )) + geom_point(aes(color = as_factor(l)))
options(pillar.sigfig=5)

df_aelo_summary <- df_aelo_sum_filt %>%
  group_by(l) %>%
  summarize(logloss = mean(ac))#
write_csv(df_aelo_summary, 
          "_posts/2024-01-15-adjusted-elo-rankings/AdjustEloFullSummary.csv")

df_aelo_summary %>%
  gt() %>%
  gt_color_rows(logloss,palette = "ggsci::default_gsea")

df_aelo_summary_filtered <- df_aelo_sum_filt %>%
  filter(l < .45) %>%
  fliter(l > .10) %>%
  group_by(l) %>%
  summarize(logloss = mean(ac))

df_aelo_filtered <- df_aelo_sum_filt %>%
  filter(l < .45) %>%
  filter(l > .10)
ggplot(df_aelo_filtered, aes(x=season, y=ac )) + 
  geom_point(aes(color = as_factor(l))) +
  labs(title = "Logloss Calculation", 
       subtitle = "The line based elo is significantly higher (worse) than the default and autocorrelation methods",
       x = "Season",
       y = "Log Loss",
       caption = "Colin Kohoutek, data from @sportsdataverse"
  ) + theme_bw() + theme(plot.caption = element_text(face = "italic",
                                                     size = 4,
                                                     color = "grey"),
                         plot.tag = element_text(face = "bold"),
                         plot.subtitle = element_text(size = 4))



write_csv(df_aelo_summary_filtered, 
          "_posts/2024-01-15-adjusted-elo-rankings/AELOFilteredSummary.csv")


df <- read_csv("_posts/2022-11-12-improving-on-our-elo-ratings/methods_logloss.csv")
df_pivot <- data %>%
  pivot_longer(!season, names_to = "group", values_to = "logloss")
ggplot(df_pivot, aes(x=season, y=logloss)) + geom_point(aes(color = group)) + 
  labs(title = "Logloss Calculation", 
       subtitle = "The line based elo is significantly higher (worse) than the default and autocorrelation methods",
       x = "Season",
       y = "Log Loss",
       caption = "Colin Kohoutek, data from @sportsdataverse"
  ) + theme_bw() + theme(plot.caption = element_text(face = "italic",
                                                     size = 4,
                                                     color = "grey"),
                         plot.tag = element_text(face = "bold"),
                         plot.subtitle = element_text(size = 4))
ggsave("_posts/2022-11-12-improving-on-our-elo-ratings/logloss_options.png")

df_pivot <- data %>%
  pivot_longer(!season, names_to = "group", values_to = "logloss") %>%
  filter(group != "line")
ggplot(df_pivot, aes(x=season, y=logloss)) + geom_point(aes(color = group)) + 
  labs(title = "Logloss, Line Rating Removed", 
       subtitle = "Autcorrelation is slightly worse than the normal rating system.",
       x = "Season",
       y = "Log Loss",
       caption = "Colin Kohoutek, data from @sportsdataverse"
  ) + theme_bw() + theme(plot.caption = element_text(face = "italic",
                                                     size = 4,
                                                     color = "grey"),
                         plot.tag = element_text(face = "bold"),
                         plot.subtitle = element_text(size = 4))


