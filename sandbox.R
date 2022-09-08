usethis::use_git()
usethis::use_github()
library(distill)
distill::create_post("Home Court Advantage")
distill::create_post(
  "ELO Rating System",
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
  summarize(win = round(mean(result.x),3))
