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
rename_post_dir("_posts/2016-11-08-sharpe-ratio") 










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


