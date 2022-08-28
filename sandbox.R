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
    df <- vroom(paste("C:/Users/ckoho/Documents/Inputs/NCAA/results_eoy_", k, 
                      "_", year, "_mbb_box_score.csv", sep = ""))
    df$year <- year
    df$k <- k
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
  group_by(k, year) %>%
  summarize(avg_logloss = mean(logloss),
            median_logloss = median(logloss),
            n = n())

df_summary_year_reduced <- df_box_score_all %>%
  group_by(k, year) %>%
  summarize(avg_logloss = mean(logloss),
            median_logloss = median(logloss),
            n = n()) %>%
  filter(k>25,
         k < 32)

ggplot(df_summary_all, aes(x = k, y = avg_logloss)) + geom_point()
ggplot(df_summary_year, aes(x = k, y = avg_logloss)) + 
  geom_point(aes(color = factor(year))) 
ggsave("k_logloss.png")
ggplot(df_summary_year, aes(x = k, y = avg_logloss)) + 
  geom_point(aes(color = factor(year)))  + ylim(.51,.54)
df <- df_summary_year %>%
  filter(k >24 & k <31) %>%
  select(k, avg_logloss, year) %>%
  pivot_wider(names_from = year, values_from = avg_logloss)
write_csv(df, "_posts/2022-08-14-elo-rating-system/logloss.csv")

df %>% mutate_at(2:7, round, 3)
df %>%
  gt() %>%
  gt_color_rows(`2009`:`2014`, palette = "ggsci::default_gsea")

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

df_summary_year <- df_box_score_all %>%
  group_by(k, year, rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n())
df_summary_all <- df_box_score_all %>%
  group_by(k, rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n())
