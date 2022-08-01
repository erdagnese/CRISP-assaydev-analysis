background_fl <- function(df) {
  #filter(sample == "NTC") -> df_NTC
  as.tbl(df) %>% 
  bind_rows(mutate(df, Cycle=NA, sample=NA)) %>% 
  bind_rows(mutate(df, Well=NA)) %>% 
  group_by(Cycle, sample) %>% 
  summarise_all(c("mean", "sd")) %>%
  subset(select = c(Cycle, FAM_mean, FAM_sd)) %>%
    rename(BG_mean = FAM_mean) %>%
    rename(BG_sd = FAM_sd) %>%
  mutate(BG_3sd = BG_sd * 3) %>%
  mutate(BG_cor = BG_mean + BG_3sd)
  }

