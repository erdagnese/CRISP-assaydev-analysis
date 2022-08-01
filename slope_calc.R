slope_calc <- function(df) {
  df %>%
  group_by(Well) %>%
    mutate(diffFAM = FAM - lag(FAM)) %>%
    mutate(diffCycle = Cycle - lag(Cycle)) %>%
    mutate(slope = diffFAM/diffCycle)
}
