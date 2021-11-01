# Removing FCT from state analysis 
df_2018 <- df_2018 %>% mutate(
  state_FCTremoved = na_if(df_2018$shstate, 140)
)
# Getting the parasite density and prevalence for each state
state_prev <- df_2018 %>% tabyl(state_FCTremoved, asexual_prev) %>%
  adorn_percentages()
state_prev <- as.data.frame(state_prev)
state_prev <- state_prev %>% rename(state = 1, drop = 2, Asexual_prev = 3)
state_density <- as.data.frame(
  ci.mean (asexual_nozero~state_FCTremoved, data=df_2018, statistic = "geometric", na.rm=TRUE))
state_density <- state_density %>% rename(Asexual_density = 1, lower_95_CI_density = 3, upper_95_CI_density  = 4, state = 5)
state_prev$state <- as.character(state_prev$state)
prev_density_2018 <- left_join(state_prev, state_density, by = c("state"))
prev_density_2018 <- prev_density_2018 %>% mutate(Asexual_prev = Asexual_prev*100)



state_10 <- df_2018 %>% filter(
  state_FCTremoved == 10
)
CI(state_10$asexual_prev, ci=0.95)
CI(df_2018$asexual_prev, ci=0.95)
con <- df_2018 %>% 
  group_by (shstate) %>%
  CI(df_2018$asexual_prev)

mutate(
  CI(df_2018$asexual_prev, ci=0.95)
)


