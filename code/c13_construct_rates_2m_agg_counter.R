
message("Constructing aggregate transition rates for counterfactual scenarios")

# flows from CPS microdata vs BLS official flows data as source for transition rates and stocks:
# with CPS microdata based transition rates on average
# - EI, II, UI transition rates are higher than transition rates constructed using BLS flows and stocks data
# - EU, IU, UU and IE transition rates are lower than transition rates constructed using BLS flows and stocks data
# thus stocks simulated using law of motion and CPS microdata based transition rates will yield
# - I stock higher than in BLS data
# - E and U stocks lower than in BLS data

# constructed transition rates from construct_flows_2m_agg.R
load(file = str_c(edir_cps, "out_flowsandrates_agg.Rdata"))

# add business cycle id
df_rates_all_agg <-
    df_flowsandrates_all_agg %>%
    mutate(# define recession/expansion periods
           cycle_id = case_when(period_1 %in% 197601:197912 ~ "E1",
                                period_1 %in% 198001:198007 ~ "R1",
                                period_1 %in% 198008:198106 ~ "E2",
                                period_1 %in% 198107:198211 ~ "R2",
                                period_1 %in% 198212:199006 ~ "E3",
                                period_1 %in% 199007:199103 ~ "R3",
                                #period_1 %in% 199104:200102 ~ "E4"
                                # see footnote 16 in Cortes, Jaimovich, Nekarda and Siu (2016)
                                # "due to the January 1994 redesign of the CPS and the discontinuities that this induces in certain transition rates,
                                # the"averages for phase E4 used in this section are calculated over the period 1994:1 to 2001:2
                                period_1 %in% 199401:200102 ~ "E4",
                                period_1 %in% 200103:200111 ~ "R4",
                                period_1 %in% 200112:200711 ~ "E5",
                                period_1 %in% 200712:200906 ~ "R5",
                                period_1 %in% 200907:201812 ~ "E6")) %>%
    mutate(yearm_1 = period_1 %>% as.character() %>% as.yearmon("%Y%m")) %>%
    select(source, period_1, yearm_1, cycle_id, lfs_1, lfs_2, seas, rate)

# plot transition rates
df_rates_all_agg %>%
    ggplot() +
        geom_line(aes(x = yearm_1, y = rate, color = seas, linetype = source)) +
        geom_rect(data = (rec_dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        # facet_grid(lfs_1 ~ lfs_2)
        facet_wrap(lfs_1 ~ lfs_2, ncol = 1, scales = "free_y")

# define transition rates for counterfactuals
df_rates_all_agg_counter <-
    # construct average transition rates by recession/expansion period
    full_join(df_rates_all_agg%>%
                  mutate(cycle_id = if_else(period_1 %in% c(199104:200102), "E4", cycle_id)),
              df_rates_all_agg %>%
                  group_by(source, cycle_id, lfs_1, lfs_2, seas) %>%
                  summarise(avg_rate = mean(rate, na.rm = TRUE)) %>%
                  filter(!is.na(cycle_id)) %>%
                  ungroup(),
              by = c("source", "cycle_id", "lfs_1", "lfs_2", "seas")) %>%
    # add transition rates for counterfactuals for CPS data (BLS data starts in 1990 and does not iunclude R1 and E1)
    group_by(source, lfs_1, lfs_2, seas) %>%
    mutate(# counterfactual 1: replace transition rates from U into E
           counter1 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs_1 == "U" & lfs_2 == "E" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "U" & lfs_2 == "E" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                TRUE                                                                        ~ avg_rate),
           # counterfactual 2: replace transition rates from I into E
           counter2 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs_1 == "I" & lfs_2 == "E" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "I" & lfs_2 == "E" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                TRUE                                                                        ~ avg_rate),
           # counterfactual 3: replace transition rates from E into I
           counter3 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs_1 == "E" & lfs_2 == "I" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "E" & lfs_2 == "I" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                TRUE                                                                        ~ avg_rate),
           # counterfactual 4: replace transition rates from E into U
           counter4 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs_1 == "E" & lfs_2 == "U" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "E" & lfs_2 == "U" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                TRUE                                                                        ~ avg_rate),
           # counterfactual 5: combines counterfactual 1, 2, 3
           counter5 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs_1 == "U" & lfs_2 == "E" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "U" & lfs_2 == "E" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                lfs_1 == "I" & lfs_2 == "E" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "I" & lfs_2 == "E" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                lfs_1 == "E" & lfs_2 == "I" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "E" & lfs_2 == "I" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                lfs_1 == "E" & lfs_2 == "U" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "E" & lfs_2 == "U" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                TRUE                                                                        ~ avg_rate)) %>%
    rename(actual_rate = rate) %>%
    gather(scenario, rate, actual_rate, avg_rate, starts_with("counter"))


# adjusted counterfactual transition rates so that they add up to 1
df_rates_all_agg_counter_adj <-
    df_rates_all_agg_counter %>%
    group_by(source, period_1, lfs_1, seas, scenario) %>%
    mutate(correction = if_else(scenario != "actual_rate" & lfs_1 == lfs_2, sum(rate) - 1, 0)) %>%
    ungroup() %>%
    mutate(rate = rate - correction)

df_rates_all_agg_counter_adj %>%
    filter(correction != 0) %$%
    table(lfs_1, lfs_2, scenario, seas, source)

save(df_rates_all_agg, df_rates_all_agg_counter_adj, file = str_c(edir_cps, "out_rates_agg_counter.Rdata"))


# datasource "CPS" or "BLS": choose CPS microdata pr BLS official data as source for transition rates and stocks
# with CPS microdata based transition rates on average
# - EI, II, UI transition rates are higher than transition rates constructed using BLS flows and stocks data
# - EU, IU, UU and IE transition rates are lower than transition rates constructed using BLS flows and stocks data
# thus stocks simulated using law of motion and CPS microdata based transition rates will yield
# - I stock higher than in BLS data
# - E and U stocks lower than in BLS data

# plot transition rates CPS vs BLS
df_rates_all_agg_counter_adj %>%
    # filter(seas == "NSA") %>%
    filter(seas == "SA") %>%
    filter(yearm_1 > "Jan 1990") %>%
    filter(scenario %in% c("actual_rate", "avg_rate")) %>%
    ggplot() +
        geom_line(aes(x = yearm_1, y = rate, col = source)) +
        geom_rect(data=(rec_dates %>% filter(Start > "Jan 1990")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        facet_wrap(~ scenario + lfs_2 + lfs_1, ncol = 6, scales = "free_y", labeller = label_both, dir = "v") +
        theme(strip.placement = "outside")

# plot transition rates CPS vs BLS
df_rates_all_agg_counter_adj %>%
    # filter(source == "BLS") %>%
    filter(source == "CPS") %>%
    filter(yearm_1 > "Jan 1990") %>%
    filter(scenario %in% c("actual_rate", "avg_rate")) %>%
    ggplot() +
        geom_line(aes(x = yearm_1, y = rate, col = seas)) +
        geom_rect(data=(rec_dates %>% filter(Start > "Jan 1990")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        facet_wrap(~ scenario + lfs_2 + lfs_1, ncol = 6, scales = "free_y", labeller = label_both, dir = "v") +
        theme(strip.placement = "outside")

# plot transition rates SA vs NSA
df_rates_all_agg_counter_adj %>%
    filter(source == "CPS") %>%
    # filter(scenario == "actual_rate") %>%
    # filter(scenario == "avg_rate") %>%
    filter(scenario == "counter5") %>%
    ggplot() +
        geom_line(aes(x = yearm_1, y = rate, col = seas)) +
        geom_rect(data=(rec_dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        # facet_grid(lfs_1 ~ lfs_2, labeller = label_both)
        # facet_grid(lfs_1 ~ lfs_2, scales = "free_y", labeller = label_both, switch = "y") +
        # facet_grid(lfs_2 ~ lfs_1, labeller = label_both, switch = "y") +
        facet_wrap(~ lfs_2 + lfs_1, ncol = 3, scales = "free_y", labeller = label_both, dir = "v") +
        theme(strip.placement = "outside")

# plot transition rates for counterfactual scenarios
g <- ggplot() +
        geom_line(aes(x = yearm_1, y = rate, col = scenario)) +
        geom_rect(data=(rec_dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        scale_color_manual(values = c("gray80", "red", "blue")) +
        # facet_grid(lfs_1 ~ lfs_2 + seas, labeller = label_both)
        # facet_grid(lfs_1 ~ lfs_2 + seas, scales = "free_y", labeller = label_both, switch = "y") +
        # facet_grid(lfs_2 ~ lfs_1 + seas, labeller = label_both, switch = "y") +
        facet_wrap(~ seas + lfs_2 + lfs_1, ncol = 6, scales = "free_y", labeller = label_both, dir = "v") +
        theme(strip.placement = "outside")

# counterfactual 1: replace transition rates from U into E
g %+% {df_rates_all_agg_counter_adj %>%
        filter(source == "CPS") %>%
        filter(scenario %in% c("actual_rate", "avg_rate", "counter1"))}

# counterfactual 2: replace transition rates from I into E
g %+% {df_rates_all_agg_counter_adj %>%
        filter(source == "CPS") %>%
        filter(scenario %in% c("actual_rate", "avg_rate", "counter2"))}

# counterfactual 3: replace transition rates from E into I
g %+% {df_rates_all_agg_counter_adj %>%
        filter(source == "CPS") %>%
        filter(scenario %in% c("actual_rate", "avg_rate", "counter3"))}

# counterfactual 4: replace transition rates from E into U
g %+% {df_rates_all_agg_counter_adj %>%
        filter(source == "CPS") %>%
        filter(scenario %in% c("actual_rate", "avg_rate", "counter4"))}

# counterfactual 5: combines counterfactual 1, 2, 3
g %+% {df_rates_all_agg_counter_adj %>%
        filter(source == "CPS") %>%
        filter(scenario %in% c("actual_rate", "avg_rate", "counter5"))}
