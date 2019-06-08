
message("Constructing occupation category specific transition rates for counterfactual scenarios")

# constructed transition rates from construct_flows_2m_occ.R
load(file = str_c(edir_cps, "out_flowsandrates_occ.Rdata"))

# add business cycle id
df_rates_cps_occ <-
    df_flowsandrates_cps_occ %>%
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
    select(period_1, yearm_1, cycle_id, status_1, status_2, seas, rate)

# plot transition rates
g <- ggplot() +
        geom_line(aes(x = yearm_1, y = rate, color = seas)) +
        geom_rect(data = (rec_dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        # facet_grid(status_1 ~ status_2)
        facet_wrap(status_1 ~ status_2, ncol = 1, scales = "free")

g %+% {df_rates_cps_occ %>%
        filter(str_sub(status_1, 1, 1) == "U" & status_2 == "E_RM") %>%
        # filter(status_1 == "U_RC" & status_2 == "E_RM") %>%
        filter(!(status_1 == status_2))}


# define transition rates for counterfactuals
df_rates_cps_occ_counter <-
    # construct average transition rates by recession/expansion period
    full_join(df_rates_cps_occ %>%
                  mutate(cycle_id = if_else(period_1 %in% 199104:200102, "E4", cycle_id)),
              df_rates_cps_occ %>%
                  group_by(cycle_id, status_1, status_2, seas) %>%
                  summarise(avg_rate = mean(rate, na.rm = TRUE)) %>%
                  filter(!is.na(cycle_id)) %>%
                  ungroup(),
              by = c("cycle_id", "status_1", "status_2", "seas")) %>%
    mutate(lfs_1 = str_sub(status_1, 1, 1),
           lfs_2 = str_sub(status_2, 1, 1)) %>%
    # add transition rates for counterfactuals
    group_by(status_1, status_2, seas) %>%
    mutate(# counterfactual 1: replace transition rates from all types of U into E_RM and E_RC with pre-polarization values
           counter1 = case_when(lfs_1 == "U" & status_2 == "E_RM" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "U" & status_2 == "E_RM" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                lfs_1 == "U" & status_2 == "E_RC" & cycle_id %in% c("R3", "R4", "R5")             ~ first(avg_rate[cycle_id == "R2"]),
                                lfs_1 == "U" & status_2 == "E_RC" & cycle_id %in% c("E4", "E5", "E6")             ~ first(avg_rate[cycle_id == "E3"]),
                                TRUE                                                                              ~ avg_rate),
           # counterfactual 2: replace transition rates from I into E_RM and E_RC with pre-polarization values
           counter2 = case_when(lfs_1 == "I" & status_2 == "E_RM" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "I" & status_2 == "E_RM" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                lfs_1 == "I" & status_2 == "E_RC" & cycle_id %in% c("R3", "R4", "R5")             ~ first(avg_rate[cycle_id == "R2"]),
                                lfs_1 == "I" & status_2 == "E_RC" & cycle_id %in% c("E4", "E5", "E6")             ~ first(avg_rate[cycle_id == "E3"]),
                                TRUE                                                                              ~ avg_rate),
           # counterfactual 3: replace transition rates from E_RM and E_RC into I with pre-polarization values
           counter3 = case_when(status_1 == "E_RM" & lfs_2 == "I" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                status_1 == "E_RM" & lfs_2 == "I" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                status_1 == "E_RC" & lfs_2 == "I" & cycle_id %in% c("R3", "R4", "R5")             ~ first(avg_rate[cycle_id == "R2"]),
                                status_1 == "E_RC" & lfs_2 == "I" & cycle_id %in% c("E4", "E5", "E6")             ~ first(avg_rate[cycle_id == "E3"]),
                                TRUE                                                                              ~ avg_rate),
           # counterfactual 4: combines counterfactual 1, 2, 3
           counter4 = case_when(lfs_1 == "U" & status_2 == "E_RM" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "U" & status_2 == "E_RM" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                lfs_1 == "U" & status_2 == "E_RC" & cycle_id %in% c("R3", "R4", "R5")             ~ first(avg_rate[cycle_id == "R2"]),
                                lfs_1 == "U" & status_2 == "E_RC" & cycle_id %in% c("E4", "E5", "E6")             ~ first(avg_rate[cycle_id == "E3"]),
                                lfs_1 == "I" & status_2 == "E_RM" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                lfs_1 == "I" & status_2 == "E_RM" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                lfs_1 == "I" & status_2 == "E_RC" & cycle_id %in% c("R3", "R4", "R5")             ~ first(avg_rate[cycle_id == "R2"]),
                                lfs_1 == "I" & status_2 == "E_RC" & cycle_id %in% c("E4", "E5", "E6")             ~ first(avg_rate[cycle_id == "E3"]),
                                status_1 == "E_RM" & lfs_2 == "I" & cycle_id %in% c("R2", "R3", "R4", "R5")       ~ first(avg_rate[cycle_id == "R1"]),
                                status_1 == "E_RM" & lfs_2 == "I" & cycle_id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg_rate[cycle_id == "E1"]),
                                status_1 == "E_RC" & lfs_2 == "I" & cycle_id %in% c("R3", "R4", "R5")             ~ first(avg_rate[cycle_id == "R2"]),
                                status_1 == "E_RC" & lfs_2 == "I" & cycle_id %in% c("E4", "E5", "E6")             ~ first(avg_rate[cycle_id == "E3"]),
                                TRUE                                                                              ~ avg_rate)) %>%
    ungroup() %>%
    select(-c(lfs_1, lfs_2)) %>%
    rename(actual_rate = rate) %>%
    gather(scenario, rate, actual_rate, avg_rate, starts_with("counter"))

# adjusted counterfactual transition rates so that they add up to 1
df_rates_cps_occ_counter_adj <-
    df_rates_cps_occ_counter  %>%
    group_by(period_1, status_1, seas, scenario) %>%
    mutate(correction = if_else(scenario != "actual_rate" & status_1 == status_2, sum(rate) - 1, 0)) %>%
    ungroup() %>%
    mutate(rate = rate - correction)

df_rates_cps_occ_counter_adj %>%
    filter(correction != 0) %$%
    table(status_1, status_2, scenario, seas)

save(df_rates_cps_occ, df_rates_cps_occ_counter_adj, file = str_c(edir_cps, "out_rates_occ_counter.Rdata"))

# plot transition rates for counterfactual scenarios
g <- ggplot() +
        geom_line(aes(x = yearm_1, y = rate, col = scenario)) +
        geom_rect(data = (rec_dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2, fill = "steelblue") +
        scale_x_yearmon() +
        scale_color_manual(values = c("gray80", "red", "blue")) +
        # facet_grid(status_1 ~ status_2 + seas, labeller = label_both)
        # facet_grid(status_1 ~ status_2 + seas, scales = "free_y", labeller = label_both, switch = "y") +
        facet_grid(status_2 + seas ~ status_1, labeller = label_both, switch = "y") +
        theme(strip.placement = "outside")

g %+% {df_rates_cps_occ_counter_adj %>%
        filter(scenario %in% c("actual_rate", "avg_rate", "counter1")) %>%
        filter(str_sub(status_1, 1, 1) == "U" & str_sub(status_2, 1, 4) == "E_RM")}

g %+% {df_rates_cps_occ_counter_adj %>%
        # filter(seas == "SA") %>%
        filter(scenario %in% c("actual_rate", "avg_rate", "counter1")) %>%
        filter(str_sub(status_1, 1, 1) == "U" & str_sub(status_2, 1, 4) == "E_RC")}

g %+% {df_rates_cps_occ_counter_adj %>%
        # filter(seas == "SA") %>%
        filter(scenario %in% c("actual_rate", "avg_rate", "counter1")) %>%
        filter(str_sub(status_1, 1, 1) == "U" & str_sub(status_2, 1, 3) == "E_R")}

g %+% {df_rates_cps_occ_counter_adj %>%
        # filter(seas == "SA") %>%
        filter(scenario %in% c("actual_rate", "avg_rate")) %>%
        filter(str_sub(status_1, 1, 3) == "E_R" & str_sub(status_2, 1, 1) == "I")}


