
# constructed transition rates from construct_flows_2m_occ.R
load(file = paste0(edir.cps, "flowsandrates_occ.Rdata"))

# add business cycle id
df.rates.cps.occ <-
    df.flowsandrates.cps.occ %>%
    mutate(# define recession/expansion periods
           cycle.id = case_when(period.1 %in% c(197601:197912) ~ "E1",
                                period.1 %in% c(198001:198007) ~ "R1",
                                period.1 %in% c(198008:198106) ~ "E2",
                                period.1 %in% c(198107:198211) ~ "R2",
                                period.1 %in% c(198212:199006) ~ "E3",
                                period.1 %in% c(199007:199103) ~ "R3",
                                #period.1 %in% c(199104:200102) ~ "E4"
                                # see footnote 16 in Cortes, Jaimovich, Nekarda and Siu (2016)
                                # "due to the January 1994 redesign of the CPS and the discontinuities that this induces in certain transition rates,
                                # the"averages for phase E4 used in this section are calculated over the period 1994:1 to 2001:2
                                period.1 %in% c(199401:200102) ~ "E4",
                                period.1 %in% c(200103:200111) ~ "R4",
                                period.1 %in% c(200112:200711) ~ "E5",
                                period.1 %in% c(200712:200906) ~ "R5",
                                period.1 %in% c(200907:201812) ~ "E6")) %>%
    select(period.1, monyear.1, cycle.id, status.1, status.2, seas, rate)

# plot transition rates
df.rates.cps.occ %>%
    # filter(str_sub(status.1, 1, 1) == "U" & status.2 == "E.RM") %>%
    # filter(status.1 == "U.RC" & status.2 == "E.RM") %>%
    # filter(status.1 == "U.RC" & status.2 == "E.RM") %>%
    # filter(!(status.1 == status.2)) %>%
    ggplot() +
        geom_line(aes(x = monyear.1, y = rate, color = seas)) +
        geom_rect(data = (rec.dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        # facet_grid(status.1 ~ status.2)
        facet_wrap(status.1 ~ status.2, ncol = 1, scales = "free_y")

# define transition rates for counterfactuals
df.rates.cps.occ.counter <-
    # construct average transition rates by recession/expansion period
    full_join(df.rates.cps.occ %>%
                  mutate(cycle.id = if_else(period.1 %in% c(199104:200102), "E4", cycle.id)),
              df.rates.cps.occ %>%
                  group_by(cycle.id, status.1, status.2, seas) %>%
                  summarise(avg.rate = mean(rate, na.rm = TRUE)) %>%
                  filter(!is.na(cycle.id)) %>%
                  ungroup(),
              by = c("cycle.id", "status.1", "status.2", "seas")) %>%
    mutate(lfs.1 = str_sub(status.1, 1, 1),
           lfs.2 = str_sub(status.2, 1, 1)) %>%
    # add transition rates for counterfactuals
    group_by(status.1, status.2, seas) %>%
    mutate(# counterfactual 1: replace transition rates from all types of U into E.RM and E.RC with pre-polarization values
           counter1 = case_when(lfs.1 == "U" & status.2 == "E.RM" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "U" & status.2 == "E.RM" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                lfs.1 == "U" & status.2 == "E.RC" & cycle.id %in% c("R3", "R4", "R5")             ~ first(avg.rate[cycle.id == "R2"]),
                                lfs.1 == "U" & status.2 == "E.RC" & cycle.id %in% c("E4", "E5", "E6")             ~ first(avg.rate[cycle.id == "E3"]),
                                TRUE                                                                              ~ avg.rate),
           # counterfactual 2: replace transition rates from I into E.RM and E.RC with pre-polarization values
           counter2 = case_when(lfs.1 == "I" & status.2 == "E.RM" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "I" & status.2 == "E.RM" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                lfs.1 == "I" & status.2 == "E.RC" & cycle.id %in% c("R3", "R4", "R5")             ~ first(avg.rate[cycle.id == "R2"]),
                                lfs.1 == "I" & status.2 == "E.RC" & cycle.id %in% c("E4", "E5", "E6")             ~ first(avg.rate[cycle.id == "E3"]),
                                TRUE                                                                              ~ avg.rate),
           # counterfactual 3: replace transition rates from E.RM and E.RC into I with pre-polarization values
           counter3 = case_when(status.1 == "E.RM" & lfs.2 == "I" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                status.1 == "E.RM" & lfs.2 == "I" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                status.1 == "E.RC" & lfs.2 == "I" & cycle.id %in% c("R3", "R4", "R5")             ~ first(avg.rate[cycle.id == "R2"]),
                                status.1 == "E.RC" & lfs.2 == "I" & cycle.id %in% c("E4", "E5", "E6")             ~ first(avg.rate[cycle.id == "E3"]),
                                TRUE                                                                              ~ avg.rate),
           # counterfactual 4: combines counterfactual 1, 2, 3
           counter4 = case_when(lfs.1 == "U" & status.2 == "E.RM" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "U" & status.2 == "E.RM" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                lfs.1 == "U" & status.2 == "E.RC" & cycle.id %in% c("R3", "R4", "R5")             ~ first(avg.rate[cycle.id == "R2"]),
                                lfs.1 == "U" & status.2 == "E.RC" & cycle.id %in% c("E4", "E5", "E6")             ~ first(avg.rate[cycle.id == "E3"]),
                                lfs.1 == "I" & status.2 == "E.RM" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "I" & status.2 == "E.RM" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                lfs.1 == "I" & status.2 == "E.RC" & cycle.id %in% c("R3", "R4", "R5")             ~ first(avg.rate[cycle.id == "R2"]),
                                lfs.1 == "I" & status.2 == "E.RC" & cycle.id %in% c("E4", "E5", "E6")             ~ first(avg.rate[cycle.id == "E3"]),
                                status.1 == "E.RM" & lfs.2 == "I" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                status.1 == "E.RM" & lfs.2 == "I" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                status.1 == "E.RC" & lfs.2 == "I" & cycle.id %in% c("R3", "R4", "R5")             ~ first(avg.rate[cycle.id == "R2"]),
                                status.1 == "E.RC" & lfs.2 == "I" & cycle.id %in% c("E4", "E5", "E6")             ~ first(avg.rate[cycle.id == "E3"]),
                                TRUE                                                                              ~ avg.rate)) %>%
    ungroup() %>%
    select(-c(lfs.1, lfs.2)) %>%
    rename(actual.rate = rate) %>%
    gather(scenario, rate, actual.rate, avg.rate, starts_with("counter"))

# adjusted counterfactual transition rates so that they add up to 1
df.rates.cps.occ.counter.adj <-
    df.rates.cps.occ.counter  %>%
    group_by(period.1, status.1, seas, scenario) %>%
    mutate(correction = if_else(scenario != "actual.rate" & status.1 == status.2, sum(rate) - 1, 0)) %>%
    ungroup() %>%
    mutate(rate = rate - correction)

df.rates.cps.occ.counter.adj %>%
    filter(correction != 0) %$%
    table(status.1, status.2, scenario, seas)

save(df.rates.cps.occ, df.rates.cps.occ.counter.adj, file = paste0(edir.cps, "rates_occ_counter.Rdata"))

# plot transition rates for counterfactual scenarios
df.rates.cps.occ.counter.adj %>%
    # filter(seas == "SA") %>%
    filter(scenario %in% c("actual.rate", "avg.rate", "counter1")) %>%
    # filter(str_sub(status.1, 1, 1) == "U" & str_sub(status.2, 1, 4) == "E.RM") %>%
    # filter(str_sub(status.1, 1, 1) == "U" & str_sub(status.2, 1, 4) == "E.RC") %>%
    filter(str_sub(status.1, 1, 1) == "U" & str_sub(status.2, 1, 3) == "E.R") %>%
    # filter(str_sub(status.1, 1, 3) == "E.R" & str_sub(status.2, 1, 1) == "I") %>%
    ggplot() +
        geom_line(aes(x = monyear.1, y = rate, col = scenario)) +
        geom_rect(data = (rec.dates %>% filter(Start > "Jan 1976")), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.1, fill = "lightblue") +
        scale_x_yearmon() +
        scale_color_manual(values = c("gray80", "red", "blue")) +
        # facet_grid(status.1 ~ status.2 + seas, labeller = label_both)
        # facet_grid(status.1 ~ status.2 + seas, scales = "free_y", labeller = label_both, switch = "y") +
        facet_grid(status.2 + seas ~ status.1, labeller = label_both, switch = "y") +
        theme(strip.placement = "outside")
