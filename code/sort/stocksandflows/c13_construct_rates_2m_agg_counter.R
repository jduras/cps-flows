
# flows from CPS microdata vs BLS official flows data as source for transition rates and stocks:
# with CPS microdata based transition rates on average
# - EI, II, UI transition rates are higher than transition rates constructed using BLS flows and stocks data
# - EU, IU, UU and IE transition rates are lower than transition rates constructed using BLS flows and stocks data
# thus stocks simulated using law of motion and CPS microdata based transition rates will yield
# - I stock higher than in BLS data
# - E and U stocks lower than in BLS data

# constructed transition rates from construct_flows_2m_agg.R
load(file = paste0(edir.cps, "flowsandrates_agg.Rdata"))

# add business cycle id
df.rates.all.agg <-
    df.flowsandrates.all.agg %>%
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
    mutate(monyear.1 = period.1 %>% as.character() %>% as.yearmon("%Y%m")) %>%
    select(source, period.1, monyear.1, cycle.id, lfs.1, lfs.2, seas, rate)

# plot transition rates
df.rates.all.agg %>%
    ggplot() +
        geom_line(aes(x = monyear.1, y = rate, color = seas, linetype = source)) +
        geom_rect(data = (rec.dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        # facet_grid(lfs.1 ~ lfs.2)
        facet_wrap(lfs.1 ~ lfs.2, ncol = 1, scales = "free_y")

# define transition rates for counterfactuals
df.rates.all.agg.counter <-
    # construct average transition rates by recession/expansion period
    full_join(df.rates.all.agg%>%
                  mutate(cycle.id = if_else(period.1 %in% c(199104:200102), "E4", cycle.id)),
              df.rates.all.agg %>%
                  group_by(source, cycle.id, lfs.1, lfs.2, seas) %>%
                  summarise(avg.rate = mean(rate, na.rm = TRUE)) %>%
                  filter(!is.na(cycle.id)) %>%
                  ungroup(),
              by = c("source", "cycle.id", "lfs.1", "lfs.2", "seas")) %>%
    # add transition rates for counterfactuals for CPS data (BLS data starts in 1990 and does not iunclude R1 and E1)
    group_by(source, lfs.1, lfs.2, seas) %>%
    mutate(# counterfactual 1: replace transition rates from U into E
           counter1 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs.1 == "U" & lfs.2 == "E" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "U" & lfs.2 == "E" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                TRUE                                                                        ~ avg.rate),
           # counterfactual 2: replace transition rates from I into E
           counter2 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs.1 == "I" & lfs.2 == "E" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "I" & lfs.2 == "E" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                TRUE                                                                        ~ avg.rate),
           # counterfactual 3: replace transition rates from E into I
           counter3 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs.1 == "E" & lfs.2 == "I" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "E" & lfs.2 == "I" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                TRUE                                                                        ~ avg.rate),
           # counterfactual 4: replace transition rates from E into U
           counter4 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs.1 == "E" & lfs.2 == "U" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "E" & lfs.2 == "U" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                TRUE                                                                        ~ avg.rate),
           # counterfactual 5: combines counterfactual 1, 2, 3
           counter5 = case_when(source == "BLS"                                                             ~ NA_real_,
                                lfs.1 == "U" & lfs.2 == "E" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "U" & lfs.2 == "E" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                lfs.1 == "I" & lfs.2 == "E" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "I" & lfs.2 == "E" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                lfs.1 == "E" & lfs.2 == "I" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "E" & lfs.2 == "I" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                lfs.1 == "E" & lfs.2 == "U" & cycle.id %in% c("R2", "R3", "R4", "R5")       ~ first(avg.rate[cycle.id == "R1"]),
                                lfs.1 == "E" & lfs.2 == "U" & cycle.id %in% c("E2", "E3", "E4", "E5", "E6") ~ first(avg.rate[cycle.id == "E1"]),
                                TRUE                                                                        ~ avg.rate)) %>%
    rename(actual.rate = rate) %>%
    gather(scenario, rate, actual.rate, avg.rate, starts_with("counter"))


# adjusted counterfactual transition rates so that they add up to 1
df.rates.all.agg.counter.adj <-
    df.rates.all.agg.counter %>%
    group_by(source, period.1, lfs.1, seas, scenario) %>%
    mutate(correction = if_else(scenario != "actual.rate" & lfs.1 == lfs.2, sum(rate) - 1, 0)) %>%
    ungroup() %>%
    mutate(rate = rate - correction)

df.rates.all.agg.counter.adj %>%
    filter(correction != 0) %$%
    table(lfs.1, lfs.2, scenario, seas, source)

save(df.rates.all.agg, df.rates.all.agg.counter.adj, file = paste0(edir.cps, "rates_agg_counter.Rdata"))


# datasource "CPS" or "BLS": choose CPS microdata pr BLS official data as source for transition rates and stocks
# with CPS microdata based transition rates on average
# - EI, II, UI transition rates are higher than transition rates constructed using BLS flows and stocks data
# - EU, IU, UU and IE transition rates are lower than transition rates constructed using BLS flows and stocks data
# thus stocks simulated using law of motion and CPS microdata based transition rates will yield
# - I stock higher than in BLS data
# - E and U stocks lower than in BLS data

# plot transition rates CPS vs BLS
df.rates.all.agg.counter.adj %>%
    # filter(seas == "NSA") %>%
    filter(seas == "SA") %>%
    filter(monyear.1 > "Jan 1990") %>%
    filter(scenario %in% c("actual.rate", "avg.rate")) %>%
    ggplot() +
    geom_line(aes(x = monyear.1, y = rate, col = source)) +
    geom_rect(data=(rec.dates %>% filter(Start > "Jan 1990")),
              aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
    scale_x_yearmon() +
    facet_wrap(~ scenario + lfs.2 + lfs.1, ncol = 6, scales = "free_y", labeller = label_both, dir = "v") +
    theme(strip.placement = "outside")

# plot transition rates CPS vs BLS
df.rates.all.agg.counter.adj %>%
    # filter(source == "BLS") %>%
    filter(source == "CPS") %>%
    filter(monyear.1 > "Jan 1990") %>%
    filter(scenario %in% c("actual.rate", "avg.rate")) %>%
    ggplot() +
    geom_line(aes(x = monyear.1, y = rate, col = seas)) +
    geom_rect(data=(rec.dates %>% filter(Start > "Jan 1990")),
              aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
    scale_x_yearmon() +
    facet_wrap(~ scenario + lfs.2 + lfs.1, ncol = 6, scales = "free_y", labeller = label_both, dir = "v") +
    theme(strip.placement = "outside")

# plot transition rates SA vs NSA
df.rates.all.agg.counter.adj %>%
    filter(source == "CPS") %>%
    # filter(scenario == "actual.rate") %>%
    # filter(scenario == "avg.rate") %>%
    filter(scenario == "counter5") %>%
    ggplot() +
        geom_line(aes(x = monyear.1, y = rate, col = seas)) +
        geom_rect(data=(rec.dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        # facet_grid(lfs.1 ~ lfs.2, labeller = label_both)
        # facet_grid(lfs.1 ~ lfs.2, scales = "free_y", labeller = label_both, switch = "y") +
        # facet_grid(lfs.2 ~ lfs.1, labeller = label_both, switch = "y") +
        facet_wrap(~ lfs.2 + lfs.1, ncol = 3, scales = "free_y", labeller = label_both, dir = "v") +
        theme(strip.placement = "outside")

# plot transition rates for counterfactual scenarios
df.rates.all.agg.counter.adj %>%
    filter(source == "CPS") %>%
    filter(scenario %in% c("actual.rate", "avg.rate", "counter1")) %>%
    ggplot() +
    geom_line(aes(x = monyear.1, y = rate, col = scenario)) +
    geom_rect(data=(rec.dates %>% filter(Start > "Jan 1976")),
              aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
    scale_x_yearmon() +
    scale_color_manual(values = c("gray80", "red", "blue")) +
    # facet_grid(lfs.1 ~ lfs.2 + seas, labeller = label_both)
    # facet_grid(lfs.1 ~ lfs.2 + seas, scales = "free_y", labeller = label_both, switch = "y") +
    # facet_grid(lfs.2 ~ lfs.1 + seas, labeller = label_both, switch = "y") +
    facet_wrap(~ seas + lfs.2 + lfs.1, ncol = 6, scales = "free_y", labeller = label_both, dir = "v") +
    theme(strip.placement = "outside")
