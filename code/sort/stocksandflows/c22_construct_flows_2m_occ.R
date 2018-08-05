
# this file constructs and plots flows and transition rates between different labor force statuses and occupation groups

#### construct stocks, flows, and transition rates, by labor force status and occupation group ####

# merged CPS data from merge_2m.R
load(file = paste0(edir.cps, "merged_2m_all.Rdata"))

# keep only variables needed to construct aggregate flows
df.merged.2m.all %<>%
    select(period.1, age.2, lfs.1, lfs.2, occ1cat.1, occ1cat.2, weight.1)

# whole sample
df.flowsandrates.cps.occ.whole.sample <-
    df.merged.2m.all %>%
    group_by(period.1, lfs.1, lfs.2, occ1cat.1, occ1cat.2) %>%
    summarise(f = sum(weight.1)) %>%
    group_by(period.1, lfs.1, occ1cat.1) %>%
    mutate(rate = f / sum(f)) %>%
    ungroup() %>%
    mutate(monyear.1 = period.1 %>% as.character() %>% as.yearmon(format = "%Y%m"),
           status.1 = paste(lfs.1, occ1cat.1, sep = "."),
           status.2 = paste(lfs.2, occ1cat.2, sep = ".")) %>%
    select(period.1, monyear.1, lfs.1, lfs.2, occ1cat.1, occ1cat.2, status.1, status.2, f, rate)

# restricted sample - age 16 to 75, exclude military and farm occupations
df.flowsandrates.cps.occ.tmp <-
    df.merged.2m.all %>%
    filter(age.2 >= 16 & age.2 <= 75) %>%
    filter(!(occ1cat.1 %in% c("FRM", "MIL") | occ1cat.2 %in% c("FRM", "MIL"))) %>%
    filter(!(lfs.1 == "E" & occ1cat.1 == "X")) %>%
    group_by(period.1, lfs.1, lfs.2, occ1cat.1, occ1cat.2) %>%
    summarise(f = sum(weight.1)) %>%
    group_by(period.1, lfs.1, occ1cat.1) %>%
    mutate(rate = f / sum(f)) %>%
    ungroup() %>%
    mutate(monyear.1 = period.1 %>% as.character() %>% as.yearmon(format = "%Y%m"),
           status.1 = paste(lfs.1, occ1cat.1, sep = "."),
           status.2 = paste(lfs.2, occ1cat.2, sep = ".")) %>%
    select(period.1, monyear.1, lfs.1, lfs.2, occ1cat.1, occ1cat.2, status.1, status.2, f, rate)

rm(df.merged.2m.all)

#### plot transition rates ####

# plot the stocks (sum of flows), by labor force status and occupation in the first month
df.flowsandrates.cps.occ.tmp %>%
    group_by(period.1, lfs.1, occ1cat.1) %>%
    mutate(s = sum(f),
           measure = paste("s", lfs.1, occ1cat.1, sep = ".")) %>%
    ungroup() %>%
    ggplot(aes(x = monyear.1, y = s, col = measure)) +
        geom_line() +
        scale_x_yearmon()

# plot transition rates by occupation group - ggplot2
df.flowsandrates.cps.occ.tmp %>%
    # filter(lfs.1 == "E", lfs.2 == "U") %>%
    # filter(occ1cat.1 != occ1cat.2) %>%
    # filter(lfs.1 == "U", lfs.2 == "I") %>%
    # filter(lfs.1 == "U", lfs.2 == "E") %>%
    # filter(occ1cat.2 %in% c("NRC", "NRM")) %>%
    filter(lfs.1 == "U", lfs.2 == "E") %>%
    ggplot(aes(x = monyear.1, y = rate)) +
        geom_line() +
        scale_x_yearmon() +
        facet_grid(occ1cat.1 ~ occ1cat.2, scales = "free")


# flag periods with changes in transition rates larger than 3*st.dev.
# these are due to changes in occupation classification
# introduced in 198301, 199401, 200301,
df.flowsandrates.cps.occ.tmp %>%
    filter(lfs.1 == "E" & occ1cat.1 == "NRC" & lfs.2 == "E") %>%
    group_by(status.1, status.2) %>%
    mutate(change = rate - lag(rate),
           stdev = sd(rate - lag(rate), na.rm = TRUE),
           flag = abs(change - mean(change, na.rm = TRUE)) > 3*stdev) %>%
    ungroup() %T>%
    {{ggplot(data = ., aes(x = monyear.1, y = rate)) +
        geom_line() +
        geom_point(data = . %>% filter(flag == TRUE), aes(x = monyear.1, y = rate), color = "red") +
        scale_x_yearmon() +
        facet_grid(status.1 ~ status.2, labeller = label_both)} %>% print()} %>%
    filter(flag == TRUE) %>%
    select(period.1, status.1, status.2, rate, change, stdev) %>%
    arrange(status.1, status.2, period.1)


# plot transition rates by occupation group - dygraph
df.flowsandrates.cps.occ.tmp %>%
    filter(lfs.1 == "E" & occ1cat.1 == "NRC" & lfs.2 == "E") %>%
    mutate(status.trans = paste(status.1, status.2, sep = ".")) %>%
    select(monyear.1, status.trans, rate) %>%
    spread(status.trans, rate) %>%
    tk_zoo(select = -monyear.1, date_var = monyear.1) %>%
    dygraph() %>%
    nber_shades(rec.dates) %>%
    dyLegend(width = 350)

#### seasonally adjust flows and transition rates ####
df.flowsandrates.cps.occ <-
    df.flowsandrates.cps.occ.tmp %>%
    select(period.1, status.1, status.2, f, rate) %>%
    complete(period.1, status.1, status.2, fill = list(rate = 0)) %>%
    # add NAs for transition rates for periods where it is not possible to match consequitive months in CPS data
    bind_rows(crossing(tibble(period.1 = c(197712, 198506, 198509, 199312, 199505, 199506, 199507, 199508)),
                       tibble(status.1 = c("E.NRC", "E.NRM", "E.RC", "E.RM",
                                           "U.NRC", "U.NRM", "U.RC", "U.RM", "U.X",
                                           "I.X"),
                              status.2 = status.1,
                              f = NA_real_,
                              rate = NA_real_))) %>%
    gather(measure, y, c(f, rate)) %>%
    complete(period.1, status.1, status.2, measure) %>%
    arrange(period.1, status.1, status.2, measure) %>%
    rename(period = period.1) %>%
    nest(c(period, y)) %>%
    sa.SSM() %>%
    unnest() %>%
    rename(period.1 = period,
           NSA = y,
           SA = y.KS) %>%
    gather(seas, value, c(NSA, SA)) %>%
    spread(measure, value) %>%
    mutate(monyear.1 = period.1 %>% as.character() %>% as.yearmon("%Y %m")) %>%
    separate(status.1, into = c("lfs.1", "occ1cat.1"), remove = FALSE) %>%
    separate(status.2, into = c("lfs.2", "occ1cat.2"), remove = FALSE) %>%
    select(period.1, monyear.1, lfs.1, lfs.2, occ1cat.1, occ1cat.2, status.1, status.2, seas, f, rate)

save(df.flowsandrates.cps.occ, df.flowsandrates.cps.occ.tmp, df.flowsandrates.cps.occ.whole.sample, file = paste0(edir.cps, "flowsandrates_occ.Rdata"))
# load(file = paste0(edir.cps, "flowsandrates_occ.Rdata"))

# plot transition rates: in and out of E, for individuals who stay in same occupation group (occ1cat.1 == occ1cat.2)
df.flowsandrates.cps.occ %>%
    # filter(seas == chosen.seas) %>%
    filter((lfs.1 == "E" & occ1cat.1 == occ1cat.2) |
           (lfs.1 == "I" & lfs.2 == "E") |
           (lfs.1 == "U" & lfs.2 == "E" & occ1cat.1 == occ1cat.2)) %>%
    mutate(lfs = paste(lfs.1, lfs.2, sep = ".")) %>%
    {ggplot(data = .) +
        geom_line(aes(x = monyear.1, y = rate, color = seas)) +
        geom_rect(data = (rec.dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        scale_color_manual(values = c("blue", "black")) +
        facet_grid(lfs ~ occ1cat.2, scales = "free_y")}

# plot SA transition rates: into I
df.flowsandrates.cps.occ %>%
    filter(lfs.1 != "I" & lfs.2 == "I") %>%
    {ggplot(data = .) +
        geom_line(aes(x = monyear.1, y = rate, color = seas)) +
        geom_rect(data = (rec.dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        scale_color_manual(values = c("blue", "black")) +
        facet_grid(status.1 ~ status.2, scales = "free_y")}
