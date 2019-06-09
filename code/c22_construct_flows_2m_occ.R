
message("Constructing occupation category specific flows and transition rates from CPS micro data")

# this file constructs and plots flows and transition rates between different labor force statuses and occupation groups

#### construct stocks, flows, and transition rates, by labor force status and occupation group ####

# merged CPS data from merge_2m.R
if (!exists("df_merged_2m_all")) load(file = str_c(edir_cps, "merged_2m_all.Rdata"))

# keep only variables needed to construct aggregate flows
df_merged_2m_all %<>%
    select(period_1, age_2, lfs_1, lfs_2, occ1cat_1, occ1cat_2, weight_1)

# whole sample
df_flowsandrates_cps_occ_whole_sample <-
    df_merged_2m_all %>%
    group_by(period_1, lfs_1, lfs_2, occ1cat_1, occ1cat_2) %>%
    summarise(f = sum(weight_1)) %>%
    group_by(period_1, lfs_1, occ1cat_1) %>%
    mutate(rate = f / sum(f)) %>%
    ungroup() %>%
    mutate(yearm_1 = period_1 %>% as.character() %>% as.yearmon(format = "%Y%m"),
           status_1 = str_c(lfs_1, occ1cat_1, sep = "_"),
           status_2 = str_c(lfs_2, occ1cat_2, sep = "_")) %>%
    select(period_1, yearm_1, lfs_1, lfs_2, occ1cat_1, occ1cat_2, status_1, status_2, f, rate)

# restricted sample - age 16 to 75, exclude military and farm occupations
df_flowsandrates_cps_occ_tmp <-
    df_merged_2m_all %>%
    filter(age_2 >= 16 & age_2 <= 75) %>%
    filter(!(occ1cat_1 %in% c("FRM", "MIL") | occ1cat_2 %in% c("FRM", "MIL"))) %>%
    filter(!(lfs_1 == "E" & occ1cat_1 == "X")) %>%
    group_by(period_1, lfs_1, lfs_2, occ1cat_1, occ1cat_2) %>%
    summarise(f = sum(weight_1)) %>%
    group_by(period_1, lfs_1, occ1cat_1) %>%
    mutate(rate = f / sum(f)) %>%
    ungroup() %>%
    mutate(yearm_1 = period_1 %>% as.character() %>% as.yearmon(format = "%Y%m"),
           status_1 = str_c(lfs_1, occ1cat_1, sep = "_"),
           status_2 = str_c(lfs_2, occ1cat_2, sep = "_")) %>%
    select(period_1, yearm_1, lfs_1, lfs_2, occ1cat_1, occ1cat_2, status_1, status_2, f, rate)

rm(df_merged_2m_all)

#### plot transition rates ####

# plot the stocks (sum of flows), by labor force status and occupation in the first month
df_flowsandrates_cps_occ_tmp %>%
    group_by(period_1, lfs_1, occ1cat_1) %>%
    mutate(s = sum(f),
           measure = str_c("s", lfs_1, occ1cat_1, sep = "_")) %>%
    ungroup() %>%
    ggplot(aes(x = yearm_1, y = s, col = measure)) +
        geom_line() +
        scale_x_yearmon()

# plot transition rates by occupation group - ggplot2
g <- ggplot() +
        geom_line(aes(x = yearm_1, y = rate)) +
        geom_rect(data = (rec_dates %>% filter(Start > "Jan 1976")), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        labs(x = "", y = "") +
        facet_grid(occ1cat_1 ~ occ1cat_2, scales = "free", labeller = label_both)

g %+% {df_flowsandrates_cps_occ_tmp %>%
            filter(lfs_1 == "E", lfs_2 == "U")} +
        labs(title = "E to U transition")
g %+% {df_flowsandrates_cps_occ_tmp %>%
            filter(lfs_1 == "E", lfs_2 == "U") %>%
            filter(occ1cat_1 != occ1cat_2)} +
        labs(title = "E to U transition")
g %+% {df_flowsandrates_cps_occ_tmp %>%
            filter(lfs_1 == "U", lfs_2 == "E")} +
        labs(title = "U to E transition")
g %+% {df_flowsandrates_cps_occ_tmp %>%
            filter(lfs_1 == "U", lfs_2 == "E") %>%
            filter(occ1cat_2 %in% c("NRC", "NRM"))} +
        labs(title = "U to E transition")
g %+% {df_flowsandrates_cps_occ_tmp %>%
            filter(lfs_1 == "U", lfs_2 == "I")} +
        labs(title = "U to I transition")

# flag periods with changes in transition rates larger than 3*st.dev.
# these are due to changes in occupation classification
# introduced in 198301, 199401, 200301,
df_flowsandrates_cps_occ_tmp %>%
    filter(lfs_1 == "E" & occ1cat_1 == "NRC" & lfs_2 == "E") %>%
    # filter(lfs_1 == "E" & lfs_2 == "E") %>%
    group_by(status_1, status_2) %>%
    mutate(change = rate - lag(rate),
           stdev = sd(rate - lag(rate), na.rm = TRUE),
           flag = abs(change - mean(change, na.rm = TRUE)) > 3*stdev) %>%
    ungroup() %T>%
    {{ggplot(data = ., aes(x = yearm_1, y = rate)) +
        geom_line() +
        geom_point(data = . %>% filter(flag == TRUE), aes(x = yearm_1, y = rate), color = "red") +
        scale_x_yearmon() +
        facet_grid(status_1 ~ status_2, labeller = label_both)} %>%
     ggplotly() %>%
     print()} %>%
    filter(flag == TRUE) %>%
    select(period_1, status_1, status_2, rate, change, stdev) %>%
    arrange(status_1, status_2, period_1)

# plot transition rates by occupation group - dygraph
df_flowsandrates_cps_occ_tmp %>%
    filter(lfs_1 == "E" & occ1cat_1 == "NRC" & lfs_2 == "E") %>%
    mutate(status_trans = str_c(status_1, status_2, sep = "_")) %>%
    select(yearm_1, status_trans, rate) %>%
    spread(status_trans, rate) %>%
    tk_zoo(select = -yearm_1, date_var = yearm_1) %>%
    dygraph() %>%
    nber_shades(rec_dates) %>%
    dyLegend(width = 350)

#### seasonally adjust flows and transition rates ####
df_flowsandrates_cps_occ <-
    df_flowsandrates_cps_occ_tmp %>%
    select(period_1, status_1, status_2, f, rate) %>%
    complete(period_1, status_1, status_2, fill = list(rate = 0)) %>%
    # add NAs for transition rates for periods where it is not possible to match consequitive months in CPS data
    bind_rows(crossing(tibble(period_1 = c(197712, 198506, 198509, 199312, 199505, 199506, 199507, 199508)),
                       tibble(status_1 = c("E_NRC", "E_NRM", "E_RC", "E_RM",
                                           "U_NRC", "U_NRM", "U_RC", "U_RM", "U_X",
                                           "I_X"),
                              status_2 = status_1,
                              f = NA_real_,
                              rate = NA_real_))) %>%
    gather(measure, y, c(f, rate)) %>%
    complete(period_1, status_1, status_2, measure) %>%
    arrange(period_1, status_1, status_2, measure) %>%
    rename(period = period_1) %>%
    nest(c(period, y)) %>%
    mutate(data = data %>%
               future_map(sa_ssm) %>%
               future_map(smooth_ssm)) %>%
    unnest() %>%
    rename(period_1 = period,
           NSA = y,
           SA = y_ks,
           KS = y_smooth) %>%
    gather(seas, value, c(NSA, SA, KS)) %>%
    spread(measure, value) %>%
    mutate(yearm_1 = period_1 %>% as.character() %>% as.yearmon("%Y %m")) %>%
    separate(status_1, into = c("lfs_1", "occ1cat_1"), remove = FALSE) %>%
    separate(status_2, into = c("lfs_2", "occ1cat_2"), remove = FALSE) %>%
    select(period_1, yearm_1, lfs_1, lfs_2, occ1cat_1, occ1cat_2, status_1, status_2, seas, f, rate)

save(df_flowsandrates_cps_occ, df_flowsandrates_cps_occ_tmp, df_flowsandrates_cps_occ_whole_sample,
     file = str_c(edir_cps, "out_flowsandrates_occ.Rdata"))
# load(file = str_c(edir_cps, "out_flowsandrates_occ.Rdata"))

# plot transition rates: in and out of E, for individuals who stay in same occupation group (occ1cat_1 == occ1cat_2)
g <- ggplot() +
        geom_line(aes(x = yearm_1, y = rate, color = seas)) +
        geom_rect(data = (rec_dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        scale_color_manual(values = c("gray40", "blue")) +
        labs(x = "", y = "") +
        facet_grid(lfs_trans ~ occ1cat_2, scales = "free_y")

g %+% {df_flowsandrates_cps_occ %>%
            filter(seas %in% c("NSA", "SA")) %>%
            filter((lfs_1 == "E" & occ1cat_1 == occ1cat_2) |
                       (lfs_1 == "I" & lfs_2 == "E") |
                       (lfs_1 == "U" & lfs_2 == "E" & occ1cat_1 == occ1cat_2)) %>%
            # filter(seas %in% c("NSA", "KS")) %>%
            # filter(seas == "KS") %>%
            mutate(lfs_trans = str_c(lfs_1, lfs_2, sep = "_"))} +
        labs(title = "Transition rates in and out of E for individuals who stay in same occupation group")

g %+% {df_flowsandrates_cps_occ %>%
            filter(seas %in% c("NSA", "SA")) %>%
            filter(lfs_1 == "U" & lfs_2 == "E") %>%
            mutate(lfs_trans = str_c(lfs_1, lfs_2, sep = "_"))} +
        labs(title = "Transition rates into E for individuals who stay in same occupation group") +
        facet_grid(lfs_trans + occ1cat_1 ~ occ1cat_2, scales = "free_y")

# plot transition rates: into I
df_flowsandrates_cps_occ %>%
    filter(seas %in% c("NSA", "SA")) %>%
    filter(lfs_1 != "I" & lfs_2 == "I") %>%
    ggplot() +
        geom_line(aes(x = yearm_1, y = rate, color = seas)) +
        geom_rect(data = (rec_dates %>% filter(Start > "Jan 1976")),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        scale_color_manual(values = c("blue", "black")) +
        facet_grid(status_1 ~ status_2, scales = "free_y")
