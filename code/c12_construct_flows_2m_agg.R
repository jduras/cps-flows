
message("Constructing aggregate flows and transition rates from CPS micro data")

# this file constructs and plots flows and transition rates between different labor force statuses

#### construct flows, and transition rates, by labor force status ####

# merged CPS data from merge_2m.R
load(file = str_c(edir_cps, "merged_2m_all.Rdata"))

# keep only variables needed to construct aggregate flows

df_flowsandrates_cps_agg <-
    df_merged_2m_all %>%
    select(period_1, lfs_1, lfs_2, weight_1) %>%
    group_by(period_1, lfs_1, lfs_2) %>%
    summarise(f = sum(weight_1)) %>%
    group_by(period_1, lfs_1) %>%
    mutate(rate = f / sum(f)) %>%
    ungroup() %>%
    complete(period_1, lfs_1, lfs_2, fill = list(rate = 0)) %>%
    # add NAs for periods where it is not possible to match consequitive months in CPS data
    bind_rows(crossing(tibble(period_1 = c(197712, 198506, 198509, 199312, 199505, 199506, 199507, 199508)),
                       tibble(lfs_1 = c("E", "U", "I"),
                              lfs_2 = lfs_1,
                              f = NA_real_,
                              rate = NA_real_))) %>%
    gather(measure, y, c(f, rate)) %>%
    complete(period_1, lfs_1, lfs_2, measure) %>%
    arrange(period_1, lfs_1, lfs_2, measure) %>%
    rename(period = period_1) %>%
    nest(c(period, y)) %>%
    sa_ssm() %>%
    unnest() %>%
    rename(period_1 = period,
           NSA = y,
           SA = y_ks) %>%
    gather(seas, value, c(NSA, SA)) %>%
    spread(measure, value) %>%
    mutate(yearm_1 = period_1 %>% as.character() %>% as.yearmon("%Y %m")) %>%
    select(period_1, yearm_1, lfs_1, lfs_2, seas, f, rate)

rm(df_merged_2m_all)

# plot transition rates
g <- ggplot() +
        geom_line(aes(x = yearm_1, y = rate, color = seas), size = .5) +
        geom_rect(data = (rec_dates %>% filter(Start > "Jan 1976")), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        labs(x = "", y = "", title = "Transition Rates (rows: period 0, columns: period 1)") +
        facet_grid(lfs_1 ~ lfs_2, scales = "free_y", switch = "y", labeller = label_both) +
        theme(strip.text.y = element_text(angle = 180))

g %+% {df_flowsandrates_cps_agg %>%
        filter(lfs_1 != lfs_2)}

g %+% {df_flowsandrates_cps_agg %>%
        filter(lfs_1 == lfs_2)}

g %+% {df_flowsandrates_cps_agg %>%
        filter(lfs_1 %in% c("E", "U") & lfs_2 %in% c("E", "U"))}

ggplotly(g)


#### merge flows and transition rates data from CPS and BLS ####

# load official stocks and flows data from BLS
load(file = str_c(odir_bls, "BLS_lf.Rdata"))

# join flows data constructed using CPS micro data  with official flows data published by BLS
df_flowsandrates_all_agg <-
    bind_rows(CPS = df_flowsandrates_cps_agg %>%
                      select(period_1, lfs_1, lfs_2, seas, f, rate),
              BLS = df_flowsandrates_bls %>%
                      select(period_1, lfs_1, lfs_2, seas, f, rate),
              .id = "source") %>%
    mutate(yearm_1 = period_1 %>% as.character() %>% as.yearmon("%Y %m")) %>%
    select(period_1, yearm_1, lfs_1, lfs_2, seas, source, f, rate)

save(df_flowsandrates_cps_agg, df_flowsandrates_all_agg, file = str_c(edir_cps, "out_flowsandrates_agg.Rdata"))
# load(file = str_c(edir_cps, "out_flowsandrates_agg.Rdata"))



#### checks: compare flows and transition rates bSed on CPS and BLS data ####

# compare transition rates constructed using 1) CPS micro data and 2) official flows data published by BLS
# on average
# - tansition rates into I (EI, II, UI) are higher in CPS than in BLS
# - transition rates into U (EU, IU, UU) and also IE are lower in CPS than in BLS

# plot transition rates constructed using micro data in CPS and official flows data published by BLS
g <- ggplot() +
        geom_line(aes(x = yearm_1, y = rate, color = source)) +
        scale_x_yearmon() +
        labs(x = "", y = "transition rate", title = "CPS vs BLS based transition rates") +
        # facet_wrap(~ lfs_1 + lfs_2 + seas, ncol = 2, scales = "free_y", labeller = label_both, strip.position = "left")}
        facet_grid(lfs_1  + lfs_2 ~ seas, scales = "free_y", switch = "y", labeller = label_both) +
        theme(# strip.text.y = element_text(angle = 180),
              legend.position = "top",
              legend.justification = "left")

g1 <- g %+% {df_flowsandrates_all_agg %>%
        filter(period_1 >= 199001)}
g1
ggplotly(g1)

g2 <- g %+% {df_flowsandrates_all_agg %>%
        filter(lfs_2 == "U") %>%
        filter(seas == "NSA")}
    g2
ggplotly(g2)

# construct and plot CPS-BLS gap for each transition rate
df_flowsandrates_all_agg %>%
    gather(measure, value, c(f, rate)) %>%
    filter(measure == "rate") %>%
    filter(period_1 >= 199001) %>%
    # filter(seas == "NSA") %>%
    spread(source, value) %>%
    mutate(gap = CPS - BLS) %>%
    filter(!is.na(gap)) %T>%
    {group_by(., lfs_1, lfs_2, seas) %>%
     summarise_at(vars(BLS, CPS, gap), funs(min, median, mean, max)) %>%
     arrange(seas, lfs_1, lfs_2) %>%
     print()} %>%
    {ggplot(., aes(x = yearm_1, y = gap, col = seas)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dotted") +
        scale_x_yearmon() +
        labs(x = "", y = "gap = CPS- BLS", title = "Difference between CPS and BLS based transition rates",
             col = "seasonal adjustment") +
        facet_grid(lfs_1 ~ lfs_2, scales = "free_y", labeller = label_both, switch = "y")}
