
# this file constructs and plots flows and transition rates between different labor force statuses

#### construct stocks, flows, and transition rates, by labor force status ####

# merged CPS data from merge_2m.R
load(file = paste0(edir.cps, "merged_2m_all.Rdata"))

# keep only variables needed to construct aggregate flows
df.merged.2m.all %<>%
    select(period.1, lfs.1, lfs.2, weight.1)

df.flowsandrates.cps.agg <-
    df.merged.2m.all %>%
    group_by(period.1, lfs.1, lfs.2) %>%
    summarise(f = sum(weight.1)) %>%
    group_by(period.1, lfs.1) %>%
    mutate(rate = f / sum(f)) %>%
    ungroup() %>%
    complete(period.1, lfs.1, lfs.2, fill = list(rate = 0)) %>%
    # add NAs for periods where it is not possible to match consequitive months in CPS data
    bind_rows(crossing(tibble(period.1 = c(197712, 198506, 198509, 199312, 199505, 199506, 199507, 199508)),
                       tibble(lfs.1 = c("E", "U", "I"),
                              lfs.2 = lfs.1,
                              f = NA_real_,
                              rate = NA_real_))) %>%
    gather(measure, y, c(f, rate)) %>%
    complete(period.1, lfs.1, lfs.2, measure) %>%
    arrange(period.1, lfs.1, lfs.2, measure) %>%
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
    select(period.1, monyear.1, lfs.1, lfs.2, seas, f, rate)

rm(df.merged.2m.all)

# plot transition rates
g <- df.flowsandrates.cps.agg %>%
    filter(lfs.1 != lfs.2) %>%
    # filter(lfs.1 %in% c("E", "U") & lfs.2 %in% c("E", "U")) %>%
    ggplot() +
        geom_line(aes(x = monyear.1, y = rate, color = seas), size = .5) +
        geom_rect(data = (rec.dates %>% filter(Start > "Jan 1976")), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        labs(x = "", y = "", title = "Transition Rates (rows: period 0, columns: period 1)") +
        facet_grid(lfs.1 ~ lfs.2, scales = "free_y", switch = "y", labeller = label_both) +
        theme(strip.text.y = element_text(angle = 180))
g

ggplotly(g)



#### merge flows and transition rates data from CPS and BLS ####

# load official stocks and flows data from BLS
load(file = paste0(odir.bls, "BLS_lf.Rdata"))

# join flows data constructed using CPS micro data  with official flows data published by BLS
df.flowsandrates.all.agg <-
    bind_rows(CPS = df.flowsandrates.cps.agg %>%
                      select(period.1, lfs.1, lfs.2, seas, f, rate),
              BLS = df.flowsandrates.bls %>%
                      select(period.1, lfs.1, lfs.2, seas, f, rate),
              .id = "source") %>%
    mutate(monyear.1 = period.1 %>% as.character() %>% as.yearmon("%Y %m")) %>%
    select(period.1, monyear.1, lfs.1, lfs.2, seas, source, f, rate)

save(df.flowsandrates.cps.agg, df.flowsandrates.all.agg, file = paste0(edir.cps, "flowsandrates_agg.Rdata"))
# load(file = paste0(edir.cps, "flowsandrates_agg.Rdata"))



#### checks: compare flows and transition rates bSed on CPS and BLS data ####

# compare transition rates constructed using 1) CPS micro data and 2) official flows data published by BLS
# on average
# - tansition rates into I (EI, II, UI) are higher in CPS than in BLS
# - transition rates into U (EU, IU, UU) and also IE are lower in CPS than in BLS

# plot transition rates constructed using micro data in CPS and official flows data published by BLS
df.flowsandrates.all.agg %>%
    filter(period.1 >= 199001) %>%
    filter(seas == "NSA") %>%
    {ggplot(., aes(x = monyear.1, y = rate, color = source)) +
        geom_line() +
        scale_x_yearmon() +
        labs(x = "", y = "transition rate", title = "CPS vs BLS based transition rates, NSA") +
        facet_wrap(~ lfs.1 + lfs.2, ncol = 1, scales = "free_y", labeller = label_both, strip.position = "left")}
        # facet_grid(lfs.1 ~ lfs.2, scales = "free_y", labeller = label_both)}

# construct and plot CPS-BLS gap for each transition rate
df.flowsandrates.all.agg %>%
    gather(measure, value, c(f, rate)) %>%
    filter(measure == "rate") %>%
    filter(period.1 >= 199001) %>%
    filter(seas == "NSA") %>%
    spread(source, value) %>%
    mutate(gap = CPS - BLS) %>%
    filter(!is.na(gap)) %T>%
    {group_by(., lfs.1, lfs.2, seas) %>%
     summarise_at(vars(BLS, CPS, gap), funs(min, median, mean, max)) %>%
     arrange(seas, lfs.1, lfs.2) %>%
     print()} %>%
    {ggplot(., aes(x = monyear.1, y = gap, col = seas)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dotted") +
        scale_x_yearmon() +
        labs(x = "", y = "gap = CPS- BLS", title = "Difference between CPS and BLS based transition rates",
             col = "seasonal adjustment") +
        facet_grid(lfs.1 ~ lfs.2, scales = "free_y", labeller = label_both, switch = "y")}

# plot flows data constructed using micro data in CPS and official flows data published by BLS
df.flowsandrates.all.agg %>%
    gather(measure, value, c(f, rate)) %>%
    filter(measure == "rate") %>%
    filter(lfs.2 == "U") %>%
    filter(seas == "NSA") %>%
    unite(measure, lfs.1, lfs.2, seas, source, sep = ".") %>%
    spread(measure, value) %>%
    tk_zoo(select = - c(period.1, monyear.1), date_var = monyear.1) %>%
    dygraph()
