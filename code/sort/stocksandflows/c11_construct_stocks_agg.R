
# compare stocks constructed from CPS micro data with stocks published BLS

# merged CPS data from merge_1m.R
load(file = paste0(edir.cps, "merged_1m_all.Rdata"))
# BLS data from extract_bls_data.R
load(file = paste0(odir.bls, "BLS_lf.Rdata"))

df.stocksandshares.cps.agg <-
    df.merged.1m.all %>%
    group_by(period, lfs) %>%
    summarise(s = sum(weight)) %>%
    group_by(period) %>%
    mutate(shr.lfs2pop = s / sum(s)) %>%
    ungroup() %>%
    gather(measure, y, c(s, shr.lfs2pop)) %>%
    nest(c(period, y)) %>%
    sa.SSM() %>%
    unnest() %>%
    rename(NSA = y,
           SA = y.KS) %>%
    gather(seas, value, c(SA, NSA)) %>%
    spread(measure, value) %>%
    select(period, lfs, seas, s, shr.lfs2pop)

# combined data: BLS and CPS based stocks and population shares
df.stocksandshares.all.agg.whole.sample <-
    bind_rows(CPS = df.stocksandshares.cps.agg,
              BLS = df.stocksandshares.bls %>%
                  select(period, lfs, seas, s, shr.lfs2pop),
              .id = "source") %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon("%Y%m")) %>%
    select(source, period, monyear, lfs, seas, s, shr.lfs2pop)

save(df.stocksandshares.all.agg.whole.sample, file = paste0(edir.cps, "stocksandshares_agg.Rdata"))

# plot time series constructed from CPS micro data and those published by BLS - ggplot2
df.stocksandshares.all.agg.whole.sample %>%
    filter(lfs != "M") %>%
    gather(measure, value, c(s, shr.lfs2pop)) %>%
    ggplot(aes(x = monyear, y = value, color = source)) +
        geom_line() +
        scale_x_yearmon() +
        facet_wrap(lfs ~ measure + seas, scale = "free_y", ncol = 4, labeller = label_both)

df.extra <-
    df.stocksandshares.all.agg.whole.sample %>%
    filter(lfs != "M") %>%
    select(-shr.lfs2pop) %>%
    spread(lfs, s) %>%
    mutate(POP  = E + U + I,
           LF   = E + U,
           LFPR   = 100 * LF / POP,
           UR     = 100 * U / LF) %>%
    gather(measure, value, c("E", "I", "U", "POP", "LF", "LFPR", "UR"))

# plot time series constructed from CPS micro data and those published by BLS - dygraph
df.extra %>%
    filter(seas == "SA") %>%
    filter(measure == "LFPR") %>%
    unite("measure", c("source", "measure", "seas")) %>%
    spread(measure, value) %>%
    tk_zoo(select = -c(period, monyear), date_var = monyear) %>%
    dygraph()

# plot gap between time series constructed from CPS micro data and those published by BLS - ggplot2
df.extra %>%
    spread(source, value) %>%
    mutate(err = CPS - BLS,
           err.percent = 100 * (CPS - BLS) / BLS) %>%
    select(-c(CPS, BLS)) %>%
    gather(type, value, c(err, err.percent)) %>%
    ggplot(aes(x = monyear, y = value, color = seas)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dotted") +
        scale_x_yearmon() +
        facet_wrap(~ measure + type, scale = "free_y", ncol = 2)
