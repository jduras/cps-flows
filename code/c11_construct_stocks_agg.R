
message("Constructing aggregate stocks from CPS micro data")

# merged CPS data from merge_1m.R
load(file = str_c(edir_cps, "merged_1m_all.Rdata"))
# BLS data from extract_bls_data.R
load(file = str_c(odir_bls, "BLS_lf.Rdata"))

df_stocksandshares_cps_agg <-
    df_merged_1m_all %>%
    group_by(period, lfs) %>%
    summarise(s = sum(weight)) %>%
    group_by(period) %>%
    mutate(shr_lfs2pop = s / sum(s)) %>%
    ungroup() %>%
    gather(measure, y, c(s, shr_lfs2pop)) %>%
    nest(c(period, y)) %>%
    mutate(data = map(data, sa_ssm)) %>%
    unnest() %>%
    rename(NSA = y,
           SA = y_ks) %>%
    gather(seas, value, c(SA, NSA)) %>%
    spread(measure, value) %>%
    select(period, lfs, seas, s, shr_lfs2pop)

# combined data: BLS and CPS based stocks and population shares
df_stocksandshares_all_agg_whole_sample <-
    bind_rows(CPS = df_stocksandshares_cps_agg,
              BLS = df_stocksandshares_bls %>%
                  select(period, lfs, seas, s, shr_lfs2pop),
              .id = "source") %>%
    mutate(yearm = period %>% as.character() %>% as.yearmon("%Y%m")) %>%
    select(source, period, yearm, lfs, seas, s, shr_lfs2pop)

save(df_stocksandshares_all_agg_whole_sample, file = str_c(edir_cps, "out_stocksandshares_agg.Rdata"))

# plot time series constructed from CPS micro data and those published by BLS - ggplot2
g <- df_stocksandshares_all_agg_whole_sample %>%
    filter(lfs != "M") %>%
    gather(measure, value, c(s, shr_lfs2pop)) %>%
    ggplot(aes(x = yearm, y = value, color = source)) +
        geom_line() +
        scale_x_yearmon() +
        # facet_grid(lfs ~ measure + seas, scale = "free_y", labeller = label_both)
        facet_wrap(lfs ~ measure + seas, scale = "free_y", ncol = 4, labeller = label_both)
g
ggplotly(g)


df_additional_indicators <-
    df_stocksandshares_all_agg_whole_sample %>%
    filter(lfs != "M") %>%
    select(-shr_lfs2pop) %>%
    spread(lfs, s) %>%
    mutate(POP  = E + U + I,
           LF   = E + U,
           LFPR   = 100 * LF / POP,
           UR     = 100 * U / LF) %>%
    gather(measure, value, c("E", "I", "U", "POP", "LF", "LFPR", "UR"))

# plot gap between time series constructed from CPS micro data and those published by BLS - ggplot2
g <- df_additional_indicators %>%
    spread(source, value) %>%
    mutate(err = CPS - BLS,
           err.percent = 100 * (CPS - BLS) / BLS) %>%
    select(-c(CPS, BLS)) %>%
    gather(type, value, c(err, err.percent)) %>%
    ggplot(aes(x = yearm, y = value, color = seas)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dotted") +
        scale_x_yearmon() +
        facet_wrap(~ measure + type, scale = "free_y", ncol = 2)
g
ggplotly(g)

# plot time series constructed from CPS micro data and those published by BLS - dygraph
df_additional_indicators %>%
    filter(seas == "SA") %>%
    filter(measure == "LFPR") %>%
    unite("measure", c("source", "measure", "seas")) %>%
    spread(measure, value) %>%
    tk_zoo(select = -c(period, yearm), date_var = yearm) %>%
    dygraph()
