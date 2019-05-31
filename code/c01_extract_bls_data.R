
# extract BLS data and keep the following time series
# (all series are number in thousands, 16 years and over)


# Not Seasonally Adjusted

#  LNU00000000  (Unadj) Population Level
#  LNU01000000  (Unadj) Civilian Labor Force Level
#  LNU02000000  (Unadj) Employment Level
#  LNU03000000  (Unadj) Unemployment Level
#  LNU05000000  (Unadj) Not in Labor Force                                         = LNU00000000-LNU01000000

#  LNU01300000  (Unadj) Labor Force Participation Rate                             = LNU01000000/LNU00000000*100
#  LNU02300000  (Unadj) Employment-Population Ratio                                = LNU02000000/LNU00000000*100
#  LNU04000000  (Unadj) Unemployment Rate                                          = LNU03000000/LNU01000000*100

#  LNU07000000  (Unadj) Labor Force Flows Employed to Employed
#  LNU07100000  (Unadj) Labor Force Flows Unemployed to Employed
#  LNU07200000  (Unadj) Labor Force Flows Not in Labor Force to Employed
#  LNU07400000  (Unadj) Labor Force Flows Employed to Unemployed
#  LNU07500000  (Unadj) Labor Force Flows Unemployed to Unemployed
#  LNU07600000  (Unadj) Labor Force Flows Not in Labor Force to Unemployed
#  LNU07800000  (Unadj) Labor Force Flows Employed to Not in Labor Force
#  LNU07900000  (Unadj) Labor Force Flows Unemployed to Not in Labor Force
#  LNU08000000  (Unadj) Labor Force Flows Not in Labor Force to Not in Labor Force


# Seasonally Adjusted

#  LNS10000000  (Seas) Population Level
#  LNS11000000  (Seas) Civilian Labor Force Level
#  LNS12000000  (Seas) Employment Level
#  LNS13000000  (Seas) Unemployment Level
#  LNS15000000  (Seas) Not in Labor Force

#  LNS11300000  (Seas) Labor Force Participation Rate                             = LNU01000000/LNU00000000*100
#  LNS12300000  (Seas) Employment-Population Ratio                                = LNU02000000/LNU00000000*100
#  LNS14000000  (Seas) Unemployment Rate                                          = LNU03000000/LNU01000000*100

#  LNS17000000  (Seas) Labor Force Flows Employed to Employed
#  LNS17100000  (Seas) Labor Force Flows Unemployed to Employed
#  LNS17200000  (Seas) Labor Force Flows Not in Labor Force to Employed
#  LNS17400000  (Seas) Labor Force Flows Employed to Unemployed
#  LNS17500000  (Seas) Labor Force Flows Unemployed to Unemployed
#  LNS17600000  (Seas) Labor Force Flows Not in Labor Force to Unemployed
#  LNS17800000  (Seas) Labor Force Flows Employed to Not in Labor Force
#  LNS17900000  (Seas) Labor Force Flows Unemployed to Not in Labor Force
#  LNS18000000  (Seas) Labor Force Flows Not in Labor Force to Not in Labor Force

datafile <- "ln.data.1.AllData"

choice <- menu(choices = c("Use existing BLS dataset", "Download new dataset from BLS server"),
               title = "Download data from BLS?")

if (choice == 2) {
    download.file(url = str_c("https://download.bls.gov/pub/time.series/ln/", datafile),
                  dest = str_c(ddir_bls, datafile))
}

print("Extracting BLS data")

df_blsdata_raw <- read_table2(file = str_c(ddir_bls, datafile), col_types = c("cicdi"))

# blsdata_raw <-
#     datafile %T>%
#     {download.file(url = str_c("https://download.bls.gov/pub/time.series/ln/", .),
#                    dest = str_c(ddir_bls, .))} %>%
#     {read_table2(file = str_c(ddir_bls, .), col_types = c("cicdi"))}

rm(datafile, choice)

df_blsdata <-
    df_blsdata_raw %>%
    filter(!is.na(series_id)) %>%
    select(-footnote_codes) %>%
    filter(series_id %in% c(# NSA stocks: POP, LF, E, U, I
                            "LNU00000000", "LNU01000000", "LNU02000000", "LNU03000000", "LNU05000000",
                            # NSA rates: LFPR, EPR, UR
                            "LNU01300000", "LNU02300000", "LNU04000000",
                            # NSA flows: EE, UE, IE
                            "LNU07000000", "LNU07100000", "LNU07200000",
                            # NSA flows: EU, UU, IU
                            "LNU07400000", "LNU07500000", "LNU07600000",
                            # NSA flows: EI, UI, II
                            "LNU07800000", "LNU07900000", "LNU08000000",
                            # SA stocks: POP, LF, E, U, I
                            "LNS10000000", "LNS11000000", "LNS12000000", "LNS13000000", "LNS15000000",
                            # SA rates: LFPR, EPR, UR
                            "LNS11300000", "LNS12300000", "LNS14000000",
                            # SA flows: EE, UE, IE
                            "LNS17000000", "LNS17100000", "LNS17200000",
                            # SA flows: EU, UU, IU
                            "LNS17400000", "LNS17500000", "LNS17600000",
                            # SA flows: EI, UI, II
                            "LNS17800000", "LNS17900000", "LNS18000000"
                            )) %>%
    # keep only monthly data, drop quarterly and annual data
    filter(!(str_detect(period, pattern = "Q") | str_detect(period, "M13"))) %>%
    mutate(period = str_c(year, str_sub(period, 2, 3)) %>% as.numeric()) %>%
    filter(period >= 197501) %>%
    select(-year)

rm(df_blsdata_raw)

# stocks
df_stocks_bls <-
    df_blsdata %>%
    filter(series_id %in% c("LNU02000000", "LNU03000000", "LNU05000000",
                            "LNS12000000", "LNS13000000", "LNS15000000")) %>%
    # spread(series_id, value) %>%
    # filter(complete.cases(.)) %>%
    # gather(series_id, value, -period) %>%
    rename(s = value) %>%
    mutate(seas = case_when(str_sub(series_id, 3, 3) == "S" ~ "SA",
                            str_sub(series_id, 3, 3) == "U" ~ "NSA"),
           lfs = case_when(series_id %in% c("LNU02000000", "LNS12000000") ~ "E",
                           series_id %in% c("LNU03000000", "LNS13000000") ~ "U",
                           series_id %in% c("LNU05000000", "LNS15000000") ~ "I",
                           TRUE                                           ~ NA_character_)) %>%
    select(series_id, period, lfs, seas, s)

# flows
df_flows_bls <-
    df_blsdata %>%
    filter(series_id %in% c("LNU07000000", "LNU07100000", "LNU07200000",
                            "LNU07400000", "LNU07500000", "LNU07600000",
                            "LNU07800000", "LNU07900000", "LNU08000000",
                            "LNS17000000", "LNS17100000", "LNS17200000",
                            "LNS17400000", "LNS17500000", "LNS17600000",
                            "LNS17800000", "LNS17900000", "LNS18000000")) %>%
    rename(f = value,
           period_2 = period) %>%
    mutate(seas = case_when(str_sub(series_id, 3, 3) == "S" ~ "SA",
                            str_sub(series_id, 3, 3) == "U" ~ "NSA"),
           lfs_1 = case_when(series_id %in% c("LNU07000000", "LNU07400000", "LNU07800000", "LNS17000000", "LNS17400000", "LNS17800000") ~ "E",
                             series_id %in% c("LNU07100000", "LNU07500000", "LNU07900000", "LNS17100000", "LNS17500000", "LNS17900000") ~ "U",
                             series_id %in% c("LNU07200000", "LNU07600000", "LNU08000000", "LNS17200000", "LNS17600000", "LNS18000000") ~ "I",
                             TRUE                                                                                                       ~ NA_character_),
           lfs_2 = case_when(series_id %in% c("LNU07000000", "LNU07100000", "LNU07200000", "LNS17000000", "LNS17100000", "LNS17200000") ~ "E",
                             series_id %in% c("LNU07400000", "LNU07500000", "LNU07600000", "LNS17400000", "LNS17500000", "LNS17600000") ~ "U",
                             series_id %in% c("LNU07800000", "LNU07900000", "LNU08000000", "LNS17800000", "LNS17900000", "LNS18000000") ~ "I",
                             TRUE                                                                                                       ~ NA_character_)) %>%
    select(series_id, period_2, lfs_1, lfs_2, seas, f)

# construct stock by adding up flows grouped by first LF status (lfs_1 is status in previous month)
df_flows_bls_sum_1 <-
    df_flows_bls %>%
    group_by(period_2, lfs_1, seas) %>%
    summarise(s = sum(f)) %>%
    group_by(lfs_1, seas) %>%
    mutate(period = lag(period_2, default = 199001)) %>%
    ungroup() %>%
    rename(lfs = lfs_1) %>%
    select(period, lfs, seas, s)

# construct stock by adding flows grouped by second LF status (lfs_2 is status in current month)
df_flows_bls_sum_2 <-
    df_flows_bls %>%
    group_by(period_2, lfs_2, seas) %>%
    summarise(s = sum(f)) %>%
    rename(period = period_2) %>%
    ungroup() %>%
    rename(lfs = lfs_2) %>%
    select(period, lfs, seas, s)

# compare the difference between stocks constructed by summing BLS flows with BLS stocks
df_blsdata %>%
    filter(series_id %in% c("LNU02000000", "LNU03000000", "LNU05000000",
                            "LNS12000000", "LNS13000000", "LNS15000000")) %>%
    mutate(series_id = recode(series_id, "LNU02000000" = "E_NSA",
                                         "LNU03000000" = "U_NSA",
                                         "LNU05000000" = "I_NSA",
                                         "LNS12000000" = "E_SA",
                                         "LNS13000000" = "U_SA",
                                         "LNS15000000" = "I_SA")) %>%
    separate(series_id, into = c("lfs", "seas")) %>%
    rename(s = value) %>%
    bind_rows(stocks = .,
              sumflows1 = df_flows_bls_sum_1,
              sumflows2 = df_flows_bls_sum_2,
              .id = "source") %>%
    spread(source, s) %>%
    mutate(err_sum_of_flows1 = (sumflows1 - stocks) / stocks,
           err_sum_of_flows2 = (sumflows2 - stocks) / stocks) %>%
    unite(measure, lfs, seas, sep = ".") %>%
    gather(source, value, -c(period, measure)) %>%
    mutate(yearm = period %>% as.character() %>% as.yearmon(format = "%Y%m")) %>%
    filter(str_sub(source, 1, 3) == "err") %>%
    filter(!is.na(value)) %>%
    separate(measure, into = c("lfs", "seas")) %>%
    ggplot(aes(x = yearm, y = value, col = source)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dotted") +
        scale_x_yearmon() +
        scale_y_continuous(labels = scales::percent) +
        scale_color_discrete(labels = c("sum in month 1", "sum in month 2")) +
        labs(x = "", y = "", title  = "Difference between stocks constructed by summing BLS flows and actual BLS stocks",
             color = "") +
        facet_grid(lfs ~ seas, scales = "free_y")

# construct population shares by LFS
df_stocksandshares_bls <-
    df_stocks_bls %>%
    group_by(period, seas) %>%
    mutate(shr_lfs2pop = s / sum(s)) %>%
    ungroup()

# plot population shares by LFS
df_stocksandshares_bls %>%
    mutate(yearm = period %>% as.character() %>% as.yearmon(format = "%Y%m")) %>%
    ggplot(aes(x = yearm, y = shr_lfs2pop)) +
        geom_line() +
        scale_x_yearmon() +
        facet_grid(lfs ~ seas, scales = "free")

# construct transition rates
df_flowsandrates_bls <-
    df_flows_bls %>%
    group_by(series_id) %>%
    mutate(period_1 = lag(period_2, default = 199001)) %>%
    group_by(period_2, lfs_1, seas) %>%
    mutate(rate = f / sum(f)) %>%
    ungroup() %>%
    select(series_id, period_1, period_2, lfs_1, lfs_2, seas, f, rate)

save(df_blsdata, df_stocksandshares_bls, df_flowsandrates_bls, file = str_c(odir_bls, "BLS_lf.Rdata"))
# load(file = str_c(odir_bls, "BLS_lf.Rdata"))

rm(df_flows_bls_sum_1, df_flows_bls_sum_2,
   df_blsdata, df_stocks_bls, df_flows_bls, df_flowsandrates_bls, df_stocksandshares_bls)
