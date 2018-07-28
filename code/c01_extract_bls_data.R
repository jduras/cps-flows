
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

choice <- menu(choices = c("Use existing BLS dataset", "Download new dataset from BLS server"),
               title = "Download data from BLS?")

if (choice == 2) {
    download.file(url = "https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData",
                  dest = paste0(ddir.bls, "ln.data.1.AllData"))
}

print("Extracting BLS data")

df.blsdata.raw <- read_table2(file = paste0(ddir.bls, "ln.data.1.AllData"), col_types = c("cicdi"))

# blsdata.raw <-
#     "ln.data.1.AllData" %T>%
#     {download.file(url = paste0("https://download.bls.gov/pub/time.series/ln/", .),
#                    dest = paste0(ddir.bls, .))} %>%
#     {read_table2(file = paste0(ddir.bls, .), col_types = c("cicdi"))}

df.blsdata <-
    df.blsdata.raw %>%
    filter(!is.na(series_id)) %>%
    select(-footnote_codes) %>%
    filter(series_id %in% c(# NSA stocks: POP, LF, E, U, I
                            "LNU00000000", "LNU01000000", "LNU02000000", "LNU03000000", "LNU05000000",
                            # NSA rates: LFPR, EPR, UR
                            "LNU01300000","LNU02300000","LNU04000000",
                            # NSA flows: EE, UE, IE
                            "LNU07000000","LNU07100000","LNU07200000",
                            # NSA flows: EU, UU, IU
                            "LNU07400000","LNU07500000","LNU07600000",
                            # NSA flows: EI, UI, II
                            "LNU07800000","LNU07900000","LNU08000000",
                            # SA stocks: POP, LF, E, U, I
                            "LNS10000000", "LNS11000000", "LNS12000000", "LNS13000000", "LNS15000000",
                            # SSA rates: LFPR, EPR, UR
                            "LNS11300000","LNS12300000","LNS14000000",
                            # SA flows: EE, UE, IE
                            "LNS17000000","LNS17100000","LNS17200000",
                            # SA flows: EU, UU, IU
                            "LNS17400000","LNS17500000","LNS17600000",
                            # SA flows: EI, UI, II
                            "LNS17800000","LNS17900000","LNS18000000"
                            )) %>%
    # keep only monthly data, drop quarterly and annual data
    filter(!(str_detect(period, pattern = "Q") | str_detect(period, "M13"))) %>%
    mutate(period = paste0(year, str_sub(period, 2, 3)) %>% as.numeric()) %>%
    filter(period >= 197501) %>%
    select(-year)

rm(df.blsdata.raw)

# stocks
df.stocks.bls <-
    df.blsdata %>%
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
df.flows.bls <-
    df.blsdata %>%
    filter(series_id %in% c("LNU07000000", "LNU07100000", "LNU07200000",
                            "LNU07400000", "LNU07500000", "LNU07600000",
                            "LNU07800000", "LNU07900000", "LNU08000000",
                            "LNS17000000", "LNS17100000", "LNS17200000",
                            "LNS17400000", "LNS17500000", "LNS17600000",
                            "LNS17800000", "LNS17900000", "LNS18000000")) %>%
    rename(f = value,
           period.2 = period) %>%
    mutate(seas = case_when(str_sub(series_id, 3, 3) == "S" ~ "SA",
                            str_sub(series_id, 3, 3) == "U" ~ "NSA"),
           lfs.1 = case_when(series_id %in% c("LNU07000000", "LNU07400000", "LNU07800000", "LNS17000000", "LNS17400000", "LNS17800000") ~ "E",
                             series_id %in% c("LNU07100000", "LNU07500000", "LNU07900000", "LNS17100000", "LNS17500000", "LNS17900000") ~ "U",
                             series_id %in% c("LNU07200000", "LNU07600000", "LNU08000000", "LNS17200000", "LNS17600000", "LNS18000000") ~ "I",
                             TRUE                                                                                                       ~ NA_character_),
           lfs.2 = case_when(series_id %in% c("LNU07000000", "LNU07100000", "LNU07200000", "LNS17000000", "LNS17100000", "LNS17200000") ~ "E",
                             series_id %in% c("LNU07400000", "LNU07500000", "LNU07600000", "LNS17400000", "LNS17500000", "LNS17600000") ~ "U",
                             series_id %in% c("LNU07800000", "LNU07900000", "LNU08000000", "LNS17800000", "LNS17900000", "LNS18000000") ~ "I",
                             TRUE                                                                                                       ~ NA_character_)) %>%
    select(series_id, period.2, lfs.1, lfs.2, seas, f)

# construct stock by adding up flows grouped by first LF status (lfs.1 is status in previous month)
df.flows.bls.sum.1 <-
    df.flows.bls %>%
    group_by(period.2, lfs.1, seas) %>%
    summarise(s = sum(f)) %>%
    group_by(lfs.1, seas) %>%
    mutate(period = lag(period.2, default = 199001)) %>%
    ungroup() %>%
    rename(lfs = lfs.1) %>%
    select(period, lfs, seas, s)

# construct stock by adding flows grouped by second LF status (lfs.2 is status in current month)
df.flows.bls.sum.2 <-
    df.flows.bls %>%
    group_by(period.2, lfs.2, seas) %>%
    summarise(s = sum(f)) %>%
    rename(period = period.2) %>%
    ungroup() %>%
    rename(lfs = lfs.2) %>%
    select(period, lfs, seas, s)

# compare the difference between stocks constructed by summing BLS flows with BLS stocks
df.blsdata %>%
    filter(series_id %in% c("LNU02000000", "LNU03000000", "LNU05000000",
                            "LNS12000000", "LNS13000000", "LNS15000000")) %>%
    mutate(series_id = case_when(series_id == "LNU02000000" ~ "E.NSA",
                                 series_id == "LNU03000000" ~ "U.NSA",
                                 series_id == "LNU05000000" ~ "I.NSA",
                                 series_id == "LNS12000000" ~ "E.SA",
                                 series_id == "LNS13000000" ~ "U.SA",
                                 series_id == "LNS15000000" ~ "I.SA")) %>%
    separate(series_id, into = c("lfs", "seas")) %>%
    rename(s = value) %>%
    bind_rows(stocks = .,
              sumflows1 = df.flows.bls.sum.1,
              sumflows2 = df.flows.bls.sum.2,
              .id = "source") %>%
    spread(source, s) %>%
    mutate(err.sum.of.flows1 = (sumflows1 - stocks) / stocks,
           err.sum.of.flows2 = (sumflows2 - stocks) / stocks) %>%
    unite(measure, lfs, seas, sep = ".") %>%
    gather(source, value, -c(period, measure)) %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon(format = "%Y%m")) %>%
    filter(str_sub(source, 1, 3) == "err") %>%
    filter(!is.na(value)) %>%
    separate(measure, into = c("lfs", "seas")) %>%
    {ggplot(., aes(x = monyear, y = value, col = source)) +
            geom_line() +
            geom_hline(yintercept = 0, linetype = "dotted") +
            scale_x_yearmon() +
            scale_y_continuous(labels = scales::percent) +
            scale_color_discrete(labels = c("sum in month 1", "sum in month 2")) +
            labs(x = "", y = "", title  = "Difference between stocks constructed by summing BLS flows and actual BLS stocks",
                 col = "") +
            facet_grid(lfs ~ seas, scales = "free_y")}

# construct population shares by LFS
df.stocksandshares.bls <-
    df.stocks.bls %>%
    group_by(period, seas) %>%
    mutate(pop = sum(s),
           popshr = s/pop) %>%
    ungroup()

# plot population shares by LFS
df.stocksandshares.bls %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon(format = "%Y%m")) %>%
    ggplot(aes(x = monyear, y = popshr)) +
        geom_line() +
        scale_x_yearmon() +
        facet_grid(lfs ~ seas, scales = "free")

# construct transition rates
df.flowsandrates.bls <-
    df.flows.bls %>%
    group_by(series_id) %>%
    mutate(period.1 = lag(period.2, default = 199001)) %>%
    group_by(period.2, lfs.1, seas) %>%
    mutate(rate = f / sum(f)) %>%
    ungroup() %>%
    select(series_id, period.1, period.2, lfs.1, lfs.2, seas, f, rate)

save(df.blsdata, df.stocksandshares.bls, df.flowsandrates.bls, file = paste0(odir.bls, "BLS_lf.Rdata"))

rm(df.flows.bls.sum.1, df.flows.bls.sum.2,
   df.blsdata, df.flows.bls, df.flowsandrates.bls, df.stocksandshares.bls)
