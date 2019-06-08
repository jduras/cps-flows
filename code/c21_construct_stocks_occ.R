
message("Constructing stocks by occupation category from CPS micro data")

#### construct stocks and shares, by labor force status and occupation group ####

# merged CPS data from merge_1m.R
if (!exists("df_merged_1m_all_sample")) load(file = str_c(edir_cps, "merged_1m_all.Rdata"))

# construct descriptive statistics in Table 1 from Cortes, Jaimovich, Nekarda, Siu
df_table_1 <-
    df_merged_1m_all_sample %>%
    # filter(period >= 197601 & period <= 198912) %>%
    filter(period >= 199001 & period <= 201212) %>%
    mutate(hsd = (educ==1) %>% as.numeric(),
           hsg = (educ %in% 2:3) %>% as.numeric(),
           clp = (educ %in% 4:5) %>% as.numeric(),
           lfs_occ1cat = str_c(lfs,"_",occ1cat),
           nwhite = if_else(white == 0, 1, 0)) %>%
    select(lfs_occ1cat, age, hsd, hsg, clp, female, nwhite, married, weight) %>%
    { bind_rows(nest(mutate(., lfs_occ1cat = "all"), -lfs_occ1cat),
                nest(., -lfs_occ1cat)) } %>%
    arrange(lfs_occ1cat) %>%
    mutate(mean.age = map_dbl(data, ~wtd.mean(.x$age, weights = .x$weight, na.rm = TRUE)),
           mean_edu_1_hsd = map_dbl(data, ~wtd.mean(.x$hsd, weights = .x$weight, na.rm = TRUE)),
           mean_edu_2_hsg = map_dbl(data, ~wtd.mean(.x$hsg, weights = .x$weight, na.rm = TRUE)),
           mean_edu_3_clp = map_dbl(data, ~wtd.mean(.x$clp, weights = .x$weight, na.rm = TRUE)),
           mean_female = map_dbl(data, ~wtd.mean(.x$female, weights = .x$weight, na.rm = TRUE)),
           mean_nwhite = map_dbl(data, ~wtd.mean(.x$nwhite, weights = .x$weight, na.rm = TRUE)),
           mean_married = map_dbl(data, ~wtd.mean(.x$married, weights = .x$weight, na.rm = TRUE)),
           nobs = map_dbl(data, ~nrow(.x))) %>%
    select(-data)

df_table_1 %>%
    gather(measure, value, -lfs_occ1cat) %>%
    spread(lfs_occ1cat, value) %>%
    select(measure, all, E_NRC, E_RC, E_RM, E_NRM, I_X)


# plot share of those not in labor force, by occupation group
# (in Cortes, Jaimovich, Nekarda, Siu all workers with lfs = I are considered as occ1cat = X)
df_merged_1m_all %>%
    select(period, lfs, occ1cat_all, weight) %>%
    filter(lfs == "I") %>%
    filter(!(occ1cat_all %in% c("FRM", "MIL"))) %>%
    group_by(period, occ1cat_all) %>%
    summarise(s = sum(weight)) %>%
    group_by(period) %>%
    mutate(shr = s / sum(s)) %>%
    ungroup() %>%
    mutate(yearm = period %>% as.character() %>% as.yearmon(format = "%Y%m")) %>%
    ggplot(aes(x = yearm, y = shr, col = occ1cat_all)) +
        geom_line() +
        scale_x_yearmon() +
        scale_y_continuous(labels = percent) +
        scale_color_discrete(labels = c("non-routine cognitive", "non-routine manual", "routine cognitive", "routine manual", "not available")) +
        labs(title = "Share of Not in Labor Force, by Occupation Group",
             x = "", y = "Share of Not in Labor Force",
             col = "Occupation Group") +
        facet_grid(occ1cat_all~ ., scales = "free_y", switch = "y") +
        theme(strip.placement = "outside")


# construct shares of population in different labor force and occupation groups
df_stocksandshares_cps_occ_whole_sample <-
    df_merged_1m_all %>%
    group_by(period, lfs, occ1cat) %>%
    summarise(seas = "NSA",
              s_occ = sum(weight)) %>%
    group_by(period, lfs) %>%
    mutate(shr_occ2lfs = s_occ / sum(s_occ)) %>%
    group_by(period) %>%
    mutate(shr_occ2pop = s_occ / sum(s_occ)) %>%
    ungroup()

df_stocksandshares_cps_occ <-
    df_merged_1m_all_sample %>%
    group_by(period, lfs, occ1cat) %>%
    summarise(seas = "NSA",
              s_occ = sum(weight)) %>%
    group_by(period, lfs) %>%
    mutate(shr_occ2lfs = s_occ / sum(s_occ)) %>%
    group_by(period) %>%
    mutate(shr_occ2pop = s_occ / sum(s_occ)) %>%
    ungroup()

df_stocksandshares_cps_occ_combined <-
    bind_rows(restricted = df_stocksandshares_cps_occ,
              whole = df_stocksandshares_cps_occ_whole_sample,
              .id = "sample") %>%
    gather(measure, y, c(s_occ, shr_occ2lfs, shr_occ2pop)) %>%
    nest(c(period, y)) %>%
    sa_ssm() %>%
    unnest() %>%
    rename(NSA = y,
           SA = y_ks) %>%
    gather(seas, value, c(SA, NSA)) %>%
    spread(measure, value)


# plot shares of population in different labor force and occupation groups
df_stocksandshares_cps_occ %>%
    mutate(yearm = period %>% as.character() %>% as.yearmon(format = "%Y%m")) %>%
    ggplot() +
        geom_line(aes(x = yearm, y = shr_occ2pop)) +
        geom_rect(data = rec_dates %>% filter(Start > "Jan 1976"), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf), alpha = 0.1) +
        scale_x_yearmon() +
        scale_y_continuous(labels = percent) +
        labs(x = "", y = "Share of population") +
        facet_grid(lfs ~ occ1cat, scales = "free_y")


# unemployment rate and population shares by occupation group
df_pop_shares_cps_occ_combined <-
    df_stocksandshares_cps_occ_combined %>%
    filter(lfs != "M") %>%
    filter(seas == "NSA") %>%
    select(sample, period, lfs, occ1cat, s_occ, shr_occ2pop) %>%
    gather(measure, value, c(s_occ, shr_occ2pop)) %>%
    unite(measure, measure, lfs, sep = "_") %>%
    spread(measure, value) %>%
    group_by(sample, period, occ1cat) %>%
    mutate(ur3 = s_occ_U / (s_occ_U + s_occ_E),
           shr_occ2pop_E_plus_U = shr_occ2pop_E + shr_occ2pop_U) %>%
    group_by(sample, occ1cat) %>%
    mutate(ur3_index_200712 = case_when(period >= 200712 ~ 100 * ur3 / ur3[period == 200712],
                                       TRUE              ~ NA_real_),
           ur3_change_200712 = ur3 - ur3[period == 200712]) %>%
    ungroup() %>%
    select(sample, period, occ1cat, shr_occ2pop_E, shr_occ2pop_U, shr_occ2pop_E_plus_U, ur3, ur3_index_200712, ur3_change_200712) %>%
    gather(measure, y, -c(sample, period, occ1cat)) %>%
    nest(c(period, y)) %>%
    sa_ssm() %>%
    unnest() %>%
    rename(NSA = y,
           SA = y_ks) %>%
    mutate(SA = if_else(measure %in% c("ur3_index_200712", "ur3_change_200712") & period < 200712, NA_real_, SA)) %>%
    gather(seas, value, c(SA, NSA))

chosen_seas <- "SA"
df_pop_shares_cps_occ_combined %>%
    filter(occ1cat %in% c("NRC", "NRM", "RC", "RM")) %>%
    filter(seas == chosen_seas) %>%
    mutate(yearm = period %>% as.character() %>% as.yearmon(format = "%Y%m"),
           measure_label = case_when(measure == "shr_occ2pop_E_plus_U" ~ "Population Share: Employed plus Unemployed",
                                     measure == "shr_occ2pop_E"        ~ "Population Share: Employed",
                                     measure == "shr_occ2pop_U"        ~ "Population Share: Unemployed",
                                     measure == "ur3"                  ~ "Unemployment Rate",
                                     measure == "ur3_index_200712"     ~ "Unemployment Rate, index Dec 2007=100",
                                     measure == "ur3_change_200712"    ~ "Unemployment Rate, change from Dec 2007")) %>%
    ggplot() +
        geom_line(aes(x = yearm, y = value, color = occ1cat, linetype = sample)) +
        geom_rect(data = rec_dates %>% filter(Start > "Jan 1976"), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf), alpha = 0.1) +
        scale_x_yearmon() +
        scale_color_discrete(labels = c("non-routine cognitive", "non-routine manual", "routine cognitive", "routine manual"))+
        labs(x = "", y = "", title = str_c("Employment and Unemployment by Occupation Group (", chosen_seas, ")"), color = "occupation group") +
        facet_wrap(~ measure_label, ncol = 3, scales = "free_y")

# unemployment share by occupation group
df_uande_shares_cps_occ_combined <-
    df_stocksandshares_cps_occ_combined %>%
    filter(lfs %in% c("E", "U")) %>%
    filter(seas == "NSA") %>%
    select(sample, period, lfs, occ1cat, shr_occ2lfs) %>%
    rename(y = shr_occ2lfs) %>%
    nest(c(period, y)) %>%
    sa_ssm() %>%
    unnest() %>%
    rename(NSA = y,
           SA = y_ks) %>%
    gather(seas, value, c(SA, NSA))

chosen_seas <- "SA"
df_uande_shares_cps_occ_combined %>%
    filter(occ1cat %in% c("NRC", "NRM", "RC", "RM")) %>%
    filter(seas == chosen_seas) %>%
    mutate(yearm = period %>% as.character() %>% as.yearmon(format = "%Y%m"),
           lfs_label = recode(lfs, "E" = "employed", "U"  = "unemployed")) %>%
    ggplot() +
        geom_line(aes(x = yearm, y = value, color = occ1cat, linetype = sample)) +
        geom_rect(data = rec_dates %>% filter(Start > "Jan 1976"), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf), alpha = 0.1) +
        scale_x_yearmon() +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        scale_color_discrete(labels = c("non-routine cognitive", "non-routine manual", "routine cognitive", "routine manual", "not available")) +
        labs(x = "", y = "", title = str_c("Shares of occupation groups within each labor force status (", chosen_seas, ")"), color = "occupation group") +
        facet_grid(~ lfs_label) +
        theme(strip.text = element_text(hjust = 0))

save(df_pop_shares_cps_occ_combined, df_uande_shares_cps_occ_combined,
     df_stocksandshares_cps_occ, df_stocksandshares_cps_occ_whole_sample, df_stocksandshares_cps_occ_combined,
     file = str_c(edir_cps, "out_stocksandshares_occ_Rdata"))

# load(file = str_c(edir_cps, "out_stocksandshares_occ_Rdata"))
