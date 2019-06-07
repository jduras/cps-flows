
#### assemble data for linear probability models ####

message("Assembling dataset for linear probability models")

# get data on state level unemployment rate
quandl_api_key('DLk9RQrfTVkD4UTKc7op')

df_state_ur_raw <-
    c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
      "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
      "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
      "UT", "VA", "VT", "WA", "WI", "WV", "WY") %>%
    str_c("UR") %>%
    tq_get(get = "economic.data", from = ymd(str_c(tfst, "01")), to = ymd(str_c(tlst, "01")))

if (nrow(df_state_ur_raw) == 0) {
    df_state_ur_raw <- read_csv(file = str_c(mdir_cps, "data/ur_raw.csv"))
} else {
    write_csv(df_state_ur_raw, path = str_c(mdir_cps, "data/ur_raw.csv"))
}

df_state_ur <-
    df_state_ur_raw %>%
    rename(state_code = symbol,
           u3_rate = price) %>%
    mutate(date = 100*year(date) + month(date),
           state_code = str_sub(state_code, 1, 2)) %>%
    select(date, state_code, u3_rate)

# help wanted index
df_hwi <- read_tsv(str_c(mdir_cps, "data/HWI_index_updated2016m12.txt"), col_types = c("id"))

df_state_codes <-
    read_csv(str_c(mdir_cps, "data/state_codes.csv"), col_types = c("iicc")) %>%
    select(gestcen, state_code)

# load merged data from merge_2m.R
load(file = str_c(edir_cps, "merged_2m_all.Rdata"))

df_lpm_data_agg <- df_merged_2m_all

rm(df_merged_2m_all)

# dataset for linear probability model
df_lpm_data_agg <-
    df_lpm_data_agg %>%
    # df_merged_2m_all %>%
    select(period_1, gestcen, age_1, educ_1, female, white, married_1, lfs_1, occ1cat_1, udur_1, weight_1, lfs_2, occ1cat_2) %>%
    filter(age_1 >= 16) %>%
    # filter(age_1 >= 16 & age_1 <= 75) %>%
    filter(!(lfs_1 == "E" & occ1cat_1 == "X")) %>%
    filter(!(occ1cat_1 %in% c("FRM", "MIL") | occ1cat_2 %in% c("FRM", "MIL"))) %>%
    left_join(df_state_codes, by = "gestcen") %>%
    left_join(df_state_ur, by = c("state_code", "period_1" = "date")) %>%
    left_join(df_hwi, by = c("period_1" = "date")) %>%
    mutate(# dependent variables
        y_E = (lfs_2 == "E") %>% as.integer(),
        y_U = (lfs_2 == "U") %>% as.integer(),
        y_I = (lfs_2 == "I") %>% as.integer(),
        # age categories
        agecat_1 = case_when(age_1 %in% 16:19 ~ 1L,
                             age_1 %in% 20:24 ~ 2L,
                             age_1 %in% 25:34 ~ 3L,
                             age_1 %in% 35:44 ~ 4L,
                             age_1 %in% 45:54 ~ 5L,
                             age_1 %in% 55:64 ~ 6L,
                             age_1 %in% 65:75 ~ 7L,
                             TRUE                ~ NA_integer_) %>% factor() %>% relevel(ref = 5),
        # education categories
        educat_1 = case_when(educ_1 == 1        ~ "hsd",
                             educ_1 %in% c(2:3) ~ "hsg",
                             educ_1 %in% c(4:5) ~ "clp") %>%factor() %>% relevel(ref = "hsg"),
        male = 1L - female,
        nwhite = 1L - white,
        # unemployment duration bins
        udurcat_1 = case_when(udur_1 %in% 0:4                ~ 1L,
                              udur_1 %in% 5:8                ~ 2L,
                              udur_1 %in% 9:12               ~ 3L,
                              udur_1 %in% 13:16              ~ 4L,
                              udur_1 %in% 17:20              ~ 5L,
                              udur_1 %in% 21:24              ~ 6L,
                              udur_1 %in% 25:28              ~ 7L,
                              udur_1 %in% 29:32              ~ 8L,
                              udur_1 %in% 33:40              ~ 9L,
                              #udur_1 %in% 41:68              ~ 10L,
                              #udur_1 >= 69                      ~ 11L
                              #udur_1 %in% 69:96              ~ 11L,
                              udur_1 %in% 41:52              ~ 10L,
                              udur_1 %in% 53:96              ~ 11L,
                              udur_1 >= 97                   ~ 12L,
                              TRUE                           ~ NA_integer_) %>% as.factor(),
        # occupation
        occ1cat_1 = occ1cat_1 %>% as.factor(),
        # state
        state_code = state_code %>% as.factor(),
        # seasonal dummies
        month_1 = (period_1 %% 100)  %>% as.factor(),
        # recession/expansion id
        cycle_id = case_when(period_1 %in% 197601:197912 ~ "E1",
                             period_1 %in% 198001:198007 ~ "R1",
                             period_1 %in% 198008:198106 ~ "E2",
                             period_1 %in% 198107:198211 ~ "R2",
                             period_1 %in% 198212:199006 ~ "E3",
                             period_1 %in% 199007:199103 ~ "R3",
                             period_1 %in% 199104:200102 ~ "E4",
                             period_1 %in% 200103:200111 ~ "R4",
                             period_1 %in% 200112:200711 ~ "E5",
                             period_1 %in% 200712:200906 ~ "R5",
                             period_1 %in% 200907:201212 ~ "E6",
                             TRUE                        ~ NA_character_)) %>%
    select(-c(gestcen, age_1, educ_1, female, white, udur_1, lfs_2))

df_lpm_data_agg <-
    bind_rows(df_lpm_data_agg,
              df_lpm_data_agg %>%
                  filter(period_1 >= 201301) %>%
                  mutate(cycle_id = "E6_since_2013"),
              df_lpm_data_agg %>%
                  filter(period_1 >= 200907) %>%
                  mutate(cycle_id = "E6_whole")) %>%
    filter(!is.na(cycle_id))

save(df_lpm_data_agg, file = str_c(edir_cps, "out_lpm_data_agg.Rdata"))
