
message("Matching and merging observations for four consecutive months")

t1 <- tfst

breaks <- c(197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

while (t1 < tlst - 2) {

    t2 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)
    t3 <- if_else(t2 %% 100 == 12, t2 + 89, t2 + 1)
    t4 <- if_else(t3 %% 100 == 12, t3 + 89, t3 + 1)

    message(str_c(" now matching: ", t1, "-", t2," with ", t2, "-", t3," and ", t3, "-", t4))

    if (!(t2 %in% breaks | t3 %in% breaks | t4 %in% breaks)) {

        # load merged data t1-t2
        load(file = str_c(edir_cps, "merged_2m_", t1, ".Rdata"))

        df_merged_2m_1 <-
            df_merged_2m %>%
            filter(mis_2 %in% c(2, 6))

        # load merged data t2-t3
        load(file = str_c(edir_cps, "merged_2m_", t2, ".Rdata"))

        df_merged_2m_2 <-
            df_merged_2m %>%
            rename_all(funs(str_replace_all(., c("\\_2" = "_3", "\\_1" = "_2")))) %>%
            filter(mis_3 %in% c(3, 7))

        # load merged data t3-t4
        load(file = str_c(edir_cps, "merged_2m_", t3, ".Rdata"))

        df_merged_2m_3 <-
            df_merged_2m %>%
            rename_all(funs(str_replace_all(., c("\\_2" = "_4", "\\_1" = "_3")))) %>%
            filter(mis_4 %in% c(4, 8))

        # matching records
        df.merged_4m <-
            df_merged_2m_1 %>%
            inner_join(df_merged_2m_2,
                       by = c("gestcen", "hid", "pid", "white", "black", "female", "age_2", "mis_2",
                              "period_2", "educ_2", "married_2", "lfs_2", "udur_2", "occ1cat_2", "weight_2"), suffix = c("_1", "_2")) %>%
            inner_join(df_merged_2m_3,
                       by = c("gestcen", "hid", "pid", "white", "black", "female", "age_3", "mis_3",
                              "period_3", "educ_3", "married_3", "lfs_3", "udur_3", "occ1cat_3", "weight_3"), suffix = c("_2", "_3"))

        save(df.merged_4m, file = str_c(edir_cps, "merged_4m_", t1, ".Rdata"))
        rm(df_merged_2m, df_merged_2m_1, df_merged_2m_2, df_merged_2m_3, df.merged_4m)

    }

    t1 <- t2
}

