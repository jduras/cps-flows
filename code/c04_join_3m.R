
message("Matching and merging observations for three consecutive months")

t1 <- tfst

breaks <- c(197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

while (t1 < tlst - 1) {

    t2 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)
    t3 <- if_else(t2 %% 100 == 12, t2 + 89, t2 + 1)

    message(str_c(" now matching: ", t1, "-", t2, " with ", t2, "-", t3))

    if (!(t2 %in% breaks | t3 %in% breaks)) {

        # load merged data t1-t2
        load(file = str_c(edir_cps, "merged_2m_", t1, ".Rdata"))

        df_merged_2m_1 <-
            df_merged_2m %>%
            filter(mis_2 %in% c(2, 3, 6, 7))

        # load merged data t2-t3
        load(file = str_c(edir_cps, "merged_2m_", t2, ".Rdata"))

        df_merged_2m_2 <-
            df_merged_2m %>%
            rename_all(funs(str_replace_all(., c("\\_2" = "_3", "\\_1" = "_2")))) %>%
            filter(mis_2 %in% c(2, 3, 6, 7))

        # merging records
        df_merged_3m <-
            inner_join(df_merged_2m_1, df_merged_2m_2,
                       by = c("gestcen", "hid", "pid", "white", "black", "female", "age_2", "mis_2",
                              "period_2", "educ_2", "married_2", "lfs_2", "udur_2", "occ1cat_2", "weight_2"), suffix = c("_1", "_2"))

        save(df_merged_3m, file = str_c(edir_cps, "merged_3m_", t1, ".Rdata"))
        rm(df_merged_2m, df_merged_2m_1, df_merged_2m_2, df_merged_3m)

    }

    t1 <- t2
}

