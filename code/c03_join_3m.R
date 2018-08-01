
print(paste("Matching and merging observations for three consecutive months"))

t1 <- tfirst

breaks <- c(197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

while (t1 < tlast-1) {

    t2 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)
    t3 <- if_else(t2 %% 100 == 12, t2 + 89, t2 + 1)

    print(paste0(" now matching: ", t1, "-", t2, " with ", t2, "-", t3))

    if (!(t2 %in% breaks | t3 %in% breaks)) {

        # load merged data t1-t2
        load(file = paste0(edir.cps, "merged_2m_", t1, ".Rdata"))

        df.merged.2m.1 <-
            df.merged.2m %>%
            filter(mis.2 %in% c(2, 3, 6, 7))

        # load merged data t2-t3
        load(file = paste0(edir.cps, "merged_2m_", t2, ".Rdata"))

        df.merged.2m.2 <-
            df.merged.2m %>%
            rename_all(funs(str_replace_all(., c("\\.2" = ".3", "\\.1" = ".2")))) %>%
            filter(mis.2 %in% c(2, 3, 6, 7))

        # merging records
        df.merged.3m <-
            inner_join(df.merged.2m.1, df.merged.2m.2,
                       by = c("gestcen", "hid", "pid", "white", "black", "female", "age.2", "mis.2",
                              "period.2", "educ.2", "married.2", "lfs.2", "unempdur.2", "occ1grp.2", "weight.2"), suffix = c(".1", ".2"))

        save(df.merged.3m, file = paste0(edir.cps, "merged_3m_", t1, ".Rdata"))
        rm(df.merged.2m, df.merged.2m.1, df.merged.2m.2, df.merged.3m)

    }

    t1 <- t2
}

