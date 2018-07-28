
print(paste("Matching and merging observations for four consecutive months"))

t1 <- tfirst

breaks <- c(197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

while (t1 < tlast-2) {

    t2 <- t1 + 1
    if (t2 %% 100 == 13) t2 <- t2 + 88
    t3 <- t2 + 1
    if (t3 %% 100 == 13) t3 <- t3 + 88
    t4 <- t3 + 1
    if (t4 %% 100 == 13) t4 <- t4 + 88

    print(paste0(" now matching: ", t1, "-", t2," with ", t2, "-", t3," and ", t3, "-", t4))

    if (!(t2 %in% breaks | t3 %in% breaks | t4 %in% breaks)) {

        # load merged data t1-t2
        load(file = paste0(edir.cps, "merged_2m_", t1, ".Rdata"))

        df.merged.2m.1 <-
            df.merged.2m %>%
            filter(mis.2 %in% c(2, 6))

        # load merged data t2-t3
        load(file = paste0(edir.cps, "merged_2m_", t2, ".Rdata"))

        df.merged.2m.2 <-
            df.merged.2m %>%
            rename_all(funs(str_replace_all(., c("\\.2" = ".3", "\\.1" = ".2")))) %>%
            filter(mis.3 %in% c(3, 7))

        # load merged data t3-t4
        load(file = paste0(edir.cps, "merged_2m_", t3, ".Rdata"))

        df.merged.2m.3 <-
            df.merged.2m %>%
            rename_all(funs(str_replace_all(., c("\\.2" = ".4", "\\.1" = ".3")))) %>%
            filter(mis.4 %in% c(4, 8))

        # matching records
        df.merged.4m <-
            df.merged.2m.1 %>%
            inner_join(df.merged.2m.2,
                       by = c("gestcen", "hid", "pid", "white", "black", "female", "age.2", "mis.2",
                              "period.2", "educ.2", "married.2", "lfs.2", "unempdur.2", "occ1grp.2", "weight.2"), suffix = c(".1", ".2")) %>%
            inner_join(df.merged.2m.3,
                       by = c("gestcen", "hid", "pid", "white", "black", "female", "age.3", "mis.3",
                              "period.3", "educ.3", "married.3", "lfs.3", "unempdur.3", "occ1grp.3", "weight.3"), suffix = c(".2", ".3"))

        save(df.merged.4m, file = paste0(edir.cps, "merged_4m_", t1, ".Rdata"))
        rm(df.merged.2m, df.merged.2m.1, df.merged.2m.2, df.merged.2m.3, df.merged.4m)

    }

    t1 <- t2
}

