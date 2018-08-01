
print(paste("Matching and merging observations for month x and x+12"))

t1 <- tfirst

breaks <- c(197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

while (t1 <= tlast - 100) {

    t2 <- t1 + 100

    print(paste(" now matching:", t1, "with", t2))

    if (!any(breaks %in% c(t1:t2))) {

        # load data for month t1
        load(file = paste0(edir.cps, "cpsb_", t1, ".Rdata"))

        df.cpsdata.1 <-
            df.cpsdata %>%
            filter(mis %in% c(1:4)) %>%
            mutate(mis.in.1 = mis,
                   age.in.1 = age)

        # load data for month t2
        load(file = paste0(edir.cps, "cpsb_", t2, ".Rdata"))

        df.cpsdata.2 <-
            df.cpsdata %>%
            filter(mis %in% c(5:8)) %>%
            mutate(mis.in.1 = mis - 4,
                   age.in.1 = age)

        # First round of matching records - people whose age did not change
        # find all individuals, matched if they agree on
        #	gestcen, hid, pid, race, female, age

        df.merged.2m.yoy.part1 <-
            inner_join(df.cpsdata.1, df.cpsdata.2,
                       by = c("gestcen", "hid", "pid", "white", "black", "female", "age.in.1", "mis.in.1"), suffix = c(".1", ".2"))

        # Second round of matching records - people whose age did change by 1

        df.unmatched.1 <-
            anti_join(df.cpsdata.1, df.cpsdata.2,
                      by = c("gestcen", "hid", "pid", "white", "black", "female", "age.in.1", "mis.in.1"))

        df.cpsdata.2 %<>%
           mutate(age.in.1 = age.in.1 - 1)

        # find all individuals, matched if they agree on
        #	gestcen, hid, pid, race, female, and age in t2 = age in t1 + 1

        df.merged.2m.yoy.part2 <-
            inner_join(df.unmatched.1, df.cpsdata.2,
                       by = c("gestcen", "hid", "pid", "white", "black", "female", "age.in.1", "mis.in.1"), suffix = c(".1", ".2"))

        # create combined data set with observations from both rounds of matching
        df.merged.2m.yoy <-
           bind_rows(df.merged.2m.yoy.part1, df.merged.2m.yoy.part2) %>%
           select(-c(mis.in.1, age.in.1))

        save(df.merged.2m.yoy, file = paste0(edir.cps, "merged_2m_yoy_", t1, ".Rdata"))
        rm(df.cpsdata, df.cpsdata.1, df.cpsdata.2, df.unmatched.1, df.merged.2m.yoy.part1, df.merged.2m.yoy.part2, df.merged.2m.yoy)

    }

    t1 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)

}

