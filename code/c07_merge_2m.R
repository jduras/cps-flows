
print(paste("Merging all matched data for two consecutive months into one dataset"))

df.merged.2m.all <- NULL

t1 <- tfirst

breaks <- c(197601, 197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

while (t1 < tlast) {

    t2 <- t1 + 1
    # go to next year if last two digits of t2=13
    if (t2 %% 100 == 13) t2 <- t2 + 88

    print(paste0(" now adding: ", t1, "-", t2))

    # there are some gaps in the series due to changes in the household identifiers in the public-use files - it is impossible to match data for some periods
    if (!(t2 %in% breaks)) {
        load(file = paste0(edir.cps, "merged_2m_", t1, ".Rdata"))

        df.merged.2m.all <-
            c(df.merged.2m.all,
              df.merged.2m %>%
                  filter(age.2 >= 16) %>%
                  filter(lfs.1 != "M" & lfs.2 != "M") %>%
                  select(-c(mis.2, period.2)) %>%
                  list())
    }

    rm(df.merged.2m)

    # go to next month
    t1 <- t2
}

print("Transforming dataset from list to tibble")

df.merged.2m.all %<>%
    bind_rows()

print("Saving dataset")

save(df.merged.2m.all, file = paste0(edir.cps, "merged_2m_all.Rdata"))
