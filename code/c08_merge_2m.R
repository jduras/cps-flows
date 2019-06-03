
tic()

message("Merging all matched data for two consecutive months into one dataset")

df_merged_2m_all <- vector("list", length(tseq) - 1)

t1 <- tfst
i <- 1

breaks <- c(197601, 197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

while (t1 < tlst) {

    t2 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)

    message(str_c(" now adding: ", t1, "-", t2))

    # there are some gaps in the series due to changes in the household identifiers in the public-use files - it is impossible to match data for some periods
    if (!(t2 %in% breaks)) {
        load(file = str_c(edir_cps, "merged_2m_", t1, ".Rdata"))

        df_merged_2m_all[[i]] <-
            df_merged_2m %>%
            filter(age_2 >= 16) %>%
            filter(lfs_1 != "M" & lfs_2 != "M") %>%
            # filter(!(occ1cat_1 == "MIL")) %>%
            select(-c(mis_2, period_2))
    }

    rm(df_merged_2m)

    # go to next month
    t1 <- t2
    i <- i + 1
}

message("Transforming merged dataset from list to tibble")

df_merged_2m_all %<>%
    bind_rows()

message("Saving merged dataset")

save(df_merged_2m_all, file = str_c(edir_cps, "merged_2m_all.Rdata"))

toc()
