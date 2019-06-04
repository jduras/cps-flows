
message("Merging all matched data months x and x+12 into one dataset")

df_merged_2m_yoy_all <- NULL

t1 <- tfst

breaks <- c(197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

while (t1 <= tlst - 100) {

    t2 <- t1 + 100

    message(str_c(" now adding: ", t1, "-", t2))

    if (!any(breaks %in% c(t1:t2))) {

        load(file = str_c(edir_cps, "merged_2m_yoy_", t1, ".Rdata"))

        df_merged_2m_yoy_all <-
            c(df_merged_2m_yoy_all,
              df_merged_2m_yoy %>%
                  filter(age_2 >= 16) %>%
                  filter(lfs_1 != "M" & lfs_2 != "M") %>%
                  list())
    }

    rm(df_merged_2m_yoy)

    t1 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)
}

message("Transforming merged dataset from list to tibble")

df_merged_2m_yoy_all %<>%
    bind_rows()

message("Saving merged dataset")

save(df_merged_2m_yoy_all, file = str_c(edir_cps, "merged_2m_yoy_all.Rdata"))
