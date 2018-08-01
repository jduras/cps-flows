
print(paste("Merging all matched data months x and x+12 into one dataset"))

df.merged.2m.yoy.all <- NULL

t1 <- tfirst

breaks <- c(197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

while (t1 <= tlast-100) {

    t2 <- t1 + 100

    print(paste0(" now adding: ", t1, "-", t2))

    if (!any(breaks %in% c(t1:t2))) {

        load(file = paste0(edir.cps, "merged_2m_yoy_", t1, ".Rdata"))

        df.merged.2m.yoy.all <-
            c(df.merged.2m.yoy.all,
              df.merged.2m.yoy %>%
                  filter(age.2 >= 16) %>%
                  filter(lfs.1 != "M" & lfs.2 != "M") %>%
                  list())
    }

    rm(df.merged.2m.yoy)

    t1 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)
}

print("Transforming dataset from list to tibble")

df.merged.2m.yoy.all %<>%
    bind_rows()

print("Saving dataset")

save(df.merged.2m.yoy.all, file = paste0(edir.cps, "merged_2m_yoy_all.Rdata"))
