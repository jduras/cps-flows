
print("Merging observations from all months into one dataset")

df.merged.1m.all <- NULL

t <- tfirst

while (t <= tlast) {

    print(paste(" now getting data for",t))

    load(file = paste0(edir.cps, "cpsb_", t, ".Rdata"))

    df.merged.1m.all <-
        c(df.merged.1m.all,
          df.cpsdata %>%
              filter(age >= 16) %>%
              filter(!(occ1cat %in% c("MIL"))) %>%
              list())

    rm(df.cpsdata)

    t <- if_else(t %% 100 == 12, t + 89, t + 1)
}

print("Transforming dataset from list to tibble")

# change from list to dataframe
df.merged.1m.all %<>%
    bind_rows()

# restricted sample as in Cortes, Jaimovich, Nekarda, Siu
df.merged.1m.all.sample <-
    df.merged.1m.all %>%
    filter(age >= 16 & age <= 75) %>%
    filter(lfs != "M") %>%
    filter(!(occ1cat %in% c("FRM", "MIL"))) %>%
    filter(!(lfs == "E" & occ1cat == "X"))

print("Saving dataset")

save(df.merged.1m.all, df.merged.1m.all.sample, file = paste0(edir.cps, "merged_1m_all.Rdata"))
