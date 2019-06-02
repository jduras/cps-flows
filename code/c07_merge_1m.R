
tic()

message("Merging observations from all months into one dataset")

df_merged_1m_all <- vector("list", length(tseq))

t <- tfst
i <- 1

while (t <= tlst) {

    message(str_c(" now getting data for ", t))

    load(file = str_c(edir_cps, "cpsb_", t, ".Rdata"))

    df_merged_1m_all[i] <-
        df_cpsdata %>%
        filter(age >= 16) %>%
        filter(!(occ1cat == "MIL"))

    rm(df_cpsdata)

    t <- if_else(t %% 100 == 12, t + 89, t + 1)
    i <- i + 1
}

message("Transforming dataset from list to tibble")

# change from list to dataframe
df_merged_1m_all %<>%
    bind_rows()

# restricted sample as in Cortes, Jaimovich, Nekarda, Siu
df_merged_1m_all_sample <-
    df_merged_1m_all %>%
    filter(age >= 16 & age <= 75) %>%
    filter(lfs != "M") %>%
    filter(!(occ1cat %in% c("FRM", "MIL"))) %>%
    filter(!(lfs == "E" & occ1cat == "X"))

message("Saving merged dataset")

save(df_merged_1m_all, df_merged_1m_all_sample, file = str_c(edir_cps, "merged_1m_all.Rdata"))

toc()
