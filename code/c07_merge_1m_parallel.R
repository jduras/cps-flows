
tic()

message("Merging observations from all months into one dataset")

df_merged_1m_all <-
    future_map(
        .progress = TRUE,
        .x = tseq,
        .f = function(t = .x) {
            load(file = str_c(edir_cps, "cpsb_", t, ".Rdata"))

            df_cpsdata %>%
                filter(age >= 16) %>%
                filter(!(occ1cat == "MIL"))
        })

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
