
tic()

message("Matching and merging observations for two consecutive months")

breaks <- c(197801, 198507, 198510, 199401, 199506, 199507, 199508, 199509)

future_walk(
    .progress = TRUE,
    .x = tseq[-1],
     .f = function(t2 = .x) {

         t1 <- if_else(t2 %% 100 == 01, t2 - 89, t2 - 1)

         if (!(t2 %in% breaks)) {

             # load data for month t1
             load(file = str_c(edir_cps, "cpsb_", t1, ".Rdata"))

             df_cpsdata_1 <-
                 df_cpsdata %>%
                 filter(!(mis %in% c(4, 8))) %>%
                 mutate(mis_in_1 = mis,
                        age_in_1 = age)

             # load data for month t2
             load(file = str_c(edir_cps, "cpsb_", t2, ".Rdata"))

             df_cpsdata_2 <-
                 df_cpsdata %>%
                 filter(!(mis %in% c(1, 5))) %>%
                 mutate(mis_in_1 = mis - 1,
                        age_in_1 = age)

             # First round of matching records - people whose age did not change between the two month
             # find all individuals, matched if they agree on
             #	gestcen, hid, pid, race, female, age

             df_merged_2m_part1 <-
                 inner_join(df_cpsdata_1, df_cpsdata_2,
                            by = c("gestcen", "hid", "pid", "white", "black", "female", "age_in_1", "mis_in_1"),
                            suffix = c("_1", "_2"))

             # Second round of matching records - people whose age did change by 1 between the two month

             df_unmatched_1 <-
                 anti_join(df_cpsdata_1, df_cpsdata_2,
                           by = c("gestcen", "hid", "pid", "white", "black", "female", "age_in_1", "mis_in_1"))

             df_cpsdata_2 %<>%
                 mutate(age_in_1 = age_in_1 - 1)

             # find all individuals, matched if they agree on
             #	gestcen, hid, pid, race, female, and age in t2 = age in t1 + 1

             df_merged_2m_part2 <-
                 inner_join(df_unmatched_1, df_cpsdata_2,
                            by = c("gestcen", "hid", "pid", "white", "black", "female", "age_in_1", "mis_in_1"),
                            suffix = c("_1", "_2"))

             # create combined data set with observations from both rounds of matching
             df_merged_2m <-
                 bind_rows(df_merged_2m_part1, df_merged_2m_part2) %>%
                 select(-c(mis_in_1, age_in_1))

             save(df_merged_2m, file = str_c(edir_cps, "merged_2m_", t1, ".Rdata"))
             rm(df_cpsdata, df_cpsdata_1, df_cpsdata_2, df_unmatched_1, df_merged_2m_part1, df_merged_2m_part2, df_merged_2m)

         }

     }

)

toc()
