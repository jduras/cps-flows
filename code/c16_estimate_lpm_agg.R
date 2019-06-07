
##### estimate linear probability models ####

message("Estimating linear probability models for transition from one labor force status to another")

if (!exists("df_lpm_data_agg")) load(file = str_c(edir_cps, "out_lpm_data_agg.Rdata"))

if (!exists("df_lpm_lresults_agg")) {
    df_lpm_data_agg <-
        df_lpm_data_agg %>%
        select(-period_1, -occ1cat_2)
}

tic()

df_lpm_lresults_agg <-
    df_lpm_data_agg %>%
    nest(-cycle_id) %>%
    # filter(str_sub(cycle_id, 1, 1) == "E")
    mutate(lpm_EU = map(data, ~lm(y_U ~ male + nwhite + married_1 + agecat_1 + educat_1 + occ1cat_1 + month_1,
                                  weight = weight_1, data = .x %>% filter(lfs_1 == "E"))),
           lpm_UE = map(data, ~lm(y_E ~ male + nwhite + married_1 + agecat_1 + educat_1 + occ1cat_1 + udurcat_1 + month_1,
                                  weight = weight_1, data = .x %>% filter(lfs_1 == "U"))),
           lpm_IE = map(data, ~lm(y_E ~ male + nwhite + married_1 + agecat_1 + educat_1 + month_1,
                                  weight = weight_1, data = .x %>% filter(lfs_1 == "I")))) %>%
    select(-data)

toc()



tic()

df_lpm_lresults_agg <-
    df_lpm_data_agg %>%
    nest(-cycle_id, -lfs_1) %>%
    arrange(cycle_id, lfs_1) %>%
    # filter(str_sub(cycle_id, 1, 1) == "E") %>%
    mutate(spc = recode(lfs_1,
                        "U" = "y_E ~ male + nwhite + married_1 + agecat_1 + educat_1 + occ1cat_1 + udurcat_1 + month_1",
                        "I" = "y_E ~ male + nwhite + married_1 + agecat_1 + educat_1 + month_1",
                        "E" = "y_U ~ male + nwhite + married_1 + agecat_1 + educat_1 + occ1cat_1 + month_1"),
           lpm = map2(data, spc, ~lm(.y, weight = weight_1, data = .x))) %>%
    select(-data)

toc()

# save(df_lpm_lresults_agg, file = str_c(edir_cps, "out_lpm_results_agg.Rdata"))



#### plot estimated coefficients with their confidence intervals ####

tic()

df_lpm_lresults_agg_coefs <-
    df_lpm_lresults_agg %>%
    gather(lfs_trans, lpm_model, starts_with("lpm_")) %>%
    mutate(lfs_trans = str_sub(lfs_trans, 5, 6),
           lpm_coefs = map(lpm_model, tidy, conf.int = TRUE)) %>%
    select(-lpm_model) %>%
    unnest() %>%
    mutate(term = if_else(str_sub(term, 1, 7) == "udurcat" & str_length(term) == 10,
                          str_c(str_sub(term, 1, 9), "0", str_sub(term, 10, 10)), term))

toc()

save(df_lpm_lresults_agg_coefs, file = str_c(edir_cps, "out_lpm_results_agg_coefs.Rdata"))

g <- ggplot() +
        geom_point(aes(x =  estimate, y = cycle_id, col = cycle_id)) +
        geom_errorbarh(aes(y = cycle_id, xmin = conf.low, xmax = conf.high, col = cycle_id, height = 0.2)) +
        geom_vline(xintercept = 0, linetype = "dotted") +
        facet_grid(term ~ lfs_trans, as.table = FALSE)

g %+% {df_lpm_lresults_agg_coefs %>%
        filter(str_sub(cycle_id, 1, 1) == "E") %>%
        filter(cycle_id != "E2") %>%
        filter(term %in% c("(Intercept)", "married_1", "nwhite", "male"))}

g %+% {df_lpm_lresults_agg_coefs %>%
            filter(str_sub(cycle_id, 1, 1) == "E") %>%
            filter(cycle_id != "E2") %>%
            filter(str_sub(term, 1, 5) != "month") %>%
            filter(str_sub(term, 1, 7) != "occ1cat") %>%
            filter(str_sub(term, 1, 7) != "udurcat") %>%
            filter(term != "(Intercept)")}

g %+% {df_lpm_lresults_agg_coefs %>%
            filter(str_sub(cycle_id, 1, 1) == "E") %>%
            filter(cycle_id != "E2") %>%
            filter(str_sub(term, 1, 7) == "occ1cat")}

g %+% {df_lpm_lresults_agg_coefs %>%
            filter(str_sub(cycle_id, 1, 1) == "E") %>%
            filter(cycle_id != "E2") %>%
            filter(str_sub(term, 1, 7) == "udurcat")}
