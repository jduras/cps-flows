

##### estimate linear probability models ####

message("Estimating linear probability models for transition from one labor force status to another")

if (!exists("df_lpm_data_occ")) load(file = str_c(edir_cps, "out_lpm_data_occ.Rdata"))

# df.tmp <-
#     df_lpm_data_occ %>%
#     filter(str_sub(cycle_id, 1, 1) == "E") %>%
#     mutate(dep = list(occ1 = list("RM", "RC")),
#            spec = modify_depth(dep, 2, ~formula(str_c("y_E_",.," ~ 1"))),
#            lpm_E = map2(data, spec, ~modify(.y, lm, data = .x))
#            )

# right hand side for linear probability models
# add: hwi, state_code, u3rate
spc_rhs <- c(U_E = "male + nwhite + married_1 + agecat_1 + educat_1 + udurcat_1 + month_1",
             E_U = "male + nwhite + married_1 + agecat_1 + educat_1 + month_1")

df_lpm_results_occ <- df_lpm_data_occ

rm(df_lpm_data_occ)

tic()

df_lpm_results_occ <-
    df_lpm_results_occ %>%
    nest(-cycle_id) %>%
    # filter(str_sub(cycle_id, 1, 1) == "E") %>%
    mutate(lpm_U_E_RM_RM = map(data, ~lm(paste("y_E_RM ~", spc_rhs["U_E"]), weight = weight_1, data = .x %>% filter(status_1 == "U_RM"))),
           lpm_U_E_RC_RC = map(data, ~lm(paste("y_E_RC ~", spc_rhs["U_E"]), weight = weight_1, data = .x %>% filter(status_1 == "U_RC"))),
           lpm_U_E_NRM_NRM = map(data, ~lm(paste("y_E_NRM ~", spc_rhs["U_E"]), weight = weight_1, data = .x %>% filter(status_1 == "U_NRM"))),
           lpm_U_E_NRC_NRC = map(data, ~lm(paste("y_E_NRC ~", spc_rhs["U_E"]), weight = weight_1, data = .x %>% filter(status_1 == "U_NRC"))),
           lpm_E_U_RM_RM = map(data, ~lm(paste("y_U_RM ~", spc_rhs["E_U"]), weight = weight_1, data = .x %>% filter(status_1 == "E_RM"))),
           lpm_E_U_RC_RC = map(data, ~lm(paste("y_U_RC ~", spc_rhs["E_U"]), weight = weight_1, data = .x %>% filter(status_1 == "E_RC"))),
           lpm_E_U_NRM_NRM = map(data, ~lm(paste("y_U_NRM ~", spc_rhs["E_U"]), weight = weight_1, data = .x %>% filter(status_1 == "E_NRM"))),
           lpm_E_U_NRC_NRC = map(data, ~lm(paste("y_U_NRC ~", spc_rhs["E_U"]), weight = weight_1, data = .x %>% filter(status_1 == "E_NRC")))) %>%
    select(-data)

toc()

# save(df_lpm_results_occ, file = str_c(edir_cps, "out_lpm_results_occ.Rdata"))



#### plot estimated coefficients with their confidence intervals ####

tic()

df_lpm_results_occ_coefs <-
    df_lpm_results_occ %>%
    gather(status_trans, lpm_model, starts_with("lpm_")) %>%
    mutate(status_trans = str_sub(status_trans, 5, -1),
           lpm_coefs = map(lpm_model, tidy, conf.int = TRUE)) %>%
    select(-lpm_model) %>%
    unnest() %>%
    mutate(term = if_else(str_sub(term, 1, 7) == "udurcat" & str_length(term) == 10,
                          str_c(str_sub(term, 1, 9),"0", str_sub(term, 10, 10)), term))

toc()

save(df_lpm_results_occ_coefs, file = str_c(edir_cps, "out_lpm_results_occ_coefs.Rdata"))

g <- ggplot() +
  geom_point(aes(x =  estimate, y = cycle_id, col = cycle_id)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = cycle_id, col = cycle_id, height = 0.2)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_grid(term ~ status_trans, as.table = FALSE)

g %+% {df_lpm_results_occ_coefs %>%
          # filter(str_sub(cycle_id, 1, 1) == "E") %>%
          # filter(cycle_id != "E2") %>%
          # filter(!(cycle_id %in% c("E6", "E6_since_2013"))) %>%
          filter(cycle_id == "E1" | str_sub(cycle_id, 1, 1) == "R") %>%
          # filter(str_sub(status_trans, 1, 3) == "E_U") %>%
          filter(str_sub(status_trans, 1, 3) == "U_E") %>%
          filter(str_sub(term, 1, 5) != "month") %>%
          filter(str_sub(term, 1, 7) != "udurcat")}
          # filter(term %in% c("(Intercept)", "married_1", "nwhite_1", "male_1"))}

g %+% {df_lpm_results_occ_coefs %>%
          # filter(str_sub(cycle_id, 1, 1) == "E") %>%
          # filter(cycle_id != "E2") %>%
          # filter(!(cycle_id %in% c("E6", "E6_since_2013"))) %>%
          filter(cycle_id == "E1" | str_sub(cycle_id, 1, 1) == "R") %>%
          # filter(str_sub(status_trans, 1, 3) == "E_U") %>%
          filter(str_sub(status_trans, 1, 3) == "U_E") %>%
          filter(str_sub(term, 1, 7) == "udurcat")}

