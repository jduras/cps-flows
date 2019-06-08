
#### perform Oaxaca-Blinder decompositions ####

message("Performing Oaxaca-Blinder decompositions for transition from one labor force status to another")

if (!exists("df_lpm_results_occ")) load(file = str_c(edir_cps, "out_lpm_results_occ.Rdata"))

all_names <-
    df_lpm_results_occ %>%
    pull(lpm_U_E_RM_RM) %>%
    pluck(1) %>%
    coefficients() %>%
    names()

agecat_1_names  <- all_names %>% str_subset("agecat_1")
occ1cat_1_names <- all_names %>% str_subset("occ1cat_1")
udurcat_1_names <- all_names %>% str_subset("udurcat_1")
month_1_names   <- all_names %>% str_subset("month_1")
states_names    <- all_names %>% str_subset("state")

# grp_labs: U = list("grp_age", "grp_udur", "grp_seas", "grp_geo")
# grp_vars: U = list(agecat_1_names, udurcat_1_names, month_1_names, states_names)

grp_labs_lfs <- list(U = list("grp_age", "grp_udur", "grp_seas"),
                     I = list("grp_age", "grp_seas"),
                     E = list("grp_age", "grp_seas"))

grp_vars_lfs <- list(U = list(agecat_1_names, udurcat_1_names, month_1_names),
                     I = list(agecat_1_names, month_1_names),
                     E = list(agecat_1_names, month_1_names))

cycle_id_baseline_occ <- c(RM = "E1", RC = "E3", NRM = "E1", NRC = "E3")

# helper function for Oaxaca-Blinder decomposition used below in pmap
lpm_oaxacablinder <-
    function(status_trans, lpm_model_baseline, lpm_model, cycle_id_baseline, cycle_id, grp_labs =  NULL, grp_vars = NULL) {
        str_pad(status_trans, 11, side = "right") %>%
            str_c(":", cycle_id_baseline, "vs.",cycle_id, sep = " ") %>%
            print()

        if (cycle_id == cycle_id_baseline) return(NA)

        oaxacablinder(lpm_model_baseline, lpm_model, model_names = c(cycle_id_baseline, cycle_id), groups_labels = grp_labs, groups_vars = grp_vars)
        }

# perform Oaxaca-Blinder decomposition
tic()

df_lpm_ob_occ <-
    df_lpm_results_occ %>%
    # filter((cycle_id %in% c("E1", "E3", "E6"))) %>%
    filter(!(cycle_id %in% c("R1", "R3", "R4", "E2"))) %>%
    gather(status_trans, lpm_model, -cycle_id) %>%
    mutate(status_trans = str_sub(status_trans, 5, -1)) %>%
    separate(status_trans, into = c("lfs_1", "lfs_2", "occ1cat_1", "occ1cat_2"), sep = "_", remove = FALSE) %>%
    group_by(status_trans) %>%
    mutate(cycle_id_baseline = cycle_id_baseline_occ[occ1cat_1],
           grp_labs = map(lfs_1, ~grp_labs_lfs[[.]]),
           grp_vars = map(lfs_1, ~grp_vars_lfs[[.]]),
           ob = pmap(list(status_trans,
                          lpm_model[cycle_id == cycle_id_baseline], lpm_model,
                          cycle_id_baseline, cycle_id,
                          grp_labs, grp_vars),
                     lpm_oaxacablinder)) %>%
    ungroup() %>%
    filter(cycle_id != cycle_id_baseline) %>%
    select(cycle_id, cycle_id_baseline, status_trans, ob)

toc()

save(df_lpm_ob_occ, file = str_c(edir_cps, "out_lpm_ob_occ.Rdata"))
# load(file = str_c(edir_cps, "out_lpm_ob_occ.Rdata"))



#### plot the results of the Oaxaca-Blinder decompositions ####

#  U_RM -> E_RM, E3-E6 vs E1
#  U_RC -> E_RC, E4-E6 vs E3


df_lpm_ob_occ_tidy <-
    df_lpm_ob_occ %>%
    mutate(ob = map(ob, tidy.oaxacablinder)) %>%
    unnest() %>%
    separate(term, into = c("component", "variable"), sep = "\\.", extra = "merge", fill = "right") %>%
    filter(component %in% c("explained", "unexplained")) %>%
    mutate(variable = if_else(str_sub(variable, 1, 7) == "udurcat" & str_length(variable) == 10,
                              str_c(str_sub(variable, 1, 9), "0",
                                    str_sub(variable, 10, 10)), variable)) %>%
    mutate(variable = if_else(variable == "total", " total", variable))

df_lpm_ob_occ_tidy %>%
    ggplot(aes(x = estimate, y = variable, xmin = conf.low, xmax = conf.high, height = 0.2)) +
    geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(title = "Oaxaca-Blinder decomposition",
             x = "", y = "") +
        facet_grid(cycle_id ~ status_trans + component, scales = "free_x")

chosen_decomposition <- "U_E"

df_lpm_ob_occ_tidy %>%
    filter(str_detect(status_trans, pattern = fixed(chosen_decomposition))) %>%
    filter(str_sub(variable, 1, 5) != "month") %>%
    filter(str_sub(variable, 1, 5) != "state") %>%
    filter(str_sub(variable, 1, 7) != "udurcat") %>%
    ggplot(aes(x = estimate, y = variable, xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen_decomposition),
             x = "", y = "") +
        facet_grid(cycle_id ~  status_trans + component, scales = "free_x")

df_lpm_ob_occ_tidy %>%
    filter(str_detect(status_trans, pattern = fixed(chosen_decomposition))) %>%
    filter(component == "explained") %>%
    filter(str_sub(variable, 1, 7) == "udurcat" | variable == " total") %>%
    ggplot(aes(x = estimate, y = variable, xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen_decomposition),
             x = "", y = "") +
        facet_grid(cycle_id ~ status_trans + component, scales = "free_x")

df_lpm_ob_occ_tidy %>%
    filter(str_detect(status_trans, pattern = fixed(chosen_decomposition))) %>%
    filter(component == "explained") %>%
    filter(str_sub(variable, 1, 7) == "udurcat" | variable == " total") %>%
    ggplot(aes(x = estimate, y = cycle_id, col = cycle_id, xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen_decomposition),
             x = "", y = "") +
        facet_grid(variable ~ status_trans + component, scales = "free_x")
