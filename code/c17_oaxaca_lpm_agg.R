
#### perform Oaxaca-Blinder decompositions ####

message("Performing Oaxaca-Blinder decompositions for transition from one labor force status to another")

if (!exists("df_lpm_lresults_agg")) load(file = str_c(edir_cps, "out_lpm_results_agg.Rdata"))

all_names <-
    df_lpm_lresults_agg %>%
    pull(lpm_UE) %>%
    pluck(1) %>%
    coefficients() %>%
    names()

agecat_1_names  <- all_names %>% str_subset("agecat_1")
occ1cat_1_names <- all_names %>% str_subset("occ1cat_1")
udurcat_1_names <- all_names %>% str_subset("udurcat_1")
month_1_names   <- all_names %>% str_subset("month_1")

grp_labs_U <- list("grp_age", "grp_occ", "grp_udur", "grp_seas")
grp_vars_U <- list(agecat_1_names, occ1cat_1_names, udurcat_1_names, month_1_names)

grp_labs_I <- list("grp_age","grp_seas")
grp_vars_I <- list(agecat_1_names, month_1_names)

grp_labs_E <- list("grp_age", "grp_occ", "grp_seas")
grp_vars_E <- list(agecat_1_names, str_subset(occ1cat_1_names, "occ1cat_1X", negate = TRUE), month_1_names)

tic()

# perform Oaxaca-Blinder decompositions
df_lpm_ob_agg <-
    df_lpm_lresults_agg %>%
    filter(!(cycle_id %in% c("R1", "R3", "R4", "E2"))) %>%
    mutate(ob_UE = pmap(list(lpm_UE[cycle_id == "E1"], lpm_UE, cycle_id),
                            ~oaxacablinder(..1, ..2, model_names = c("E1", ..3), groups_labels = grp_labs_U, groups_vars = grp_vars_U)),
           ob_IE = pmap(list(lpm_IE[cycle_id == "E1"], lpm_IE, cycle_id),
                            ~oaxacablinder(..1, ..2, model_names = c("E1", ..3), groups_labels = grp_labs_I, groups_vars = grp_vars_I)),
           ob_EU = pmap(list(lpm_EU[cycle_id == "E1"], lpm_EU, cycle_id),
                            ~oaxacablinder(..1, ..2, model_names = c("E1", ..3), groups_labels = grp_labs_E, groups_vars = grp_vars_E))) %>%
    filter(cycle_id != "E1") %>%
    select(cycle_id, starts_with("ob_")) %>%
    gather(lfs_trans, ob, -cycle_id) %>%
    mutate(lfs_trans = str_sub(lfs_trans, 4, 5))

toc()

save(df_lpm_ob_agg, file = str_c(edir_cps, "out_lpm_ob_agg.Rdata"))
# load(file = str_c(edir_cps, "out_lpm_ob_agg.Rdata"))



#### plot the results of the Oaxaca-Blinder decompositions ####

df_lpm_ob_agg_tidy <-
    df_lpm_ob_agg %>%
    mutate(ob = map(ob, tidy.oaxacablinder)) %>%
    unnest() %>%
    separate(term, into = c("component", "variable"), sep = "\\.", extra = "merge", fill = "right") %>%
    filter(component %in% c("explained", "unexplained")) %>%
    mutate(variable = if_else(str_sub(variable, 1, 7) == "udurcat" & str_length(variable) == 10, str_c(str_sub(variable, 1, 9), "0", str_sub(variable, 10, 10)), variable)) %>%
    mutate(variable = if_else(variable == "total", " total", variable))

g <- ggplot() +
    geom_point(aes(x = estimate, y = variable)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = variable, height = 0.2)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    labs(x = "", y = "") +
    facet_grid(cycle_id ~ component, scales = "free_x")

chosen_decomposition <- "UE"

g %+% {df_lpm_ob_agg_tidy %>%
        filter(lfs_trans == chosen_decomposition) %>%
        filter(cycle_id %in% c("R5", "E6")) %>%
        filter(str_sub(variable, 1, 5) != "month") %>%
        filter(str_sub(variable, 1, 5) != "state") %>%
        filter(str_sub(variable, 1, 7) != "udurcat")} +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen_decomposition))

chosen_decomposition <- "IE"

g %+% {df_lpm_ob_agg_tidy %>%
        filter(lfs_trans == chosen_decomposition) %>%
        filter(cycle_id %in% c("R5", "E6")) %>%
        filter(str_sub(variable, 1, 5) != "month") %>%
        filter(str_sub(variable, 1, 5) != "state") %>%
        filter(str_sub(variable, 1, 7) != "udurcat")} +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen_decomposition))

chosen_decomposition <- "EU"

g %+% {df_lpm_ob_agg_tidy %>%
        filter(lfs_trans == chosen_decomposition) %>%
        filter(cycle_id %in% c("R5", "E6")) %>%
        filter(str_sub(variable, 1, 5) != "month") %>%
        filter(str_sub(variable, 1, 5) != "state") %>%
        filter(str_sub(variable, 1, 7) != "udurcat")} +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen_decomposition))

