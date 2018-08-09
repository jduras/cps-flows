
#### perform Oaxaca-Blinder decompositions ####

all.names <-
    df.lpm.lresults.agg$lpm.EU[[1]] %>%
    model.matrix() %>%
    colnames()

agecat.1.names  <- all.names %>% str_subset(fixed("agecat.1"))
occ1cat.1.names <- all.names %>% str_subset(fixed("occ1cat.1"))
udurcat.1.names <- all.names %>% str_subset(fixed("udurcat.1"))
month.1.names   <- all.names %>% str_subset(fixed("month.1"))

grp.labs.U <- list("grp.age", "grp.occ", "grp.udur", "grp.seas")
grp.vars.U <- list(agecat.1.names, occ1cat.1.names, udurcat.1.names, month.1.names)

grp.labs.I <- list("grp.age","grp.seas")
grp.vars.I <- list(agecat.1.names, month.1.names)

grp.labs.E <- list("grp.age", "grp.occ", "grp.seas")
grp.vars.E <- list(agecat.1.names, occ1cat.1.names, month.1.names)

# perform Oaxaca-Blinder decompositions
df.lpm.ob.agg <-
    df.lpm.lresults.agg %>%
    filter(!(cycle.id %in% c("R1", "R3", "R4", "E2"))) %>%
    mutate(ob.UE = pmap(list(lpm.UE[cycle.id == "E1"], lpm.UE, cycle.id),
                            ~oaxacablinder(..1, ..2, model.names = c("E1", ..3), groups.labels = grp.labs.U, groups.vars = grp.vars.U)),
           ob.IE = pmap(list(lpm.IE[cycle.id == "E1"], lpm.IE, cycle.id),
                            ~oaxacablinder(..1, ..2, model.names = c("E1", ..3), groups.labels = grp.labs.I, groups.vars = grp.vars.I)),
           ob.EU = pmap(list(lpm.EU[cycle.id == "E1"], lpm.EU, cycle.id),
                            ~oaxacablinder(..1, ..2, model.names = c("E1", ..3), groups.labels = grp.labs.E, groups.vars = grp.vars.E))) %>%
    filter(cycle.id != "E1") %>%
    select(cycle.id, starts_with("ob.")) %>%
    gather(lfs.trans, ob, -cycle.id) %>%
    mutate(lfs.trans = str_sub(lfs.trans, 4, 5))

save(df.lpm.ob.agg, file = paste0(edir.cps, "lpm_ob_agg.Rdata"))
# load(file = paste0(edir.cps, "lpm_ob_agg.Rdata"))



#### plot the results of the Oaxaca-Blinder decompositions ####

df.lpm.ob.agg.tidy <-
    df.lpm.ob.agg %>%
    mutate(ob = map(ob, tidy.oaxacablinder)) %>%
    unnest() %>%
    separate(term, into = c("component", "variable"), sep = "\\.", extra = "merge", fill = "right") %>%
    filter(component %in% c("explained", "unexplained")) %>%
    mutate(variable = if_else(str_sub(variable, 1, 7) == "udurcat" & str_length(variable) == 10, paste0(str_sub(variable, 1, 9), "0", str_sub(variable, 10, 10)), variable)) %>%
    mutate(variable = if_else(variable == "total", " total", variable))

chosen.decomposition <- "UE"

df.lpm.ob.agg.tidy %>%
    filter(lfs.trans == chosen.decomposition) %>%
    filter(cycle.id %in% c("R5", "E6")) %>%
    filter(str_sub(variable, 1, 5) != "month") %>%
    filter(str_sub(variable, 1, 5) != "state") %>%
    filter(str_sub(variable, 1, 7) != "udurcat") %>%
    ggplot(aes(x = estimate, y = variable, xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen.decomposition),
             x = "", y = "") +
        facet_grid(cycle.id ~ component, scales = "free_x")
