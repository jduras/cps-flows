
#### perform Oaxaca-Blinder decompositions ####

all.names <-
    df.lpm.results.occ$lpm.U.E.RM.RM[[1]] %>%
    model.matrix() %>%
    colnames()

agecat.1.names  <- all.names %>% str_subset(fixed("agecat.1"))
occ1cat.1.names <- all.names %>% str_subset(fixed("occ1cat.1"))
udurcat.1.names <- all.names %>% str_subset(fixed("udurcat.1"))
month.1.names   <- all.names %>% str_subset(fixed("month.1"))
states.names    <- all.names %>% str_subset(fixed("state"))

# grp.labs: U = list("grp.age", "grp.udur", "grp.seas", "grp.geo")
# grp.vars: U = list(agecat.1.names, udurcat.1.names, month.1.names, states.names)

grp.labs.lfs <- list(U = list("grp.age", "grp.udur", "grp.seas"),
                     I = list("grp.age", "grp.seas"),
                     E = list("grp.age", "grp.seas"))

grp.vars.lfs <- list(U = list(agecat.1.names, udurcat.1.names, month.1.names),
                     I = list(agecat.1.names, month.1.names),
                     E = list(agecat.1.names, month.1.names))

cycle.id.baseline.occ <- c(RM = "E1", RC = "E3", NRM = "E1", NRC = "E3")

# helper function for Oaxaca-Blinder decomposition used below in pmap
lpm.oaxacablinder <- function(status.trans, lpm.model.baseline, lpm.model, cycle.id.baseline, cycle.id, grp.labs =  NULL, grp.vars = NULL) {
    str_pad(status.trans, 11, side = "right") %>%
        str_c(":", cycle.id.baseline, "vs.",cycle.id, sep = " ") %>% print()
    if (cycle.id == cycle.id.baseline) return(NA)
    oaxacablinder(lpm.model.baseline, lpm.model,
                  model.names = c(cycle.id.baseline, cycle.id),
                  groups.labels = grp.labs, groups.vars = grp.vars)
}

# perform Oaxaca-Blinder decomposition
print("Performing Oaxaca-Blinder decompositions")

df.lpm.ob.occ <-
    df.lpm.results.occ %>%
    # filter((cycle.id %in% c("E1", "E3", "E6"))) %>%
    filter(!(cycle.id %in% c("R1", "R3", "R4", "E2"))) %>%
    gather(status.trans, lpm.model, -cycle.id) %>%
    mutate(status.trans = str_sub(status.trans, 5, -1)) %>%
    separate(status.trans, into = c("lfs.1", "lfs.2", "occ1cat.1", "occ1cat.2"), sep = "\\.", remove = FALSE) %>%
    group_by(status.trans) %>%
    mutate(cycle.id.baseline = cycle.id.baseline.occ[occ1cat.1],
           grp.labs = map(lfs.1, ~grp.labs.lfs[[.]]),
           grp.vars = map(lfs.1, ~grp.vars.lfs[[.]]),
           ob = pmap(list(status.trans,
                          lpm.model[cycle.id == cycle.id.baseline], lpm.model,
                          cycle.id.baseline, cycle.id,
                          grp.labs, grp.vars),
                     lpm.oaxacablinder)) %>%
    ungroup() %>%
    filter(cycle.id != cycle.id.baseline) %>%
    select(cycle.id, cycle.id.baseline, status.trans, ob)

save(df.lpm.ob.occ, file = paste0(edir.cps, "lpm_ob_occ.Rdata"))
# load(file = paste0(edir.cps, "lpm_ob_occ.Rdata"))



#### plot the results of the Oaxaca-Blinder decompositions ####

#  U.RM -> E.RM, E3-E6 vs E1
#  U.RC -> E.RC, E4-E6 vs E3


df.lpm.ob.occ.tidy <-
    df.lpm.ob.occ %>%
    mutate(ob = map(ob, tidy.oaxacablinder)) %>%
    unnest() %>%
    separate(term, into = c("component", "variable"), sep = "\\.", extra = "merge", fill = "right") %>%
    filter(component %in% c("explained", "unexplained")) %>%
    mutate(variable = if_else(str_sub(variable, 1, 7) == "udurcat" & str_length(variable) == 10, paste0(str_sub(variable, 1, 9), "0", str_sub(variable, 10, 10)), variable)) %>%
    mutate(variable = if_else(variable == "total", " total", variable))

df.lpm.ob.occ.tidy %>%
    ggplot(aes(x = estimate, y = variable, xmin = conf.low, xmax = conf.high, height = 0.2)) +
    geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(title = "Oaxaca-Blinder decomposition",
             x = "", y = "") +
        facet_grid(cycle.id ~ status.trans + component, scales = "free_x")

chosen.decomposition <- "U.E"

df.lpm.ob.occ.tidy %>%
    filter(str_detect(status.trans, pattern = fixed(chosen.decomposition))) %>%
    filter(str_sub(variable, 1, 5) != "month") %>%
    filter(str_sub(variable, 1, 5) != "state") %>%
    filter(str_sub(variable, 1, 7) != "udurcat") %>%
    ggplot(aes(x = estimate, y = variable, xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen.decomposition),
             x = "", y = "") +
        facet_grid(cycle.id ~  status.trans + component, scales = "free_x")

df.lpm.ob.occ.tidy %>%
    filter(str_detect(status.trans, pattern = fixed(chosen.decomposition))) %>%
    filter(component == "explained") %>%
    filter(str_sub(variable, 1, 7) == "udurcat" | variable == " total") %>%
    ggplot(aes(x = estimate, y = variable, xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen.decomposition),
             x = "", y = "") +
        facet_grid(cycle.id ~ status.trans + component, scales = "free_x")

df.lpm.ob.occ.tidy %>%
    filter(str_detect(status.trans, pattern = fixed(chosen.decomposition))) %>%
    filter(component == "explained") %>%
    filter(str_sub(variable, 1, 7) == "udurcat" | variable == " total") %>%
    ggplot(aes(x = estimate, y = cycle.id, col = cycle.id, xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(title = paste("Oaxaca-Blinder decomposition for", chosen.decomposition),
             x = "", y = "") +
        facet_grid(variable ~ status.trans + component, scales = "free_x")
