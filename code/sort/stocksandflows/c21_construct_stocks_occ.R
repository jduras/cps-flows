
#### construct stocks and shares, by labor force status and occupation group ####

# merged CPS data from merge_1m.R
load(file = paste0(edir.cps, "merged_1m_all.Rdata"))


# construct descriptive statistics in Table 1 from Cortes, Jaimovich, Nekarda, Siu
df.table.1 <-
    df.merged.1m.all.sample %>%
    # filter(period >= 197601 & period <= 198912) %>%
    filter(period >= 199001 & period <= 201212) %>%
    mutate(hsd = (educ==1) %>% as.numeric(),
           hsg = (educ %in% c(2:3)) %>% as.numeric(),
           clp = (educ %in% c(4:5)) %>% as.numeric(),
           lfs.occ1cat = paste0(lfs,"_",occ1cat),
           nwhite = if_else(white == 0, 1, 0)) %>%
    select(lfs.occ1cat, age, hsd, hsg, clp, female, nwhite, married, weight) %>%
    { bind_rows(nest(mutate(., lfs.occ1cat = "all"), -lfs.occ1cat),
                nest(., -lfs.occ1cat)) } %>%
    arrange(lfs.occ1cat) %>%
    mutate(mean.age = map_dbl(data, ~wtd.mean(.x$age, weights = .x$weight, na.rm = TRUE)),
           mean.edu.1.hsd = map_dbl(data, ~wtd.mean(.x$hsd, weights = .x$weight, na.rm = TRUE)),
           mean.edu.2.hsg = map_dbl(data, ~wtd.mean(.x$hsg, weights = .x$weight, na.rm = TRUE)),
           mean.edu.3.clp = map_dbl(data, ~wtd.mean(.x$clp, weights = .x$weight, na.rm = TRUE)),
           mean.female = map_dbl(data, ~wtd.mean(.x$female, weights = .x$weight, na.rm = TRUE)),
           mean.nwhite = map_dbl(data, ~wtd.mean(.x$nwhite, weights = .x$weight, na.rm = TRUE)),
           mean.married = map_dbl(data, ~wtd.mean(.x$married, weights = .x$weight, na.rm = TRUE)),
           nobs = map_dbl(data, ~nrow(.x))) %>%
    select(-data)

df.table.1 %>%
    gather(measure, value, -lfs.occ1cat) %>%
    spread(lfs.occ1cat, value) %>%
    select(measure, all, E_NRC, E_RC, E_RM, E_NRM, I_X)


# plot share of those not in labor force, by occupation group
# (in Cortes, Jaimovich, Nekarda, Siu all workers with lfs = I are considered as occ1cat = X)
df.merged.1m.all %>%
    select(period, lfs, occ1cat.all, weight) %>%
    filter(lfs == "I") %>%
    filter(!(occ1cat.all %in% c("FRM", "MIL"))) %>%
    group_by(period, occ1cat.all) %>%
    summarise(s = sum(weight)) %>%
    group_by(period) %>%
    mutate(shr = s / sum(s)) %>%
    ungroup() %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon(format = "%Y%m")) %>%
    ggplot(aes(x = monyear, y = shr, col = occ1cat.all)) +
        geom_line() +
        scale_x_yearmon() +
        scale_y_continuous(labels = percent) +
        scale_color_discrete(labels = c("non-routine cognitive", "non-routine manual", "routine cognitive", "routine manual", "not available")) +
        labs(title="Share of Not in Labor Force, by Occupation Group",
             x="", y="Share of Not in Labor Force",
             col="Occupation Group") +
        facet_grid(occ1cat.all~ ., scales = "free_y", switch = "y") +
        theme(strip.placement = "outside")


# construct shares of population in different labor force and occupation groups
df.stocksandshares.cps.occ.whole.sample <-
    df.merged.1m.all %>%
    group_by(period, lfs, occ1cat) %>%
    summarise(seas = "NSA",
              s.occ = sum(weight)) %>%
    group_by(period, lfs) %>%
    mutate(shr.occ2lfs = s.occ / sum(s.occ)) %>%
    group_by(period) %>%
    mutate(shr.occ2pop = s.occ / sum(s.occ)) %>%
    ungroup()

df.stocksandshares.cps.occ <-
    df.merged.1m.all.sample %>%
    group_by(period, lfs, occ1cat) %>%
    summarise(seas = "NSA",
              s.occ = sum(weight)) %>%
    group_by(period, lfs) %>%
    mutate(shr.occ2lfs = s.occ / sum(s.occ)) %>%
    group_by(period) %>%
    mutate(shr.occ2pop = s.occ / sum(s.occ)) %>%
    ungroup()

df.stocksandshares.cps.occ.combined <-
    bind_rows(restricted = df.stocksandshares.cps.occ,
              whole = df.stocksandshares.cps.occ.whole.sample,
              .id = "sample") %>%
    gather(measure, y, c(s.occ, shr.occ2lfs, shr.occ2pop)) %>%
    nest(c(period, y)) %>%
    sa.SSM() %>%
    unnest() %>%
    rename(NSA = y,
           SA = y.KS) %>%
    gather(seas, value, c(SA, NSA)) %>%
    spread(measure, value)


# plot shares of population in different labor force and occupation groups
df.stocksandshares.cps.occ %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon(format = "%Y%m")) %>%
    ggplot() +
        geom_line(aes(x = monyear, y = shr.occ2pop)) +
        geom_rect(data = rec.dates %>% filter(Start > "Jan 1976"), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf), alpha = 0.1) +
        scale_x_yearmon() +
        scale_y_continuous(labels = percent) +
        labs(x = "", y = "Share of population") +
        facet_grid(lfs ~ occ1cat, scales = "free_y")


# unemployment rate and population shares by occupation group
df.pop.shares.cps.occ.combined <-
    df.stocksandshares.cps.occ.combined %>%
    filter(lfs != "M") %>%
    select(sample, period, lfs, occ1cat, s.occ, shr.occ2pop) %>%
    gather(measure, value, c(s.occ, shr.occ2pop)) %>%
    unite(measure, measure, lfs, sep = ".") %>%
    spread(measure, value) %>%
    group_by(sample, period, occ1cat) %>%
    mutate(ur3 = s.occ.U / (s.occ.U + s.occ.E),
           shr.occ2pop.E.plus.U = shr.occ2pop.E + shr.occ2pop.U) %>%
    group_by(sample, occ1cat) %>%
    mutate(ur3.index.200712 = case_when(period >= 200712 ~ ur3 / ur3[period == 200712],
                                       TRUE              ~ NA_real_),
           ur3.change.200712 = ur3 - ur3[period == 200712]) %>%
    ungroup() %>%
    select(sample, period, occ1cat, shr.occ2pop.E, shr.occ2pop.U, shr.occ2pop.E.plus.U, ur3, ur3.index.200712, ur3.change.200712) %>%
    gather(measure, y, -c(sample, period, occ1cat)) %>%
    nest(c(period, y)) %>%
    sa.SSM() %>%
    unnest() %>%
    rename(NSA = y,
           SA = y.KS) %>%
    mutate(SA = if_else(measure %in% c("ur3.index.200712", "ur3.change.200712") & period < 200712, NA_real_, SA)) %>%
    gather(seas, value, c(SA, NSA))

chosen.seas <- "SA"
df.pop.shares.cps.occ.combined %>%
    filter(occ1cat %in% c("NRC", "NRM", "RC", "RM")) %>%
    filter(seas == chosen.seas) %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon(format = "%Y%m"),
           measure.label = case_when(measure == "shr.occ2pop.E.plus.U" ~ "Population Share: Employed plus Unemployed",
                                     measure == "shr.occ2pop.E"        ~ "Population Share: Employed",
                                     measure == "shr.occ2pop.U"        ~ "Population Share: Unemployed",
                                     measure == "ur3"                  ~ "Unemployment Rate",
                                     measure == "ur3.index.200712"     ~ "Unemployment Rate, index Dec 2007=100",
                                     measure == "ur3.change.200712"    ~ "Unemployment Rate, change from Dec 2007")) %>%
    ggplot() +
        geom_line(aes(x = monyear, y = value, color = occ1cat, linetype = sample)) +
        geom_rect(data = rec.dates %>% filter(Start > "Jan 1976"), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf), alpha = 0.1) +
        scale_x_yearmon() +
        scale_color_discrete(labels = c("non-routine cognitive", "non-routine manual", "routine cognitive", "routine manual"))+
        labs(x = "", y = "", title = paste0("Employment and Unemployment by Occupation Group (", chosen.seas, ")"), color = "occupation group") +
        facet_wrap(~ measure.label, ncol = 3, scales = "free_y")

# unemployment share by occupation group
df.uande.shares.cps.occ.combined <-
    df.stocksandshares.cps.occ.combined %>%
    filter(lfs %in% c("E", "U")) %>%
    select(sample, period, lfs, occ1cat, shr.occ2lfs) %>%
    rename(y = shr.occ2lfs) %>%
    nest(c(period, y)) %>%
    sa.SSM() %>%
    unnest() %>%
    rename(NSA = y,
           SA = y.KS) %>%
    gather(seas, value, c(SA, NSA))

chosen.seas <- "SA"
df.uande.shares.cps.occ.combined %>%
    filter(occ1cat %in% c("NRC", "NRM", "RC", "RM")) %>%
    filter(seas == chosen.seas) %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon(format = "%Y%m")) %>%
    ggplot() +
        geom_line(aes(x = monyear, y = value, color = occ1cat, linetype = sample)) +
        geom_rect(data = rec.dates %>% filter(Start > "Jan 1976"), aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf), alpha = 0.1) +
        scale_x_yearmon() +
        scale_y_continuous(labels = percent) +
        scale_color_discrete(labels = c("non-routine cognitive", "non-routine manual", "routine cognitive", "routine manual", "not available")) +
        labs(x = "", y = "", title = paste0("Shares of occupation groups within each labor force status (", chosen.seas, ")"), color = "occupation group") +
        facet_grid(~ lfs)

save(df.pop.shares.cps.occ.combined, df.uande.shares.cps.occ.combined,
     df.stocksandshares.cps.occ, df.stocksandshares.cps.occ.whole.sample, df.stocksandshares.cps.occ.combined,
     file = paste0(edir.cps, "stocksandshares_occ.Rdata"))

# load(file = paste0(edir.cps, "stocksandshares_occ.Rdata"))
