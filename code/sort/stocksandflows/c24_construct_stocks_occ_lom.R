
# construct lfs population shares for counterfactual transition rates using law of motion

# population shares from construct_stocks_occ.R
load(file = paste0(edir.cps, "stocksandshares_occ.Rdata"))
# transition rates from construct_rates_2m_occ_counter.R
load(file = paste0(edir.cps, "rates_occ_counter.Rdata"))

chosen.seas <- "SA"
chosen.measure <- "shr.occ2pop"

# period from which the law of motion will start
tinitial <- 197601
# tinitial <- 198001
# tinitial <- 200712

# in case of the actual.rate scenario with CPS NSA rates use transition rates from year ago
# for periods where it is not possible to match consequitive months in CPS data
if (chosen.seas == "NSA")
    df.rates.cps.occ.counter.adj %<>%
    group_by(seas, scenario) %>%
    mutate(rate = if_else(seas == "NSA" & scenario == "actual.rate" &
                              period.1 %in% c(197712, 198506, 198509, 199312, 199505, 199506, 199507, 199508),
                          lag(rate, 12), rate)) %>%
    ungroup()

# actual data and dates for which the lom will be simulated
df.shares.occ <-
    df.stocksandshares.cps.occ.combined %>%
    filter(lfs != "M") %>%
    filter(sample == "restricted") %>%
    filter(measure == chosen.measure) %>%
    filter(seas == chosen.seas) %>%
    filter(period >= tinitial) %>%
    unite(status, c(lfs, occ1cat), sep = ".") %>%
    select(period, seas, measure, status, value)

dates <-
    df.shares.occ  %>%
    spread(status, value) %>%
    select(period) %>%
    as.matrix()

# initialize results of simulation
sim.results <- list()

sim.init <-
    matrix(NA_real_, length(dates), 10) %>%
    set_rownames(dates) %>%
    set_colnames(c("E.NRC", "E.NRM", "E.RC", "E.RM",
                   "I.X",
                   "U.NRC", "U.NRM", "U.RC", "U.RM", "U.X"))

sim.init[1,] <-
    df.shares.occ %>%
    filter(period == tinitial) %>%
    spread(status, value) %>%
    select(E.NRC, E.NRM, E.RC, E.RM,
           I.X,
           U.NRC, U.NRM, U.RC, U.RM, U.X) %>%
    as.matrix()

# simulations <- c("actual.rate", "avg.rate")
simulations <- c("actual.rate", "avg.rate", "counter1", "counter2", "counter3", "counter4")

for (simulation in simulations) {

    sim <- sim.init
    t1 <- tinitial

    # loop for law of motion
    while (t1 < tlast) {
        t2 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)

        print(paste("Constructing shares using law of motion: scenario =", simulation, "date =", t1))

        # transition matrix
        trans.matrix <-
            df.rates.cps.occ.counter.adj %>%
            filter(seas == chosen.seas) %>%
            filter(scenario == simulation) %>%
            filter(period.1 == t1) %>%
            select(status.1, status.2, rate) %>%
            spread(status.2, rate) %>%
            as.data.frame() %>%
            column_to_rownames("status.1") %>%
            as.matrix()

        # construct shares using law of motion
        sim[as.character(t2), ] <- sim[as.character(t1), ] %*% trans.matrix

        t1 <- t2
    }

    sim.results[[simulation]] <- sim
}

# simulated and actual time series
df.shares.lom.occ.counter <-
    bind_rows(tibble(seas = chosen.seas,
                     scenario = simulations,
                     measure = chosen.measure,
                     values = map(sim.results, ~.x %>% as_tibble(rownames = "period"))) %>%
                  unnest(values) %>%
                  mutate(period = as.numeric(period)),
              df.shares.occ %>%
                  spread(status, value) %>%
                  mutate(scenario = "actual")) %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon(format = "%Y%m"),
           E.NR  = E.NRC + E.NRM,
           E.R   = E.RC  + E.RM,
           E.C   = E.NRC + E.RC,
           E.M   = E.NRM + E.RM,
           E.ALL = E.C   + E.M) %>%
    gather(status, value, starts_with("E.")) %>%
    select(period, monyear, scenario, seas, measure, status, value)

save(df.shares.lom.occ.counter, file = paste0(edir.cps, "shares_occ_lom_", chosen.seas, "_", tinitial, ".Rdata"))

# load(file = paste0(edir.cps, "shares_occ_lom_", chosen.seas, "_", tinitial, ".Rdata"))


# plot simulated and actual shares
df.shares.lom.occ.counter %>%
    separate(status, into = c("lfs", "occ1cat"), sep = "\\.") %>%
    # filter(occ1cat == "RC" | occ1cat == "RM") %>%
    # filter(scenario %in% c("actual", "actual.rate", "avg.rate", "counter1")) %>%
    mutate(occ1cat = factor(occ1cat, levels = c("ALL", "M", "C", "R", "RM", "RC", "NR", "NRM", "NRC"))) %>%
    ggplot() +
        geom_line(aes(x = monyear, y = value, col = scenario)) +
        geom_rect(data = (rec.dates %>% filter(Start > as.yearmon(as.character(tinitial), format = "%Y%m"))),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        scale_color_manual(values = c("black", "green", "blue", "red", "orange", "magenta", "pink")) +
        labs(x = "", y = "", title = "") +
        # facet_grid(occ1cat ~ lfs, scales = "free_y")
        facet_wrap(~ occ1cat, scales = "free_y",  ncol = 3, dir = "h")
