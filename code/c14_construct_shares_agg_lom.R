
print("Constructing population shares using law of motion")

# construct lfs population shares for counterfactual transition rates using law of motion

# population shares from construct_stocks_agg.R
load(file = paste0(edir.cps, "stocksandshares_agg.Rdata"))
# transition rates from construct_rates_2m_agg_counter.R
load(file = paste0(edir.cps, "rates_agg_counter.Rdata"))

chosen.source <- "CPS"
chosen.seas <- "NSA"
chosen.measure <- "shr.lfs2pop"

# period from which the law of motion will start, for chosen.source == BLS this has to be 199001 or later
tinitial <- 197601
# tinitial <- 198001
# tinitial <- 200712
if (chosen.source == "BLS") tinitial <- max(tinitial, 199001)

# in case of the actual.rate scenario with CPS NSA rates use transition rates from year ago
# for periods where it is not possible to match consequitive months in CPS data
if (chosen.source == "CPS" & chosen.seas == "NSA")
    df.rates.all.agg.counter.adj %<>%
    group_by(source, seas, scenario) %>%
    mutate(rate = if_else(source == "CPS" & seas == "NSA" & scenario == "actual.rate" &
                              period.1 %in% c(197712, 198506, 198509, 199312, 199505, 199506, 199507, 199508),
                          lag(rate, 12), rate)) %>%
    ungroup()

# actual data and dates for which the lom will be simulated
df.shares.agg <-
    df.stocksandshares.all.agg.whole.sample %>%
    filter(lfs != "M") %>%
    gather(measure, value, c(s, shr.lfs2pop)) %>%
    filter(measure == chosen.measure) %>%
    filter(source == chosen.source) %>%
    filter(seas == chosen.seas) %>%
    filter(period >= tinitial) %>%
    select(source, period, seas, measure, lfs, value)

dates <-
    df.shares.agg %>%
    spread(lfs, value) %>%
    select(period) %>%
    as.matrix()

# initialize results of simulation
sim.results <- list()

sim.init <-
    matrix(NA_real_, length(dates), 3) %>%
    set_rownames(dates) %>%
    set_colnames(c("E", "I", "U"))

sim.init[1, ] <-
    df.shares.agg %>%
    filter(period == tinitial) %>%
    spread(lfs, value) %>%
    select(E, I, U) %>%
    as.matrix()

# simulations <- c("actual.rate", "avg.rate")
simulations <- c("actual.rate", "avg.rate", "counter1", "counter2", "counter3", "counter4", "counter5")

for (simulation in simulations) {

    sim <- sim.init
    t1 <- tinitial

    # loop for law of motion
    while (t1 < tlast) {
        t2 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)

        print(paste("Constructing shares using law of motion:  scenario =", simulation, " date =", t1))

        # transition matrix
        trans.matrix <-
            df.rates.all.agg.counter.adj %>%
            filter(source == chosen.source) %>%
            filter(seas == chosen.seas) %>%
            filter(scenario == simulation) %>%
            filter(period.1 == t1) %>%
            select(lfs.1, lfs.2, rate) %>%
            spread(lfs.2, rate) %>%
            as.data.frame() %>%
            column_to_rownames("lfs.1") %>%
            as.matrix()

        # construct shares using law of motion
        sim[as.character(t2), ] <- sim[as.character(t1), ] %*% trans.matrix

        t1 <- t2
    }

    sim.results[[simulation]] <- sim
}

# simulated and actual time series
df.shares.lom.agg.counter <-
    bind_rows(tibble(source = chosen.source,
                     seas = chosen.seas,
                     scenario = simulations,
                     measure = chosen.measure,
                     values = map(sim.results, ~.x %>% as_tibble(rownames = "period"))) %>%
                  unnest(values) %>%
                  gather(lfs, value, c("E", "I", "U")) %>%
                  mutate(period = as.numeric(period)),
              df.shares.agg %>%
                  mutate(scenario = "actual"))

save(df.shares.lom.agg.counter, file = paste0(edir.cps, "shares_agg_lom_", chosen.source, "_", chosen.seas, "_", tinitial, ".Rdata"))

# load(file = paste0(edir.cps, "shares_agg_lom_", chosen.source,"_",chosen.seas,".Rdata"))

# plot simulated and actual shares
df.shares.lom.agg.counter %>%
    mutate(monyear = period %>% as.character() %>% as.yearmon("%Y%m"),
           type = if_else(scenario == "actual", "actual", "simulated")) %>%
    # filter(scenario %in% c("actual", "actual.rate", "avg.rate", "counter1")) %>%
    filter(seas == chosen.seas) %>%
    ggplot(data = .) +
        geom_line(aes(x = monyear, y = value, col = scenario)) +
        geom_rect(data = (rec.dates %>% filter(Start > as.yearmon(as.character(tinitial), format = "%Y%m"))),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        labs(x = "", y = "", title = "") +
        facet_wrap(~ lfs, ncol = 1, scales = "free_y")
