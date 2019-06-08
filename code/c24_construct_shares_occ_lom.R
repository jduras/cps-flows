
# construct lfs population shares for counterfactual transition rates using law of motion

# population shares from c21_construct_stocks_occ.R
load(file = str_c(edir_cps, "out_stocksandshares_occ.Rdata"))
# transition rates from c23_construct_rates_2m_occ_counter.R
load(file = str_c(edir_cps, "out_rates_occ_counter.Rdata"))

chosen_seas <- "SA"
chosen_measure <- "shr_occ2pop"

# period from which the law of motion will start
tinitial <- 197601
# tinitial <- 198001
# tinitial <- 200712

# in case of the actual_rate scenario with CPS NSA rates use transition rates from year ago
# for periods where it is not possible to match consequitive months in CPS data
if (chosen_seas == "NSA")
    df_rates_cps_occ_counter_adj %<>%
    group_by(seas, scenario) %>%
    mutate(rate = if_else(seas == "NSA" & scenario == "actual_rate" &
                              period_1 %in% c(197712, 198506, 198509, 199312, 199505, 199506, 199507, 199508),
                          lag(rate, 12), rate)) %>%
    ungroup()

# actual data and dates for which the lom will be simulated
df_shares_occ <-
    df_stocksandshares_cps_occ_combined %>%
    filter(lfs != "M") %>%
    filter(sample == "restricted") %>%
    gather(measure, value, c(s_occ, shr_occ2lfs, shr_occ2pop)) %>%
    filter(measure == chosen_measure) %>%
    filter(seas == chosen_seas) %>%
    filter(period >= tinitial) %>%
    unite(status, c(lfs, occ1cat), sep = "_") %>%
    select(period, seas, measure, status, value)

dates <-
    df_shares_occ  %>%
    spread(status, value) %>%
    select(period) %>%
    as.matrix()

sim_init <-
    matrix(NA_real_, length(dates), 10) %>%
    set_rownames(dates) %>%
    set_colnames(c("E_NRC", "E_NRM", "E_RC", "E_RM",
                   "I_X",
                   "U_NRC", "U_NRM", "U_RC", "U_RM", "U_X"))

sim_init[1, ] <-
    df_shares_occ %>%
    filter(period == tinitial) %>%
    spread(status, value) %>%
    select(E_NRC, E_NRM, E_RC, E_RM,
           I_X,
           U_NRC, U_NRM, U_RC, U_RM, U_X) %>%
    as.matrix()

# simulations <- c("actual_rate", "avg_rate")
simulations <- c("actual_rate", "avg_rate", "counter1", "counter2", "counter3", "counter4")


# initialize results of simulation
sim_results <- vector("list", length(simulations))

tic()

message("Constructing population shares by occupation category using law of motion")

sim_results <-
    future_map_dfr(
        .x = set_names(simulations),
        .id = "scenario",
        .f = function(simulation = .x) {

        sim <- sim_init
        t1 <- tinitial

        message(str_c(" done: scenario ", simulation))

    	df_tmp <- df_rates_cps_occ_counter_adj %>%
                filter(seas == chosen_seas) %>%
                filter(scenario == simulation)

        # loop for law of motion
        # note: tseq may be ordered by (month, year) instead of month+year so while is used here instead of map
        while (t1 < tlst) {
            t2 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)

            # message(str_c("Constructing shares using law of motion: scenario =", simulation, "date =", t1))

            # transition matrix
            trans_matrix <-
                df_tmp %>%
                filter(period_1 == t1) %>%
     	        select(status_1, status_2, rate) %>%
                spread(status_2, rate) %>%
                select(-status_1) %>%
                as.matrix()

            # construct shares using law of motion
            sim[as.character(t2), ] <- sim[as.character(t1), ] %*% trans_matrix

            t1 <- t2
        }

        sim %>%
            as_tibble(rownames = "period")
        })

toc()

# simulated and actual time series
df_shares_lom_occ_counter <-
    bind_rows(df_shares_occ %>%
                  spread(status, value) %>%
                  mutate(period = as.character(period),
                         scenario = "actual"),
              sim_results %>%
                  mutate(seas = chosen_seas,
                         measure = chosen_measure) %>%
                  select(period, seas, measure, everything())) %>%
    mutate(yearm = period %>% as.yearmon(format = "%Y%m"),
           U_NR  = U_NRC + U_NRM,
           U_R   = U_RC  + U_RM,
           U_C   = U_NRC + U_RC,
           U_M   = U_NRM + U_RM,
           U_ALL = U_C   + U_M + U_X,
           E_NR  = E_NRC + E_NRM,
           E_R   = E_RC  + E_RM,
           E_C   = E_NRC + E_RC,
           E_M   = E_NRM + E_RM,
           E_ALL = E_C   + E_M) %>%
    gather(status, value, c("E_NRC", "E_NRM", "E_RC", "E_RM",
                            "E_NR", "E_R", "E_C", "E_M", "E_ALL",
                            "I_X",
                            "U_NRC", "U_NRM", "U_RC", "U_RM", "U_X",
                            "U_NR", "U_R", "U_C", "U_M", "U_ALL")) %>%
    select(period, yearm, scenario, seas, measure, status, value)

save(df_shares_lom_occ_counter, file = str_c(edir_cps, "out_shares_occ_lom_", chosen_seas, "_", tinitial, ".Rdata"))

# load(file = str_c(edir_cps, "shares_occ_lom_", chosen_seas, "_", tinitial, ".Rdata"))


# plot simulated and actual shares
g <- ggplot() +
    geom_line(aes(x = yearm, y = value, col = scenario)) +
    geom_rect(data = (rec_dates %>% filter(Start > as.yearmon(as.character(tinitial), format = "%Y%m"))),
              aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
    scale_x_yearmon() +
    scale_color_manual(values = c("black", "green", "blue", "red", "orange", "magenta", "pink")) +
    labs(x = "", y = "", title = "")

# all lfs
g %+% {df_shares_lom_occ_counter %>%
            separate(status, into = c("lfs", "occ1cat"), sep = "_") %>%
            # filter(scenario %in% c("actual", "actual_rate", "avg.rate", "counter1")) %>%
            mutate(occ1cat = factor(occ1cat, levels = c("ALL", "M", "C", "R", "RM", "RC", "NR", "NRM", "NRC", "X")))} +
    facet_grid(lfs ~ occ1cat, scales = "free_y")

# employed
g %+% {df_shares_lom_occ_counter %>%
            separate(status, into = c("lfs", "occ1cat"), sep = "_") %>%
            filter(lfs == "E") %>%
            # filter(scenario %in% c("actual", "actual_rate", "avg.rate", "counter1")) %>%
            mutate(occ1cat = factor(occ1cat, levels = c("ALL", "M", "C", "R", "RM", "RC", "NR", "NRM", "NRC", "X")))} +
    facet_wrap(~ occ1cat, scales = "free_y",  ncol = 3, dir = "h")

# unemployed
g %+% {df_shares_lom_occ_counter %>%
        separate(status, into = c("lfs", "occ1cat"), sep = "_") %>%
        filter(lfs == "U") %>%
        # filter(scenario %in% c("actual", "actual_rate", "avg.rate", "counter1")) %>%
        mutate(occ1cat = factor(occ1cat, levels = c("ALL", "M", "C", "R", "RM", "RC", "NR", "NRM", "NRC", "X")))} +
    facet_wrap(~ occ1cat, scales = "free_y",  ncol = 3, dir = "h")

# not in labor force
g %+% {df_shares_lom_occ_counter %>%
        separate(status, into = c("lfs", "occ1cat"), sep = "_") %>%
        filter(lfs == "I")}
