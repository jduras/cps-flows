
# construct lfs population shares for counterfactual transition rates using law of motion

# population shares from construct_stocks_agg.R
load(file = str_c(edir_cps, "out_stocksandshares_agg.Rdata"))
# transition rates from construct_rates_2m_agg_counter.R
load(file = str_c(edir_cps, "out_rates_agg_counter.Rdata"))

chosen_source <- "CPS"
chosen_seas <- "SA"
chosen_measure <- "shr_lfs2pop"

# period from which the law of motion will start, for chosen_source == BLS this has to be 199001 or later
tinitial <- 197601
# tinitial <- 198001
# tinitial <- 200712
if (chosen_source == "BLS") tinitial <- max(tinitial, 199001)

# in case of the actual_rate scenario with CPS NSA rates use transition rates from year ago
# for periods where it is not possible to match consequitive months in CPS data
if (chosen_source == "CPS" & chosen_seas == "NSA")
    df_rates_all_agg_counter_adj %<>%
    group_by(source, seas, scenario) %>%
    mutate(rate = if_else(source == "CPS" & seas == "NSA" & scenario == "actual_rate" &
                              period_1 %in% c(197712, 198506, 198509, 199312, 199505, 199506, 199507, 199508),
                          lag(rate, 12), rate)) %>%
    ungroup()

# actual data and dates for which the lom will be simulated
df_shares_agg <-
    df_stocksandshares_all_agg_whole_sample %>%
    filter(lfs != "M") %>%
    gather(measure, value, c(s, shr_lfs2pop)) %>%
    filter(measure == chosen_measure) %>%
    filter(source == chosen_source) %>%
    filter(seas == chosen_seas) %>%
    filter(period >= tinitial) %>%
    select(source, period, seas, measure, lfs, value)

dates <-
    df_shares_agg %>%
    spread(lfs, value) %>%
    select(period) %>%
    as.matrix()

sim_init <-
    matrix(NA_real_, length(dates), 3) %>%
    set_rownames(dates) %>%
    set_colnames(c("E", "I", "U"))

sim_init[1, ] <-
    df_shares_agg %>%
    filter(period == tinitial) %>%
    spread(lfs, value) %>%
    select(E, I, U) %>%
    as.matrix()

# simulations <- c("actual_rate", "avg_rate")
simulations <- c("actual_rate", "avg_rate", "counter1", "counter2", "counter3", "counter4", "counter5")

# initialize results of simulation
sim_results <- vector("list", length(simulations))

tic()

message("Constructing population shares using law of motion")

sim_results <-
    future_map_dfr(
        .x = set_names(simulations),
        .id = "scenario",
        .f = function(simulation = .x) {

            sim <- sim_init
            t1 <- tinitial

            message(str_c(" done: scenario ", simulation))

            df_tmp <- df_rates_all_agg_counter_adj %>%
                filter(source == chosen_source) %>%
                filter(seas == chosen_seas) %>%
                filter(scenario == simulation)

            # loop for law of motion
            # note: tseq may be ordered by (month, year) instead of month+year so while is used here instead of map
            while (t1 < tlst) {
                t2 <- if_else(t1 %% 100 == 12, t1 + 89, t1 + 1)

                # message(str_c("Constructing shares using law of motion:  scenario = ", simulation, " date = ", t1))

                # transition matrix
                trans_matrix <-
                    df_tmp %>%
                    filter(period_1 == t1) %>%
                    select(lfs_1, lfs_2, rate) %>%
                    spread(lfs_2, rate) %>%
                    select(-lfs_1) %>%
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
df_shares_lom_agg_counter <-
    bind_rows(df_shares_agg %>%
                  mutate(scenario = "actual"),
              sim_results %>%
                  mutate(period = as.numeric(period),
                         source = chosen_source,
                         seas = chosen_seas,
                         measure = chosen_measure) %>%
                  select(source, period, seas, measure, everything()) %>%
                  gather(lfs, value, c("E", "I", "U")))

save(df_shares_lom_agg_counter, file = str_c(edir_cps, "out_shares_agg_lom_", chosen_source, "_", chosen_seas, "_", tinitial, ".Rdata"))

# load(file = str_c(edir_cps, "out_shares_agg_lom_", chosen_source,"_",chosen_seas,".Rdata"))

# plot simulated and actual shares
df_shares_lom_agg_counter %>%
    mutate(yearm = period %>% as.character() %>% as.yearmon("%Y%m"),
           type = if_else(scenario == "actual", "actual", "simulated"),
           lfs_label = recode(lfs, "E" = "employed", "U" = "unemployed", "I" = "not in labor force")) %>%
    # filter(scenario %in% c("actual", "actual_rate", "avg_rate", "counter1")) %>%
    filter(seas == chosen_seas) %>%
    ggplot() +
        geom_line(aes(x = yearm, y = value, col = scenario)) +
        geom_rect(data = (rec_dates %>% filter(Start > as.yearmon(as.character(tinitial), format = "%Y%m"))),
                  aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), alpha = 0.2) +
        scale_x_yearmon() +
        scale_color_manual(values = c("black", brewer.pal(length(simulations), "Set1"))) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(x = "", y = "",
             title = "Population Shares by Labor Force Status",
             subtitle = "Actual vs Simulated Using Law of Motion") +
        facet_wrap(~ lfs_label, ncol = 1, scales = "free_y") +
        theme(strip.text = element_text(hjust = 0))
