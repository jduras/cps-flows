
# recession dates
rec_dates <-
    nberDates() %>%
    as_tibble() %>%
    mutate_all(funs(. %>% as.character() %>% as.yearmon("%Y%m%d"))) %>%
    as_tibble()

# define function for NBER recession shading
nber_shades <- function(g, periods){
    periods <- as.data.frame(periods)
    for(i in seq_along(periods[, 1])){
        g <- dyShading(g, from = periods[i, 1], to = periods[i, 2], color = "#F0F0F8")
    }
    g
}


# define plot function with NBER recession shading
myplot <- function(x, col = "black", lwd = NULL, lty = NULL, ...){
    # get dates for NBER recessions
    rec.dates <-
        nberDates() %>%
        as_tibble() %>%
        mutate_all(funs(. %>% as.character() %>% as.yearmon("%Y%m%d"))) %>%
        as.data.frame()

    # create plot with NBER recession shading
    g <- dygraph(x, ...) %>%
        dyOptions(colors = col) %>%
        nber_shades(rec.dates)  %>%
        dyLimit(0, strokePattern = "dotted") %>%
        dyLegend(width = 750) %>%
        dyLimit(0)

    # apply line width and line type, if provided
    s <- rep(list(list(axis="y")), length(g$x$data)-1)
    if (!is.null(lwd)) s <- lapply(seq_along(s), function(x,i) {x[[i]]$strokeWidth<-lwd[i]; x[[i]]}, x=s)
    if (!is.null(lty)) s <- lapply(seq_along(s), function(x,i) {x[[i]]$strokePattern<-dygraphs:::resolveStrokePattern(lty[i]); x[[i]]}, x=s)
    names(s) <- g$x$attrs$labels[-1]
    g$x$attrs$series <- s

    #return final plot
    g
}


# funstion sa_ssm uses local level model with monthly seasonal component and performs Kalman smoothing
sa_ssm <- function(df) {

    # input: df is expected to be a tibble with data stored in a column named y (double)
    # ou2tput: df_ssm contains the same elements as df but an extra column named y_ks (double) which is SA y

    df %>%
        mutate(
            # define state space model - local level with seasonality
            y_ks = SSModel(y ~ SSMtrend(degree = 1, Q = NA) +
                               SSMseasonal(period = 12, sea.type = "dummy", Q = NA), H = NA,
                           data = .) %>%
                # maximum likelihood estimation
                fitSSM(inits = log(c(0.01, 0.01, 0.01)), method = "Nelder-Mead") %$%
                # Kalman smoothing
                KFS(model, smoothing = c("state", "disturbance")) %$%
                # extract smoothed level + disturbance
                {alphahat[, "level"] + epshat[, 1]})
}

smooth_ssm <- function(df) {

    # input: df is expected to be a tibble with data stored in a column named y (double)
    # output: df_ssm contains the same elements as df but an extra column named y_ks (double) which is Kalman smoothed y

    df_ssm <-
        df %>% mutate(
            # define state space model - local level with seasonality
            y_smooth = SSModel(y ~ SSMtrend(degree = 1, Q = NA) +
                                   SSMseasonal(period = 12, sea.type = "dummy", Q = NA), H = NA,
                               data = .) %>%
                # maximum likelihood estimation
                fitSSM(inits = log(c(0.01, 0.01, 0.01)), method = "Nelder-Mead") %$%
                # Kalman smoothing
                KFS(model, smoothing = c("state", "disturbance")) %$%
                # extract smoothed level + disturbance
                {alphahat[, "level"]})
}

