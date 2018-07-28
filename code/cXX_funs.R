
# recession dates
rec.dates <-
    nberDates() %>%
    as_tibble() %>%
    mutate_all(funs(. %>% as.character() %>% as.yearmon("%Y%m%d"))) %>%
    as.data.frame()


# define function for NBER recession shading
nber_shades <- function(g, periods){
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




# funstion seasonal.SSM uses local level model with monthly seasonal component and performs Kalman smoothing
seasonal.SSM <- function(df) {

    # input: df is expected to be a tibble with list column named data,
    #        each element in data column is itself a tibble with a column named period (double of form YYYYMM) and a column named y (double)

    # output: df.SSM contains the same elements as df but data tibble have an extra column named y.KS (double) which is Kalman smoothed y

    df.SSM <-
        df %>% mutate(data = map(data, ~(.x %>%
                                             mutate(y.KS = .x %>%
                                                        # convert date
                                                        mutate(monyear = as.yearmon(as.character(period), format="%Y %m")) %>%
                                                        # convert data to ts format
                                                        xts(x = .[, !(names(.) %in% c("monyear","period"))], order.by=.$monyear) %>% as.zoo() %>% as.ts() %>%
                                                        # define state space model - local level with seasonality
                                                        SSModel(y ~ SSMtrend(degree=1, Q=NA) + SSMseasonal(period=12, sea.type="dummy", Q=NA), H=NA, data=.) %>%
                                                        # maximum likelihood estimation
                                                        fitSSM(inits=log(c(0.01,0.01,0.01)), method="Nelder-Mead") %$%
                                                        # Kalman smoothing
                                                        KFS(model, smoothing=c("state","mean")) %>%
                                                        # extract smooted level
                                                        signal(states = "level") %$% signal))))

    }
