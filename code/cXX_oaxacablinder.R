oaxacablinder <- function(model.A, model.B, weight = NULL, model.names = NULL, groups.labels = NULL, groups.vars = NULL) {

    # two way Blinder-Oaxaca decomposition, see
    #  Jann, Ben (2008). The Blinder-Oaxaca decomposition for linear regression models. The Stata Journal 8(4): 453-479.

    if (class(model.A) != "lm") stop("model A is not lm")
    if (class(model.B) != "lm") stop("model B is not lm")

    # dependent variables
    yA <- model.response(model.A$model)
    yB <- model.response(model.B$model)
    # model matrices
    XA <- model.matrix(model.A)
    XB <- model.matrix(model.B)
    # number of observations
    nA <- nrow(XA)
    nB <- nrow(XB)
    # weights
    wA <- weights(model.A)
    wB <- weights(model.B)
    # variance-covariance matrix - explanatory variables
    if (is.null(wA)) {
        VXA <- cov(XA) / nrow(XA)
    } else {
        VXA <- cov.wt(XA, wA, method = "ML")$cov / (sum(wA) - 1)
    }
    if (is.null(wB)) {
        VXB <- cov(XB) / nrow(XB)
    } else {
        VXB <- cov.wt(XB, wB, method = "ML")$cov / (sum(wB) - 1)
    }
    # mean values for explanatory variables
    XA <- apply(XA, MARGIN = 2, FUN = wtd.mean, weights = wA) %>% as.matrix()
    XB <- apply(XB, MARGIN = 2, FUN = wtd.mean, weights = wB) %>% as.matrix()

    # number of coefficients
    k <- length(model.A$coefficients)
    # estimated coefficients, model for group A and model for group B
    bA <- coefficients(model.A) %>% as.matrix()
    bB <- coefficients(model.B) %>% as.matrix()
    # variance-covariance matrices for estimated coefficients
    if (is.null(wA)) {
        VbA <- vcov(model.A)
    } else {
        VbA <- vcov(model.A) * (nA - k) / (sum(wA) - k)
    }
    if (is.null(wB)) {
        VbB <- vcov(model.B)
    } else {
        VbB <- vcov(model.B) * (nB - k) / (sum(wB) - k)
    }

    # model weights in decomposition
    #  1 for Oaxaca (1973)
    #  0 for Blinder (1973)
    #  0.5 for Reimers (1983)
    if (is.null(weight)) {
        w <- diag(1, k)
    } else {
        w <- diag(weight, k)
    }
    rownames(w) <- rownames(bA)
    colnames(w) <- rownames(w)
    Iw <- diag(1, k) - w

    # overall outcome difference
    R <- wtd.mean(yA, weights = wA) - wtd.mean(yB, weights = wB)

    # since R = (XA-XB)*bA + XB*(bA-bB) = XA*bA - XB*bB
    # and var(X*b) = t(X)*Vb*X + t(b)*VX*b + trace(VX*Vb)
    # we have
    VR <- t(XA) %*% VbA %*% XA + t(bA) %*% VXA %*% bA + sum(diag(VXA %*% VbA)) +
          t(XB) %*% VbB %*% XB + t(bB) %*% VXB %*% bB + sum(diag(VXB %*% VbB))

    # Blinder-Oaxaca decomposition - see Jann (2008) p.3

    # define beta*
    bstar <- w %*% bA + Iw %*% bB

    # combine defined groups of vaiables with the remaining (ungrouped) variables
    all.groups.labels <-
        rownames(bA) %>%
        setdiff(unlist(groups.vars)) %>%
        as.list() %>%
        append(groups.labels)
    all.groups.vars <-
        rownames(bA) %>%
        setdiff(unlist(groups.vars)) %>%
        as.list() %>%
        append(groups.vars)

    # decomposition - part explained by quantities (predictors)
    Q <- list()
    Q$detailed <- (XA-XB) * bstar
    Q$total <- colSums(Q$detailed)

    Q$grouped <- matrix(NA, nrow = length(all.groups.labels), ncol = 1, dimnames = list(unlist(all.groups.labels), NULL))
    for (i in seq(all.groups.vars)) {
        g <- all.groups.vars[[i]]
        Q$grouped[i,] <- t(XA[g,] - XB[g,]) %*% bstar[g,]
    }

    # decomposition - unexplained part
    U <- list()
    U$detailed <- t( t(XA) %*% Iw + t(XB) %*% w ) * (bA - bB)
    U$total <- colSums(U$detailed)

    U$grouped <- matrix(NA, nrow = length(all.groups.labels), ncol = 1, dimnames = list(unlist(all.groups.labels), NULL))
    for (i in seq(all.groups.vars)) {
        g <- all.groups.vars[[i]]
        U$grouped[i, ] <- ( t(XA[g,]) %*% Iw[g,g] + t(XB[g,]) %*% w[g,g] ) %*% (bA[g,] - bB[g,])
    }

    # variance for each part - see Jann (2008) p.7 eq.(9)-(11)
    VQ <- list()
    VQ$detailed <- (XA-XB) * diag( w %*% VbA %*% t(w) + Iw %*% VbB %*% t(Iw) ) * (XA-XB) + bstar * diag(VXA+VXB) * bstar
    VQ$total <- t(XA-XB) %*% ( w %*% VbA %*% t(w) + Iw %*% VbB %*% t(Iw) ) %*% (XA-XB) + t(bstar) %*% (VXA+VXB) %*% bstar

    VQ$grouped <- matrix(NA, nrow = length(all.groups.labels), ncol = 1, dimnames = list(unlist(all.groups.labels), NULL))
    for (i in seq(all.groups.vars)) {
        g <- all.groups.vars[[i]]
        VQ$grouped[i,] <- t(XA[g,] - XB[g,]) %*% ( w[g,g] %*% VbA[g,g] %*% t(w[g,g]) + Iw[g,g] %*% VbB[g,g] %*% t(Iw[g,g]) ) %*% (XA[g,] - XB[g,]) +
                     t(bstar[g,]) %*% (VXA[g,g] + VXB[g,g]) %*% bstar[g,]
    }

    VU <- list()
    VU$detailed <- ( t(Iw) %*% XA + t(w) %*% XB ) * diag(VbA+VbB) * ( t(Iw) %*% XA + t(w) %*% XB ) +
                (bA-bB) * diag( t(Iw) %*% VXA %*% Iw + t(w) %*% VXB %*% w ) * (bA-bB)
    VU$total <- t( t(Iw) %*% XA + t(w) %*% XB ) %*% (VbA+VbB) %*% ( t(Iw) %*% XA + t(w) %*% XB ) +
            t(bA-bB) %*% (t(Iw) %*% VXA %*% Iw + t(w) %*% VXB %*% w) %*% (bA-bB)

    VU$grouped <- matrix(NA, nrow = length(all.groups.labels), ncol = 1, dimnames = list(unlist(all.groups.labels), NULL))
    for (i in seq(all.groups.vars)) {
        g <- all.groups.vars[[i]]
        VU$grouped[i,] <- t( t(Iw[g,g]) %*% XA[g,] + t(w[g,g]) %*% XB[g,] ) %*% (VbA[g,g] + VbB[g,g]) %*% ( t(Iw[g,g]) %*% XA[g,] + t(w[g,g]) %*% XB[g,] ) +
            t(bA[g,] - bB[g,]) %*% (t(Iw[g,g]) %*% VXA[g,g] %*% Iw[g,g] + t(w[g,g]) %*% VXB[g,g] %*% w[g,g]) %*% (bA[g,]-bB[g,])
    }

    if (!is.null(model.names)) {
        grp.desc <- str_pad(model.names, max(str_length(model.names)), "right")
    } else {
        grp.desc <- str_pad(c(as.character(model.A$call[4]), as.character(model.B$call[4])),
                            max(str_length(as.character(model.A$call[4])),
                                str_length(as.character(model.B$call[4]))), "right")
    }

    # results
    results <- list()
    results$call <- match.call()
    results$yvar <- colnames(model.A$model)[1]
    results$group1 <- paste("Group 1: ", grp.desc[1], "        N of obs 1: ", nrow(model.matrix(model.A)))
    results$group2 <- paste("Group 2: ", grp.desc[2], "        N of obs 2: ", nrow(model.matrix(model.B)))
    results$w <- w
    results$R <- R
    results$VR <- VR
    results$Q <- Q
    results$U <- U
    results$VQ <- VQ
    results$VU <- VU
    class(results) <- "oaxacablinder"
    return(results)
}


print.oaxacablinder <- function(results, detailed = FALSE, conf.level = 0.95) {

    cat("\n Blinder-Oaxaca decomposition\n\n Call:\n")
    print(results$call)

    cat(paste("\n", results$group1))
    cat(paste("\n", results$group2, "\n"))

    cat(paste("\n decomposition using weight = ", results$w[1, 1], "\n\n"))

    cat(paste(results$yvar, "\n"))

    chosen.detail <- if_else(detailed, "detailed", "grouped")

    k <- length(results$Q[[chosen.detail]][, 1])

    output <-
        matrix(nrow = 3 + k - 1 + k, ncol = 6) %>%
        set_colnames(c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")) %>%
        set_rownames(c("difference",
                       "explained",
                       paste(" ", rownames(results$Q[[chosen.detail]])[2:k]),
                       "unexplained",
                       paste(" ", rownames(results$U[[chosen.detail]]))))

    output[, 1] <- results %$% c(R,
                                 Q[["total"]],
                                 Q[[chosen.detail]][2:k],
                                 U[["total"]],
                                 U[[chosen.detail]])
    output[, 2] <- results %$% c(VR,
                                 VQ[["total"]],
                                 VQ[[chosen.detail]][2:k],
                                 VU[["total"]],
                                 VU[[chosen.detail]]) %>% sqrt()
    output[, 3] <- output[, 1] / output[, 2]
    output[, 4] <- 2*pnorm(-abs(output[, 3]))
    output[, 5] <- output[, 1] - qnorm(conf.level + (1 - conf.level) / 2)*output[, 2]
    output[, 6] <- output[, 1] + qnorm(conf.level + (1 - conf.level) / 2)*output[, 2]

    print(zapsmall(output))

    cat("\n")
}


tidy.oaxacablinder <- function(results, detailed = FALSE, conf.level = 0.95) {

    chosen.detail <- if_else(detailed, "detailed", "grouped")

    k <- length(results$Q[[chosen.detail]][, 1])

    tibble(term = c("difference",
                    "explained.total",
                    paste0("explained.", rownames(results$Q[[chosen.detail]])[2:k]),
                    "unexplained.total",
                    paste0("unexplained.", rownames(results$U[[chosen.detail]]))),
           estimate = results %$% c(R,
                                    Q[["total"]],
                                    Q[[chosen.detail]][2:k],
                                    U[["total"]],
                                    U[[chosen.detail]]),
           std.error = results %$% c(VR,
                                     VQ[["total"]],
                                     VQ[[chosen.detail]][2:k],
                                     VU[["total"]],
                                     VU[[chosen.detail]]) %>% sqrt()) %>%
    mutate(statistic = estimate / std.error,
           p.value = 2*pnorm(-abs(statistic)),
           conf.low = estimate - qnorm(conf.level + (1 - conf.level) / 2)*std.error,
           conf.high = estimate + qnorm(conf.level + (1 - conf.level) / 2)*std.error)
}


plot.oaxacablinder <- function(results, detailed = FALSE, conf.level = 0.95) {

    tidy.oaxacablinder(results, detailed, conf.level) %>%
    separate(term, into = c("component", "variable"), sep = "\\.", extra = "merge", fill = "right") %>%
    filter(component %in% c("explained", "unexplained")) %>%
    mutate(variable = if_else(variable == "total", " total", variable)) %>%
    ggplot(aes(x = estimate, y = variable, xmin = conf.low, xmax = conf.high, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(x = "", y = "") +
        # facet_grid(. ~ component, scales="free_x")
        # facet_grid(. ~ component)
        facet_wrap(~component, ncol = 1) +
        theme_bw()
}
