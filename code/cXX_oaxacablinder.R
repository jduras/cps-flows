oaxacablinder <- function(model_A, model_B, model_names = c("A", "B"), groups_labels = NULL, groups_vars = NULL) {

    # two way Blinder-Oaxaca decomposition with A as reference group, see
    #  Jann, Ben (2008). The Blinder-Oaxaca decomposition for linear regression models. The Stata Journal 8(4): 453-479.

    model <- list(A = model_A, B = model_B)

    if (class(model$A) != "lm") stop("model A is not lm")
    if (class(model$B) != "lm") stop("model B is not lm")

    # estimated coefficients, model for group A and model for group B
    b <- map(model, . %>% coefficients() %>% as.matrix())

    if (!setequal(rownames(b$A), rownames(b$B))) warning("Models A and B are specified with different predictors")

    # extract information about estimated linear regression models
    y <- map(model, . %>% pluck("model") %>% model.response())      # dependent variables
    X <- map(model, model.matrix)                                   # model matrices
    w <- map(model, weights)                                        # weights
    n <- map(X, nrow)                                               # number of observations
    k <- map(b, nrow)                                               # number of coefficients

    # mean values for explanatory variables mX, variance-covariance matrix VX and variance-covariance matrices for estimated coefficients Vb
    if (is.null(w$A)) {
        mX <- map(X, . %>% colMeans() %>% as.matrix())
        VX <- map2(X, n, ~cov(.x)/.y)
        Vb <- map(model, vcov)
    } else {
        nw <- map(w, ~.x / sum(.x))
        mX <- map2(X, nw, ~colSums(.y * .x))
        VX <- pmap(list(X, mX, nw, w),
                   function(X, mX, nw, w) {crossprod( sqrt(nw) * (X - mX) ) / (sum(w) - 1)})
        mX <- map(mX, as.matrix)
        Vb <- pmap(list(model, w, n, k),
                   function(model, w, n, k) {vcov(model) * (n - k) / (sum(w) - k)})
    }

    # overall outcome difference
    # R <- wtd.mean(y$A, weights = w$A) - wtd.mean(y$B, weights = w$B)
    R <- map2(y, w, ~wtd.mean(.x, weights = .y)) %>% reduce(`-`)

    # since R = (mX$A-mX$B)*bA + mX$B*(bA-bB) = mX$A*bA - mX$B*bB
    # and var(X*b) = t(X)*Vb*X + t(b)*VX*b + trace(VX*Vb)
    # we have
    VR <- t(mX$A) %*% Vb$A %*% mX$A + t(b$A) %*% VX$A %*% b$A + sum(diag(VX$A %*% Vb$A)) +
          t(mX$B) %*% Vb$B %*% mX$B + t(b$B) %*% VX$B %*% b$B + sum(diag(VX$B %*% Vb$B))
    # VR <- pmap(list(mX, VX, b, Vb),
    #            function(mX, VX, b, Vb) {
    #                t(mX) %*% Vb %*% mX + t(b) %*% VX %*% b + sum(diag(VX %*% Vb))
    #                }) %>%
    #     reduce(`+`)

    # Blinder-Oaxaca decomposition - see Jann (2008) p.3

    # combine defined groups of variables with the remaining (not grouped) variables
    not_in_groups <-
        rownames(b$A) %>%
        setdiff(unlist(groups_vars)) %>%
        as.list()
    all_groups_labels <-
        not_in_groups %>%
        append(groups_labels)
    all_groups_vars <-
        not_in_groups %>%
        append(groups_vars)

    # decomposition - part explained by differences in quantities (predictors)
    Q <- list("detailed" = NULL, "grouped" = NULL, "total" = NULL)
    Q$detailed <- (mX$A - mX$B) * b$A
    Q$grouped <- map_dbl(all_groups_vars, ~sum(Q$detailed[., ])) %>%
                    set_names(all_groups_labels) %>%
                    as.matrix(nrow = length(all_groups_labels), ncol = 1)
    Q$total <- sum(Q$detailed)

    # decomposition - unexplained part
    U <- list("detailed" = NULL, "grouped" = NULL, "total" = NULL)
    U$detailed <- mX$B * (b$A - b$B)
    U$grouped <- map_dbl(all_groups_vars, ~sum(U$detailed[., ])) %>%
                    set_names(all_groups_labels) %>%
                    as.matrix(nrow = length(all_groups_labels), ncol = 1)
    U$total <- sum(U$detailed)

    # variance for explained part - see Jann (2008) eq.(10)
    VQ <- list(
        detailed = (mX$A - mX$B) * diag(Vb$A) * (mX$A-mX$B) + b$A %*% diag(VX$A + VX$B) %*% b$A,
        grouped = map_dbl(all_groups_vars,
                          ~(t(mX$A[., ] - mX$B[., ]) %*% Vb$A[., .] %*% (mX$A[., ] - mX$B[., ]) + t(b$A[., ]) %*% (VX$A[., .] + VX$B[., .]) %*% b$A[., ])) %>%
                    set_names(all_groups_labels) %>%
                    as.matrix(nrow = length(all_groups_labels), ncol = 1),
        total = t(mX$A - mX$B) %*% Vb$A %*% (mX$A - mX$B) + t(b$A) %*% (VX$A + VX$B) %*% b$A)

    # variance for unexplained part - see Jann (2008) eq.(11)
    VU <- list(
        detailed = mX$B * diag(Vb$A + Vb$B) * mX$B + (b$A - b$B) * diag(VX$B) * (b$A - b$B),
        grouped = map_dbl(all_groups_vars,
                          ~(t(mX$B[., ]) %*% (Vb$A[., .] + Vb$B[., .]) %*% mX$B[., ] + t(b$A[., ] - b$B[., ]) %*% VX$B[., .] %*% (b$A[., ] - b$B[., ]))) %>%
                    set_names(all_groups_labels) %>%
                    as.matrix(nrow = length(all_groups_labels), ncol = 1),
        total = t(mX$B) %*% (Vb$A + Vb$B) %*% mX$B + t(b$A - b$B) %*% VX$B %*% (b$A - b$B))

    groups_desc <-
        list(str_pad(model_names, max(str_length(model_names)), "right"), n, seq_along(n)) %>%
        pmap(function(groups_desc, n , idx) str_c("Group ", idx, ": ", groups_desc, "        N of obs ", idx, ": ", n)) %>%
        set_names("A", "B")

    # results
    results <- list(
        call = match.call(),
        y_name = colnames(model_A$model)[1],
        groups = groups_desc,
        R = R,
        Q = Q,
        U = U,
        VR = VR,
        VQ = VQ,
        VU = VU)
    class(results) <- "oaxacablinder"

    return(results)
}


print.oaxacablinder <- function(results, detailed = FALSE, conf.level = 0.95) {

    cat("\n Blinder-Oaxaca decomposition\n\n Call:\n")
    print(results$call)

    cat(paste("\n", results$groups$A))
    cat(paste("\n", results$groups$B, "\n"))

    cat(paste("\n decomposition using weight = 1 \n\n"))

    cat(paste(results$y_name, "\n"))

    chosen_detail <- if_else(detailed, "detailed", "grouped")

    k <- length(results$Q[[chosen_detail]][, 1])

    output <-
        matrix(nrow = 3 + k - 1 + k, ncol = 6) %>%
        set_colnames(c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")) %>%
        set_rownames(c("difference",
                       "explained",
                       paste(" ", rownames(results$Q[[chosen_detail]])[2:k]),
                       "unexplained",
                       paste(" ", rownames(results$U[[chosen_detail]]))))

    output[, 1] <- results %$% c(R,
                                 Q[["total"]],
                                 Q[[chosen_detail]][2:k],
                                 U[["total"]],
                                 U[[chosen_detail]])
    output[, 2] <- results %$% c(VR,
                                 VQ[["total"]],
                                 VQ[[chosen_detail]][2:k],
                                 VU[["total"]],
                                 VU[[chosen_detail]]) %>% sqrt()
    output[, 3] <- output[, 1] / output[, 2]
    output[, 4] <- 2*pnorm(-abs(output[, 3]))
    output[, 5] <- output[, 1] - qnorm(conf.level + (1 - conf.level) / 2)*output[, 2]
    output[, 6] <- output[, 1] + qnorm(conf.level + (1 - conf.level) / 2)*output[, 2]

    print(zapsmall(output))

    cat("\n")
}


tidy.oaxacablinder <- function(results, detailed = FALSE, conf.level = 0.95) {

    chosen_detail <- if_else(detailed, "detailed", "grouped")

    k <- length(results$Q[[chosen_detail]][, 1])

    tibble(term = c("difference",
                    "explained.total",
                    paste0("explained.", rownames(results$Q[[chosen_detail]])[2:k]),
                    "unexplained.total",
                    paste0("unexplained.", rownames(results$U[[chosen_detail]]))),
           estimate = results %$% c(R,
                                    Q[["total"]],
                                    Q[[chosen_detail]][2:k],
                                    U[["total"]],
                                    U[[chosen_detail]]),
           std.error = results %$% c(VR,
                                     VQ[["total"]],
                                     VQ[[chosen_detail]][2:k],
                                     VU[["total"]],
                                     VU[[chosen_detail]]) %>% sqrt()) %>%
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

