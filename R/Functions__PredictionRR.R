
#' Predict resistance and resilience
#'
#' @param x A data frame in wide format, i.e., columns represent predictor
#' variables and rows are treated as units.
#' @param var_predictors A vector of character strings. The names of the
#' predictor variables required by the workflows.
#' @param rf_workflow_rst A workflow object. A `"tidymodels"` workflow of a
#' `"randomForest"` model to predict resistance with `var_predictors` from `x`.
#' @param rf_workflow_rsl A workflow object. A `"tidymodels"` workflow of a
#' `"randomForest"` model to predict resilience with `var_predictors` from `x`.
#' @param nrr A vector of character strings. The names of the output variables.
#' @param rr_levels A vector of character strings. The names of
#' resistance and resilience categories (in increasing order).
#' @param seed An integer value. Random number generator seed; note that
#' predicted ties in most likely categories are broken at random.
#'
#' @return A data frame in wide format, i.e., columns represent predicted
#' variables (as identified by `nrr` plus any column copied from `x` not
#' identified as predictor by `var_predictors`) and rows are treated as units
#' (the same rows as `x`).
#'
#' @export
predict_rr <- function(
  x,
  var_predictors,
  rf_workflow_rst,
  rf_workflow_rsl,
  nrr,
  rr_levels = c("L", "ML", "M", "H+MH"),
  seed = 1234L
) {

  rrs <- strsplit(nrr, split = "_", fixed = TRUE) |>
    lapply(FUN = function(x) x[[1L]]) |>
    unlist() |>
    unique()

  stopifnot(
    requireNamespace("data.table"),
    requireNamespace("randomForest"),
    requireNamespace("tidymodels"),
    var_predictors %in% colnames(x),
    identical(rrs, c("Resistance", "Resilience"))
  )

  #--- Prepare predictors
  # Add (empty) columns used by `predict()` methods of `rf_workflow_rst/rsl`
  dtx <- data.table::as.data.table(
    data.frame(
      x,
      JFSP_ID = NA_character_,
      NA_L3NAME = NA_character_,
      coords.x1 = NA_real_,
      coords.x2 = NA_real_,
      Resistance = factor(NA, levels = rr_levels),
      Resilience = factor(NA, levels = rr_levels),
      stringsAsFactors = FALSE
    )
  )

  set.seed(seed) # `randomForest:::predict.randomForest()` breaks ties at random

  n_cases <- nrow(x)
  stopifnot(is.finite(n_cases))
  idsxm <- stats::complete.cases(x)
  res0 <- rep(NA, n_cases)


  #--- Prepare output container
  var_header <- setdiff(colnames(x), var_predictors)

  res <- as.data.frame(array(
    dim = c(n_cases, length(var_header) + length(nrr)),
    dimnames = list(NULL, c(var_header, nrr))
  ))

  res[, var_header] <- x[, var_header]


  #--- Make predictions for all cases
  for (kr in rrs) {
    # Predict `response` (as multi-level factor)
    id_res <- grep(paste0("^", kr, "_response$"), colnames(res))

    tmp <- predict(
      switch(kr, Resistance = rf_workflow_rst, Resilience = rf_workflow_rsl),
      dtx[idsxm, ]
    )[, 1, drop = TRUE]

    tmp0 <- res0
    # Convert to numeric so that we can convert to long-format
    # and combine with probabilities for return object
    tmp0[idsxm] <- as.numeric(tmp)
    res[, id_res] <- tmp0


    # Predict `probabilities` (as array)
    id_res <- grep(paste0("^", kr, "_prob"), colnames(res))

    tmp <- predict(
      switch(kr, Resistance = rf_workflow_rst, Resilience = rf_workflow_rsl),
      dtx[idsxm, ],
      type = "prob"
    )

    tmp0 <- array(
      dim = c(n_cases, ncol(tmp)),
      dimnames = list(NULL, sub(".pred", "pred", colnames(tmp)))
    )
    tmp0[idsxm, ] <- as.matrix(tmp)
    stopifnot(paste0("pred_", rr_levels) == colnames(tmp0))
    res[, id_res] <- tmp0

    # Calculate index as weighted sum across class probabilities
    id_res <- grep(paste0("^", kr, "_index$"), colnames(res))

    res[, id_res] <- calc_RR_index(tmp0)
  }

  res
}



predict_rr_long <- function(
  x_long,
  var_header,
  scen,
  rf_workflow_rst,
  rf_workflow_rsl,
  req_nrr,
  req_rr_levels,
  req_rrs,
  req_predictors,
  sep = "!XX!",
  seed = 1234L
) {
  # remove "group", make sure "site" is first element
  var_header <- setdiff(var_header, "group")
  var_header <- c("site", setdiff(var_header, "site"))

  n_header <- length(var_header)

  site2 <- x_long[["site"]]

  if (n_header > 1) {
    # Additional "header" variables must not result in additional unique units
    # besides "site"
    stopifnot(
      identical(
        unique(x_long[["site"]]),
        unique(x_long[, var_header])[["site"]]
      )
    )

    # `reshape()` with multiple `idvar` is very time and memory expensive
    # --> combine `var_header` into one variable and use that as `idvar` instead
    for (kvar in setdiff(var_header, "site")) {
      site2 <- paste(site2, x_long[[kvar]], sep = sep)
    }

    typeof_header <- lapply(var_header, function(kvar) typeof(x_long[[kvar]]))
  }


  #--- Subset metrics data and convert to wide format
  xmk <- stats::reshape(
    data.frame(
      site2 = site2,
      x_long[, c("group", scen), drop = FALSE],
      stringsAsFactors = FALSE
    ),
    direction = "wide",
    idvar = "site2",
    timevar = "group"
  )

  colnames(xmk) <- gsub(paste0(scen, "."), "", colnames(xmk))

  #--- Predict
  res <- predict_rr(
    x = xmk,
    var_predictors = req_predictors,
    rf_workflow_rst = rf_workflow_rst,
    rf_workflow_rsl = rf_workflow_rsl,
    nrr = req_nrr,
    rr_levels = req_rr_levels,
    seed = seed
  )


  #--- Convert back to long format
  xres <- stats::reshape(
    res[, c("site2", req_nrr), drop = FALSE],
    direction = "long",
    varying = req_nrr,
    v.names = scen,
    idvar = "site2",
    timevar = "group",
    times = req_nrr
  )

  rownames(xres) <- NULL


  #--- Retrieve original header columns
  if (n_header > 1) {
    tmp <- do.call(
      rbind,
      strsplit(xres[["site2"]], split = sep, fixed = TRUE)
    )
    colnames(tmp) <- var_header
    tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)

    for (k in seq_along(var_header)) {
      if (!identical(typeof(tmp[[var_header[[k]]]]), typeof_header[[k]])) {
        tmp[[var_header[[k]]]] <- as(tmp[[var_header[[k]]]], typeof_header[[k]])
      }
    }

    xres <- data.frame(tmp, xres[, -1, drop = FALSE], stringsAsFactors = FALSE)

  } else {
    colnames(xres)[[1L]] <- "site"
  }


  xres
}


#' Calculate response level from class probabilities
#'
#' From `randomForest:::predict.randomForest()` -> `classForest()`
# nolint start: line_length_linter.
#' \url{https://github.com/cran/randomForest/blob/6cedb5fbf26ca857afda687f42c5ba535d61eaf0/src/rf.c#L567}
# nolint end: line_length_linter.
#'
#' ```
#'   Aggregated prediction is the class with the maximum votes/cutoff
#'   crit = (countts[j + n * *nclass] / *ntree) / cutoff[j];
#'   ...
#'   Break ties at random:
#' ```
#'
#' @param x A two-dimensional numerical object. Rows represent sites and
#' columns represent class probabilities.
#' @param cutoffs_probs A numerical vector of length equal to number of columns
#' of `x`.
#' @param seed An integer value. Set the random number generator seed.
#'
#' @return An integer vector of length equal to the number of rows of `x`
#' representing the class (column of `x`) with the largest value, i.e.,
#' the most likely category.
#'
#' @examples
#' x <- rbind(
#'   c(0.1, 0.4, 0.4, 0.1),
#'   c(0.7, 0.1, 0.1, 0.1),
#'   c(NA, 0.1, 0.1, 0.1)
#' )
#' calc_RF_response_from_probs(x) ## expected outcome: c(2 or 3, 1, NA)
#'
#' @export
calc_RF_response_from_probs <- function(
  x,
  cutoffs_probs = rep(1. / ncol(x), ncol(x)),
  seed = NULL
) {
  # Calculate probabilities/cutoff = votes/ntree/cutoff ~ votes/cutoff
  xtmp <- sweep(
    x,
    MARGIN = 2L,
    STATS = cutoffs_probs,
    FUN = "/"
  )

  # Aggregated prediction is the class with the maximum votes/cutoff
  set.seed(seed)
  tol <- sqrt(.Machine[["double.eps"]])

  apply(
    X = xtmp,
    MARGIN = 1L,
    FUN = function(p) {
      if (anyNA(p)) {
        NA_integer_
      } else {
        tmp <- abs(p - max(p)) < tol
        if (sum(tmp) > 1L) {
          # Break ties at random
          sample(which(tmp), size = 1L)
        } else {
          which.max(p)
        }
      }
    }
  )
}


#' Calculate weighted sum across class probabilities
#'
#' Default weights applied to resilience and resistance categories
#' low, medium-low, medium, and high (the last including medium-high) results
#' in an index varying from -1 (low probabilities dominate) to
#' +1 (high probabilities dominate) with assumptions of distances among
#' classes being constant and equal.
#'
#' @param x A two-dimensional numerical object. Rows represent sites and
#' columns represent class probabilities.
#' @param weights A numerical vector of length equal to number of columns
#' of `x`.
#'
#' @return A numeric vector of length equal to the number of rows of `x`
#' representing the weighted sum of the class probabilities.
#'
#' @examples
#' x <- rbind(
#'   c(0.1, 0.4, 0.4, 0.1),
#'   c(0.7, 0.1, 0.1, 0.1),
#'   c(NA, 0.1, 0.1, 0.1),
#'   c(1, 0, 0, 0),
#'   c(0, 1, 0, 0),
#'   c(0, 0, 0, 1),
#'   c(0.5, 0, 0, 0.5),
#'   c(0, 0.5, 0.5, 0)
#' )
#' calc_RR_index(x) ## expected outcome: c(0, -0.6, NA, -1, -1 / 3, 1, 0, 0)
#'
#' p <- seq(0, 1, by = 0.01)
#' xg <- expand.grid(L = p, ML = p, M = p, H = p)
#' ids1 <- abs(rowSums(xg) - 1) < sqrt(.Machine[["double.eps"]])
#' xg <- as.matrix(xg[ids1, , drop = FALSE])
#' ri <- calc_RR_index(xg)
#'
#' if (requireNamespace("graphics") && requireNamespace("stats")) {
#'   par_prev <- graphics::par(
#'     mfrow = c(2L, 2L),
#'     mar = c(2.5, 2.5, 0.5, 0.5),
#'     mgp = c(1, 0, 0),
#'     tcl = 0.3
#'   )
#'   for (k in 1:4) {
#'     x <- xg[, k]
#'     graphics::plot(
#'       x,
#'       ri,
#'       pch = 16,
#'       col = "darkgray",
#'       xlab = paste0("Probability (", colnames(xg)[k], ")"),
#'       ylab = "Continuous index"
#'     )
#'     graphics::lines(
#'       p,
#'       stats::predict(stats::lm(ri ~ x), newdata = data.frame(x = p)),
#'       col = "red",
#'       lwd = 2
#'     )
#'   }
#'   graphics::par(par_prev)
#' }
#'
#' @export
calc_RR_index <- function(x, weights = c(-1, -1 / 3, 1 / 3, 1)) {
  stopifnot(length(weights) == NCOL(x))

  drop(x %*% weights)
}


#' Uncertainty metrics
#'
#' @param x A two-dimension numeric object. Rows represent cases.
#'
#' @return A numeric vector of length equal to the number of rows of `x`.
#'
#' @examples
#' ncats <- 4 # Number of categories
#' n <- 1e3 # Number of samples
#'
#' p_fix <- 0.4 # Fix probability
#'
#' # Simulate probabilities of remaining categories given `p_fix`
#' set.seed(1234)
#'
#' # Draw `ncats - 1` random numbers so that they sum up to `1 - pfix`
#' ps <- replicate(
#'   n = n,
#'   expr = {
#'     tmp <- runif(n = ncats - 1)
#'     tmp / sum(tmp) * (1 - p_fix)
#'   }
#' )
#' x <- cbind(rep(p_fix, n), t(ps))
#'
#' pfirst <- calc_pfirst(x)
#' excessp <- calc_excessp(x)
#'
#' if (interactive() && requireNamespace("graphics")) {
#'   plot(
#'     excessp,
#'     pfirst,
#'     main = paste("Fixed p =", p_fix),
#'     xlim = c(0, 1),
#'     ylim = c(0, 1)
#'   )
#'   graphics::abline(0, 1, col= "red")
#'   graphics::abline(h = p_fix, lty = 2)
#'   graphics::abline(v = p_fix, lty = 2)
#' }
#'
#' @name rfuncertainty
NULL

#' @rdname rfuncertainty
#'
#' @section Details:
#' `calc_pfirst()` returns the maximum value per row of `x`.
#' For `x` representing class probabilities predicted by a random forest,
#' then `calc_pfirst()` represents, as measure of prediction certainty,
#' the probability of the most likely category. This metric varies between
#'   * 1, being completely certain
#'   * `1 / (#categories)`, being completely uncertain.
#'
#' @export
calc_pfirst <- function(x) {
  stopifnot(requireNamespace("matrixStats"))

  matrixStats::rowMaxs(as.matrix(x))
}


#' @rdname rfuncertainty
#'
#' @section Details:
#' `calc_excessp()` returns the difference between the largest and second
#' largest value per row of `x`.
#' For `x` representing class probabilities predicted by a random forest,
#' then `calc_excessp()` represents, as measure of prediction certainty,
#' how much the probability of the most likely category exceeds the second most
#' likely category. This metric varies between
#'   * 1, being completely certain
#'   * 0, being completely uncertain.
#'
#' @examples
#' ps <- rbind(
#'   c(0, 1, 0, 0),
#'   c(0.5, 0, 0, 0.5),
#'   c(0.4, 0, 0.1, 0.4),
#'   c(0.5, 0.4, 0.1, 0),
#'   c(0.25, 0.25, 0.25, 0.25)
#' )
#' calc_excessp(ps) ## expect (c(1, 0, 0, 0.1, 0))
#'
#' @export
calc_excessp <- function(x) {
  stopifnot(requireNamespace("matrixStats"))

  x <- as.matrix(x)
  # Calculate difference between pfirst and each probability
  pfirst <- matrixStats::rowMaxs(x)
  d <- -sweep(x, MARGIN = 1, STATS = pfirst, FUN = "-")
  # Account for multiple equally largest values -> excessp = 0
  nfirsts <- matrixStats::rowCounts(abs(d) < sqrt(.Machine[["double.eps"]]))
  # Ignore differences between pfirst and itself
  d[d <= 0] <- NA
  # Calculate smallest difference
  res <- matrixStats::rowMins(d, na.rm = TRUE)
  res[nfirsts > 1] <- 0
  res
}


#' Identify most important `R&R` predictor (`MIRRP`)
#'
#' We define the `MIRRP` as the predictor that is
#' associated with the largest, positive distance.
#' There is no `MIRRP` if maximum distance value is shared by
#' at least 50% of predictors. Otherwise, ties are resolved at
#' random.
#'
#' @examples
#' x <- rbind(
#'   1:5,
#'   5:1,
#'   rep(4, 5),
#'   c(4, 4, 1, 1, 1),
#'   rep(-1, 5)
#' )
#' newRR3::calc_MIRRP_from_dists(x, seed = 123) # c(5, 1, NA, 1, NA)
#'
#' @export
calc_MIRRP_from_dists <- function(
  x,
  tol = sqrt(.Machine[["double.eps"]]),
  seed = NULL
) {
  stopifnot(length(dim(x)) == 2L)

  if (is.null(seed) || !is.na(seed)) set.seed(seed = seed)

  apply(
    x,
    MARGIN = 1L,
    FUN = function(x) {
      # Suppressed warnings:
      #   "In max(x, na.rm = TRUE) : no non-missing arguments
      #   to max; returning -Inf"
      mx <- suppressWarnings(max(x, na.rm = TRUE))
      ismx <- which(abs(x - mx) < tol & x > 0)
      n <- length(ismx)
      if (n == 1L) {
        ismx
      } else if (n >= length(x) / 2 || n == 0L) {
        NA_integer_
      } else {
        sample(ismx, size = 1L)
      }
    }
  )
}



#' Identify column index of nearest value by rows
#'
#' @examples
#' loc <- c(1.5, 2, 4, 2)
#' mat <- rbind(
#'   1:5,
#'   5:1,
#'   rep(4, 5),
#'   c(4, 4, 2, 1, 1)
#' )
#' newRR3::calc_nearest_column(loc, mat) # c(1 or 2, 4, any of 1:5, 3)
#'
#' @export
calc_nearest_column <- function(
  x,
  mat,
  tol = sqrt(.Machine[["double.eps"]]),
  seed = NULL
) {
  stopifnot(
    length(dim(mat)) == 2L,
    length(x) == nrow(mat)
  )

  if (is.null(seed) || !is.na(seed)) set.seed(seed = seed)

  apply(
    abs(sweep(mat, MARGIN = 1L, STATS = x, FUN = "-")),
    MARGIN = 1L,
    FUN = function(d) {
      if (anyNA(d)) {
        NA_real_
      } else {
        tmp <- which(abs(d - min(d)) < tol)
        if (length(tmp) > 1L) {
          # Break ties at random
          sample(tmp, size = 1L)
        } else {
          tmp
        }
      }
    }
  )
}
