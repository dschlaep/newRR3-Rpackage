

#' Load and extract variable importance from fitted models for R&R
#'
#' Load variable importance if already extracted from fitted models for R&R;
#' otherwise, extract variable importance and write to disk.
#'
#' @param fname_varimp_data File name with path for the
#' extracted training data (stored as `"rds"` file).
#' @param fnames_mrrrf A named vector of length two with file names to the
#' fitted workflow objects for `"Resilience"` and `"Resistance"` stored as
#' `"rds"` files on disk. Only used if `fname_varimp_data` doesn't already
#' exist.
#'
#' @return A list with a data frame containing the variable importance for
#' each model.
#'
#' @export
get_varimp_rfworkflows <- function(
  fname_varimp_data,
  fnames_mrrrf,
  dir_dataraw
) {
  if (file.exists(fname_varimp_data)) {
    xvarimp <- readRDS(fname_varimp_data)

  } else {
    xvarimp <- if (TRUE) {
      # Chambers et al. 2023: Figure 2
      tmp <- utils::read.csv(
        file.path(
          dir_dataraw,
          "prepared_manually",
          # nolint start: line_length_linter.
          "Predictors19_variableImportance__Chambers2023Fig2__20211214_RR_RF_model-v20220324rf.csv"
          # nolint end: line_length_linter.
        )
      )

      list(
        Resilience = stats::setNames(
          tmp[["RSL_varimp_Chambers2023fig2a"]],
          tmp[["varname"]]
        ),
        Resistance = stats::setNames(
          tmp[["RST_varimp_Chambers2023fig2a"]],
          tmp[["varname"]]
        )
      )

    } else {
      #--- Load resilience and resistance fitted randomForest model workflow
      # (Jessi Brown, 2022-Feb-08)
      # nolint start: object_usage_linter.
      resil.wf <- readRDS(
        fnames_mrrrf[grep("Resilience", basename(fnames_mrrrf), fixed = TRUE)]
      )

      resist.wf <- readRDS(
        fnames_mrrrf[grep("Resistance", basename(fnames_mrrrf), fixed = TRUE)]
      )
      # nolint end: object_usage_linter.

      #--- Extract variable importance from workflow objects
      stopifnot(!is.null(resil.wf), !is.null(resist.wf))
      stop("Extract values from Chambers et al. 2023: Figure 2")
    }

    dir.create(
      dirname(fname_varimp_data),
      recursive = TRUE,
      showWarnings = FALSE
    )
    saveRDS(xvarimp, file = fname_varimp_data)
  }

  xvarimp
}

#' Load and extract training data from fitted models for R&R
#'
#' Load training data if already extracted from fitted models for R&R;
#' otherwise, extract training data and write to disk.
#'
#' @param fname_training File name with path for the
#' extracted training data (stored as `"rds"` file).
#' @param fnames_mrrrf A named vector of length two with file names to the
#' fitted workflow objects for `"Resilience"` and `"Resistance"` stored as
#' `"rds"` files on disk. Only used if `fname_training` doesn't already exist.
#'
#' @return A data frame with the training data.
#' @export
get_training_rfworkflows <- function(
  fname_training,
  fnames_mrrrf
) {
  if (file.exists(fname_training)) {
    xtraining <- readRDS(fname_training)

  } else {
    #--- Load resilience and resistance fitted randomForest model workflow
    # (Jessi Brown, 2022-Feb-08)
    resil.wf <- readRDS(
      fnames_mrrrf[grep("Resilience", basename(fnames_mrrrf), fixed = TRUE)]
    )

    resist.wf <- readRDS(
      fnames_mrrrf[grep("Resistance", basename(fnames_mrrrf), fixed = TRUE)]
    )

    #--- Extract training data sets from workflow objects
    trsl <- as.data.frame(
      resil.wf[["pre"]][["actions"]][["recipe"]][["recipe"]][["template"]]
    )
    trst <- as.data.frame(
      resist.wf[["pre"]][["actions"]][["recipe"]][["recipe"]][["template"]]
    )

    xtraining <- rbind(
      trst,
      trsl[!trsl[["JFSP_ID"]] %in% trst[["JFSP_ID"]], , drop = FALSE]
    )

    dir.create(dirname(fname_training), recursive = TRUE, showWarnings = FALSE)
    saveRDS(xtraining, file = fname_training)
  }


  xtraining
}


#--- Novelty ------
#' Multivariate novelty (extrapolation)
#'
#' @param traindata A numerical matrix. The training data with
#' variables organized in columns.
#' @param newdata A numerical matrix. The new data for which to calculate the
#' degree of novelty compared to `traindata`. Columns must
#' match those of `traindata`.
#' @param trimmed `NULL` (don't trim data for threshold) or a representation
#' of a version number corresponding to how `CAST::trainDI()` applies trimming,
#' see `calc_AOArdi()` for details.
#' @param cl A optional cluster object,
#' e.g., created by [parallel::makeCluster()];
#' utilized by [CAST::aoa()] and obsolete with `CAST` `v0.7.1`.
#' @param weight An optional data frame with variable weights. The data frame
#' must contain one row of weights with columns
#' for each variable in `traindata`.
#'
#'
#' @return A numerical vector corresponding to rows in `newdata`
#' where values larger than one identify novelty and values in `[0, 1]`
#' represent absence of novelty (see [is_novel()].
#' `NA` are propagated from `newdata`(unlike [CAST::aoa()]).
#'
#'
#' @section [calc_NT1()]:
#' NT1 ranges from one to unbounded positive values, as defined here;
#' originally, NT1 ranged from zero to unbounded negative values.
#' NT1 values of one indicate similarity within the range of individual
#' covariates and NT1 values larger than one indicate novelty
#' outside the range of individual covariates."
#' (Mesgaran et al. 2014).
#'
#' If the argument `trimmed` is a version number of `CAST`, then a modified
#' range is utilized as implemented in [CAST::aoa()], i.e.,
#' "outlier-removed" training minima and maxima; otherwise, the
#' original range is used, i.e., the training minima and maxima.
#'
#' @section [calc_NT2()]:
#' "NT2 can range from zero up to unbounded positive values. NT2 values
#' ranging from zero to one indicate similarity (in terms of both univariate
#' range and multivariate combination), with values closer to zero being more
#' similar. Values larger than one are indicative of novel combinations"
#' (Mesgaran et al. 2014).
#'
#' Calculates the Mahalanobis distance of each observation to the environmental
#' center of the reference set for both the reference and the projection data
#' set and calculate the ratio between the two.
#'
#' If the argument `trimmed` is a version number of `CAST`, then a modified
#' threshold is utilized as implemented in [CAST::aoa()], i.e.,
#' an "outlier-removed maximum training distance"; otherwise, the
#' original threshold is used, i.e., the maximum training distance.
#'
#'
#' @section [calc_AOArdi()]:
#' Implemented as [CAST::aoa()] but return value is `rdi = DI / threshold`
#' such that the area of applicability is identified by `rdi <= 1` and
#' areas of novelty by `rdi > 1`.
#'
#' Currently, no weighting based on variable importance and no calibration
#' based on cross-validation of models is performed.
#'
#' `NA` are propagated (unlike [CAST::aoa()]).
#'
#' `CAST` changed calculation of the threshold based on training data `di`;
#' see [issue #46](https://github.com/HannaMeyer/CAST/issues/46):
#'    - `v0.7.0`: threshold is the value in `di` closest to, but not more than
#'      `quantile(di, 0.75) + (1.5 * IQR(di))`
#'    - `v0.7.1`: threshold is `quantile(di, 0.75) + (1.5 * IQR(di))`
#'
#'
#' @references Mesgaran, M. B., R. D. Cousens, B. L. Webber, and J. Franklin.
#' 2014. Here be dragons: a tool for quantifying novelty due to covariate
#' range and correlation change when projecting species distribution models.
#' Diversity and Distributions 20:1147-1159.
#' \doi{10.1111/ddi.12209}
#'
#' @references Meyer, H., Pebesma, E. (2021) Predicting into unknown space?
#' Estimating the area of applicability of spatial prediction models.
#' Methods in Ecology and Evolution 12: 1620-1633.
#' \doi{10.1111/2041-210X.13650}
#'
#'
#' @seealso [CAST::aoa()]
#'
#' @examples
#' n <- 100
#' v <- 40
#' offset <- 10
#' set.seed(234)
#'
#' a <- c(
#'   # point cluster
#'   seq_len(n),
#'   # outliers
#'   c(-1.5, 0, 0, 1.5) * n
#' )
#' b <- c(
#'   # point cluster
#'   sample(x = v, size = n, replace = TRUE) + seq_len(n),
#'   # outliers
#'   c(n, -n / 2, 2 * n, n)
#' )
#' traindata <- data.frame(a = c(a, 1.5 * n + a), b = c(b, 0.5 * n + b))
#'
#' newdata <- data.frame(
#'   a = sample(
#'     x = seq(min(traindata[[1L]]) - offset, max(traindata[[1L]]) + offset),
#'     size = 5 * n,
#'     replace = TRUE
#'   ),
#'   b = sample(
#'     x = seq(min(traindata[[2L]]) - offset, max(traindata[[2L]]) + offset),
#'     size = 5 * n,
#'     replace = TRUE
#'   )
#' )
#'
#' trained_NT1 <- train_NT1(traindata = traindata)
#' trained_NT1mod <- train_NT1(traindata = traindata, trimmed = "0.7.0")
#' trained_NT2 <- train_NT2(traindata = traindata)
#' trained_NT2mod <- train_NT2(traindata = traindata, trimmed = "0.7.0")
#' trained_AOA <- CAST::trainDI(train = traindata)
#' trained_AOAw <- CAST::trainDI(
#'   train = traindata,
#'   weight = data.frame(a = 1, b = 10)
#' )
#'
#' xnovel <- list(
#'   NT1 = calc_NT1(newdata = newdata, trained = trained_NT1),
#'   NT1mod070 = calc_NT1(newdata = newdata, trained = trained_NT1mod),
#'   NT2 = calc_NT2(newdata = newdata, trained = trained_NT2),
#'   NT2mod070 = calc_NT2(newdata = newdata, trained = trained_NT2mod),
#'   AOArdi070 =
#'     calc_AOArdi(newdata = newdata, trained = trained_AOA, trimmed = "0.7.0"),
#'   AOArdi071 = calc_AOArdi(newdata = newdata, trained = trained_AOA)
#' )
#'
#' res <- lapply(
#'   seq_along(xnovel),
#'   function(k) {
#'     ids <- is_novel(xnovel[[k]])
#'     metric <- names(xnovel)[[k]]
#'     rbind(
#'       data.frame(
#'         metric = metric, type = "training",
#'         x = traindata[[1L]], y = traindata[[2L]]
#'       ),
#'       data.frame(
#'         metric = metric, type = "novel",
#'         x = newdata[[1L]][ids], y = newdata[[2L]][ids]
#'       ),
#'       data.frame(
#'         metric = metric, type = "not novel",
#'         x = newdata[[1L]][!ids], y = newdata[[2L]][!ids]
#'       )
#'     )
#'   }
#' )
#'
#' res <- do.call(rbind, res)
#'
#' if (interactive() && requireNamespace("ggplot2")) {
#'   ggplot2::ggplot(res) +
#'     ggplot2::aes(x = x, y = y, color = type, shape = type) +
#'     ggplot2::facet_wrap(ggplot2::vars(metric), ncol = 2L) +
#'     ggplot2::geom_point() +
#'     ggplot2::scale_color_discrete(type = newRR3::colors_okabeito()) +
#'     newRR3::ggplot2_clean_theme()
#' }
#'
#' @name novelty
NULL


#' @rdname novelty
#' @aliases train_NT2
#' @export
train_NT2 <- function(traindata, trimmed = NULL) {
  # Calculate the center of reference data: average and covariance matrix
  ref_av  <- colMeans(traindata, na.rm = TRUE)
  ref_cov <- stats::var(traindata, na.rm = TRUE)

  # Mahalanobis distance of reference data to center of reference data
  mah_ref <- stats::mahalanobis(
    x = traindata,
    center = ref_av,
    cov = ref_cov
  )

  # Distance threshold to determine if a point falls inside/outside training
  tmp <- mah_ref[is.finite(mah_ref)]

  mah_threshold <- if (isTRUE(as.numeric_version(trimmed) >= "0.7.1")) {
    # nolint start: commented_code_linter, line_length_linter.
    # CAST `v0.7.1`: threshold is `quantile(di, 0.75) + (1.5 * IQR(di))`
    # as described in Meyer & Pebesma 2021:
    # see https://github.com/HannaMeyer/CAST/commit/c92a3f2923545268db86e5bc1da6d8966d797d94
    #   thres <- stats::quantile(TrainDI, 0.75) + 1.5 * stats::IQR(TrainDI)
    # nolint end: commented_code_linter, line_length_linter.
    stats::quantile(tmp, 0.75) + 1.5 * stats::IQR(tmp)

  } else if (isTRUE(as.numeric_version(trimmed) == "0.7.0")) {
    # nolint start: commented_code_linter, line_length_linter.
    # CAST `v0.7.0`: threshold is the value in `di` closest to,
    # but not more than `quantile(di, 0.75) + (1.5 * IQR(di))`:
    #   thres <- grDevices::boxplot.stats(TrainDI)$stats[5]
    #   thres <- max(TrainDI[TrainDI <= stats::quantile(TrainDI, 0.75) + 1.5 * stats::IQR(TrainDI)])
    # nolint end: commented_code_linter, line_length_linter.
    max(tmp[tmp <= stats::quantile(tmp, 0.75) + 1.5 * stats::IQR(tmp)])

  } else if (isTRUE(as.numeric_version(trimmed) >= "0.0.0")) {
    stop("Option for ", shQuote(trimmed), " is not implemented.")

  } else {
    max(tmp)
  }

  list(
    center = ref_av,
    cov = ref_cov,
    mdistance = mah_ref,
    threshold = mah_threshold
  )
}

#' @rdname novelty
#' @aliases calc_NT2
#' @export
calc_NT2 <- function(
  newdata,
  traindata = NULL,
  trained = NULL,
  trimmed = NULL
) {

  if (is.null(trained)) {
    trained <- train_NT2(traindata = traindata, trimmed = trimmed)
  }

  stopifnot(
    identical(names(trained[["center"]]), colnames(newdata)),
    identical(colnames(trained[["cov"]]), colnames(newdata)),
    identical(rownames(trained[["cov"]]), colnames(newdata))
  )

  # Mahalanobis distance of projected data to center of reference data
  md_newdata <- stats::mahalanobis(
    x = newdata,
    center = trained[["center"]],
    cov = trained[["cov"]]
  )

  # Ratio
  md_newdata / trained[["threshold"]]
}



#' @rdname novelty
#' @aliases train_NT1
#' @export
train_NT1 <- function(traindata, trimmed = NULL) {
  stopifnot(requireNamespace("matrixStats"))
  traindata <- as.matrix(traindata)

  lowhigh <- if (isTRUE(as.numeric_version(trimmed) >= "0.7.1")) {
    # nolint start: commented_code_linter, line_length_linter.
    # CAST `v0.7.1`: threshold is `quantile(di, 0.75) + (1.5 * IQR(di))`
    # as described in Meyer & Pebesma 2021:
    # see https://github.com/HannaMeyer/CAST/commit/c92a3f2923545268db86e5bc1da6d8966d797d94
    #   thres <- stats::quantile(TrainDI, 0.75) + 1.5 * stats::IQR(TrainDI)
    #   CAST::trainDI(train = traindata)[["threshold"]]
    # nolint end: commented_code_linter, line_length_linter.
    apply(
      traindata,
      MARGIN = 2L,
      FUN = function(x) {
        stats::quantile(x, c(0.25, 0.75)) + c(-1.5, 1.5) * stats::IQR(x)
      }
    )

  } else if (isTRUE(as.numeric_version(trimmed) == "0.7.0")) {
    # nolint start: commented_code_linter, line_length_linter.
    # CAST `v0.7.0`: threshold is the value in `di` closest to,
    # but not more than `quantile(di, 0.75) + (1.5 * IQR(di))`:
    #   thres <- grDevices::boxplot.stats(TrainDI)$stats[5]
    #   thres <- max(TrainDI[TrainDI <= stats::quantile(TrainDI, 0.75) + 1.5 * stats::IQR(TrainDI)])
    # nolint end: commented_code_linter, line_length_linter.
    apply(
      traindata,
      MARGIN = 2L,
      FUN = function(x) {
        tmp <- stats::quantile(x, c(0.25, 0.75)) + c(-1.5, 1.5) * stats::IQR(x)
        range(x[x >= tmp[[1L]] & x <= tmp[[2L]]])
      }
    )

  } else if (isTRUE(as.numeric_version(trimmed) >= "0.0.0")) {
    stop("Option for ", shQuote(trimmed), " is not implemented.")

  } else {
    t(matrixStats::colRanges(traindata, na.rm = TRUE))
  }

  list(
    lowhigh = lowhigh,
    diffs = matrixStats::colDiffs(lowhigh)
  )
}


#' @rdname novelty
#' @aliases calc_NT1
#' @export
calc_NT1 <- function(
  newdata,
  traindata = NULL,
  trained = NULL,
  trimmed = NULL
) {

  if (is.null(trained)) {
    trained <- train_NT1(traindata = traindata, trimmed = trimmed)
  }

  newdata <- as.matrix(newdata)

  stopifnot(
    identical(colnames(trained[["lowhigh"]]), colnames(newdata)),
    identical(colnames(trained[["diffs"]]), colnames(newdata))
  )

  lowhigh <- array(
    trained[["lowhigh"]],
    dim = c(dim(trained[["lowhigh"]]), nrow(newdata)),
    dimnames = list(c("low", "high"), colnames(newdata), NULL)
  )

  diffs <- matrix(
    trained[["diffs"]],
    nrow = nrow(newdata),
    ncol = ncol(newdata),
    byrow = TRUE
  )

  iud <- array(0, dim = c(dim(newdata), 3L))
  iud[, , 2L] <- newdata - t(lowhigh["low", , ])
  iud[, , 3L] <- t(lowhigh["high", , ]) - newdata

  UDs <- apply(iud, 1L:2L, min) / diffs

  1 - rowSums(UDs)
}


#' @rdname novelty
#' @aliases calc_AOArdi
#' @export
calc_AOArdi <- function(
  newdata,
  trained,
  cl = NULL,
  weight = NA,
  trimmed = NULL
) {

  stopifnot(requireNamespace("CAST"))

  list_aoa_args <- list(
    newdata = newdata,
    trainDI = trained,
    weight = weight
  )

  if (getNamespaceVersion("CAST") < "0.7.1") {
    # nolint start: commented_code_linter, line_length_linter.
    # `CAST:aoa()` lost argument `cl` with `v0.7.1`, see
    # https://github.com/HannaMeyer/CAST/commit/eb9daff82b6272ef55815d7af5a86699d2f4ee35
    # nolint end: commented_code_linter, line_length_linter.
    list_aoa_args[["cl"]] <- cl
  }

  tmp <- suppressMessages(
    do.call(CAST::aoa, args = list_aoa_args)
  )

  # Distance threshold to determine if a point falls inside/outside training
  di <- tmp[["parameters"]][["trainDI"]]

  threshold <- if (isTRUE(as.numeric_version(trimmed) >= "0.7.1")) {
    # nolint start: commented_code_linter, line_length_linter.
    # CAST `v0.7.1`: threshold is `quantile(di, 0.75) + (1.5 * IQR(di))`
    # as described in Meyer & Pebesma 2021:
    # see https://github.com/HannaMeyer/CAST/commit/c92a3f2923545268db86e5bc1da6d8966d797d94
    #   thres <- stats::quantile(TrainDI, 0.75) + 1.5 * stats::IQR(TrainDI)
    # nolint end: commented_code_linter, line_length_linter.
    stats::quantile(di, 0.75) + 1.5 * stats::IQR(di)

  } else if (isTRUE(as.numeric_version(trimmed) == "0.7.0")) {
    # nolint start: commented_code_linter, line_length_linter.
    # CAST `v0.7.0`: threshold is the value in `di` closest to,
    # but not more than `quantile(di, 0.75) + (1.5 * IQR(di))`:
    #   thres <- grDevices::boxplot.stats(TrainDI)$stats[5]
    #   thres <- max(TrainDI[TrainDI <= stats::quantile(TrainDI, 0.75) + 1.5 * stats::IQR(TrainDI)])
    # nolint end: commented_code_linter, line_length_linter.
    max(di[di <= stats::quantile(di, 0.75) + 1.5 * stats::IQR(di)])

  } else if (isTRUE(as.numeric_version(trimmed) >= "0.0.0")) {
    stop("Option for ", shQuote(trimmed), " is not implemented.")

  } else {
    tmp[["parameters"]][["threshold"]]
  }

  # CAST: "AOA" is calculated such that
  #   - area of applicability where DI <= threshold
  #   - area of extrapolation where DI > threshold
  # Note: missing values are considered area of applicability

  # Here
  #   - propagate missing values
  #   - calculate rdi = DI / threshold
  #     such that aoa where rdi <= 1 and area of extrapolation where rdi > 1
  tmp[["DI"]] / threshold
}


#' Determine if `NT2` or distance-ratio of `AOA` represents novel conditions
#'
#' @param x A numerical vector with `NT2` or distance-ratios of `AOA`.
#' @return
#' A logical vector of the same length as `x` for `is_novel()`;
#' an integer vector of the same length as `x` for `is_novel_lvls()` where
#' a value of `1` indicates `"novel"` and a value of `2` indicates `"notnovel"`.
#'
#' @export
is_novel <- function(x) {
  x > 1
}

#' @rdname is_novel
#'
#' @examples
#' xt <- seq(0, 2, by = 0.5)
#' cbind(x = xt, lgl = is_novel(xt), lvls = is_novel_lvls(xt))
#'
#' @export
is_novel_lvls <- function(x) {
  2L - as.integer(is_novel(x))
}
