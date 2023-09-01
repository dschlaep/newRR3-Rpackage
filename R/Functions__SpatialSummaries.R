
#' Create identifying tag, suitable for file names
#'
#' @param x A character string or vector of character strings.
#'
#' @return A copy of `x` where multiple strings are collapsed by `"_"` and
#' all non alpha-numeric characters removed.
#'
#' @examples
#' create_tu_tag("test12a")
#' create_tu_tag(c("test12a", "test12b", "test13a"))
#' create_tu_tag("_test12a.")
#' create_tu_tag(c("_test12a.", "/test12b?", "-test13a|"))
#'
#' @export
create_tu_tag <- function(x) {
  paste(gsub("[^[:alnum:]]", "", x), collapse = "_")
}


#' Weighted quantiles of a variable in a data frame
#'
#' @param x A two dimensional object with columns names corresponding to
#' `var` and `var_count`.
#' @param var A character string identifying the variable in `x` for which
#' to take quantiles.
#' @param var_count A character string identifying the column in `x` that
#' provides the frequency weights for `var`.
#' @param probs A numeric vector of probabilities with values in `[0, 1]`.
#' @param na.rm A logical value.
#' @param ... Silently ignored.
#'
#' @seealso [Hmisc::wtd.quantile()], [fun_acrossgeo_categorical()]
#'
#' @return A named numeric vector of length equal to the length of `probs`.
#'
#' @examples
#' x <- cbind(
#'   x = c(1:10, NA),
#'   count.x = c(rep(4, 5), rep(1, 3), rep(0, 2), 1)
#' )
#' fun_acrossgeo_numeric(x, var = "x")
#' fun_acrossgeo_numeric(x, var = "x", na.rm = FALSE)
#' fun_acrossgeo_numeric(x[1:8, ], var = "x")
#'
#' @export
fun_acrossgeo_numeric <- function(
  x,
  var,
  var_count = "count.x",
  probs = c(0.05, 0.5, 0.95),
  na.rm = TRUE,
  ...
) {
  stopifnot(requireNamespace("Hmisc"))

  if (isTRUE(na.rm)) {
    # Deal with NAs: if `na.rm` is TRUE, remove NAs
    # If `x` ends up with no rows, then return NAs
    x <- x[, c(var, var_count), drop = FALSE]
    x <- x[stats::complete.cases(x), , drop = FALSE]
  }

  Hmisc::wtd.quantile(
    x[, var, drop = TRUE],
    weights = x[, var_count, drop = TRUE],
    normwt = FALSE, # FALSE: weights represent frequency counts
    probs = probs,
    na.rm = FALSE
  )
}


#' Counts of an integer or categorical variable in a data frame
#'
#' @param x A two dimensional object with columns names corresponding to
#' `var` and `var_count`.
#' @param var A character string identifying the variable in `x` for which
#' to sum up counts.
#' @param var_count A character string identifying the column in `x` that
#' provides the counts for `var`.
#' @param levels A vector of character strings with (possible) levels of
#' `x[[var]]` for which a count is returned.
#'
#' @param ... Silently ignored.
#'
#' @seealso [fun_acrossgeo_numeric()]
#'
#' @return A named numeric vector of length equal to the length of `levels`
#' (or unique elements of `x[[var]]` if `levels` is `NULL`)
#' plus 1 (the count of `NA`).
#'
#' @examples
#' x <- cbind(
#'   x = c(rep(1, 5), 4, rep(2, 4), NA),
#'   count.x = c(rep(4, 5), rep(1, 3), rep(0, 2), 1)
#' )
#' fun_acrossgeo_categorical(x, var = "x")
#' fun_acrossgeo_categorical(x, var = "x", levels = 0:5)
#'
#' x <- data.frame(
#'   x = c(rep("a", 5), "d", rep("b", 4), NA),
#'   count.x = c(rep(4, 5), rep(1, 3), rep(0, 2), 1)
#' )
#' fun_acrossgeo_categorical(x, var = "x")
#' fun_acrossgeo_categorical(x, var = "x", levels = letters[1:5])
#' fun_acrossgeo_categorical(x, var = "x", levels = c("b", "e", "d"))
#'
#' @export
fun_acrossgeo_categorical <- function(
  x,
  var,
  var_count = "count.x",
  levels = NULL,
  ...
) {
  # Sum up frequencies of each categories
  isc <- stats::complete.cases(x[, c(var, var_count), drop = FALSE])
  v <- x[isc, var, drop = TRUE]
  w <- x[isc, var_count, drop = TRUE]

  if (is.null(levels)) {
    levels <- sort(unique(v))
  }

  resc <- vapply(
    levels,
    function(cat) {
      # prevent integer overflow with `as.numeric()`
      sum(as.numeric(w[v == cat]))
    },
    FUN.VALUE = NA_real_
  )
  names(resc) <- levels

  # Sum up frequencies of NAs
  resna <- sum(x[!isc & is.na(x[, var]), var_count], na.rm = TRUE)

  c(resc, `NA` = resna)
}



#' Summarize across geographic space
#'
#' @param x A two-dimensional object with columns `"value"`
#' (original spatial unit identifiers) and `varname`
#' (variable to be summarized); rows represent original spatial units.
#' @param varname A character string naming the variable to be summarized, i.e.,
#' a column name of `x`.
#' @param is_var_categorical A logical value to describe if `x[[var]]` contains
#' numerical or categorical data; see also `fun`.
#' @param ctus A list defining spatial units. Each element of `ctus` defines
#' one spatial unit across which `x` is summarized. Each of these elements
#' contains three named elements:
#'   * `"dft"`: a data frame where rows represent intersections between
#'     spatial units to summarize over and the original spatial units
#'     represented by rows of `x` and the two columns represent
#'     * `"suid"`: original spatial unit identifier linking to `x[["value"]]`
#'     * `"count"`: count of pixels within spatial intersection of
#'       `x[["value"]]` and this element of `ctus`
#'   * `"ctus_orig"`: a vector of character strings naming the
#'     original spatial units involved.
#'   * `"ctus"`: a character string representing the concatenated values of
#'      `"ctus_orig"` and cleaned up to be used as part of a file name.
#' @param fun A function with arguments `x`, `var`, and `var_count`.
#' If `is_var_categorical` is `TRUE`, then `fun` should return the sums of
#' `x[[var_count]]` for each distinct value of `x[[var]]` and of `NA`,
#' see [fun_acrossgeo_categorical()] for details.
#' If `is_var_categorical` is `TRUE`, then `fun` should return
#' quantiles of `x[[var]]` weighted by `x[[var_count]]`,
#' see [fun_acrossgeo_numeric()] for details.
#' @param levels An optional vector of the unique values that
#' `x[[var]]` might have taken.
#' Passed to `fun` if `is_var_categorical` is `TRUE`.
#'
#' @return A data frame where rows represent spatial summaries for
#' each element of `ctus`. Columns include `"tag"` (the value of `"ctus"`),
#' columns for each element of `"ctus_orig"`, and columns of the
#' summarized values of `x` (corresponding to the output of `fun`).
#'
#' @examples
#' xn <- data.frame(value = 1:3, var = c(0.5, 0.1, 2))
#' xc1 <- data.frame(value = 1:3, var = c(2, 1, 4))
#' xc2 <- data.frame(value = 1:3, var = c("a", "c", "d"))
#'
#' ctus <- list(
#'   list(
#'     dft = data.frame(suid = c(1, 3), count = c(100, 0)),
#'     ctus = "a1_b3",
#'     ctus_orig = c(polyA = "a1", polyB = "b3")
#'   ),
#'   list(
#'     dft = data.frame(suid = c(2, 3), count = c(50, 50)),
#'     ctus = "a1_b2",
#'     ctus_orig = c(polyA = "a1", polyB = "b2")
#'   )
#' )
#'
#' tabulate_acrgeo(
#'   x = xn,
#'   varname = "var",
#'   is_var_categorical = FALSE,
#'   ctus = ctus,
#'   fun = fun_acrossgeo_numeric
#' )
#'
#' tabulate_acrgeo(
#'   x = xc1,
#'   varname = "var",
#'   is_var_categorical = TRUE,
#'   ctus = ctus,
#'   fun = fun_acrossgeo_categorical
#' )
#'
#' tabulate_acrgeo(
#'   x = xc1,
#'   varname = "var",
#'   is_var_categorical = TRUE,
#'   ctus = ctus,
#'   fun = fun_acrossgeo_categorical,
#'   levels = 0:5
#' )
#'
#' tabulate_acrgeo(
#'   x = xc2,
#'   varname = "var",
#'   is_var_categorical = TRUE,
#'   ctus = ctus,
#'   fun = fun_acrossgeo_categorical,
#'   levels = letters[1:5]
#' )
#'
#' @export
tabulate_acrgeo <- function(
  x,
  varname,
  is_var_categorical,
  ctus,
  fun,
  levels = NULL
) {
  fun <- match.fun(fun)
  is_var_categorical <- isTRUE(as.logical(is_var_categorical)[[1L]])

  stopifnot(
    vapply(
      ctus,
      function(ctu) setequal(c("dft", "ctus", "ctus_orig"), names(ctu)),
      FUN.VALUE = NA
    ),
    vapply(
      ctus,
      function(ctu) setequal(c("suid", "count"), colnames(ctu[["dft"]])),
      FUN.VALUE = NA
    ),
    setequal(c("value", varname), colnames(x)),
    is.function(fun)
  )

  # Determine output length
  tmp_args <- list(x = cbind(x = 1, count.x = 1), var = "x")

  if (is_var_categorical) {
    if (is.null(levels)) {
      levels <- sort(unique(x[, varname, drop = TRUE]))
    }

    tmp_args[["levels"]] <- levels
  }

  tmp_res <- rep(NA_real_, times = length(do.call(fun, args = tmp_args)))


  # Merge tabulation units with data and apply across-space function
  rest <- vapply(
    ctus,
    function(ctu) {
      fun(
        x = merge(
          ctu[["dft"]],
          x,
          by.x = "suid",
          by.y = "value",
          all.x = TRUE,
          all.y = FALSE
        ),
        var = varname,
        var_count = "count",
        levels = levels # ignored if `fun()` is numeric
      )
    },
    FUN.VALUE = tmp_res
  )

  # Prepare header information
  htmp <- vapply(
    ctus,
    function(ctu) {
      c(tag = ctu[["ctus"]], ctu[["ctus_orig"]])
    },
    FUN.VALUE = rep(
      NA_character_,
      times = 1L + length(ctus[[1L]][["ctus_orig"]])
    )
  )

  data.frame(t(htmp), t(rest), stringsAsFactors = FALSE)
}



#' Locate tabulated `csv`-spreadsheet files
#' @export
locate_tabulated_files <- function(filenames, simslices, extent, variables) {
  stopifnot(length(extent) == 1L)

  lapply(
    simslices,
    function(simslice) {
      vapply(
        variables,
        function(var) {
          tmp <- grep(
            pattern = paste0(
              simslice,
              "([[:alnum:]_-])*",
              "__acrgeo-", extent,
              "__", var, ".csv"
            ),
            x = basename(filenames)
          )
          if (length(tmp) == 1L) filenames[tmp] else NA_character_
        },
        FUN.VALUE = NA_character_
      )
    }
  )
}


#' Read files and prepare data frame for stacked bar plots
#' @export
prepare_df_for_stackedbars <- function(
  filenames,
  simslices,
  extent,
  variables
) {

  tmp <- mapply( # nolint: undesirable_function_linter.
    function(fn, am, var) {
      stopifnot(grepl(var, basename(fn)))
      tmp <- utils::read.csv(fn)
      cbind(
        Var = var,
        SimSlice = am,
        SpatialUnit = tmp[[2L]],
        utils::stack(tmp[, -(1:2), drop = FALSE])
      )
    },
    unlist(filenames),
    rep(simslices, times = lengths(filenames)),
    rep(variables, times = length(filenames)),
    SIMPLIFY = FALSE
  ) |>
    do.call(rbind, args = _)

  # remove masked out or not simulated areas
  res <- tmp[tmp[["ind"]] != "NA.", , drop = FALSE]
  res[["ind"]] <- droplevels(res[["ind"]])

  rownames(res) <- NULL

  # Beautify labels
  colnames(res)[colnames(res) %in% "ind"] <- "Response"
  res[["Var"]] <- gsub("_response", "", res[["Var"]], fixed = TRUE)
  res[["SimSlice"]] <- gsub("_", "\n", res[["SimSlice"]], fixed = TRUE)
  substr(res[["SimSlice"]], 1L, 1L) <- toupper(
    substr(res[["SimSlice"]], 1L, 1L)
  )

  res
}
