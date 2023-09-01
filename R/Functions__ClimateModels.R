
#' Select a function based on matching a name against listed options
#'
#' @param x One character string. The name for which a function is returned
#' either based on a match against an element of `options` or `fun_default`.
#' @param options A list or `NULL`. Elements of `options` consists of lists
#' with two named elements: `vars` (a vector of character strings against which
#' `x` is compared) and `fun` (a function to be returned if `x` matches an
#' element of `vars`; see requirements under `return`).
#' @param fun_default A function (see requirements under `return`).
#'
#' @return A function that takes two arguments `x` and `y`
#' (both vector of equal length) and
#' which returns a vector of equal length to `x` and `y`.
#'
#' @examples
#' opts <- list(
#'   part1 = list(vars = c("a", "c", "d"), fun = function(x, y) paste(y, y)),
#'   part2 = list(vars = c("b", "e"), fun = function(x, y) paste(x, x))
#' )
#' deffun <- function(x, y) x + y
#'
#' fun_d <- select_function("d", options = opts, deffun)
#' fun_e <- select_function("e", options = opts, deffun)
#' fun_f <- select_function("f", options = opts, deffun)
#'
#' fun_d(0:2, 33:35)
#' fun_e(0:2, 33:35)
#' fun_f(0:2, 33:35)
#'
#' @export
# nolint start: undesirable_function_linter.
select_function <- function(
  x,
  options = list(list(vars = NULL, fun = NULL)),
  fun_default = function(x, y) x - y
) {

  # Check arguments
  stopifnot(
    length(x) == 1L,
    is.null(options) ||
      all(
        unlist(
          lapply(
            options,
            function(e) is.list(e) && all(c("vars", "fun") %in% names(e))
          )
        )
      )
  )


  # Identify if `x` matches any of the options
  id_excpt <- which(
    vapply(
      options,
      function(e) isTRUE(x %in% e[["vars"]]),
      FUN.VALUE = NA
    )
  )

  n_excpt <- length(id_excpt)

  # Get appropriate function for `x`
  resfun <- if (n_excpt == 0L) {
    fun_default

  } else if (n_excpt == 1L) {
    options[[id_excpt]][["fun"]]

  } else {
    stop(shQuote(x), " matches multiple options.")
  }

  # Check resulting function: arguments `x` and `y`; vectorized
  stopifnot(
    identical(names(formals(resfun)), c("x", "y")),
    {
      n <- 3L
      tmp <- resfun(seq_len(n), seq_len(n))
      is.vector(tmp) && identical(length(tmp), n)
    }
  )

  resfun
}
# nolint end: undesirable_function_linter.


#' Frequency of elements in `x` with the same sign as `x_central`
#'
#' @examples
#' x <- seq(-5, 10)
#' agreement_frequency(x)
#' agreement_frequency(x, x_central = -10)
#' agreement_frequency(x, x_central = 0)
#'
#' x <- matrix(rnorm(100), nrow = 20)
#' agreement_frequency(x)
#' agreement_frequency(x, x_central = -10)
#' agreement_frequency(x, x_central = 0)
#' agreement_frequency(x, x_central = c(rep(1, 10), rep(-1, 10)))
#'
#' @export
agreement_frequency <- function(x, x_central = NULL) {
  if (is.null(dim(x)) || isTRUE(length(dim(x)) != 2L)) {
    x <- matrix(
      data = as.vector(unlist(x)),
      nrow = 1L
    )
  }

  if (is.null(x_central)) {
    x_central <- apply(x, MARGIN = 1L, stats::median, na.rm = TRUE)
  }

  apply(
    X = cbind(x_central, x),
    MARGIN = 1L,
    FUN = function(x) {
      # frequency of how many elements of `x` have the same sign as `x_central`
      mean(sign(x[[1L]]) == sign(x[-1L]), na.rm = TRUE)
    }
  )
}


#' Robustness is agreement larger than a frequency threshold
#'
#' @examples
#' x <- seq(-5, 10)
#' robustness(agreement_frequency(x), frq_robust = 0.9)
#'
#' x <- matrix(rnorm(100), nrow = 20)
#' robustness(
#'   agreement_frequency(x, x_central = c(rep(1, 10), rep(-1, 10))),
#'   frq_robust = 0.7
#' )
#'
#' @export
robustness <- function(x, frq_robust) {
  as.integer(x > frq_robust)
}





# Cross categories of `x` (e.g., RR levels, defined in `x_conv`) with
# robust/nonrobust categories in `xrobust`
# to create robust/nonrobust x-category combinations (defined in `robust_conv`)

#' Combine categorical data with robust/unrobust classifier
#'
#' @param x An integer vector. Factor values corresponding to
#' categories of `x_conv[["levels"]]`
#'
#' @examples
#' set.seed(3454)
#' N <- 10L
#'
#' xlevels <- letters[1:4]
#' x_conv <- make_convolution(xlevels)
#' robust_levels <- c("nonrobust", "robust")
#' robust_conv <- make_2elems_convolution(
#'   levels1 = x_conv[["levels"]],
#'   levels2 = robust_levels,
#'   signs = x_conv[["sign"]]
#' )
#'
#' x <- sample(
#'   x = seq_along(x_conv[["levels"]]),
#'   size = N,
#'   replace = TRUE
#' )
#' xrobust <- sample(
#'   x = seq_along(robust_levels) - 1L,
#'   size = N,
#'   replace = TRUE
#' )
#'
#' rx <- robustify(x, xrobust, x_conv, robust_conv, robust_levels)
#'
#' data.frame(
#'   x,
#'   xf = factor(
#'     x,
#'     levels = seq_along(x_conv[["levels"]]),
#'     labels = x_conv[["levels"]]
#'   ),
#'   xrobust,
#'   xrobustf = factor(
#'     1L + xrobust,
#'     levels = seq_along(robust_levels),
#'     labels = robust_levels
#'   ),
#'   rx,
#'   rxf = factor(
#'     rx,
#'     levels = seq_along(robust_conv[["levels"]]),
#'     labels = robust_conv[["levels"]]
#'   )
#' )
#'
#' @export
robustify <- function(x, xrobust, x_conv, robust_conv, robust_levels) {
  stopifnot(
    length(x) == length(xrobust),
    c("levels", "sign") %in% names(x_conv),
    max(x, na.rm = TRUE) <= length(x_conv[["levels"]]),
    c("robust", "x", "levels", "sign") %in% names(robust_conv),
    max(xrobust, na.rm = TRUE) <= 1
  )

  res <- merge(
    data.frame(
      id = seq_along(x),
      robust = robust_levels[1L + xrobust],
      x = x_conv[["levels"]][x]
    ),
    robust_conv,
    by = c("robust", "x"),
    all.x = TRUE,
    sort = FALSE
  )
  res <- res[order(res[["id"]]), , drop = FALSE]

  stopifnot(
    nrow(res) == length(x)
  )

  match(res[, "levels", drop = TRUE], robust_conv[["levels"]], nomatch = NA)
}



#' Simplify convoluted categories (deltas of a category) to direction
#'
#' @param x An integer vector representing convoluted levels.
#' @param x_conv A data frame describing convoluted `"levels"` of `x`
#' and their `"sign"`.
#' @param simple_conv A data frame describing simple `"levels"`
#' and their `"sign"`.
#'
#' @return An integer vector representing levels of
#' `simple_conv[["levels"]]` that were obtained from translating
#' `x` via their sign (i.e., direction) matched to the sign of levels
#' of `simple_conv`.
#'
#' @seealso [simplify_robustdelta()]
#'
#' @examples
#' Ncats <- 4L
#' n <- 10L
#' x <- sample(x = Ncats, size = n, replace = TRUE)
#' x_conv <- list(
#'   levels = letters[seq_len(Ncats)],
#'   sign = sign(seq_len(Ncats) - Ncats / 2L)
#' )
#' simple_conv <- list(
#'   levels = c("decrease", "stable", "increase"),
#'   sign = c(-1L, 0L, 1L)
#' )
#'
#' xsimple <- simplify_delta(x, x_conv, simple_conv)
#'
#' data.frame(
#'   x = x_conv[["levels"]][x],
#'   sign = x_conv[["sign"]][x],
#'   simple = simple_conv[["levels"]][xsimple]
#' )
#'
#' @export
simplify_delta <- function(x, x_conv, simple_conv) {
  req_convs <- c("levels", "sign")
  stopifnot(
    req_convs %in% names(simple_conv),
    req_convs %in% names(x_conv),
    max(x, na.rm = TRUE) <= length(x_conv[["levels"]])
  )

  match(x_conv[["sign"]][x], table = simple_conv[["sign"]])
}

#' Simplify robust convoluted categories (deltas of a category) to direction
#'
#' @param x An integer vector representing robust convoluted levels.
#' @param x_conv A data frame describing `"robust"` convoluted `"levels"` of `x`
#' and their `"sign"`.
#' @param simple_conv A data frame describing `"robust"` simple `"levels"`
#' and their `"sign"`.
#'
#' @return An integer vector representing levels of
#' `simple_conv[["levels"]]` that were obtained from translating
#' `x` via their sign (i.e., direction) and robustness
#' matched to the sign and robustness of levels of `simple_conv`.
#'
#' @seealso [simplify_delta()]
#'
#' @examples
#' Ncats <- 2L * 4L
#' n <- 20L
#' x <- sample(x = Ncats, size = n, replace = TRUE)
#'
#' tmpr <- rep(c("nonrobust", "robust"), times = Ncats / 2L)
#' x_conv <- list(
#'   levels = paste0(tmpr, "_", rep(letters[seq_len(Ncats)], each = 2L)),
#'   robust = tmpr,
#'   sign = rep(sign(seq_len(Ncats) - Ncats / 2L), each = 2L)
#' )
#' tmpr <- rep(c("nonrobust", "robust"), times = 3L)
#' simple_conv <- list(
#'   levels = paste0(tmpr, "_", rep(c("decr", "stable", "incr"), each = 2L)),
#'   robust = tmpr,
#'   sign = rep(c(-1L, 0L, 1L), each = 2L)
#' )
#'
#' xsimple <- simplify_robustdelta(x, x_conv, simple_conv)
#'
#' data.frame(
#'   x = x_conv[["levels"]][x],
#'   sign = x_conv[["sign"]][x],
#'   simple = simple_conv[["levels"]][xsimple]
#' )
#'
#' @export
simplify_robustdelta <- function(x, x_conv, simple_conv) {
  req_convs <- c("levels", "sign", "robust")
  stopifnot(
    req_convs %in% names(simple_conv),
    req_convs %in% names(x_conv),
    max(x, na.rm = TRUE) <= length(x_conv[["levels"]])
  )

  res <- merge(
    data.frame(
      id = seq_along(x),
      robust = x_conv[["robust"]][x],
      sign = x_conv[["sign"]][x],
      stringsAsFactors = FALSE
    ),
    data.frame(
      simple_level = seq_along(simple_conv[["levels"]]),
      robust = simple_conv[["robust"]],
      sign = simple_conv[["sign"]],
      stringsAsFactors = FALSE
    ),
    all.x = TRUE,
    sort = FALSE
  )

  res[["simple_level"]][order(res[["id"]])]
}



#' Convert convoluted (change) categories to simplified change categories
#'
#' @examples
#' n = 20L
#'
#' # R & R categories and convolution
#' x_lvls <- c("L", "ML", "M", "H+MH")
#' x_conv <- make_convolution(x_lvls)
#'
#' # Example data
#' x <- sample(x = nrow(x_conv), size = n, replace = TRUE)
#'
#' # Simple decrease/stable/increase categories
#' dsi <- data.frame(
#'   levels = c("decrease", "stable", "increase"),
#'   sign = c(-1L, 0L, 1L)
#' )
#'
#' # Target convolution of R&R categories and simple decreases/stable/increases
#' sdo_from <- make_2elems_convolution(
#'   levels1 = dsi[["levels"]],
#'   levels2 = x_lvls,
#'   signs = dsi[["sign"]],
#'   names = c("basic", "from")
#' )
#' sdo_to <- make_2elems_convolution(
#'   levels1 = dsi[["levels"]],
#'   levels2 = x_lvls,
#'   signs = dsi[["sign"]],
#'   names = c("basic", "to")
#' )
#'
#' # Translation from convoluted R&R to simplified changes in R&R categories
#' xres <- simplify_delta_by(
#'   x = x,
#'   x_conv = x_conv,
#'   sdo = sdo_from,
#'   by = "from"
#' )
#'
#' data.frame(
#'   x = x_conv[["levels"]][x],
#'   sign = x_conv[["sign"]][x],
#'   simple = sdo_from[["levels"]][xres]
#' )
#'
#' xres <- simplify_delta_by(
#'   x = x,
#'   x_conv = x_conv,
#'   sdo = sdo_to,
#'   by = "to"
#' )
#'
#' data.frame(
#'   x = x_conv[["levels"]][x],
#'   sign = x_conv[["sign"]][x],
#'   simple = sdo_to[["levels"]][xres]
#' )
#'
#' @export
simplify_delta_by <- function(x, x_conv, sdo, by = c("from", "to")) {
  req_convs <- c("levels", "sign", by)
  stopifnot(
    req_convs %in% names(sdo),
    req_convs %in% names(x_conv),
    max(x, na.rm = TRUE) <= length(x_conv[["levels"]])
  )

  res <- merge(
    data.frame(
      id = seq_along(x),
      drct = x_conv[[by]][x],
      sign = x_conv[["sign"]][x],
      stringsAsFactors = FALSE
    ),
    data.frame(
      simpleby_level = seq_along(sdo[["levels"]]),
      drct = sdo[[by]],
      sign = sdo[["sign"]],
      stringsAsFactors = FALSE
    ),
    all.x = TRUE,
    sort = FALSE
  )

  res[["simpleby_level"]][order(res[["id"]])]
}



#' Sort RR (robust) deltas by reference category
#' @export
sort_deltas_by_reference <- function(
  x,
  ref_lvls,
  x_conv,
  simple_conv,
  is_robust
) {
  var_from <- if (is_robust) "x_from" else "from"

  stopifnot(
    c("levels", var_from) %in% names(x_conv),
    "levels" %in% names(simple_conv)
  )

  tmp_resp_ids <- match(x, table = x_conv[["levels"]])

  fun_simplify <- if (is_robust) {
    newRR3::simplify_robustdelta
  } else {
    newRR3::simplify_delta
  }

  tmp_simple_ids <- fun_simplify(
    x = tmp_resp_ids,
    x_conv = x_conv,
    simple_conv = simple_conv
  )

  data.frame(
    # Calculate simplified (robust) deltas
    Response = factor(
      simple_conv[["levels"]][tmp_simple_ids],
      levels = simple_conv[["levels"]],
      ordered = TRUE
    ),

    # Identify reference category (e.g., historical RR)
    Reference = factor(
      x_conv[[var_from]][tmp_resp_ids],
      levels = ref_lvls,
      ordered = TRUE
    ),

    stringsAsFactors = FALSE
  )
}
