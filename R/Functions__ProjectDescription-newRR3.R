

# v3 corresponds to version_simdata = 20221001
# v2 corresponds to version_simdata = 20221001
# v1 corresponds to version_simdata = 20220526


#------ PROJECT FILE NAMES ------

# nolint start: line_length_linter.

#' Output folder and file names
#'
#' @section Name scheme:
#' * Name scheme for output folders is
#'   `"[experiment]_[time]_[output-set-dir]_[value-type-dir]_[variable-set]"`
#'
#' * Name scheme for output files with raster attribute tables is
#'   `"[variable-set]__[model-conditions](__[method-description(s)])+.csv"`
#'
#'   The scheme expands, for output from individual simulation runs, to
#'   `"[variable-set]__[scenario-id]_[GCM]_[experiment]_[time]__[value-type-file]-[output-set-file].csv"`
#'
#'   and, for output from across-model summaries, to
#'   `[variable-set]__[experiment]_[time]__[value-type-file]-[output-set-file]-[acrmod-summary].csv"`
#'
#' * Name scheme for output files with across-geography summaries is
#'   `[variable-set]__[experiment]_[time]__[method-description]__[acrgeo-summary]__[variable-name].csv"`
#'
#' @section Name elements:
#' The name scheme uses a fixed vocabulary with the following elements:
#'   * `variable-set` = `{Preds19, Preds19-Novelty, RR, RR-RFcertainty}`
#'   * `variable-name` = `{[variable names]}`
#'   * `scenario-id` = `{sc1..sc41}`
#'   * `GCM` = `{[global climate models]}`
#'   * `experiment` = `{ambient, historical, RCP45, RCP85}`
#'   * `time` = `{1980-2020-clim, 1950-2005-clim, 2029-2064-clim, 2064-2099-clim}`
#'   * `output-set-dir` = `{IndRuns, AcrMod}`
#'   * `output-set-file` = `{sim, acrmod}`
#'   * `value-type-dir` = `{Values, Deltas}`
#'   * `value-type-file` = `{value, delta, robust090delta, simpledelta, robust090simpledelta}`
#'   * `acrmod-summary` = `{low, med, high, agree, robust090}`
#'   * `acrgeo-summary` = `{argeo-[spatial units]}`
#
#' @name output_names
NULL

# nolint end: line_length_linter.


#' @describeIn output_names Output sub-directory name
#'
#' @export
name_output_folder <- function(
  experiment_time,
  outputset,
  valuetype,
  variableset
) {
  paste(experiment_time, outputset, valuetype, variableset, sep = "_")
}

#' @describeIn output_names Three part output file name
#'
#' @export
name1_output_ratcsv <- function(
  variableset,
  modelconditions,
  methoddescription
) {
  paste0(
    paste(variableset, modelconditions, methoddescription, sep = "__"),
    ".csv"
  )
}

#' @describeIn output_names Output file name with value type and output set
#'
#' @export
name2_output_ratcsv <- function(
  variableset,
  modelconditions,
  valuetype,
  outputset
) {
  name1_output_ratcsv(
    variableset,
    modelconditions,
    methoddescription = paste0(valuetype, "-", outputset)
  )
}



#------ CONVOLUTION OF CATEGORICAL VARIABLES ------


#' Convolution of two categorical variables
#'
#' This can be used to represent deltas of two categorical variables `x - y`.
#'
#' @param to A vector with character strings representing categories (levels)
#' under target conditions.
#' @param from A vector with character strings representing categories (levels)
#' under reference conditions.
#' @param clevels A vector with character strings representing the full set of
#' convoluted categories (levels).
#' @param sep A character string used as separator between values of
#' `to` and `from`.
#'
#' @return
#' A vector with character strings representing the convoluted categories,
#' if `clevels` is `NULL`.
#' If `clevels` is provided, then an integer vector representing the numerical
#' value of the convoluted categories relative to the set of `clevels`.
#'
#' @examples
#' n_cats <- 3L
#' cats <- letters[seq_len(n_cats)]
#' set.seed(456)
#' x <- sample(x = cats, size = 10L, replace = TRUE)
#' y <- sample(x = cats[-1L], size = 10L, replace = TRUE)
#' all_convoluted_cats <- {
#'   tmp <- expand.grid(cats, cats)
#'   paste(tmp[[1L]], tmp[[2L]], sep = "_from_")
#' }
#'
#' cc1 <- newRR3::convolve_levels(to = x, from = y)
#' cc2 <- newRR3::convolve_levels(
#'   to = x,
#'   from = y,
#'   clevels = all_convoluted_cats
#' )
#'
#' fcc1 <- factor(cc1, levels = all_convoluted_cats)
#' all.equal(as.integer(fcc1), cc2)
#'
#' fcc2 <- factor(all_convoluted_cats[cc2], levels = all_convoluted_cats)
#' all.equal(fcc1, fcc2)
#'
#' @export
convolve_levels <- function(to, from, clevels = NULL, sep = "_from_") {
  res <- paste(to, from, sep = as.character(sep)[[1L]])

  if (is.null(clevels)) {
    res
  } else {
    match(res, clevels)
  }
}

#' @export
deconvolve_into_levels <- function(x, clevels = NULL, bi_scale = FALSE) {
  res <- data.frame(
    do.call(
      rbind,
      strsplit(
        if (is.null(clevels)) x else clevels[x],
        split = "_from_",
        fixed = TRUE
      )
    ),
    stringsAsFactors = FALSE
  )
  colnames(res) <- c("to", "from")
  res <- res[, c("from", "to"), drop = FALSE]

  if (isTRUE(bi_scale)) {
    rrl <- unique(res[, "from"])
    res[, "bi_scale"] <- paste(
      match(res[, "from"], rrl),
      match(res[, "to"], rrl),
      sep = "-"
    )
  }

  res
}


#' Description of convolution of two categorical variables
#'
make_convolution_2way <- function(
  levels1,
  levels2,
  sep = "_from_",
  signs = NULL,
  convolve_method = c("to-from", "from-to")
) {
  convolve_method <- match.arg(convolve_method)

  res <- expand.grid(
    from = levels1,
    to = levels2,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )

  res[, "bi_scale"] <- paste(
    match(res[, "from"], levels1),
    match(res[, "to"], levels2),
    sep = "-"
  )

  if (!is.null(signs)) {
    res[, "sign"] <- if (isTRUE(identical(signs, "automatic"))) {
      vapply(
        strsplit(res[, "bi_scale"], split = "-", fixed = TRUE),
        function(x) {
          x <- as.integer(x)
          if (x[[2L]] > x[[1L]]) 1L else if (x[[2L]] < x[[1L]]) -1L else 0L
        },
        FUN.VALUE = NA_integer_
      )
    } else {
      signs[match(res[, "to"], levels2)]
    }
  }

  res[, "levels"] <- switch(
    EXPR = convolve_method,
    "to-from" = convolve_levels(res[, "to"], res[, "from"], sep = sep),
    "from-to" = convolve_levels(res[, "from"], res[, "to"], sep = sep)
  )

  res
}

#' Description of convolution of a categorical variable with itself
#' @export
make_convolution <- function(levels) {
  make_convolution_2way(
    levels1 = levels,
    levels2 = levels,
    sep = "_from_",
    signs = "automatic",
    convolve_method = "to-from"
  )
}


#' @export
make_2elems_convolution <- function(
  levels1,
  levels2,
  signs = NULL,
  names = c("x", "robust")
) {
  res <- make_convolution_2way(
    levels1 = levels2,
    levels2 = levels1,
    sep = "_",
    signs = signs,
    convolve_method = "from-to"
  )
  names(res)[1:2] <- names[2:1]

  res
}





#------ EXTRACT INFORMATION FROM RUNTIME OBJECTS ------

#' Detects categorical variables based on hard-coded set of tags
#'
#' @param x A vector of character strings containing variable names.
#' @param tags_categorical A vector of character strings containing patterns
#' that identify categorical variables.
#' @param tags_noncategorical A vector of character strings containing patterns
#' that identify any variable as not categorical (overrules `tags_categorical`).
#'
#' @return A logical vector indicating whether each element of `x` is
#' categorical `TRUE` or not
#' (based on pattern matches against `tags_categorical` and
#' `tags_noncategorical`).
#'
#' @section Notes:
#' * Robustness (across models) of any variable is categorical;
#' however, if robustness is combined with the delta, then only already
#' categorical variables are categorical.
#' * Agreement (across models) of any variable is numerical.
#'
#' @examples
#' ## Expected outcome: FALSE
#' newRR3::is_categorical("cat")
#' ## Expected outcome: TRUE
#' newRR3::is_categorical("cat", tags_categorical = "cat")
#' ## Expected outcome: FALSE
#' newRR3::is_categorical("cat_acrmodagree", tags_categorical = "cat")
#' ## Expected outcome: TRUE
#' newRR3::is_categorical("cat_acrmodrobust")
#' ## Expected outcome: FALSE
#' newRR3::is_categorical("cat_acrmodrobust090delta")
#' ## Expected outcome: TRUE
#' newRR3::is_categorical("cat_acrmodrobust090delta", tags_categorical = "cat")
#' ## Expected outcome: FALSE, TRUE, FALSE, FALSE
#' newRR3::is_categorical(c("a", "a_acrmodrobust", "b", "b_nonrobust"))
#'
#' @export
is_categorical <- function(
  x,
  tags_categorical = c(
    "Resistance_response",
    "Resilience_response",
    "Resistance_mirrp",
    "Resilience_mirrp",
    "_(acrmodrobust$)|(acrmodrobust_)", # this excludes "_robust090*delta"
    "_isnovel"
  ),
  tags_noncategorical = "acrmodagree"
) {
  res1 <- vapply(
    tags_categorical,
    FUN = grepl,
    FUN.VALUE = rep(NA, length = length(x)),
    x = x
  )

  res2 <- vapply(
    tags_noncategorical,
    FUN = grepl,
    FUN.VALUE = rep(NA, length = length(x)),
    x = x
  )

  if (length(x) > 1) {
    apply(res1, MARGIN = 1L, FUN = any) & !apply(res2, MARGIN = 1L, FUN = any)
  } else {
    any(res1) && !any(res2)
  }
}

#' Standardize variable group identifiers
#'
#' @param x A vector of character strings representing
#' variable group identifiers.
#' @param tags A named vector of character strings representing
#' all possible variable group identifiers where names represent standardized
#' names.
#'
#' @return A vector of character strings where each value of `x` is
#' translated in matching name of `tags`.
#'
#' @examples
#' tags <- c(first = "1st", second = "2nd", third = "3rd")
#' standardize_vtags(c("2nd", "5th", "3rd"), tags)
#'
#' @export
standardize_vtags <- function(x, tags) {
  names(tags)[match(x, tags)]
}


#' Remove suffices appended to derived variables
#'
#' @param x A vector of character strings.
#'
#' @return A modified version of `x` representing base variable names.
#'
#' @examples
#' simplify_variables_names(
#'   c(
#'     "R_response",
#'     "R_prob.pred_H.MH_robustdelta_acrmoddistr",
#'     "R_prob.pred_H.MH_acrmoddistr_robustdelta"
#'   )
#' )
#'
#' @export
simplify_variables_names <- function(
  x,
  tags_to_remove = c(
    "delta", "robustdelta", "simpledelta",
    "acrmoddistr", "acrmodagree", "acrmodrobust"
  )
) {
  tmp_ptrn <- paste0(
    "(", tags_to_remove, ")",
    collapse = "|"
  )

  strsplit(x, split = "_", fixed = TRUE) |>
    lapply(
      function(tmp) gsub(tmp_ptrn, "", x = tmp)
    ) |>
    lapply(function(tmp) paste0(tmp[nchar(tmp) > 0L], collapse = "_")) |>
    unlist()
}


#' Extract file base name elements separated by `__`
#'
#' @param fnames A vector of character strings representing file names.
#' @param pos An integer value identifying the nth-element.
#'
#' @return A vector of character strings with the `pos`-nth element of the
#' `basename()` of each `fnames` as separated by `__`.
#'
#' @examples
#' extract_fname_element(
#'   file.path("here", "RR__climate-projection__delta-acrmod.csv"),
#'   pos = 1L
#' )
#' extract_fname_element(
#'   file.path("here", "RR__climate-projection__delta-acrmod.csv"),
#'   pos = 3L
#' )
#'
#' @export
extract_fname_element <- function(fnames, pos) {
  basename(fnames) |>
    gsub(".csv$", "", x = _) |>
    strsplit(split = "__", fixed = TRUE) |>
    lapply(function(x) x[[as.integer(pos)]]) |>
    unlist()
}


#' Find levels of a categorical variable
#'
#' @seealso [newRR3::is_categorical()]
#'
#' @examples
#' meta <- newRR3::get_project_description(
#'   include_variables = TRUE,
#'   include_scenarios = TRUE,
#'   include_deltas = TRUE,
#'   include_summaries = TRUE,
#'   dir_data = system.file("extdata", "data-raw", package = "newRR3"),
#'   tag_prj = "newRR3",
#'   tag_sims = ""
#' )
#'
#' get_levels("rr", "value-sim", meta)
#' get_levels("rr", "value-sim", meta, return_labels = FALSE)
#' get_levels("rr", "delta-acrmod-robust", meta)
#' get_levels("rr", "delta-acrmod", meta)
#' get_levels("rr", "simpledelta-acrmod", meta)
#' get_levels("rr", "robustdelta-acrmod", meta)
#' get_levels("rr", "robustsimpledelta-acrmod", meta)
#' get_levels("rr", "robust090delta-acrmod", meta)
#' get_levels("preds", "delta-acrmod", meta)
#'
#' fn <- file.path("here", "RR__climate-projection__delta-acrmod.csv")
#' get_levels(
#'   vtag = standardize_vtags(
#'     extract_fname_element(fn, pos = 1L),
#'     tags = meta[["varsets"]][["tags"]]
#'   ),
#'   vdesc = extract_fname_element(fn, pos = 3L),
#'   meta = meta
#' )
#'
#' @export
# nolint start: cyclocomp_linter.
get_levels <- function(vtag, vdesc, meta, return_labels = TRUE) {
  if (grepl("delta-acrmod-robust", vdesc, fixed = TRUE)) {
    #--- robustness
    res <- if (return_labels) {
      meta[["acrmod"]][["robust"]][["levels"]]
    } else {
      c(0L, 1L)
    }

    return(res)

  } else {
    has_values <- grepl("value-", vdesc, fixed = TRUE)

    lvls <- if (grepl("simpledelta-", vdesc, fixed = TRUE)) {
      #--- simplified deltas
      if (grepl("robust[[:alnum:]]*simpledelta-", vdesc)) {
        # simplified robust (delta) categories
        meta[["delta"]][["basic"]][["robust"]][["levels"]]

      } else if (grepl("simpledelta-", vdesc, fixed = TRUE)) {
        # simplified (delta) categories
        meta[["delta"]][["basic"]][["full"]][["levels"]]
      }

    } else if (vtag == "rr") {
      #--- RR categories

      if (has_values) {
        # RR: categorical predictions
        meta[["varsets"]][[vtag]][["levels"]]

      } else if (grepl("robust[[:alnum:]]*delta-", vdesc)) {
        # RR: robustified convoluted (delta) categories
        meta[["delta"]][[vtag]][["robust"]][["levels"]]

      } else if (grepl("delta-", vdesc, fixed = TRUE)) {
        # RR: convoluted (delta) categories
        meta[["delta"]][[vtag]][["full"]][["levels"]]
      }

    } else if (vtag == "mirrp" && has_values) {
      meta[["varsets"]][["preds"]][["varname"]]

    } else if (vtag == "novelty" && has_values) {
      meta[["varsets"]][[vtag]][["levels"]]
    }

    return(if (return_labels) lvls else seq_along(lvls))
  }
}
# nolint end: cyclocomp_linter.



#------ PROJECT DESCRIPTION ------

#' List project description / metadata
#'
#' @param include_variables A logical value. Include description about
#' R&R categories and R&R predictor variables. Additional information is loaded
#' from disk using `dir_data`.
#' @param include_scenarios A logical value. Include description about
#' climate scenarios represented by `rSFSW2` simulation experiments.
#' Additional information is loaded from disk using `dir_data`.
#' @param include_deltas A logical value. Include descriptions and functions
#' to determine changes between a scenario and a reference.
#' @param include_summaries A logical value. Include descriptions and
#' functions to summarize values across space and across `GCMs`.
#' @param dir_data A character string. The path to the local `data` folder,
#' required only if `include_variables` and/or `include_scenarios`.
#'
#' @return A named list.
#'
#' @examples
#' meta <- newRR3::get_project_description(
#'   include_variables = TRUE,
#'   include_scenarios = TRUE,
#'   include_deltas = TRUE,
#'   include_summaries = TRUE,
#'   dir_data = system.file("extdata", "data-raw", package = "newRR3"),
#'   tag_prj = "newRR3",
#'   tag_sims = ""
#' )
#'
#' @export
# nolint start: cyclocomp_linter.
get_project_description <- function(
  include_variables = FALSE,
  include_scenarios = FALSE,
  include_deltas = FALSE,
  include_summaries = FALSE,
  dir_data = "data-raw",
  tag_prj = "newRR_wall2wall_future",
  simexps = c("hist", "rcp45", "rcp85"),
  varsets_tags = c(
    preds = "Preds19",
    novelty = "Preds19-Novelty",
    rr = "RR",
    rfcertainty = "RR-RFcertainty",
    mirrp = "MIRRP"
  ),
  versions = list(
    # date when inputs for simulation experiment were prepared
    version_inputs = "20220314",
    # date of simulation experiment
    version_simexp = "20220408",
    # date when metrics were calculated from output of simulation experiment
    version_simdata = "20221001",
    # date of random-forest model to predict RR indicators
    version_rf = "v20220324rf"
  ),
  tag_sims = "SOILWAT2_simulations",
  isClimatology = TRUE,
  tag_scen1 = "ambient_1980-2020"
) {

  stopifnot(
    names(varsets_tags) %in% c(
      "preds", "novelty", "rr", "rfcertainty", "mirrp"
    ),
    names(versions) %in% c(
      "version_inputs", "version_simexp", "version_simdata", "version_rf"
    )
  )


  #------ Project settings ------
  meta <- list(
    v = as.numeric_version("3.2.1"),

    # Simulation experiments
    # Future wall-to-wall split into three parts
    # -> separate scenario and time periods
    simexps = list(
      list = simexps,
      tag_scen1 = tag_scen1
    ),

    isClimatology = isClimatology,

    method = "gsu",
    tag_rr = "native-gridded-rf-predictedRR_from_su-predictors19",
    tag_prj = tag_prj,

    rat_header = list(
      full = c("value", "count"),
      rangelands = c("value", "count"),
      combined = c("value", "count_sim", "count_rangelands")
    )
  )

  meta <- c(
    meta,
    #--- Versioning ------
    if (inherits(versions, "list")) versions else as.list(versions)
  )

  meta[["simexps"]][["tag0"]] <- paste0(
    meta[["version_simexp"]], "_",
    meta[["tag_prj"]],
    if (isTRUE(tag_sims > 0L)) paste0("_", tag_sims)
  )

  meta[["simexps"]][["tags"]] <- vapply(
    meta[["simexps"]][["list"]],
    function(simexp) {
      sub(
        meta[["tag_prj"]],
        paste0(meta[["tag_prj"]], "-", simexp),
        meta[["simexps"]][["tag0"]]
      )
    },
    FUN.VALUE = NA_character_
  )



  #------ Load variables ------
  meta[["varsets"]] <- list()

  meta[["varsets"]][["tags"]] <- varsets_tags


  if (isTRUE(as.logical(include_variables))) {

    dir_data_vardesc <- file.path(dir_data, "prepared_manually")
    stopifnot(
      dir.exists(dir_data),
      dir.exists(dir_data_vardesc)
    )


    list_req_columns <- c("varname", "short10")

    #--- * Predictors of RR (Jessi Brown, 2022-Feb-07) ------
    if ("preds" %in% names(meta[["varsets"]][["tags"]])) {
      tmp <- utils::read.csv(
        file = file.path(
          dir_data_vardesc,
          "description_predictors.csv"
        ),
        stringsAsFactors = FALSE
      )

      ids <- !is.na(tmp[["varname"]]) & nchar(tmp[["varname"]]) > 0L

      meta[["varsets"]][["preds"]] <- as.list(tmp[ids, , drop = FALSE])

      stopifnot(
        c("varname", "short10") %in% names(meta[["varsets"]][["preds"]]),
        lengths(meta[["varsets"]][["preds"]]) > 0L
      )

      # Check: varnames are based on the following list
      fname_rfpreds <- file.path(
        dir_data,
        "mrrrf",
        "rr_rf_predictors_v20220207.rds"
      )

      if (file.exists(fname_rfpreds)) {
        tmp <- readRDS(fname_rfpreds)
        if (!all(names(tmp) %in% meta[["varsets"]][["preds"]][["varname"]])) {
          warning("Predictor names don't match RR-RF predictors.")
        }
      }
    }


    #--- * Predictor novelty ------
    if ("novelty" %in% names(meta[["varsets"]][["tags"]])) {
      meta[["varsets"]][["novelty"]] <- as.list(
        utils::read.csv(
          file = file.path(
            dir_data_vardesc,
            "description_predictor-novelty.csv"
          ),
          stringsAsFactors = FALSE
        )
      )

      # order of levels, see `is_novel_lvls()`
      meta[["varsets"]][["novelty"]][["levels"]] <- c("novel", "notnovel")
    }


    #--- * RR: resilience and resistance indicators ------
    if ("rr" %in% names(meta[["varsets"]][["tags"]])) {
      meta[["varsets"]][["rr"]] <- as.list(
        utils::read.csv(
          file = file.path(
            dir_data_vardesc,
            "description_rr.csv"
          ),
          stringsAsFactors = FALSE
        )
      )

      meta[["varsets"]][["rr"]][["rrs"]] <- c(
        RST = "Resistance",
        RSL = "Resilience"
      )
      meta[["varsets"]][["rr"]][["levels"]] <- c("L", "ML", "M", "H+MH")

      # Check: varnames are based on the following list
      pred_types <- c("response", "prob", "index")
      rr_varnames <- apply(
        expand.grid(
          c(
            pred_types[[1L]],
            paste(
              paste0(pred_types[[2L]], ".pred"),
              sub(
                pattern = "+",
                replacement = ".",
                x = meta[["varsets"]][["rr"]][["levels"]],
                fixed = TRUE
              ),
              sep = "_"
            ),
            pred_types[[3L]]
          ),
          meta[["varsets"]][["rr"]][["rrs"]]
        )[, 2:1],
        MARGIN = 1L,
        paste0,
        collapse = "_"
      )

      stopifnot(identical(rr_varnames, meta[["varsets"]][["rr"]][["varname"]]))
    }


    #--- * RF model prediction certainty ------
    if ("rfcertainty" %in% names(meta[["varsets"]][["tags"]])) {
      meta[["varsets"]][["rfcertainty"]] <- as.list(
        utils::read.csv(
          file = file.path(
            dir_data_vardesc,
            "description_rr-rfcertainty.csv"
          ),
          stringsAsFactors = FALSE
        )
      )

      tmp <- strsplit(
        meta[["varsets"]][["rfcertainty"]][["varname"]],
        split = "_",
        fixed = TRUE
      )

      meta[["varsets"]][["rfcertainty"]][["tags"]] <- unique(
        vapply(
          tmp,
          FUN = function(x) x[[2L]],
          FUN.VALUE = NA_character_
        )
      )

      # Check that they are based on RR vocabulary
      tmprr <- vapply(
        tmp,
        FUN = function(x) x[[1L]],
        FUN.VALUE = NA_character_
      )
      stopifnot(
        tmprr %in% meta[["varsets"]][["rr"]][["rrs"]],
        table(tmprr) == length(meta[["varsets"]][["rfcertainty"]][["tags"]])
      )
    }


    #--- * Most important RR predictors ------
    if (
      all(c("rr", "preds", "mirrp") %in% names(meta[["varsets"]][["tags"]]))
    ) {
      meta[["varsets"]][["mirrp"]] <- list(
        varname = c(
          # R&R MIRRPs
          paste0(meta[["varsets"]][["rr"]][["rrs"]], "_mirrp"),
          # Euclidean (L2) distances to historical
          unlist(
            lapply(
              meta[["varsets"]][["rr"]][["rrs"]],
              function(rrs) {
                paste0(
                  meta[["varsets"]][["preds"]][["varname"]],
                  "_dL2",
                  rrs
                )
              }
            )
          )
        ),
        short10 = c(
          paste0(names(meta[["varsets"]][["rr"]][["rrs"]]), "_mirrp"),
          unlist(
            lapply(
              names(meta[["varsets"]][["rr"]][["rrs"]]),
              function(rrs) {
                paste0(
                  "Pred",
                  seq_along(meta[["varsets"]][["preds"]][["short10"]]),
                  "d",
                  rrs
                )
              }
            )
          )
        )
      )


      #--- * Check availability of minimal information of variable sets ------
      stopifnot(
        vapply(
          names(meta[["varsets"]][["tags"]]),
          function(vt) {
            all(list_req_columns %in% names(meta[["varsets"]][[vt]]))
          },
          FUN.VALUE = NA
        )
      )
    }
  }


  #------ Load scenario information ------
  if (isTRUE(as.logical(include_scenarios))) {

    dir_data_scens <- file.path(dir_data, "scens")
    stopifnot(dir.exists(dir_data_scens))

    tmp <- stats::setNames(
      vector(mode = "list", length = length(meta[["simexps"]][["list"]])),
      meta[["simexps"]][["list"]]
    )
    meta[["simexps"]][["tag_out"]] <- tmp
    meta[["simexps"]][["tag_out_short"]] <- tmp
    meta[["simexps"]][["meta_scen"]] <- tmp
    meta[["simexps"]][["tag_scen"]] <- tmp
    meta[["simexps"]][["tag_scen_path"]] <- tmp
    meta[["simexps"]][["tag_scen_histref"]] <- tmp
    meta[["simexps"]][["tag_acrmod"]] <- tmp


    tmps <- strsplit(meta[["tag_prj"]], split = "_", fixed = TRUE)[[1L]]

    for (k0 in seq_along(meta[["simexps"]][["list"]])) {
      tmp <- try(
        utils::read.csv(
          file.path(
            dir_data_scens,
            meta[["simexps"]][["tags"]][[k0]],
            "Table_ScenarioDescription.csv"
          )
        ),
        silent = TRUE
      )

      if (inherits(tmp, "try-error")) {
        warning("Could not locate simulation scenario information.")

      } else {
        meta[["simexps"]][["meta_scen"]][[k0]] <- tmp

        meta[["simexps"]][["tag_out"]][[k0]] <- paste0(
          "v", meta[["simexps"]][["tags"]][[k0]],
          "__rr_", meta[["version_rf"]]
        )

        meta[["simexps"]][["tag_out_short"]][[k0]] <- paste0(
          tmps[[length(tmps)]], "-", meta[["simexps"]][["list"]][[k0]]
        )


        # Replace "YYYYtoYYYY" with "YYYY-YYYY"
        tmp <- gsub(
          "([[:digit:]])to([[:digit:]])",
          "\\1-\\2",
          meta[["simexps"]][["meta_scen"]][[k0]][, "DeltaStr_yrs"],
          fixed = FALSE
        )

        # Scenario-specific path element
        meta[["simexps"]][["tag_scen_path"]][[k0]] <- paste0(
          tmp, if (isClimatology) "-clim"
        )

        meta[["simexps"]][["tag_scen_path"]][[k0]][[1L]] <- paste0(
          meta[["simexps"]][["tag_scen1"]], if (isClimatology) "-clim"
        )

        # Identifications of scenario
        meta[["simexps"]][["tag_scen"]][[k0]] <- paste0(
          "sc", seq_len(nrow(meta[["simexps"]][["meta_scen"]][[k0]])), "_",
          meta[["simexps"]][["meta_scen"]][[k0]][, "Model"], "_",
          tmp,
          if (isClimatology) "-clim"
        )
        meta[["simexps"]][["tag_scen"]][[k0]][[1L]] <- paste0(
          "sc1_NA_", meta[["simexps"]][["tag_scen1"]],
          if (isClimatology) "-clim"
        )

        # Identification of historical reference for scenario
        ids <- match(
          meta[["simexps"]][["meta_scen"]][[k0]][["Model"]],
          meta[["simexps"]][["meta_scen"]][[1L]][["Model"]]
        )
        ids[[1L]] <- NA_integer_

        meta[["simexps"]][["tag_scen_histref"]][[k0]] <-
          meta[["simexps"]][["tag_scen"]][[1L]][ids]


        # Identifications of across-model aggregations
        meta[["simexps"]][["tag_acrmod"]][[k0]] <- unique(
          stats::na.exclude(tmp)
        )
      }
    }
  }



  #--- Load deltas and convolutions ------
  if (isTRUE(as.logical(include_deltas))) {
    meta[["delta"]] <- list()

    #--- * basic delta categories and convolution ------
    meta[["delta"]][["basic"]] <- list(
      full = as.list(
        data.frame(
          levels = c("decrease", "stable", "increase"),
          sign = c(-1L, 0L, 1L),
          bi_scale = c("2-1", "1-1", "1-2"),
          stringsAsFactors = FALSE
        )
      )
    )


    #--- * delta RR: convolution of RR levels ------
    if (!is.null(meta[["varsets"]][["rr"]])) {
      meta[["delta"]][["rr"]] <- list(
        full = as.list(
          make_convolution(meta[["varsets"]][["rr"]][["levels"]])
        )
      )
    }
  }



  #------ Load aggregations across geographic space and across GCMs ------
  if (isTRUE(as.logical(include_summaries))) {

    #--- * Aggregations across GCMs ------
    # for each scenario x time-period combination
    meta[["acrmod"]] <- list()

    meta[["acrmod"]][["signal"]] <- list(
      tag = c("low", "med", "high"),
      probs = c(0.1, 0.5, 0.9)
    )

    meta[["acrmod"]][["signal"]][["fun_numeric"]] <- function(x, ...) {
      stats::setNames(
        stats::quantile(
          x,
          probs = meta[["acrmod"]][["signal"]][["probs"]],
          na.rm = TRUE
        ),
        meta[["acrmod"]][["signal"]][["tag"]]
      )
    }

    stopifnot(
      length(meta[["acrmod"]][["signal"]][["tag"]]) ==
        length(meta[["acrmod"]][["signal"]][["probs"]]),
      identical(
        meta[["acrmod"]][["signal"]][["tag"]],
        names(meta[["acrmod"]][["signal"]][["fun_numeric"]](1.))
      )
    )



    #--- * Robust agreement across GCMs ------
    # frequency above which signal is considered robust
    # see Bradford et al. 2020 Global Change Biology
    frq <- 0.9

    meta[["acrmod"]][["robust"]] <- list(
      frq = frq,

      tag = c(
        "agree",
        paste0("robust", formatC(round(100 * frq), width = 3L, flag = "0"))
      ),

      levels = c("nonrobust", "robust")
    )
  }


  #------ Load robust deltas ------
  if (isTRUE(all(as.logical(c(include_deltas, include_summaries))))) {

    #--- * Robustify basic delta ------
    meta[["delta"]][["basic"]][["robust"]] <- as.list(
      make_2elems_convolution(
        levels1 = meta[["delta"]][["basic"]][["full"]][["levels"]],
        levels2 = meta[["acrmod"]][["robust"]][["levels"]],
        signs = meta[["delta"]][["basic"]][["full"]][["sign"]]
      )
    )


    if (FALSE) {
      # Robustify RR
      make_2elems_convolution(
        levels1 = meta[["varsets"]][["rr"]][["levels"]],
        levels2 = meta[["acrmod"]][["robust"]][["levels"]]
      )
    }


    #--- * Robustify delta RR ------
    if (!is.null(meta[["delta"]][["rr"]])) {
      meta[["delta"]][["rr"]][["robust"]] <- as.list(
        make_2elems_convolution(
          levels1 = meta[["delta"]][["rr"]][["full"]][["levels"]],
          levels2 = meta[["acrmod"]][["robust"]][["levels"]],
          signs = meta[["delta"]][["rr"]][["full"]][["sign"]]
        )
      )

      tmp <- as.list(
        deconvolve_into_levels(meta[["delta"]][["rr"]][["robust"]][["x"]])
      )

      meta[["delta"]][["rr"]][["robust"]][["x_from"]] <- tmp[["from"]]
      meta[["delta"]][["rr"]][["robust"]][["x_to"]] <- tmp[["to"]]
    }
  }

  #--- return project description -----
  meta
}
# nolint end: cyclocomp_linter.
