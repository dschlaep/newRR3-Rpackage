#--- Function to get values from RAT
#' @export
prepare_rat <- function(x, scen, req_factors, vars, template) {
  if (is.null(scen)) {
    xmk <- x

  } else {
    #--- * Subset metrics data and convert to wide format ------
    stopifnot(c("site", "sw2_id", "group") %in% colnames(x))

    xmk <- stats::reshape(
      x[, c("site", "group", scen), drop = FALSE],
      direction = "wide",
      idvar = "site",
      timevar = "group"
    )

    colnames(xmk) <- gsub(paste0(scen, "."), "", colnames(xmk))

    # Add sw2_id
    xmk <- merge(xmk, unique(x[, c("site", "sw2_id")]), all.x = TRUE)
  }

  stopifnot(vars %in% colnames(xmk))


  #--- * Convert to factors ------
  for (k1 in seq_along(req_factors)) {
    xmk[, names(req_factors)[k1]] <- factor(
      xmk[, names(req_factors)[k1]],
      levels = seq_along(req_factors[[k1]]),
      labels = req_factors[[k1]]
    )
  }


  #--- * Create raster attribute table ------
  merge(
    template,
    y = xmk[, c("sw2_id", vars)],
    by.x = "value",
    by.y = "sw2_id",
    all.x = TRUE
  )
}



#' Make sure `RAT` fields "value" and "count" are integers
#'
#' @param x A data frame used as raster attribute value table
#'
#' @return A data frame corresponding to `x` where
#' columns matching `"value"` and `"count"` are converted to integers.
#'
#' @examples
#' n <- 3L
#' x <- data.frame(
#'   value = as.numeric(seq_len(n)),
#'   count = sample(10, size = n),
#'   var1 = runif(n = n),
#'   var2 = letters[seq_len(n)]
#' )
#'
#' xi <- integerize_rat_header(x)
#'
#' @export
integerize_rat_header <- function(x, headers = c("^value$", "^count")) {
  hs <- unlist(
    lapply(headers, grep, x = colnames(x))
  )

  for (v in hs) {
    if (!inherits(x[[v]], "integer")) {
      sna <- sum(is.na(x[[v]]))
      class(x[[v]]) <- "integer"
      stopifnot(identical(sna, sum(is.na(x[[v]]))))
    }
  }
  x
}


#' Make sure that all field names are shorter than `minlength` characters
#'
#' @examples
#' shorten_variable_names(c("short10", "verylongname"))
#' shorten_variable_names("mean_annual_temperature")
#'
#' @export
shorten_variable_names <- function(x, minlength = 10) {
  x0 <- x

  while (
    # nolint start: implicit_assignment_linter.
    length(ids <- which(nchar(x) > 10L)) > 0L
    # nolint end: implicit_assignment_linter.
  ) {
    minlength <- minlength - 0.25
    if (minlength < 2) {
      warning("Not able to uniquely abbreviate column names.")
      break
    }
    x[ids] <- abbreviate(x[ids], minlength = round(minlength))

    warning(
      "Column names shortened to meet DBF requirements:",
      paste("from", shQuote(x0), "to", shQuote(x), collapse = ", ")
    )
  }

  if (any(x != x)) summary(warnings())

  x
}


#' Create ESRI value attribute table `"VAT"` as `".vat.dbf"` file
#'
#' A `"VAT"` contains fields `"VALUE"` and `"COUNT"`;
#' all field names are restricted to 11 characters and cannot contain periods.
#' See [foreign::write.dbf]
write_esri_vat <- function(x, filename, rename_cns = NULL) {
  stopifnot(
    grepl(".vat.dbf$", filename),
    requireNamespace("foreign")
  )

  cns <- colnames(x)

  cn_value <- grep("^value$", cns)
  stopifnot(length(cn_value) == 1L)
  cn_count <- grep("^count$", cns)

  cns_vars <- setdiff(cns, cns[c(cn_value, cn_count)])

  cns_vars_dbf <- if (is.null(rename_cns)) {
    cns_vars
  } else {
    ids <- match(simplify_variables_names(cns_vars), rename_cns[["from"]])
    rename_cns[["to"]][ids]
  }

  # Make sure that all field names are shorter than 11 characters
  cns_vars_dbf <- shorten_variable_names(cns_vars_dbf, minlength = 10)


  vat <- data.frame(
    VALUE = x[, cn_value, drop = TRUE],
    COUNT = if (length(cn_count) == 1L) {
      x[, cn_count, drop = TRUE]
    } else {
      NA_integer_
    },
    x[, cns_vars]
  )

  colnames(vat)[-(1:2)] <- cns_vars_dbf


  # Remove any missing VALUE
  vat <- vat[!is.na(vat[["VALUE"]]), , drop = FALSE]

  # Write to disk
  foreign::write.dbf(vat, file = filename)
}




#' Make a (`COG`) `GeoTIFF`
#'
#' @param rast_template A file name or a `terra` `SpatRaster` object.
#' @param rat A `data.frame` object. The raster (value) attribute table
#' where the first column is `"Value"` that are matched against the values
#' of `rast_template`.
#' @param fname A file name. The file name of the resulting `GeoTIFF`.
#' @param names_rat The names assigned to the layers of the `SpatRaster`
#' (if not missing).
#' @param add_esri_vat A logical value. If `TRUE`, then
#' write an `ESRI`-formatted value attribute table
#' (in addition to the `GDAL`-formatted raster attribute table).
#' @param vat_rename_cns List to rename attributes for the value attribute table
#' (`ESRI` attribute names are restricted to 10 or fewer characters).
#' @param overwrite A logical value. Should an existing `fname` be overwritten?
#' @param filetype Passed to [terra::writeRaster], e.g., `"GTiff"`, `"COG"`
#' @param gdal_write_options Passed to [terra::writeRaster].
#'
#' @name make_geotiff
NULL

#' Create a `GeoTIFF` from template `GeoTIFF` after adding spreadsheet as `RAT`
#'
#' @inheritParams make_geotiff
#'
#' @seealso [make_geotiff_catalyzed()]
#'
#' @export
make_geotiff_with_rat <- function(
  rast_template,
  rat,
  fname,
  names_rat,
  add_esri_vat = TRUE,
  vat_rename_cns = NULL,
  overwrite = FALSE,
  filetype = "GTiff",
  gdal_write_options = c("COMPRESS=LZW", "TFW=YES", "TILED=YES")
) {
  stopifnot(
    requireNamespace("terra"),
    overwrite || !file.exists(fname)
  )

  tmpd <- dirname(fname)
  if (!dir.exists(tmpd)) {
    dir.create(tmpd, recursive = TRUE, showWarnings = FALSE)
  }

  x <- if (inherits(rast_template, "SpatRaster")) {
    rast_template
  } else {
    terra::rast(rast_template)
  }

  levels(x) <- integerize_rat_header(rat)
  if (!missing(names_rat)) names(x) <- names_rat

  res <- terra::writeRaster(
    x,
    filename = fname,
    overwrite = overwrite,
    filetype = filetype,
    wopt = list(gdal = gdal_write_options)
  )

  if (add_esri_vat) {
    write_esri_vat(
      rat,
      filename = paste0(fname, ".vat.dbf"),
      rename_cns = vat_rename_cns
    )
  }

  invisible(res)
}


#' Create a `GeoTIFF` from template `GeoTIFF` using values from a spreadsheet
#'
#' @inheritParams make_geotiff
#' @param rat2 A `data.frame` object. A secondary raster (value) attribute table
#' that can be used to describe category levels
#' (useful if the content from `rat` that is catalyzed into
#' the resulting `GeoTIFF` are categorical).
#'
#' @seealso [make_geotiff_with_rat()]
#'
#' @export
make_geotiff_catalyzed <- function(
  rast_template,
  rat,
  fname,
  names_rat,
  rat2 = NULL,
  add_esri_vat = TRUE,
  vat_rename_cns = NULL,
  overwrite = FALSE,
  filetype = "GTiff",
  gdal_write_options = c("COMPRESS=LZW", "TFW=YES", "TILED=YES")
) {
  stopifnot(
    requireNamespace("terra"),
    overwrite || !file.exists(fname)
  )

  tmpd <- dirname(fname)
  if (!dir.exists(tmpd)) {
    dir.create(tmpd, recursive = TRUE, showWarnings = FALSE)
  }

  x <- if (inherits(rast_template, "SpatRaster")) {
    rast_template
  } else {
    terra::rast(rast_template)
  }


  if (!missing(rat)) {
    rat <- integerize_rat_header(rat)

    levels(x) <- rat

    if (!missing(names_rat)) names(x) <- names_rat
  }

  fname_used <- if (is.null(rat2)) {
    fname
  } else {
    tempfile(fileext = paste0(".", tools::file_ext(fname)))
  }

  tmpx <- terra::catalyze(
    x,
    filename = fname_used,
    overwrite = overwrite,
    filetype = filetype,
    wopt = list(gdal = gdal_write_options)
  )

  if (!is.null(rat2)) {
    # Add secondary RAT (after catalyzation)
    levels(tmpx) <- rat2

    tmpx <- terra::writeRaster(
      tmpx,
      filename = fname,
      overwrite = overwrite,
      filetype = filetype,
      wopt = list(gdal = gdal_write_options)
    )

    if (add_esri_vat) {
      write_esri_vat(
        rat2,
        filename = paste0(fname, ".vat.dbf"),
        rename_cns = vat_rename_cns
      )
    }
  }

  invisible(tmpx)
}



#' Load and downsample GeoTIFF with RAT as `terra` raster
#' @export
process_raster <- function(fname_geotiffs, downsample = 4L) {
  stopifnot(
    requireNamespace("terra"),
    requireNamespace("stars")
  )

  if (is.null(fname_geotiffs)) return(NULL)

  res <- list()

  # Load all fname_geotiffs; if multiple geotiffs, then multiple layers
  # Note: "value" = "suid" must be identical for all geotiffs (in this project)
  gsu_terra <- terra::rast(fname_geotiffs)

  # Save RATs (for each layer)
  res[["gsu_rats"]] <- terra::cats(gsu_terra)



  #--- * Convert terra object with RAT to stars object ------
  #--- ** Downsample to prevent integer overflow during ggplot2 ------

  # nolint start: commented_code_linter, object_usage_linter.

  # Using: `stars::geom_stars(data = x, downsample = downsample)`
  #   downsample = 0: integer overflow
  #   downsample = 2: segfault
  #   downsample = 4 ==> works well

  # Error in if (n > 0) c(NA_integer_, -n) else integer() :
  # missing value where TRUE/FALSE needed
  # In addition: Warning messages:
  #   1: In rep.fac * nx : NAs produced by integer overflow
  # 2: In .set_row_names(as.integer(prod(d))) :
  #   NAs introduced by coercion to integer range
  if (FALSE) {
    # error if x * y > .Machine[["integer.max"]] = 2^31 - 1 = 2147483647
    # tmp1 <- expand.grid(x = seq_len(55300), y = seq_len(59851))
    # tmp2 <- tidyr::expand_grid(x = seq_len(55300), y = seq_len(59851))
    # Error:
    #    Long vectors are not yet supported. Requested output size must be
    #    less than 2147483647.
    # ==> downsample = 2 so that 55300/2*59851/2 = 827440075 < 2147483647
  }
  # nolint end: commented_code_linter, object_usage_linter.


  # stars v0.5.5 cannot handle general RATs (as used here);
  # it handles only color-table RATs
  # --> downsample terra raster using value (= suid = sw2_id) and delete RAT
  #     (only use one layer because value = suid are identical for all layers);
  #     convert to stars; & manually add RAT values back in (just for plotting)
  if (!is.null(levels(gsu_terra))) {
    terra::activeCat(gsu_terra, layer = 1L) <- "value"
    levels(gsu_terra) <- NULL
  }

  ds_gsu_terra <- terra::spatSample(
    gsu_terra[[1L]],
    size = min(
      .Machine[["integer.max"]] / 10L,
      ceiling(terra::ncell(gsu_terra[[1L]]) / downsample ^ 2L)
    ),
    method = "regular",
    as.raster = TRUE
  )

  res[["xstars"]] <- stars::st_as_stars(
    ds_gsu_terra,
    ignore_file = TRUE,
    as_attributes = FALSE
  )

  # IDs to link downsampled stars raster to original RAT values
  res[["ids_rat"]] <- match(
    res[["xstars"]][[1]],
    res[["gsu_rats"]][[1]][, "value"],
    nomatch = 0
  )

  res
}


#' @export
prepare_stars <- function(xstars, x, ids_rat = NULL, rat_values = NULL) {
  if (is.null(ids_rat)) {
    # Assume that `xstars[[1]]` contains the "values"
    ids_rat <- match(xstars[[1L]], table = rat_values, nomatch = 0L)
  }

  xstars[[1]][ids_rat > 0L] <- x[ids_rat]
  xstars[[1]][ids_rat == 0L] <- NA

  if (is.factor(x)) {
    lvls <- levels(x)
    tmp <- factor(xstars[[1]], levels = seq_along(lvls), labels = lvls)
    attr(tmp, "dim") <- dim(xstars[[1]])
    xstars[[1]] <- tmp
  }

  xstars
}
