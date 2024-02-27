#------ Functions to import local objects into the project directory ------

#' Import files into project by symbolic linking or by copying
#'
#' @param files_imported File path(s) relative to current working directory.
#' @param files_external File path(s) relative to current working directory.
#' @param method A character string, either `"symlink"` or `"copy"`.
#' @param add_nparents An integer value. The level of sub-folders where
#' the `"symlink"`s will be created relative to current working directory.
#'
import_to_project <- function(
  files_imported,
  files_external,
  method = c("symlink", "copy"),
  add_nparents = 0L
) {
  method <- match.arg(method)

  stopifnot(
    length(files_imported) == length(files_external),
    file.exists(files_external)
  )

  # Create local paths
  lapply(
    dirname(files_imported),
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )


  if (method == "symlink") {
    # Translate paths relative to current working directory to
    # sub-folder location of symlinks
    dir_parent <- "."

    for (k in seq_len(add_nparents)) {
      dir_parent <- file.path(dir_parent, "..")
    }

    file.symlink(
      from = file.path(dir_parent, files_external),
      to = files_imported
    )

  } else if (method == "copy") {
    tmp_args <- list(
      from = files_external,
      to = files_imported,
      copy.date = TRUE,
      copy.mode = TRUE
    )

    if (length(files_imported) != 1L) {
      tmp_args[["recursive"]] <- FALSE
    }

    do.call(file.copy, args = tmp_args)
  }
}


#' Import predictor variable names for R&R models (Chambers et al. 2023)
#'
#' @export
import_predictors_variablenames <- function(
  dir_dataraw = file.path("..", "data-raw"),
  dir_plotsim = file.path("..", "..", "..", "Prod043b_newRR_PlotSimulations"),
  method_to_import = c("symlink", "copy")
) {
  method_to_import <- match.arg(method_to_import)

  fnames_RR2022predictornames <- file.path(
    dir_dataraw,
    "mrrrf",
    "rr_rf_predictors_v20220207.rds"
  )

  is_to_import <- !file.exists(fnames_RR2022predictornames)

  if (any(is_to_import)) {
    dir_mrrrf <- file.path(
      dir_plotsim,
      "3_Analysis",
      "20211214_RR_RF_model",
      "rr_rf_predictors"
    )
    stopifnot(dir.exists(dir_mrrrf))

    res <- import_to_project(
      files_imported = fnames_RR2022predictornames[is_to_import],
      files_external = file.path(
        dir_mrrrf,
        basename(fnames_RR2022predictornames)
      )[is_to_import],
      method = method_to_import,
      add_nparents = 1L
    )
    stopifnot(res)
  }

  invisible(fnames_RR2022predictornames)
}


#' Import `rSW2metrics` extraction object with predictors for R&R models
#' @export
import_predictors_rsw2metrics <- function(
  dir_dataraw = file.path("..", "data-raw"),
  dir_extractmetrics = file.path("..", "..", "2_CalculateDerivedMetrics"),
  metric_name = "RR2022predictors",
  list_simexps = NULL,
  version_simdata = NULL,
  tag_simexp0 = NULL,
  method_to_import = c("symlink", "copy")
) {
  method_to_import <- match.arg(method_to_import)

  fnames_predictors <- file.path(
    dir_dataraw,
    "rsw2metrics",
    paste0("Outputs_future-", list_simexps),
    paste0(metric_name, ".rds")
  )

  is_to_import <- !file.exists(fnames_predictors)

  if (any(is_to_import)) {
    dir_metrics <- file.path(
      dir_extractmetrics,
      paste0(version_simdata, "_metrics_from_", tag_simexp0),
      paste0("Outputs_future-", list_simexps)
    )
    stopifnot(dir.exists(dir_metrics))

    res <- import_to_project(
      files_imported = fnames_predictors[is_to_import],
      files_external = file.path(
        dir_metrics,
        basename(fnames_predictors)
      )[is_to_import],
      method = method_to_import,
      add_nparents = 2L
    )
    stopifnot(res)
  }

  invisible(fnames_predictors)
}


#' Import template raster with encoded gridded simulation units
#' @export
import_gsu_template <- function(
  dir_dataraw = file.path("..", "data-raw"),
  dir_mappingdata = file.path("..", "..", "0_Inputs", "Data_for_Mapping"),
  mask_set = c("sim", "rangelands", "combined"),
  version_inputs = NULL,
  method_to_import = c("symlink", "copy")
) {
  method_to_import <- match.arg(method_to_import)
  mask_set <- match.arg(mask_set)

  tag_gsu_tif <- switch(
    EXPR = mask_set,
    full = ,
    sim = "gsu_",
    rangelands = "gsu_masked_",
    combined = "gsu_combined_"
  )

  fnames_gsu <- file.path(
    dir_dataraw,
    "gsu",
    paste0(
      tag_gsu_tif, "v", version_inputs,
      c(".tif", ".tfw", "_rat.csv")
    )
  )

  is_to_import <- !file.exists(fnames_gsu)

  if (any(is_to_import)) {
    dir_gsus <- file.path(
      dir_mappingdata,
      paste0("v", version_inputs)
    )
    stopifnot(dir.exists(dir_gsus))

    res <- import_to_project(
      files_imported = fnames_gsu[is_to_import],
      files_external = file.path(
        dir_gsus,
        basename(fnames_gsu)
      )[is_to_import],
      method = method_to_import,
      add_nparents = 1L
    )
    stopifnot(res)
  }

  invisible(fnames_gsu)
}


#' Import climate scenarios represented by `rSFSW2` experiments
#' @export
import_scenarios_rsfsw2 <- function(
  dir_dataraw = file.path("..", "data-raw"),
  dir_simexps = file.path("..", "..", "1_SOILWAT2_Simulations"),
  tag_simexp = NULL,
  method_to_import = c("symlink", "copy")
) {
  method_to_import <- match.arg(method_to_import)

  fnames_scens <- file.path(
    dir_dataraw,
    "scens",
    tag_simexp,
    "Table_ScenarioDescription.csv"
  )

  is_to_import <- !file.exists(fnames_scens)

  if (any(is_to_import)) {
    dir_scens <- file.path(
      dir_simexps,
      tag_simexp,
      "4_Simulation"
    )
    stopifnot(dir.exists(dir_scens))

    res <- import_to_project(
      files_imported = fnames_scens[is_to_import],
      files_external = file.path(
        dir_scens,
        basename(fnames_scens)
      )[is_to_import],
      method = method_to_import,
      add_nparents = 2L
    )
    stopifnot(res)
  }

  invisible(fnames_scens)
}


#' Import workflows to predict R&R (Chambers et al. 2023)
#' @export
import_predictrr_rfworkflows <- function(
  dir_dataraw = file.path("..", "data-raw"),
  dir_plotsim = file.path("..", "..", "..", "Prod043b_newRR_PlotSimulations"),
  version_rf = NULL,
  method_to_import = c("symlink", "copy")
) {
  method_to_import <- match.arg(method_to_import)

  fnames_mrrrf <- file.path(
    dir_dataraw,
    "mrrrf",
    c(
      "Biomewide_Resilience_19Var_RFWorkflow_2022-03-24.Rds",
      "Biomewide_Resistance_19Var_RFWorkflow_2022-03-07.Rds"
    )
  )

  is_to_import <- !file.exists(fnames_mrrrf)

  if (any(is_to_import)) {
    dir_mrrrf <- file.path(
      dir_plotsim,
      "3_Analysis",
      "20211214_RR_RF_model",
      version_rf
    )
    stopifnot(dir.exists(dir_mrrrf))

    res <- import_to_project(
      files_imported = fnames_mrrrf[is_to_import],
      files_external = file.path(
        dir_mrrrf,
        basename(fnames_mrrrf)
      )[is_to_import],
      method = method_to_import,
      add_nparents = 1L
    )
    stopifnot(res)
  }

  invisible(fnames_mrrrf)
}



#------ Functions to download and access external data ------

#' Download all data attached to a Science Base Data Release
download_from_ScienceBase <- function(
  doi,
  fnames_downloaded_data,
  fnames_downloadraw,
  dir_unzip = NULL,
  dir_downloadraw = unique(dirname(fnames_downloadraw))
) {
  if (!all(file.exists(fnames_downloaded_data))) {

    if (!all(file.exists(fnames_downloadraw))) {
      stopifnot(
        requireNamespace("sbtools"),
        requireNamespace("curl") && curl::has_internet()
      )

      tmp <- sbtools::query_sb_doi(as.character(doi))
      stopifnot(length(tmp) == 1L)

      tmpf <- sbtools::item_list_files(tmp[[1L]])
      stopifnot(basename(fnames_downloadraw) %in% tmpf[["fname"]])

      dir.create(dir_downloadraw, recursive = TRUE, showWarnings = FALSE)

      fnames_res <- sbtools::item_file_download(
        sb_id = tmp[[1L]][["id"]],
        dest_dir = dir_downloadraw,
        overwrite_file = TRUE
      )

      stopifnot(fnames_downloadraw %in% fnames_res)
    }

    ids_zipped <- grep(".zip$", fnames_downloadraw)
    for (k in ids_zipped) {
      utils::unzip(fnames_downloadraw[[k]], exdir = dir_unzip)
    }

    stopifnot(file.exists(fnames_downloaded_data))
  }

  invisible(fnames_downloaded_data)
}


#' Download 30-m raster of Sagebrush Ecological Integrity (`"SEI"`)
#' by the Sagebrush Conservation Design
#'
#' @references Doherty, K., Theobald, D.M., Holdrege, M.C., Wiechman, L.A., and
#' Bradford, J.B., 2022, Biome-wide sagebrush core habitat and growth areas
#' estimated from a threat-based conservation design:
# nolint start: nonportable_path_linter.
#' U.S. Geological Survey data release, \url{https://doi.org/10.5066/P94Y5CDV}
# nolint end: nonportable_path_linter.
download_scd1sei_data <- function(
  path,
  method_to_import = "download"
) {
  method_to_import <- match.arg(method_to_import)

  dir_data <- file.path(path, "scd1sei")

  fnames_downloaded_data <- download_from_ScienceBase(
    doi = "10.5066/P94Y5CDV", # nolint: nonportable_path_linter.
    fnames_downloaded_data = file.path(
      dir_data,
      "Rasters",
      c("SEI_1998_2001_30_Current.tif", "SEI_2017_2020_30_Current.tif")
    ),
    dir_unzip = dir_data,
    fnames_downloadraw = file.path(
      dir_data,
      "download-raw",
      c("Raster_Data.zip", "Sagebrush_SLCD_Metadata.xml")
    )

  )

  invisible(fnames_downloaded_data)
}


#' Import 30-m raster of Sagebrush Ecological Integrity (`SEI`)
#' by the Sagebrush Conservation Design
#' @export
import_scd1sei_raster <- function(
  dir_dataraw = file.path("..", "data-raw"),
  years = NA,
  ...
) {
  fnames_scd1sei_raster <- download_scd1sei_data(
    path = dir_dataraw,
    method_to_import = "download"
  )

  invisible(
    switch(
      EXPR = as.character(years),
      `2020` = ,
      `2017_2020` = fnames_scd1sei_raster[[2L]],
      `2001` = ,
      `1998_2001` = fnames_scd1sei_raster[[1L]],
      fnames_scd1sei_raster[1:2]
    )
  )
}


#' Download the "Sagebrush Biome" polygon
#'
#' @references Jeffries, M.I., and Finn, S.P., 2019,
#' The Sagebrush Biome Range Extent, as Derived from Classified Landsat Imagery:
#' U.S. Geological Survey data release, \url{https://doi.org/10.5066/P950H8HS}
download_sagebiome_polygon <- function(
  path,
  method_to_import = "download"
) {
  method_to_import <- match.arg(method_to_import)

  dir_data <- file.path(path, "sagebiome")

  tmp <- file.path(
    dir_data,
    "download-raw",
    paste(
      "US_Sagebrush_Biome_2019",
      c("cpg", "dbf", "prj", "sbn", "sbx", "shp", "shp.xml", "shx", "xml"),
      sep = "."
    )
  )

  fnames_downloaded_data <- download_from_ScienceBase(
    doi = "10.5066/P950H8HS", # nolint: nonportable_path_linter.
    fnames_downloaded_data = tmp,
    dir_unzip = dir_data,
    fnames_downloadraw = tmp
  )

  invisible(
    fnames_downloaded_data[grep(".shp$", basename(fnames_downloaded_data))]
  )
}


#' Import the "Sagebrush Biome" polygon
#' @export
import_sagebiome_polygon <- function(
  dir_dataraw = file.path("..", "data-raw"),
  ...
) {
  invisible(
    download_sagebiome_polygon(
      path = dir_dataraw,
      method_to_import = "download"
    )
  )
}


#' Download EPA ecoregions L3 polygon
#'
#' @references \url{https://www.epa.gov/eco-research/ecoregions-north-america}
download_EPAecoregionL3_polygon <- function(
  path,
  method_to_import = "download"
) {
  method_to_import <- match.arg(method_to_import)

  dir_data <- file.path(path, "EPAecoregionL3")

  fname_data <- file.path(
    dir_data,
    "WesternUS",
    "epa_ecoregions_iii_sf_aea.rds"
  )

  if (!file.exists(fname_data)) {
    stopifnot(
      requireNamespace("sf"),
      requireNamespace("curl") && curl::has_internet()
    )

    #--- Download
    dir_dataraw <- file.path(dir_data, "download-raw")

    url <- "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na"
    fname_url <- file.path(url, "NA_CEC_Eco_Level3.zip")

    zip_data <- file.path(dir_dataraw, basename(fname_url))
    dir_data_tmp <- tools::file_path_sans_ext(zip_data)

    if (!file.exists(zip_data)) {
      dir.create(dir_dataraw, recursive = TRUE, showWarnings = FALSE)
      utils::download.file(url = fname_url, destfile = zip_data)
      utils::unzip(zip_data, exdir = dir_data_tmp)
    }


    #--- Subset to Western US
    tmp_epal3 <- sf::st_read(dir_data_tmp) |>
      sf::st_transform(crs = "EPSG:6350")

    # Crop to western US
    extent_USwest <- c(
      xmin = -2517004,
      xmax = 347000,
      ymin = 313000,
      ymax = 4357000
    )

    tmp_epal3 <- suppressWarnings(
      # Suppress:
      #    attribute variables are assumed to be spatially constant
      #    throughout all geometries
      sf::st_crop(tmp_epal3, y = extent_USwest)
    )

    if (!all(sf::st_is_valid(tmp_epal3))) {
      tmp_epal3 <- sf::st_make_valid(tmp_epal3)
      stopifnot(sf::st_is_valid(tmp_epal3))
    }


    #--- Store on disk
    dir.create(dirname(fname_data), recursive = TRUE, showWarnings = FALSE)
    saveRDS(tmp_epal3, file = fname_data)

    stopifnot(file.exists(fname_data))
  }

  invisible(fname_data)
}

#' Import EPA ecoregions L3 polygon
#' @export
import_EPAecoregionL3_polygon <- function(
  dir_dataraw = file.path("..", "data-raw"),
  ...
) {
  invisible(
    download_EPAecoregionL3_polygon(
      path = dir_dataraw,
      method_to_import = "download"
    )
  )
}



#' Download Natural Earth's North American political boundaries "admin1"
#'
# nolint start: line_length_linter.
#' @references \url{https://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
# nolint end: line_length_linter.
download_NEadmin1_polygon <- function(
  path,
  method_to_import = "download",
  crs = "EPSG:6350",
  ...
) {
  method_to_import <- match.arg(method_to_import)

  dir_data <- file.path(path, "NEadmin1")

  fname_data <- file.path(
    dir_data,
    "NorthAmerica",
    "ne_admin1_NorthAmerica_sf.rds"
  )

  if (!file.exists(fname_data)) {
    stopifnot(
      requireNamespace("sf"),
      requireNamespace("curl") && curl::has_internet()
    )

    #--- Download
    dir_dataraw <- file.path(dir_data, "download-raw")

    url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural" # nolint: line_length_linter.
    fname_url <- file.path(url, "ne_10m_admin_1_states_provinces.zip")

    zip_data <- file.path(dir_dataraw, basename(fname_url))
    dir_data_tmp <- tools::file_path_sans_ext(zip_data)

    if (!file.exists(zip_data)) {
      dir.create(dir_dataraw, recursive = TRUE, showWarnings = FALSE)
      utils::download.file(url = fname_url, destfile = zip_data)
      utils::unzip(zip_data, exdir = dir_data_tmp)
    }

    #--- Subset to North America
    tmp_admin1 <- sf::st_read(dir_data_tmp)

    ids <-
      tmp_admin1[, "geonunit", drop = TRUE] %in%
      c("Canada", "Mexico", "United States of America")
    tmp_admin1 <- tmp_admin1[ids, ]

    if (!all(sf::st_is_valid(tmp_admin1))) {
      tmp_admin1 <- sf::st_make_valid(tmp_admin1)
      stopifnot(sf::st_is_valid(tmp_admin1))
    }


    #--- Reproject if requested
    crs <- sf::st_crs(crs)
    if (!is.na(crs) && sf::st_crs(tmp_admin1) != crs) {
      tmp_admin1 <- sf::st_transform(tmp_admin1, crs = crs)
    }


    #--- Store on disk
    dir.create(dirname(fname_data), recursive = TRUE, showWarnings = FALSE)
    saveRDS(tmp_admin1, file = fname_data)

    stopifnot(file.exists(fname_data))
  }

  invisible(fname_data)
}


#' Import Natural Earth's North American political boundaries of states
#' and provinces
#' @export
import_NEadmin1_polygon <- function(
  dir_dataraw = file.path("..", "data-raw"),
  ...
) {
  invisible(
    download_NEadmin1_polygon(
      path = dir_dataraw,
      method_to_import = "download",
      ...
    )
  )
}



#' USA Federal Lands by ESRI
#'
# nolint start: line_length_linter.
#' @references \url{https://www.arcgis.com/home/item.html?id=5e92f2e0930848faa40480bcb4fdc44e#overview}
#' as referenced by \url{https://www.sciencebase.gov/catalog/item/4fc50e63e4b00e9c12d8c322}
# nolint end: line_length_linter.
#'
#' Last download on 2022-Aug-16
download_ESRIfederallands_polgyon <- function(
  path,
  method_to_import = "download"
) {
  method_to_import <- match.arg(method_to_import)

  dir_data <- file.path(path, "FederalLands_ESRI")

  fname_data <- file.path(
    dir_data,
    "USAFederalLands_ESRI.gpkg"
  )

  if (!file.exists(fname_data)) {
    stopifnot(
      requireNamespace("sf"),
      requireNamespace("httr"),
      requireNamespace("curl") && curl::has_internet()
    )

    #--- Download
    # nolint start: nonportable_path_linter.
    url <- list(
      hostname = "services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services",
      scheme = "https",
      path = "USA_Federal_Lands/FeatureServer/0/query",
      query = list(
        where = "1=1",
        outFields = "*",
        returnGeometry = "true",
        f = "geojson"
      )
    )
    # nolint end: nonportable_path_linter.

    attr(url, "class") <- "url"

    request <- httr::build_url(url)

    fl <-
      sf::st_read(request) |>
      sf::st_make_valid()

    sf::st_write(fl, dsn = fname_data, driver = "GPKG")


    stopifnot(file.exists(fname_data))
  }

  invisible(fname_data)
}


#' Import USA Federal Lands by ESRI
#' @export
import_ESRIfederallands_polgyon <- function(
  dir_dataraw = file.path("..", "data-raw"),
  ...
) {
  invisible(
    download_ESRIfederallands_polgyon(
      path = dir_dataraw,
      method_to_import = "download"
    )
  )
}



#' Download BLM National Surface Management Agency Area Polygons -
#' National Geospatial Data Asset (`NGDA`)
#'
# nolint start: line_length_linter.
#' @references \url{https://catalog.data.gov/dataset/blm-national-surface-management-agency-area-polygons-national-geospatial-data-asset-ngda},
#' referenced by \url{https://www.sciencebase.gov/catalog/item/59b83c14e4b08b1644df5f6e}
# nolint end: line_length_linter.
download_SMAfederallands_polgyon <- function(
  path,
  method_to_import = "download"
) {
  method_to_import <- match.arg(method_to_import)

  dir_data <- file.path(path, "FederalLands_SMA")

  fname_data <- file.path(
    dir_data,
    "Data",
    "SMA_WM.gdb"
  )

  if (!file.exists(fname_data)) {
    stopifnot(
      requireNamespace("sf"),
      requireNamespace("curl") && curl::has_internet()
    )

    #--- Download
    dir_dataraw <- file.path(dir_data, "download-raw")

    # nolint start: line_length_linter.
    url <- "https://blm-egis.maps.arcgis.com/sharing/rest/content/items/6bf2e737c59d4111be92420ee5ab0b46/data"
    # nolint end: line_length_linter.
    fname_url <- file.path(url, "SMA_WM.gdb.zip")

    zip_data <- file.path(dir_dataraw, basename(fname_url))

    if (!file.exists(zip_data)) {
      dir.create(dir_dataraw, recursive = TRUE, showWarnings = FALSE)
      utils::download.file(url = fname_url, destfile = zip_data)
      # Download by navigating manually to the website is most likely required
      utils::unzip(zip_data, exdir = dirname(fname_data))
    }

    stopifnot(file.exists(fname_data))
  }

  invisible(fname_data)
}

#' Import USA Federal Lands by `SMA`
#' @export
import_SMAfederallands_polgyon <- function(
  dir_dataraw = file.path("..", "data-raw"),
  ...
) {
  invisible(
    download_SMAfederallands_polgyon(
      path = dir_dataraw,
      method_to_import = "download"
    )
  )
}
