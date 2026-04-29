#' Coerce an object to a gmov track
#'
#' Converts a track-like object to the minimal track representation used by
#' gmov. The returned object is a tibble with numeric `x_` and `y_` columns.
#' Many `amt` tracks use these coordinate names and should work when they are
#' data-frame-like.
#' Coordinates are assumed to be in a common planar coordinate system. gmov does
#' not transform track coordinates.
#'
#' @param x A track-like object. Supported inputs include data frames with
#'   `x_` and `y_` columns, data frames with `x` and `y` columns, `amt`-style
#'   step objects with `x1_`, `y1_`, `x2_`, and `y2_`, and `sf` point objects.
#'   Step objects are converted to the corresponding sequence of segment
#'   endpoints.
#' @param x_col,y_col Optional character names of the coordinate columns.
#' @param ... Currently unused.
#'
#' @return A tibble with at least `x_` and `y_` columns.
#' @export
#'
#' @examples
#' trk <- data.frame(x = c(0, 1, 2), y = c(0, 1, 1))
#' as_gmov_track(trk)
as_gmov_track <- function(x, x_col = NULL, y_col = NULL, ...) {
  UseMethod("as_gmov_track")
}

#' @export
as_gmov_track.default <- function(x, x_col = NULL, y_col = NULL, ...) {
  if (inherits(x, "sf")) {
    return(as_gmov_track_sf(x, x_col = x_col, y_col = y_col, ...))
  }

  if (!is.data.frame(x)) {
    stop("`x` must be a data frame, an sf point object, or an amt-compatible track.", call. = FALSE)
  }

  out <- tibble::as_tibble(x)
  out <- standardize_track_columns(out, x_col = x_col, y_col = y_col)
  validate_track(out)
}

as_gmov_track_sf <- function(x, x_col = NULL, y_col = NULL, ...) {
  geom <- sf::st_geometry(x)
  geom_type <- unique(as.character(sf::st_geometry_type(geom)))

  if (!all(geom_type %in% c("POINT"))) {
    stop("`sf` tracks must contain only POINT geometries.", call. = FALSE)
  }

  coords <- sf::st_coordinates(geom)
  out <- tibble::as_tibble(sf::st_drop_geometry(x))
  out$x_ <- coords[, 1]
  out$y_ <- coords[, 2]
  validate_track(out)
}

#' Coerce simulated tracks to a gmov simulation list
#'
#' Converts simulated trajectories to a named list of gmov tracks. The preferred
#' input is a list of track-like objects. A single data frame can also be used
#' if it contains a simulation identifier column.
#'
#' @param simulated A list of track-like objects, or a data frame containing
#'   multiple tracks.
#' @param id_col Optional character name of the simulation identifier column
#'   when `simulated` is a data frame.
#' @param ... Passed to [as_gmov_track()].
#'
#' @return A named list of gmov tracks.
#' @export
#'
#' @examples
#' sims <- list(
#'   data.frame(x = c(0, 1), y = c(0, 0)),
#'   data.frame(x = c(0, 0), y = c(0, 1))
#' )
#' as_gmov_simulations(sims)
as_gmov_simulations <- function(simulated, id_col = NULL, ...) {
  if (inherits(simulated, "gmov_simulations")) {
    return(simulated)
  }

  if (is.list(simulated) && !is.data.frame(simulated)) {
    sims <- lapply(simulated, as_gmov_track, ...)
  } else if (is.data.frame(simulated)) {
    sim_tbl <- tibble::as_tibble(simulated)
    id_col <- id_col %||% detect_simulation_id(sim_tbl)

    if (is.null(id_col)) {
      stop(
        "`simulated` is a data frame, so it must contain a simulation identifier column.",
        call. = FALSE
      )
    }

    sims <- lapply(split(sim_tbl, sim_tbl[[id_col]]), as_gmov_track, ...)
  } else {
    stop("`simulated` must be a list of tracks or a data frame of simulated tracks.", call. = FALSE)
  }

  if (length(sims) < 1) {
    stop("At least one simulated track is required.", call. = FALSE)
  }

  if (is.null(names(sims)) || any(names(sims) == "")) {
    names(sims) <- paste0("sim_", seq_along(sims))
  }

  class(sims) <- c("gmov_simulations", "list")
  sims
}

standardize_track_columns <- function(out, x_col = NULL, y_col = NULL) {
  if (!is.null(x_col) || !is.null(y_col)) {
    if (is.null(x_col) || is.null(y_col)) {
      stop("Both `x_col` and `y_col` must be supplied together.", call. = FALSE)
    }
    missing_cols <- setdiff(c(x_col, y_col), names(out))
    if (length(missing_cols) > 0) {
      stop("Missing coordinate columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    out$x_ <- out[[x_col]]
    out$y_ <- out[[y_col]]
    return(out)
  }

  if (all(c("x_", "y_") %in% names(out))) {
    return(out)
  }

  if (all(c("x", "y") %in% names(out))) {
    out$x_ <- out$x
    out$y_ <- out$y
    return(out)
  }

  if (all(c("x1_", "y1_", "x2_", "y2_") %in% names(out))) {
    out <- tibble::tibble(
      x_ = c(out$x1_[1], out$x2_),
      y_ = c(out$y1_[1], out$y2_)
    )
    return(out)
  }

  stop(
    "Could not find coordinate columns. Use `x_`/`y_`, `x`/`y`, or supply `x_col` and `y_col`.",
    call. = FALSE
  )
}

validate_track <- function(out) {
  if (!is.numeric(out$x_) || !is.numeric(out$y_)) {
    stop("Coordinate columns `x_` and `y_` must be numeric.", call. = FALSE)
  }

  keep <- is.finite(out$x_) & is.finite(out$y_)
  if (!all(keep)) {
    warning("Removed rows with missing or non-finite coordinates.", call. = FALSE)
    out <- out[keep, , drop = FALSE]
  }

  if (nrow(out) < 2) {
    stop("A track must contain at least two finite locations.", call. = FALSE)
  }

  tibble::as_tibble(out)
}

detect_simulation_id <- function(x) {
  candidates <- c("sim_id", "sim_id_", ".simulation", "simulation", "replicate")
  candidates[candidates %in% names(x)][1] %||% NULL
}
