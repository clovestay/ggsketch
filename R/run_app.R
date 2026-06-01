.pkg_env <- new.env(parent = emptyenv())

#' @export
set_plot_data <- function(caller_env) {

  obj_names <- ls(envir = caller_env)

  eligible_names <- obj_names[
    vapply(obj_names, function(x) {
      obj <- get(x, envir = caller_env)
      inherits(obj, "ggplot") | inherits(obj, "grob")
    }, logical(1))
  ]

  eligible_plots <- lapply(eligible_names, function(x) {
    obj <- get(x, envir = caller_env)
    obj
  })

  names(eligible_plots) <- eligible_names

  .pkg_env$eligible <- eligible_plots

}
#' @export
get_plot_data <- function() {
  return(
    .pkg_env$eligible
  )
}

#' @export
save_sketch_layout <- function(df) {
  .pkg_env$saved_layout <- df
}

#' @export
get_sketch_layout <- function() {
  return(.pkg_env$saved_layout)
}

#' @export
clear_sketch_layout <- function() {
  .pkg_env$saved_layout <- NULL
}


#' @export
toggle_autosave <- function(do.autosave) {
  .pkg_env$autosave <- do.autosave
}

#' @export
fetch_autosave <- function(do.autosave) {
  if(is.logical(.pkg_env$autosave)) {
    return(.pkg_env$autosave)
  }
  return(T)
}

#' Run the Shiny application
#'
#' @param ... Arguments passed to shiny::runApp()
#'
#' @export
run_app <- function(...) {

  app_dir <- system.file(
    "shinyapp",
    package = "ggsketch"
  )

  if (app_dir == "") {
    stop("Could not find app directory")
  }

  set_plot_data(parent.frame())
  shiny::runApp(app_dir, port = 8080)
  # return(callr::r_bg(function(app_dir) {
  #   pkgload::load_all(".")
  #   shiny::runApp(app_dir, port = 8080)
  # }, args = list(app_dir = app_dir)))
}




