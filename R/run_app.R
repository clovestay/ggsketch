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
get_plot_data <- function() {
  return(
    .pkg_env$eligible
  )
}

save_sketch_layout <- function(df) {
  .pkg_env$saved_layout <- df
}

get_sketch_layout <- function() {
  return(.pkg_env$saved_layout)
}

clear_sketch_layout <- function() {
  .pkg_env$saved_layout <- NULL
}

toggle_autosave <- function(do.autosave) {
  .pkg_env$autosave <- do.autosave
}

fetch_autosave <- function(do.autosave) {
  if(is.logical(.pkg_env$autosave)) {
    return(.pkg_env$autosave)
  }
  return(T)
}


save_render_options <- function(opts) {
  .pkg_env$render_options = opts
}
get_render_options <- function(opt = NULL) {
  if(!is.null(opt)) {
    return(.pkg_env$render_options[[opt]])
  }
  return(.pkg_env$render_options)
}
default_render_options <- function(opt = NULL) {
  defs <- list(
    renderAddLetterLabels = T,
    renderLabelChoice = "lowercase",
    renderUseFixedSize = F,
    renderBaseSize = 3,
    renderAddMargins = 6,
    renderWidth = NA,
    renderHeight = NA
  )
  if(!is.null(opt)) {
    return(
      defs[[opt]]
    )
  }
  return(defs)
}
try_load_options <- function() {
  if(is.null(.pkg_env$render_options)) {
    .pkg_env$render_options = default_render_options()
  }
}

plot_errored <- function(err) {
  cli::cli_alert_danger(err)
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

  ggsketch:::try_load_options()
  ggsketch:::set_plot_data(parent.frame())
  cli::cli_alert_success("Opened app!")
  shiny::runApp(app_dir, launch.browser = T, quiet = T)
}




