# app.R - robust wrapper for single-file app pitchmap.R
# It handles:
#  1) pitchmap.R returning a shiny.appobj via shinyApp(...)
#  2) pitchmap.R defining ui and server but not calling shinyApp(...)
#  3) useful error if neither is present

# Use a fresh environment to source the single-file app
app_env <- new.env(parent = globalenv())

# Source pitchmap.R and capture returned value (if any)
sourced <- tryCatch(
  source("pitchmap.R", local = app_env),
  error = function(e) stop("Error while sourcing pitchmap.R: ", e$message)
)

# 'source' returns a list with element $value = value of last expression in the file (if any)
maybe_app <- NULL
if (is.list(sourced) && "value" %in% names(sourced)) maybe_app <- sourced$value

# Case A: pitchmap.R returned a shiny.appobj directly
if (!is.null(maybe_app) && inherits(maybe_app, "shiny.appobj")) {
  shiny_app <- maybe_app
} else {
  # Case B: pitchmap.R defined ui and server inside app_env
  if (exists("ui", envir = app_env, inherits = FALSE) &&
      exists("server", envir = app_env, inherits = FALSE)) {
    shiny_app <- shiny::shinyApp(ui = get("ui", envir = app_env),
                                 server = get("server", envir = app_env))
  } else {
    stop("pitchmap.R did not return a shiny.appobj and did not define both 'ui' and 'server'.\n",
         "Please ensure pitchmap.R either ends with shinyApp(ui, server) or defines 'ui' and 'server' objects.")
  }
}

# Finally, run the shiny app (this also returns the app object so shiny::runApp('.') works)
shiny_app
