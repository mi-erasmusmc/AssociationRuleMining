drawSankey <- function(data) {
  
  x <- rjson::toJSON(data)
  
  # create the widget
  htmlwidgets::createWidget("sigma", x, width = "100%", height = "400px")
}

drawSankeyOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "sigma", width, height, package = "sigma")
}

renderdrawSanky <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, sigmaOutput, env, quoted = TRUE)
}
