# MicrobeStudio Shiny Application

options(shiny.useragg = TRUE)
options(bitmapType = "cairo")

library(shiny.telemetry)
source("src/ui.R")
source("src/server.R")
shinyApp(ui = ui, server = server)
