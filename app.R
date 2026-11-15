# MicrobeStudio Shiny Application

options(shiny.useragg = TRUE)
options(bitmapType = "cairo")

library(shiny.telemetry)

# Core, unit-tested functions (validation, normalization, provenance)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

source("src/ui.R")
source("src/server.R")
shinyApp(ui = ui, server = server)
