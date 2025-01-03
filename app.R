# Load necessary libraries
library(shiny)
library(bs4Dash)
library(tidyverse)
library(bslib)
library(DT)
library(scales)
library(lubridate)
library(zoo)
library(ChainLadder)
library(shinycssloaders)
library(plotly)

options(shiny.maxRequestSize = 10000000 * 1024^2)  # 10000000 MB

#Sourcing the Modules to the main App
source("modules/dataOverviewModule.R", local = TRUE)[1]
source("modules/bootstrappingModule.R", local = TRUE)[1]
source("modules/riskMarginResultsModule.R", local = TRUE)[1]

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  # Darker background for the navbar for contrast
  navbar_fg = "#ffffff"  # White text color for readability
)

# UI
ui <- bs4DashPage(
  title = "Risk Adjustment Model",
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  header = bs4DashNavbar(
    fixed = "TRUE",
    status = "white",
    skin = "dark",
    sidebarIcon = NULL,
    controlbarIcon = NULL,
    tags$li(
      class = "text-center header-title-container",  # Added a new class for more specific styling
      tags$h4("Risk Adjustment Dashboard", class = "header-title")
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    tags$div(
      class = "menu-container",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      bs4SidebarMenuItem("Bootstrapping Results", tabName = "bootstrapping_results", icon = icon("sync-alt")),
      bs4SidebarMenuItem("Risk Margin Download", tabName = "risk_margin_download", icon = icon("download"))
    )),
    div(class = "sidebar-logo",
        img(src = "images/kenbright.png")
    )
  ),
  body = bs4DashBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      tags$link(href = "https://fonts.googleapis.com/css?family=Mulish", rel = "stylesheet"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.1/moment.min.js"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon")
    ),
    bs4TabItems(
      # Data Overview Tab
      bs4TabItem(
        tabName = "data_overview",
        dataOverviewUI("data_overview_id")
      ),
      # Bootstrapping Results Tab
      bs4TabItem(
        tabName = "bootstrapping_results",
        bootstrappingUI("bootstrapping_id")
      ),
      # Risk Margin Download Tab
      bs4TabItem(
        tabName = "risk_margin_download",
        riskMarginResultsUI("riskMarginResults_id")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

   data <- dataOverviewServer("data_overview_id") 

   boot_out <- bootstrappingServer("bootstrapping_id", data)

   risk_margin_data <- boot_out$risk_margin_data

   all_boot_results <- boot_out$bootResults

   riskMarginResultsServer("riskMarginResults_id", risk_margin_data, all_boot_results)
}

# Run the application
shinyApp(ui, server)
