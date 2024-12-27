# Risk Adjustment Shiny Application

A Shiny application for **risk adjustment** and **bootstrapping** analyses, including data overview, bootstrap modeling, and risk margin calculation. Built with [**bs4Dash**](https://rinterface.github.io/bs4Dash/), [**tidyverse**](https://www.tidyverse.org/), and [**ChainLadder**](https://cran.r-project.org/web/packages/ChainLadder/index.html), this app streamlines actuarial processes for claims data modeling.

---

## Table of Contents

1. [Features](#features)  
2. [Project Structure](#project-structure)  
3. [Requirements](#requirements)  
4. [Installation](#installation)  
5. [Usage](#usage)  
7. [Modules](#modules)  
   - [Data Overview Module](#data-overview-module)  
   - [Bootstrapping Module](#bootstrapping-module)  
   - [Risk Margin Results Module](#risk-margin-results-module)  

---

## Features

- **Interactive Dashboard**: Provides a fluid navigation between data overview, bootstrapping results, and risk margin summaries.  
- **Data Upload & Validation**: Upload claims data in Excel or CSV format and automatically validate columns, data types, and date formats.  
- **Bootstrapping Analysis**: Uses _ChainLadder_ to fit and simulate IBNR estimates with customizable outlier handling.  
- **Risk Margin Computations**: Calculates risk margins at specified confidence levels (e.g., 75% CI).  
- **Customizable Theming**: Provides a modern look via custom [bslib](https://rstudio.github.io/bslib/) theme.

---

## Project Structure


1. **app.R**  
   - Loads libraries, defines global options, and sources the module files.  
   - Constructs the UI (using `bs4DashPage`).  
   - Defines the server logic, tying together the modules.  

2. **modules/**  
   - **dataOverviewModule.R**: Contains UI and Server functions for uploading and previewing claims data.  
   - **bootstrappingModule.R**: Contains UI and Server for executing bootstrapping and outlier handling.  
   - **riskMarginResultsModule.R**: (Not shown in the snippet above but included) Handles summarizing and/or downloading computed risk margins.

---

## Requirements

To run this Shiny application, ensure you have **R (≥ 4.0)** and the following packages:

- **shiny**  
- **bs4Dash**  
- **tidyverse**  
- **bslib**  
- **DT**  
- **scales**  
- **lubridate**  
- **zoo**  
- **ChainLadder**  
- **shinycssloaders**  
- **plotly**  
- **readxl** (if uploading Excel files)

Install missing packages with:
```r
install.packages(c(
  "shiny", 
  "bs4Dash", 
  "tidyverse", 
  "bslib", 
  "DT", 
  "scales", 
  "lubridate", 
  "zoo", 
  "ChainLadder", 
  "shinycssloaders", 
  "plotly",
  "readxl"
))
```
## Usage

1. **Launch the App:**
    ```r
    # In R or RStudio console
    shiny::runApp("path_to_cloned_repo")

    ```
2. **Navigate the Dashboard:**

   - **Data Overview:** Upload your claims dataset, check the preview table, and ensure fields/dates are parsed correctly.
   - **Bootstrapping Results:** Configure your bootstrapping approach, select the appropriate statutory class, handle outliers, and generate your IBNR estimates.
   - **Risk Margin Download:** Generate and/or download the risk margin results, as needed.


## Modules

1. **Data Overview Module**

   **UI (dataOverviewUI):**
   - Displays a file upload control and a data table preview.

  **Server (dataOverviewServer):**
    - Validates columns, parses dates, and returns a reactive dataset to other modules.

2. **Bootstrapping Module**

   **UI (bootstrappingUI):**
   - Allows users to select a statutory class, year range, and outlier-handling method (remove via quantiles or numeric thresholds).
   - Includes an action button to trigger the bootstrapping analysis.

   **Server (bootstrappingServer):**
   - Applies filtering and outlier removal.
   - Builds a cumulative triangle with ChainLadder.
   - Runs BootChainLadder to generate IBNR estimates.
   - Computes risk margins at specified confidence levels.
   - Returns summary tables and plots.

3. **Risk Margin Results Module**

   **UI (riskMarginResultsUI):**
   - Typically provides UI elements to present final risk margins or allow for downloading results.

   **Server (riskMarginResultsServer):**
   - Receives the reactive risk margin data from bootstrapping, potentially displays or exports it.


## Contributing
- **Fork** the repository.
- **Create a branch** for your feature (git checkout -b feature/awesomeFeature).
- **Commit** your changes (git commit -m 'Add awesomeFeature').
- **Push** to your branch (git push origin feature/awesomeFeature).
- **Open a Pull Request** and describe your updates.