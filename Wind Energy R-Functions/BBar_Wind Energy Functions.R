#'##################################################################################################
#'
#' Bently's Wind Energy Functions
#'
#' Author: Bently Bartee
#' Copy-write is me
#'
#'##################################################################################################


#---------------------------------------------------------------------------------------------------
#' Render Power Curve Charts and Outputs for each WTG, save file as a .html file and also creates a
#' zip file to hold the .html file

Power_Curve_Charts_Render <- function(File.Path,
                                      Site.Name,
                                      wtgData_YR,
                                      ULimage_path,
                                      wtgType,
                                      MAXCAP) {
  # List of packages
  packages <- c(
    "rmarkdown",
    "zip",
    "progress",
    "ggplot2",
    "gridExtra",
    "dplyr",
    "data.table",
    "cowplot"
  )
  for (pkg in packages) {
    # If the package is not installed, install it
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    # Then load it
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  
  # Verify data availability
  if (missing(wtgData_YR) ||
      !is.data.frame(wtgData_YR) || nrow(wtgData_YR) == 0) {
    stop("Error: Required dataframe 'wtgData_YR' is missing or empty.")
  }
  if (missing(ULimage_path) || !file.exists(ULimage_path)) {
    stop("Error: Required image file 'ULimage.PNG' not found at the specified path.")
  }
  if (missing(wtgType)) {
    stop("Error: 'wtgType' must be specified.")
  }
  if (missing(MAXCAP) || !is.numeric(MAXCAP)) {
    stop("Error: 'MAXCAP' must be a numeric value.")
  }
  
  # Create a progress bar
  pb <- progress_bar$new(total = 3,
                         # Three main steps: folder creation, rendering, zipping
                         format = " Overall Progress [:bar] :percent eta :eta",
                         clear = FALSE)
  pb$tick(0) # Initialize progress bar
  
  # Create folder to export data
  folder_path <- file.path("./OutPut", Site.Name)
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  pb$tick() # Increment progress after folder creation
  
  # Prepare R Markdown content dynamically
  rmd_content <- c(
    "---",
    paste0("title: '`r Site.Name` Yearly Graphs'"),
    'author: "UL Solutions Asset Advisory Services"',
    paste0("date: '`r format(Sys.Date(), \'%B %d, %Y\')`'"),
    "output: html_document",
    "---",
    "",
    "<div style=\"position: absolute; top: 0; right: 0;\">",
    paste0(
      "   <img src=\"",
      basename(ULimage_path),
      "\" width=\"200px\" align=\"right\">"
    ),
    "</div>",
    "",
    "```{r setup, include=FALSE}",
    "# Load required libraries",
    "required_packages <- c(\"ggplot2\", \"gridExtra\", \"dplyr\", \"data.table\", \"cowplot\")",
    "",
    "# Install missing packages if needed",
    "installed_packages <- required_packages %in% rownames(installed.packages())",
    "if (any(!installed_packages)) {",
    "  install.packages(required_packages[!installed_packages])",
    "}",
    "",
    "# Load packages",
    "lapply(required_packages, require, character.only = TRUE)",
    "",
    "# Verify data availability",
    "if (!exists(\"wtgData\")) {",
    "  wtgData <- wtgData_YR",
    "  if (!exists(\"wtgData\")) {",
    "    stop(\"Required dataset 'wtgData_YR' not found\")",
    "  }",
    "}",
    "```",
    "## Yearly Charts",
    "Note these are all 10 min timestamps. ",
    "Find each WTG and loop for each WTG to create a power curve chart.",
    "We filter for all realpower that is > 0",
    "",
    "```{r Graphs, echo=FALSE, message = FALSE, warning=FALSE, comment=NA, out.width=\"175%\", out.height=\"150%\"}",
    "",
    "# Find each WTG and loop for each WTG to create a power curve chart.",
    "# We filter for all realpower that is > 0",
    "# Initialize an empty list to store the power curve plots",
    "wtgList <- sort(unique(wtgData$WTG))",
    "",
    "powercurves <- list()",
    "",
    "for (i in 1:length(wtgList)) {",
    "  # Extract the current WTG",
    "  wt <- wtgList[i]",
    "  wtg <- subset(wtgData, wtgData$WTG == wt & wtgData$RealPower > 0) ",
    "  # Check if the subset has any rows",
    "  if (nrow(wtg) == 0) {",
    "    cat(paste0(\"No data found for WTG-\", wt, \". Skipping this WTG.\\n\"))",
    "    next # This will skip to the next iteration of the loop",
    "  }",
    "  powercurve <-",
    "    wtg %>%",
    "    ggplot(aes(x = WindSpeed, y = RealPower)) +",
    "    geom_point(color = \"#00689C\") +",
    "    geom_smooth(",
    "      aes(x = WindSpeed, y = ifelse(OEM_Power > max(RealPower), max(RealPower), OEM_Power)),",
    "      method = \"loess\",",
    "      se = FALSE,",
    "      span = 0.2,",
    "      color = \"#04B404\",",
    "      size = 1.1",
    "    ) +",
    "    coord_cartesian(ylim = c(0, MAXCAP+100)) +",
    "    geom_line(aes(x = WindSpeed, ",
    "                    y = rep(unique(Rating), length(WindSpeed)), ",
    "                    color = paste(\"Rated Power \\n\", unique(Rating), \"(kW)\"))",
    "              ,size = .65, ",
    "              linetype = \"dotdash\") +",
    "    scale_color_manual(values = \"#CA0123\") +",
    "    labs(x = \"Windspeed\", y = \"RealPower\", title = paste0(\"WTG-\", wt, \" Power Curve Year 2023\"), color = \"\") +",
    "    scale_x_continuous(limits = c(min(wtg$WindSpeed), max(wtg$WindSpeed), by = 1),",
    "                       breaks = seq(0, max(wtg$WindSpeed), by = 1))+",
    "    scale_y_continuous(limits = c(min(wtg$RealPower), max(wtg$RealPower) + 100, by = 1),",
    "                       breaks = seq(0, max(wtg$RealPower) + 200, by = 200)) +",
    "    theme_half_open() +",
    "    background_grid() ",
    "",
    "  powercurve <- powercurve + ",
    "    theme(axis.text.x = element_text(size = 6),",
    "          axis.text.y = element_text(size = 6),",
    "          axis.title.x = element_text(size = 8),",
    "          axis.title.y = element_text(size = 8),",
    "          legend.text = element_text(size = 8),",
    "          plot.title = element_text(size = 10)  # Change the title size here",
    "    ) +",
    "    annotate(",
    "      \"text\",",
    "      x = max(wtg$WindSpeed) * 0.95,",
    "      y = as.numeric(unique(wtg$Rating)),",
    "      label = wtgType,",
    "      hjust = 1,",
    "      vjust = -.25,",
    "      color = \"#04B404\"",
    "    )",
    "",
    "  comboplot <- gridExtra::grid.arrange(powercurve, ncol = 1)",
    " ",
    "  # Add the power curve plot to the list",
    "  powercurves[[wt]] <- comboplot",
    "  print(comboplot) # Print each plot within the loop for rendering",
    "}",
    "",
    "# Return the list of power curve plots",
    "invisible(powercurves)",
    "```"
  )
  
  # Create a temporary R Markdown file
  temp_rmd_file <- file.path(folder_path, "temp_report.Rmd")
  writeLines(rmd_content, temp_rmd_file)
  
  # Copy the UL image to the output folder
  file.copy(ULimage_path, file.path(folder_path, basename(ULimage_path)), overwrite = TRUE)
  
  # Render the R Markdown file
  rmarkdown::render(
    temp_rmd_file,
    output_format = "html_document",
    output_dir = folder_path,
    output_file = paste0(Sys.Date(), "_", Site.Name, "_YearlyGraphs.html"),
    params = list(Site.Name = Site.Name) # Pass Site.Name as a parameter
  )
  pb$tick() # Increment progress after rendering
  
  # File output will output to your FilePath in a folder named SCADACheck_Outputs
  # Create a zip file
  setwd(folder_path)
  cat(paste("Saving YearlyGraphs report for", Site.Name, "\n"))
  
  zip_filename <- paste0(Sys.Date(), "_", Site.Name, "_YearlyGraphs.zip")
  html_filename <- paste0(Sys.Date(), "_", Site.Name, "_YearlyGraphs.html")
  files_to_zip <- c(html_filename, basename(ULimage_path)) # Include the image in the zip
  
  zip(zipfile = zip_filename, files = files_to_zip)
  setwd("../..")
  pb$tick() # Increment progress after zipping
  
  # Clean up the temporary R Markdown file
  file.remove(temp_rmd_file)
  
  cat("\n*** YearlyGraphs report is rendered and complete ***\n")
}


#'---------------------------------------------------------------------------------------------------
#' Calculates wind turbulence (standard deviation of wind speed / mean wind speed)

calculate_Wind_Turbulence <- function(data, wind_speed_column) {
  # Calculates wind turbulence (standard deviation of wind speed / mean wind speed)
  # and adds it as a 'Turbulence' column to the input data frame.
  
  if (!wind_speed_column %in% names(data)) {
    stop(
      paste(
        "Error: Wind speed column '",
        wind_speed_column,
        "' not found in the data frame."
      )
    )
  }
  wind_speed_sd <- sd(data[[wind_speed_column]], na.rm = TRUE)
  mean_wind_speed <- mean(data[[wind_speed_column]], na.rm = TRUE)
  if (mean_wind_speed == 0) {
    data$Turbulence <- NA  # Avoid division by zero
    warning("Warning: Mean wind speed is zero, turbulence set to NA.")
  } else {
    data$Turbulence <- wind_speed_sd / mean_wind_speed
  }
  return(data)
}

#'---------------------------------------------------------------------------------------------------
#' Calculates air density using the air.density() function (assuming it's available)

calculate_air_density <- function(data, temp_col, pressure_col) {
  # Calculates air density using the air.density() function (assuming it's available)
  # for each row of the input data frame and adds it as an 'AirDensity' column.
  
  # Check if the package is installed
  if (!requireNamespace("rWind", quietly = TRUE)) {
    # Install the package if not installed
    install.packages("rWind")
  }
  
  if (!temp_col %in% names(data)) {
    stop(paste(
      "Error: Air temperature column '",
      temp_col,
      "' not found in the data frame."
    ))
  }
  if (!pressure_col %in% names(data)) {
    stop(paste(
      "Error: Air pressure column '",
      pressure_col,
      "' not found in the data frame."
    ))
  }
  data$AirDensity <- rWind::air.density(TempAir = data[[temp_col]], AirPressure = data[[pressure_col]])
  return(data)
}

#'---------------------------------------------------------------------------------------------------
#' Normalizes wind speed to a standard air density.

normalize_wind_speed <- function(data,
                                 wind_speed_col = "wind.speed",
                                 air_density_col = "AirDensity",
                                 standard_air_density = 1.225) {
  # Normalizes wind speed to a standard air density.
  if (!wind_speed_col %in% names(data)) {
    stop(paste(
      "Error: Wind speed column '",
      wind_speed_col,
      "' not found in the data frame."
    ))
  }
  if (!air_density_col %in% names(data)) {
    stop(
      paste(
        "Error: Air density column '",
        air_density_col,
        "' not found in the data frame."
      )
    )
  }
  normalized_ws <- (data[[wind_speed_col]] * ((data[[air_density_col]] / standard_air_density) ^ (1 / 3)))
  return(normalized_ws)
}

#'---------------------------------------------------------------------------------------------------
#'Calculates air density and density adjusts wind speed data based on the input temperature.
# Can also add in elevation, pressure, or RH% data as variable arguments

density_Adj_WindSpeed = function(ws,
                                 temperature,
                                 pressure = 1013.25,
                                 rh = .5,
                                 elevation = 0,
                                 norm_rho = 0) {
  # Calculates air density and density adjusts wind speed data based on the input temperature.
  # Can also add in elevation, pressure, or RH% data as variable arguments
  # SYNTAX:
  #   densityAdjWindspeed(ws,temperature,...)
  # INPUTS:
  #   required:
  #       -ws: in m/s
  #       -temperature: in Celsius
  #   variable arguments:
  #       -pressure = mb
  #       -rh  =  % of 1
  #       -elevation = meters
  
  #### calculate air density ####
  #if only temperature data is entered
  if (pressure == 1013.25 & rh == .5) {
    density = (1013.25 * 100) / (287 * (temperature + 273.15)) * exp((-9.81 *
                                                                        elevation * (1.0397 - 0.000025 * elevation)) / (287 * (temperature + 273.15)))
  }
  #if pressure and RH is entered
  else if ((pressure != 1013.25 & rh != .5)) {
    density = 1 / (temperature + 273.15) *
      ((pressure * 100 / 287.05) -
         (rh *
            (0.0000205 * exp(
              0.0631846 * (temperature + 273.15)
            )) *
            (1 / 287.05 - 1 / 461.5)))
  }
  # if just temperature and pressure data is entered
  else {
    density   = pressure * 100 / 287 / (temperature + 273.15)
  }
  
  #### apply density adjustment to the wind speeds ####
  if (norm_rho == 0) {
    norm_rho <- mean(density, na.rm = TRUE)
  }
  
  adj_coe = (density / norm_rho) ^ (1 / 3)
  dc_ws = ws * adj_coe
  
  #### output the results to a list ####
  output <- list(dc_ws, density)
  names(output) <- c("dc_ws", "density")
  return(output)
}


#'---------------------------------------------------------------------------------------------------
#' Calculates the capacity factor based on measured power and reference power.

calculate_capacity_factor <- function(data,
                                      reference_power,
                                      measured_power_col) {
  # data: Your data frame.
  # reference_power: The rated power capacity of your wind turbine (in MW).
  # measured_power_col: A character string specifying the name of the measured active power column).
  
  if (!measured_power_col %in% names(data)) {
    stop(
      paste(
        "Error: Measured power column '",
        measured_power_col,
        "' not found in the data frame."
      )
    )
  }
  # total possible electrical output (tpe) assuming 1 MW capacity over 24 hours
  tpe <- 1 * 24 * reference_power
  # actual electrical output of the plant over the same period (aeo)
  aeo <- data[[measured_power_col]]
  # Calculate the capacity factor (CF = (AEO / TPE) * 100) - Assuming percentage, so multiply by 100
  CF <- (aeo / tpe) * 100
  return(CF)
}

#'---------------------------------------------------------------------------------------------------
#' #Read NextEra File

read_NextEraScada_xlsx <- function(file.loc, xlsxName, rating = 0) {
  # List of packages
  packages <- c("openxlsx", "dplyr")
  for (pkg in packages) {
    # If the package is not installed, install it
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    # Then load it
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  
  if (rating == 0) {
    rating <- 0
  } else{
    rating <- rating
  }
  
  GEFile <- openxlsx::read.xlsx(
    xlsxFile = paste0(file.loc, xlsxName),
    sheet = 1,
    startRow = 11,
    fillMergedCells = TRUE
  )
  #Correct Column Names and WTG Column Values
  colnames(GEFile) <- c("WTG", "TimeStamp10Min", "RealPower", "WindSpeed")
  #GEFile <- GEFile[, colnames(GEFile) %in% c("System.Name", "Time.Stamp", "Power(kW)", "Wind.speed(m/s)")]
  #names <- names(GEFile)
  #names <- gsub("xml:space=\"preserve\">", "", names)
  GEFile$WTG <- gsub("xml:space=\"preserve\">", "", GEFile$WTG)
  GEFile$TimeStamp10Min <- openxlsx::convertToDateTime(GEFile$TimeStamp10Min, origin = "1900-01-01")
  #select on the rows needed
  GEFile <- GEFile[, c(1:4)]
  #remove NAs
  GEFile <- GEFile %>%
    mutate_at(vars(RealPower, WindSpeed), ~ ifelse(is.na(.), 0, .))
  GEFile$Rating <- rating
  
  GEFile <- GEFile[GEFile$WindSpeed <= 50, ]# delete windspeed values greater than 50ms
  
  if (any(GEFile$WTG != 0)) {
    GEFile <- subset(GEFile, WTG != 0)
  }
  
  GEFile <- data.frame(GEFile)
  return(GEFile)
}



#'---------------------------------------------------------------------------------------------------
#' This function calculates the eastward and northward wind components from wind speed and direction, 
#' and then determines the wind direction from these components.

calc_wind_components_and_direction <- function(ws, wd) {
  # Break wind into east/west (u) and north/south (v) components
  u <- -ws * sin(wd * (pi / 180))
  v <- -ws * cos(wd * (pi / 180))
  
  # Calculate wind direction from u and v components
  if (!is.na(u) & !is.na(v)) {
    if (u == 0) {
      if (v > 0) {
        wind_direction <- 180
      } else if (v < 0) {
        wind_direction <- 0
      } else {
        wind_direction <- 0
      }
    } else {
      raw_angle <- 90 - (180 / pi) * atan2(v, u)
      if (raw_angle < 0) {
        wind_direction <- raw_angle + 360
      } else if (raw_angle > 360) {
        wind_direction <- raw_angle - 360
      } else {
        wind_direction <- raw_angle
      }
    }
  } else {
    wind_direction <- NA
  }
  
  return(list(u = u, v = v, wind_direction = wind_direction))
}

# function test routine
for (i in 0:360) {
  wind_data <- calc_wind_components_and_direction(5, i)
  print(paste(i, round(wind_data$u, 1), round(wind_data$v, 1), round(wind_data$wind_direction, 1), sep = " : "))
}


#'---------------------------------------------------------------------------------------------------
#' This function calculates the smaller angular difference between two wind directions, 
#' ensuring the result is always between 0 and 180 degrees.

wind_Direction_delta <- function (windDir_1, windDir_2) {
  d1 <- windDir_1 * (pi / 180)
  d2 <- windDir_2 * (pi / 180)
  delta_dir_rad <- abs(d1 - d2)
  if (delta_dir_rad > pi) {
    delta_dir_rad <- 2 * pi - delta_dir_rad
  }
  delta_dir_deg <- delta_dir_rad * (180 / pi)
  return(delta_dir_deg)
}

#'---------------------------------------------------------------------------------------------------
#' A function to calculate the zonal wind speed from wind speed/direction vectors
calc_u_zonal_wind_speed <- function(ws, wd) {
          u <- -ws*sin(wd*(pi/180))  #u is zonal (east/west) wind component
          return(u)
        }
		
#'---------------------------------------------------------------------------------------------------
#' A function to calculate the meridional wind speed from wind speed/direction vectors		
calc_v_meridional_wind_speed <- function(ws, wd) {
  v <- -ws*cos(wd*(pi/180))  #v is the meridional (north/south) wind component
  return(v)
}


#'---------------------------------------------------------------------------------------------------
#' A function to calculate wind direction from meridional and zonal wind vector components
windDir_meridional_zonal_wind_direction <- function(u, v) {
  if(!is.na(u) & !is.na(v)){
    if(u==0) {
      if(v>0) {
        wd <- 180
      } else if(v<0) {
        wd <- 0
      } else {
        wd <- 0
      }
    } else {
      if(90-(180/pi)*(atan2(v,u))<180) {
        wd <- 90-(180/pi)*(atan2(v,u))+180
      } else if (90-(180/pi)*(atan2(v,u))>180) {
        wd <- 90-(180/pi)*(atan2(v,u))-180
      } else {
        wd <- 0
      }
    }
  } else {
    wd <- NA
  }
  return(wd)
}