# File name: functions.R
# Path: './R/functions.R'

# Author: NK Chan
# Purpose: Script for housing custom functions required for project.

# NB: Strictly speaking, this is a project and not a package. That said, using
# the Roxygen2 documentation style for writing functions is helpful for keeping
# information about custom functions organized.


#' @title Check, install, and load required packages
#'
#' @description Automated method for checking, installing, and loading a list
#' of packages provided by the user. Function asks the user before installing.
#'
#' @param ... A list or vector containing the names of packages as strings to
#' be loaded and installed.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' pkgs <- c("ggplot2", "tidyverse")
#' using(pkgs)
#' }
#'
#' @author N. Chan
#' @export
#'

using <- function(...) {
  libs <- unlist(list(...))
  req <- suppressWarnings(unlist(lapply(libs, require, character.only = TRUE)))
  need <- libs[req == FALSE]
  n <- length(need)
  
  if (n > 0) {
    libsmsg <-
      if (n > 2) {
        paste(paste(need[1:(n - 1)], collapse = ", "), ",", sep = "")
      } else
        need[1]
    
    if (n > 1) {
      libsmsg <- paste(libsmsg, " and ", need[n], sep = "")
    }
    
    libsmsg <-
      paste(
        "The following packages could not be found: ",
        libsmsg,
        "\n\r\n\rInstall missing packages?",
        collapse = ""
      )
    
    # Checks if R is in interactive mode. If yes, then prompt user for
    # interactive response. If no, prompt user for input from stdin.
    if (interactive()) {
      if (!(askYesNo(libsmsg, default = FALSE) %in% c(NA, FALSE))) {
        install.packages(need)
        lapply(need, require, character.only = TRUE)
      } else {
        stop("required packages were not installed or loaded")
      }
      
    } else {
      cat(libsmsg, "(yes/No/cancel) ")
      response <- readLines("stdin", n = 1)
      input <- pmatch(tolower(response), c("yes", "no", "cancel"))
      
      if (!nchar(response) | input %in% c(2, 3)) {
        stop("required packages were not installed or loaded")
      } else if (is.na(input)) {
        stop("Unrecognized response ", dQuote(response))
      } else {
        install.packages(need)
        lapply(need, require, character.only = TRUE)
      }
    }
    
  }
  
  return(invisible(NULL))
}


#' @title Check, install, and load required packages from GitHub
#'
#' @description Automated method for checking, installing, and loading a list
#' of packages from GitHub provided by the user. Function asks the user before
#' installing.
#'
#' @param libs A list or vector containing the names of packages as strings to
#' be loaded and installed.
#' @param repos A list or vector containing the repository address of packages
#' to be installed and loaded as strings using the format "username/repo".
#' Must be same length as 'libs'. 
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' pkgs <- c("ggplot2", "tidyverse")
#' using_github(pkgs)
#' }
#'
#' @author N. Chan
#' @export
#'

using_github <- function(libs, repos) {
  
  if (length(libs) != length(repos)) {
    stop("Lengths of 'libs' and 'repos' must be equal.")
  }
  
  libs <- unlist(list(libs))
  repos <- unlist(list(repos))
  req <- suppressWarnings(unlist(lapply(libs, require, character.only = TRUE)))
  need <- libs[req == FALSE]
  repos <- repos[req == FALSE]
  n <- length(need)
  
  if (n > 0) {
    libsmsg <-
      if (n > 2) {
        paste(paste(need[1:(n - 1)], collapse = ", "), ",", sep = "")
      } else
        need[1]
    
    if (n > 1) {
      libsmsg <- paste(libsmsg, " and ", need[n], sep = "")
    }
    
    libsmsg <-
      paste(
        "The following packages could not be found: ",
        libsmsg,
        "\n\r\n\rInstall missing packages?",
        collapse = ""
      )
    
    # Checks if R is in interactive mode. If yes, then prompt user for
    # interactive response. If no, prompt user for input from stdin.
    if (interactive()) {
      if (!(askYesNo(libsmsg, default = FALSE) %in% c(NA, FALSE))) {
        remotes::install_github(repos)
        lapply(need, require, character.only = TRUE)
      } else {
        stop("required packages were not installed or loaded")
      }
      
    } else {
      cat(libsmsg, "(yes/No/cancel) ")
      response <- readLines("stdin", n = 1)
      input <- pmatch(tolower(response), c("yes", "no", "cancel"))
      
      if (!nchar(response) | input %in% c(2, 3)) {
        stop("required packages were not installed or loaded")
      } else if (is.na(input)) {
        stop("Unrecognized response ", dQuote(response))
      } else {
        remotes::install_github(repos)
        lapply(need, require, character.only = TRUE)
      }
    }
    
  }
  
  return(invisible(NULL))
}


#' @title Plot missing values
#'
#' @description Make a ggplot heatmap of missing values in the data
#'
#' @param data A data frame
#' @param title A string. The title of the plot. Default is NULL, which
#'   automatically assigns "Plot of NA values".
#' @param subtitle A string. A subtitle for the plot. Default is NULL.
#' @param print_ggplotly A logical. TRUE to print ggplotly plot. Default is
#'   FALSE.
#' @param save_data A logical. TRUE to return the data used to make the plot.
#'   Default is FALSE.
#'
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#' mydata <- airquality
#' plot_missing(mydata)
#' }
#'
#' @author N. Chan
#' @export
#' 

plot_missing <- function(data, 
                         title = NULL, 
                         subtitle = NULL,
                         print_ggplotly = F, 
                         save_data = F) {
  
  plot_data <- data %>% 
    transmute_all(is.na) %>% 
    rowid_to_column() %>% 
    pivot_longer(-rowid, names_to = "name", values_to = "value")
  
  if (sum(plot_data$value) != 0) {
    plot_colours <- c("blue", "yellow")
  } else {
    plot_colours <- c("blue")
  }
  
  if (is.null(title)) {
    plot_title <- "Plot of NA values"
  } else {
    plot_title <- as.character(title)
  }
  
  if (is.null(subtitle)) {
    plot_subtitle <- NULL
  } else {
    plot_subtitle <- as.character(subtitle)
  }
  
  plot_out <- ggplot(plot_data, aes(y = name, x = rowid)) + 
    geom_raster(aes(fill = value)) +
    scale_fill_manual(values = plot_colours, name = "Missing") +
    labs(y = "Column Name", x = "Row ID", title = plot_title, subtitle = plot_subtitle) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold"))
  
  if (print_ggplotly) {
    plot_out <- ggplotly(plot_out)
  }
  
  if (save_data) {
    plot_out <- list(
      data = plot_data,
      plot = plot_out
    )
  }
  
  return(plot_out)
  
}


#' @title Get attributes and a table from a vector
#'
#' @description
#'
#' @param x A vector. This should be a column of a tibble imported by
#'   haven::read_sav() from an SPSS file containing a data dictionary.
#'
#' @return A list with 3 objects. `Question` stores the `label` attribute,
#'   `Values` stores the `labels` attribute, and `Table` stores a table of the
#'   vector listing counts by unique values.
#'
#' @examples
#' \dontrun{
#' mydata <- airquality
#' plot_missing(mydata)
#' }
#'
#' @author N. Chan
#' @export
#' 

get_var_info <- function(x) {
  a <- attr(x, "label", exact = TRUE)
  b <- attr(x, "labels", exact = TRUE)
  d <- table(x, useNA = "ifany")
  return(list("Question" = a, "Values" = b, "Table" = d))
}
