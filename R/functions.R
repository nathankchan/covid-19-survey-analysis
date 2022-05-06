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


#' @title Get the original variable name after data doubling methods
#'
#' @description Retrieves a variable's name after running
#'   `ours::disjunctive_coding()`, `ours::escofier_coding()`, or
#'   `ours::thermometer_coding()`.
#'
#' @param x A list or vector containing the names of variables suffixed by `+`,
#'   `-`, or `.{label}`.
#'
#' @return A vector containing the names of the variables with suffixes removed
#'
#' @examples
#' \dontrun{
#' mydata <- c("var1+", "var1-", "var2.A", "var2.B")
#' varname_from_disjunc(mydata)
#' }
#'
#' @author N. Chan
#' @export
#' 

varname_from_disjunc <- Vectorize(
  FUN = function(x) {
    if (grepl("(\\+$)|(\\-$)", x = x)) {
      varname <- str_replace(x, "(\\+$)|(\\-$)", "")
    } else {
      varname <- str_replace(x, "(\\.[\\s\\S]{1,})", "")
    }
    return(varname)
  },
  vectorize.args = "x"
)


#' @title Get outlier statistics following MCD and corrmax transformation
#'
#' @description Identifies outliers based on Mahalanobis distances and robust
#'   distances computed from outputs of `ours::mixed_mcd_data()` and
#'   `ours::generalized_corrmax()`. Modified from code in OuRS.
#'
#' @param mcd_results The output of `ours::mixed_mcd_data()`
#' @param mcd_corrmax The output of `ours::generalized_corrmax()`. Should be
#'   computed using the same data that was used to make `mcd_results`.
#' @param data The original data frame fed to `ours`.
#' @param md_thresh The threshold value used to identify outliers from
#'   Mahalanobis and robust distance metrics. Default is the square root of the
#'   97.5% quantile of a chi square distribution with $k$ degrees of freedom,
#'   where $k$ is the original number of variables in the dataset (i.e., before
#'   data doubling).
#' @param cor_thresh The threshold value used to identify the most important
#'   contributors to outlier's distance metrics. Default is 10.
#'
#' @return A list containing two data frames. `$md_scores_outliers` lists all
#'   cases with distance metrics greater than `md_thresh`. `$contributions`
#'   lists the most important variables contributing to cases containing
#'   outliers.
#'
#' @examples
#' \dontrun{
#' mydata <- c("var1+", "var1-", "var2.A", "var2.B")
#' varname_from_disjunc(mydata)
#' }
#'
#' @author N. Chan
#' @export
#' 

get_outlier_stats <-
  function(mcd_results,
           mcd_corrmax,
           data,
           md_thresh = NULL,
           cor_thresh = 1) {
    mcd_res <- mcd_results
    sqrt_rob_md <- mcd_res$dists$robust_mahal_dists
    sqrt_stand_md <- mcd_res$dists$mahal_dists
    md_scores <- sqrt(cbind(sqrt_stand_md, sqrt_rob_md))
    
    md_scores_all <- md_scores[order(md_scores[, 2], md_scores[, 1], decreasing = T), , drop = F]
    
    if (is.null(md_thresh)) {
      chisq_df <-
        mcd_corrmax$contributions %>% colnames %>% varname_from_disjunc %>% unique %>% length
      md_thresh <- sqrt(qchisq(p = 1 - 0.025, df = chisq_df))
    } else {
      md_thresh <- md_thresh
    }
    
    md_scores_outliers <-
      md_scores[which(md_scores[, 1] > md_thresh |
                        md_scores[, 2] > md_thresh), , drop = F]
    
    # browser()
    
    if (length(md_scores_outliers) > 1) {
      md_scores_outliers <-
        md_scores_outliers[order(md_scores_outliers[, 2], md_scores_outliers[, 1], decreasing = T), , drop = F]
      
      # cor_res <- mcd_corrmax$contributions
      cor_res <- mcd_corrmax$contributions
      cor_res <-
        cor_res[as.numeric(rownames(md_scores_outliers)), , drop = F]
      rownames(cor_res) <- rownames(md_scores_outliers)
      
      cor_contrib_per_obs_df <- get_varwise_sum(cor_res, data = data)
      cor_contrib_per_obs_df <- cor_contrib_per_obs_df[which(cor_contrib_per_obs_df$contribution > cor_thresh), ]
      
      out <- list(
        md_thresh = md_thresh,
        md_scores = 
          list(all = md_scores_all,
               outliers = md_scores_outliers),
        cor_thresh = cor_thresh,
        contributions = cor_contrib_per_obs_df
        )
    } else {
      out <- list(
        md_thresh = md_thresh,
        md_scores = 
          list(all = md_scores_all,
               outliers = NULL),
        cor_thresh = cor_thresh,
        contributions = NULL
        )
    }
    
    return(out)
  }


#' @title Make distance and variable contribution plots
#'
#' @description Makes (1) a plot of sqrt Mahalanobis distance vs sqrt robust
#'   distance and (2) a plot of variable contributions to distance values
#'
#' @param outlier_stats The output of `get_outlier_stats()`
#' @param mcd_corrmax The output of `ours::generalized_corrmax()`. Should be
#'   computed using the same data that was used to make `outlier_stats`.
#' @param data The original data frame fed to `ours`
#'
#' @return A list containing two ggplot2 objects. `$md_plot` contains the sqrt MD
#'   vs sqrt RD plot, and `$corr_plot` contains the variable contributions plot
#'
#' @examples
#' \dontrun{
#' }
#'
#' @author N. Chan
#' @export
#' 

plot_outliers <- function(outlier_stats,
                          mcd_corrmax,
                          data) {
  
  md_plot_data <- as.data.frame(outlier_stats$md_scores$all)
  md_plot_outliers <- as.data.frame(outlier_stats$md_scores$outliers)
  md_plot_thresh <- outlier_stats$md_thresh
  cor_thresh <- outlier_stats$cor_thresh
  
  md_plot <-
    ggplot(md_plot_data, aes(x = sqrt_stand_md, y = sqrt_rob_md)) + 
    geom_point() +
    geom_hline(aes(yintercept = md_plot_thresh), linetype = 2, color = "red") +
    geom_vline(aes(xintercept = md_plot_thresh), linetype = 2, color = "red")
  
  if (!(is.null(md_plot_outliers))) {
    md_plot_outliers$label <- rownames(md_plot_outliers)
    md_plot <- md_plot +
      geom_point(data = md_plot_outliers, aes(x = sqrt_stand_md, y = sqrt_rob_md), color = "red") +
      geom_text(data = md_plot_outliers, aes(x = sqrt_stand_md, y = sqrt_rob_md, label = label), color = "red", nudge_x = 0.075, nudge_y = 0.15, angle = 45)
    
    corr_percent_contrib <- mcd_corrmax$percentage_contributions
    rownames(corr_percent_contrib) <- rownames(data)
    corr_plot_outliers_data <- get_varwise_sum(corr_percent_contrib, data = data)
    corr_plot_outliers_data <-
      corr_plot_outliers_data[which(corr_plot_outliers_data$subject %in% rownames(md_plot_outliers)), ]
    corr_plot_outliers <-
      ggplot(corr_plot_outliers_data, aes(x = subject, y = varname, fill = contribution)) +
      geom_tile() +
      scale_fill_continuous(type = "viridis", name = "percentage_contribution") + theme_classic()

  } else {
    corr_plot_outliers <- NULL
  }
  
  md_plot <- md_plot + theme_classic()
  
  cor_res <- mcd_corrmax$contributions
  rownames(cor_res) <- rownames(data)
  corr_plot_data <- get_varwise_sum(cor_res, data = data)

  corr_plot <-
    ggplot(corr_plot_data, aes(x = subject, y = varname, fill = contribution)) + 
    geom_tile() +
    scale_fill_continuous(type = "viridis") + theme_classic()
  
  return(list(
    md_plot = md_plot,
    corr_plot = corr_plot, 
    corr_plot_outliers = corr_plot_outliers
  ))
  
}




#' @title Get variable-wise values by observation
#'
#' @description Recombines values across columns according to some "basename"
#'
#' @param mat A matrix of numerical data with disjunctive data (e.g., columns
#'   are named "var1+", "var1-", "var2.A", "var2.B", etc.). Usually the output
#'   of data transformed by `ours::disjunctive_coding()`,
#'   `ours::escofier_coding()`, or `ours::thermometer_coding()`.
#' @param data The original data frame fed to functions in `ours`.
#'
#' @return A matrix with the same number of rows as `data`. Columns contain the
#'   row-wise sums of their respective variables in `data` according to the
#'   unique "basename".
#'
#' @examples
#' \dontrun{
#' }
#'
#' @author N. Chan
#' @export
#' 

get_varwise_sum <- function(mat,
                            data) {
  cor_res <- mat
  
  cor_contrib_per_obs <-
    apply(cor_res,
          1,
          function (x) {
            out <- x
            names(out) <-
              varname_from_disjunc(names(out))
            out <-
              tapply(out, names(out), sum)
            out <- sort(out, decreasing = T)
            out <- data.frame(
              subject = rep_len(NA, length(out)),
              varname = names(out),
              contribution = out
            )
            return(out)
          },
          simplify = F)
  
  cor_contrib_per_obs_df <- do.call("rbind", cor_contrib_per_obs)
  cor_contrib_per_obs_df$subject <-
    rep(names(cor_contrib_per_obs), unlist(lapply(cor_contrib_per_obs, nrow)))
  rownames(cor_contrib_per_obs_df) <- NULL
  
  
  cor_contrib_per_obs_df$value <-
    apply(cor_contrib_per_obs_df, 1,
          function(x) {
            as.character(data[x[["subject"]],
                              x[["varname"]]])
          }, simplify = T)
  
  return(cor_contrib_per_obs_df)
}
