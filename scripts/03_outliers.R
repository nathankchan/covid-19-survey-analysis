# File name: 03_outliers.R
# Path: './scripts/03_outliers.R'

# Author: NK Chan
# Purpose: Identifies outliers present in COVID-19 Survey Data

source(paste0(getwd(), "/scripts/02_cleandata.R"))

# Start with outlier analysis to identify anomalous data. Compute Mahalanobis
# distances (MD) and robust distances (RD), then identify the variables that
# contribute the most to anomalous observations.

# Outlier analysis is an art. It is up to the user to determine if
# outliers identified in this analysis are "true" outliers (i.e., the data is
# real despite being statistically anomalous). By default, outliers are assumed
# to be any observations with Mahalanobis distance or robust distance greater
# than the "critical threshold". To aid with identifying problematic variables,
# the greatest contributors to the respective observation's MD are also listed.

# The critical threshold follows a $\chi^2_k$ distribution, where $k$ is the
# number of variables (i.e., columns) in the dataset. We can exploit this
# property and enable automated detection of outliers by testing MD and RD
# values against a critical threshold. In practice, this threshold is usually
# set to $\chi^2_{k, 0.975}$. Cases that exceed this threshold will be
# identified as containing outliers.

# Once outliers are identified, the user may choose to either (1) replace the
# specific outlier with another value (e.g., imputation to mean or by MICE), (2)
# perform case-wise exclusion of outliers (remove all other observations in the
# row), or (3) recode problematic variables (e.g., transform the data to look
# approximately normal, remove rarely used categories, etc.).


# It looks like tibbles do not play well with OuRS; specifically,
# mixed_data_mcd() doesn't know how to handle ordinal data unless data are
# encoded as numerics. However, R stores ordinals and factors as characters by
# default, so we have to work around this. To this end, it's probably quicker to
# just call the functions that mixed_data_mcd() uses to prepare the data.

# # SKIP THIS SECTION
# # mixed_data_mcd() requires that we pre-specify the data types
# # contained in DATA. "n" for categorical, "c" for continuous (centered), "z" for
# # continuous (scaled & centered), "o" for ordinal, and "x" for nothing.
# 
# # Note that this MCD approach requires data to be scaled to make valid
# # inferences across different variables/data types (i.e., use "z" not "c" for
# # continuous data)
# 
# # mcd_colcodes <- vector(mode = "character", length = ncol(coviddata_imp))
# # mcd_colcodes[names(coviddata_imp) %in% vars_imp$cat] <- "n"
# # mcd_colcodes[names(coviddata_imp) %in% vars_imp$con] <- "z"
# # mcd_colcodes[names(coviddata_imp) %in% vars_imp$ord] <- "o"
# # END SKIP

# Code below for testing purposes (analysis with a more reasonably-sized dataset)

# mydata <- iris
# mydata[1:3, "Species"] <- NA
# mydata$Species <- addNA(mydata$Species)
# levels(mydata$Species)[4] <- "beige"
# mcd_data <- cbind.data.frame(
#   escofier_coding(mydata[,1:4]),
#   disjunctive_coding(as.data.frame(mydata$Species))
# )

# mydata <- mpg
# mcd_data <- cbind.data.frame(
#   disjunctive_coding(mydata[, c(1:2, 4, 6:7, 10:11)]),
#   thermometer_coding(mydata[, c(5)]),
#   escofier_coding(mydata[, c(3, 8:9)])
# )

# mydata <- mtcars
# mcd_data <- cbind.data.frame(
#   disjunctive_coding(mydata[, c(8:9)]),
#   thermometer_coding(mydata[, c(2, 10:11)]),
#   escofier_coding(mydata[, c(1,3:7)])
# )

# set.seed(1)
# mcd_results <- mixed_data_mcd(mcd_data)
# mcd_corrmax <-
#   generalized_corrmax(as.matrix(mcd_data),
#                       mcd_results$cov$loadings,
#                       mcd_results$cov$singular.values)
# outlier_stats <- get_outlier_stats(
#   mcd_results = mcd_results,
#   mcd_corrmax = mcd_corrmax,
#   data = mydata,
#   md_thresh = NULL,
#   cor_thresh = 0.1
# )
# outlier_plots <- plot_outliers(
#   outlier_stats = outlier_stats,
#   mcd_corrmax = mcd_corrmax,
#   data = mydata)


# Prep data for MCD
mcd_data <- bind_cols(
  disjunctive_coding(coviddata_imp[names(coviddata_imp) %in% vars_imp$cat]),
  escofier_coding(coviddata_imp[names(coviddata_imp) %in% vars_imp$con]),
  thermometer_coding(apply(coviddata_imp[names(coviddata_imp) %in% vars_imp$ord], 2, as.numeric))
)

if (file.exists(paste0(getwd(), "/output/mcd_results.RDS"))) {
  mcd_results <-
    readRDS(file = paste0(getwd(), "/output/mcd_results.RDS"))
  message("mcd_results read in from ./output/mcd_results.RDS")
} else {
  set.seed(1)
  mcd_results <- mixed_data_mcd(DATA = mcd_data)
  saveRDS(object = mcd_results,
          file = paste0(getwd(), "/output/mcd_results.RDS"))
  message("mcd_results saved to ./output/mcd_results.RDS")
}

if (file.exists(paste0(getwd(), "/output/mcd_corrmax.RDS"))) {
  mcd_corrmax <-
    readRDS(file = paste0(getwd(), "/output/mcd_corrmax.RDS"))
  message("mcd_corrmax read in from ./output/mcd_corrmax.RDS")
} else {
  mcd_corrmax <-
    generalized_corrmax(as.matrix(mcd_data),
                        mcd_results$cov$loadings,
                        mcd_results$cov$singular.values)
  saveRDS(object = mcd_corrmax,
          file = paste0(getwd(), "/output/mcd_corrmax.RDS"))
  message("mcd_corrmax saved to ./output/mcd_corrmax.RDS")
}

# if (file.exists(paste0(getwd(), "/output/outlier_stats.RDS"))) {
#   outlier_stats <-
#     readRDS(file = paste0(getwd(), "/output/outlier_stats.RDS"))
#   message("outlier_stats read in from ./output/outlier_stats.RDS")
# } else {
  outlier_stats <-
    get_outlier_stats(
      mcd_results = mcd_results,
      mcd_corrmax = mcd_corrmax,
      data = coviddata_imp,
      md_thresh = NULL,
      cor_thresh = 10
    )
  saveRDS(object = outlier_stats,
          file = paste0(getwd(), "/output/outlier_stats.RDS"))
  message("outlier_stats saved to ./output/outlier_stats.RDS")
# }

# 
# if (file.exists(paste0(getwd(), "/output/outlier_plots.RDS"))) {
#   outlier_plots <-
#     readRDS(file = paste0(getwd(), "/output/outlier_plots.RDS"))
#   message("outlier_plots read in from ./output/outlier_plots.RDS")
# } else {
  outlier_plots <- plot_outliers(outlier_stats = outlier_stats,
                                 mcd_corrmax = mcd_corrmax,
                                 data = coviddata_imp)
  saveRDS(object = outlier_plots,
          file = paste0(getwd(), "/output/outlier_plots.RDS"))
  message("outlier_plots saved to ./output/outlier_plots.RDS")
# }

ggsave(outlier_plots$md_plot, filename = "output/plot_md.png")
ggsave(outlier_plots$corr_plot, filename = "output/plot_corr.png")
if (!(is.null(outlier_plots$corr_plot_outliers))) {
  ggsave(outlier_plots$corr_plot_outliers, filename = "output/plot_corr_outliers.png")
}

message("./scripts/03_outliers.R was executed")
