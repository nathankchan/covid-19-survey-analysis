# File name: 03_outliers.R
# Path: './scripts/03_outliers.R'

# Author: NK Chan
# Purpose: Identifies outliers present in COVID-19 Survey Data

source(paste0(getwd(), "/scripts/02_cleandata.R"))

# Start with outlier analysis to identify anomalous data. Use OuRS to compute
# Mahalanobis distances (MD), then identify the variables that contribute the
# most to anomalous observations.

# Outlier analysis is a bit of an art. It is up to the user to determine if
# outliers identified in this analysis are "true" outliers (i.e., the data is
# real despite being statistically anomalous). By default, outliers are assumed
# to be any observations with Mahalanobis distance > 0.75 (robust or standard).
# To aid with identifying problematic variables, the greatest contributors to
# the respective observation's MD are also listed.

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


# SKIP THIS SECTION
# mixed_data_mcd() requires that we pre-specify the data types
# contained in DATA. "n" for categorical, "c" for continuous (centered), "z" for
# continuous (scaled & centered), "o" for ordinal, and "x" for nothing.

# Note that this MCD approach requires data to be scaled to make valid
# inferences across different variables/data types (i.e., use "z" not "c" for
# continuous data)

# mcd_colcodes <- vector(mode = "character", length = ncol(coviddata_imp))
# mcd_colcodes[names(coviddata_imp) %in% vars_imp$cat] <- "n"
# mcd_colcodes[names(coviddata_imp) %in% vars_imp$con] <- "z"
# mcd_colcodes[names(coviddata_imp) %in% vars_imp$ord] <- "o"
# END SKIP


# Prep data for mcd
mcd_data <- bind_cols(
  disjunctive_coding(coviddata_imp[names(coviddata_imp) %in% vars_imp$cat]),
  escofier_coding(coviddata_imp[names(coviddata_imp) %in% vars_imp$con]),
  thermometer_coding(apply(coviddata_imp[names(coviddata_imp) %in% vars_imp$ord], 2, as.numeric))
)

if (file.exists(paste0(getwd(), "/output/mcd_results.RDS"))) {
  mcd_results <- readRDS(file = paste0(getwd(), "/output/mcd_results.RDS"))
} else {
  mcd_results <- mixed_data_mcd(DATA = mcd_data)
  saveRDS(object = mcd_results, 
          file = paste0(getwd(), "/output/mcd_results.RDS"))
}

if (file.exists(paste0(getwd(), "/output/mcd_corrmax.RDS"))) {
  mcd_corrmax <- readRDS(file = paste0(getwd(), "/output/mcd_corrmax.RDS"))
} else {
  mcd_corrmax <- generalized_corrmax(as.matrix(mcd_data), mcd_results$cov$loadings, mcd_results$cov$singular.values)
  saveRDS(object = mcd_corrmax, 
        file = paste0(getwd(), "/output/mcd_corrmax.RDS"))
}


# Code below used for testing purposes (i.e., analysis with a more reasonably-sized dataset)

# mydata <- iris
# mydata[1:3, "Species"] <- NA
# mydata$Species <- addNA(mydata$Species)
# levels(mydata$Species)[4] <- "beige"
# 
# mcd_data <- cbind.data.frame(
#   escofier_coding(mydata[,1:4]),
#   disjunctive_coding(as.data.frame(mydata$Species))
# )
# 
# mcd_results <- mixed_data_mcd(mcd_data)
# 
# mcd_corrmax <- generalized_corrmax(as.matrix(mcd_data), mcd_results$cov$loadings, mcd_results$cov$singular.values)



# 1) Get the robust md and md from mcd_results$dists, take sqrt, then set cut
# off to 0.75
# 2) For these observations, get the contributions from mcd_corrmax, then sort
# by contribution and set cut off to 10

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

get_outlier_stats <-
  function(mcd_results,
           mcd_corrmax,
           md_thresh = 0.75,
           cor_thresh = 10) {
    mcd_res <- mcd_results
    rob_md <- mcd_res$dists$robust_mahal_dists
    stand_md <- mcd_res$dists$mahal_dists
    md_scores <- sqrt(cbind(stand_md, rob_md))
    md_scores_outliers <-
      md_scores[which(md_scores[, 1] > md_thresh |
                        md_scores[, 2] > md_thresh), , drop = F]
    md_scores_outliers <-
      md_scores_outliers[order(md_scores_outliers[, 2], md_scores_outliers[, 1], decreasing = T), , drop = F]
    
    cor_res <- mcd_corrmax$contributions
    cor_res <-
      cor_res[as.numeric(rownames(md_scores_outliers)), , drop = F]
    rownames(cor_res) <- rownames(md_scores_outliers)
    
    cor_contrib_per_obs <- apply(cor_res,
                                 1,
                                 function (x) {
                                   out <- sort(x, decreasing = T)
                                   names(out) <- varname_from_disjunc(names(out))
                                   out <- tapply(out, names(out), sum)
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
    cor_contrib_per_obs_df <-
      cor_contrib_per_obs_df[which(cor_contrib_per_obs_df$contribution > cor_thresh), ]
    
    out <- list(md_scores_outliers = md_scores_outliers,
                contributions = cor_contrib_per_obs_df)
    
    return(out)
  }



if (file.exists(paste0(getwd(), "/output/outlier_stats.RDS"))) {
  outlier_stats <- readRDS(file = paste0(getwd(), "/output/outlier_stats.RDS"))
} else {
  outlier_stats <-
    get_outlier_stats(mcd_results = mcd_results,
                      mcd_corrmax = mcd_corrmax,
                      cor_thresh = 1)
  saveRDS(object = outlier_stats, 
          file = paste0(getwd(), "/output/outlier_stats.RDS"))
}

# Make diagnostic plots for outlier analysis

# md_plot_data <-
#   data.frame(sqrt(
#     cbind(
#       sqrt_stand_md = mcd_results$dists$mahal_dists,
#       sqrt_rob_md = mcd_results$dists$robust_mahal_dists
#     )
#   ))
# md_plot <-
#   ggplot(md_plot_data, aes(x = sqrt_stand_md, y = sqrt_rob_md)) + geom_point()
# # ggsave(md_plot, filename = "output/md_plot.png")
# 
# corr_plot_data <-
#   data.frame(mcd_corrmax$percentage_contributions) %>%
#   gather(key = "varname",
#          value = "value",
#          factor_key = F) %>%
#   cbind.data.frame(., index = rep(seq(
#     1, nrow(mcd_corrmax$percentage_contributions)
#   ), ncol(mcd_corrmax$percentage_contributions)))
# corr_plot <-
#   ggplot(corr_plot_data, aes(x = index, y = varname, fill = value)) + geom_tile()
# # ggsave(corr_plot, filename = "output/corr_plot.png")
# 
# rm(md_plot_data,
#    corr_plot_data)

message("./scripts/03_analyze.R was executed")

