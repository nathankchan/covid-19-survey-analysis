# File name: 04_analyze.R
# Path: './scripts/04_analyze.R'

# Author: NK Chan
# Purpose: Performs inferential statistics on COVID-19 Survey Data

source(paste0(getwd(), "/scripts/03_outliers.R"))

# We're lucky that this dataset doesn't have any extremely obvious outliers.
# However, the outlier analysis does pick up on a couple of "rare" categories
# (e.g., 31/8070 people read magazines as their source of health information
# (CVSQ1)).

# These observations are probably not a problem! We can leave them alone. 

# For this analysis, we will use "generalized partial least squares" (GPLS) to
# perform a type of multiple correspondence analysis (MCA). This will enable us
# to investigate associations between groups of variables encoded as response
# and predictor matrices, albeit in component space. Thus, to interpret results
# from the analysis, we will need to examine the contribution that each response
# variable makes to the respective component, then generate an interpretation
# for what the component may represent.



### HERE THERE BE DRAGONS ###
### !YE HAVE BEEN WARNED! ###

### Code below was copied from an old project.
### It is unfortunately in a useable state... sigh.
### Needs some heavy refactoring to make it readable.


var.from.disjunc.names <- function(x) {
  varnames <- str_replace(rownames(x),
                          "(\\.[\\s\\S]{1,})|(\\+$)|(\\-$)",
                          "")
  return(varnames)
}

# add.var.labels <- function(x, data = coviddata_imp) {
#   # x <- varname_from_disjunc(x)
#   var.labels <- ifelse(test = is.null(attr(data[,x], "label")),
#                        yes = "NA",
#                        no = attr(data[,x], "label"))
#   
#   value.labels <- attr(data[,x], "labels")
#   value.strings <- ifelse(test = is.null(value.labels),
#                           yes = "NA", 
#                           no = paste(value.labels, names(value.labels), collapse = ", "))
#   
#   # if (any(value.strings == "")) {
#   #   value.strings <- c("NA")
#   # } 
#   
#   return(c(var.labels, value.strings))
# }

add.var.labels <- function(x, datadict = datadict_imp) {
  var.labels <- ifelse(test = is.null(datadict[[x]]$Question),
                       yes = "NA",
                       no = datadict[[x]]$Question)
  
  value.labels <- datadict[[x]]$Values
  value.strings <- ifelse(test = is.null(value.labels),
                          yes = "NA", 
                          no = paste(value.labels, names(value.labels), collapse = "; "))

  return(c(var.labels, value.strings))
}
V.add.var.labels <- Vectorize(add.var.labels, SIMPLIFY = F)

varwise.contrib <- function(x) {
  pred.factorscores <- x$TExPosition.Data$fi
  pred.contrib <- x$TExPosition.Data$ci
  resp.factorscores <- x$TExPosition.Data$fj
  resp.contrib <- x$TExPosition.Data$cj
  comp.contrib <- x$TExPosition.Data$t

  if (length(comp.contrib) == 1) {
    pred.compwise.contrib <- pred.contrib * comp.contrib
    resp.compwise.contrib <- resp.contrib * comp.contrib
    pred.varwise.contrib <-
      as.data.frame(tapply(
        pred.compwise.contrib[, 1],
        var.from.disjunc.names(pred.compwise.contrib),
        sum
      ))
    colnames(pred.varwise.contrib) <- "Pred.Contrib.Compwise1"
    pred.varwise.contrib$Pred.Varwise.Threshold <- 
      table(var.from.disjunc.names(pred.contrib[,1,drop=F])) / sum(table(var.from.disjunc.names(pred.contrib[,1,drop=F]))) * 100
    pred.varwise.contrib.thresholdexceeded <- as.data.frame(pred.varwise.contrib$Pred.Contrib.Compwise1 >= pred.varwise.contrib$Pred.Varwise.Threshold)
    colnames(pred.varwise.contrib.thresholdexceeded) <- "Pred.Threshold.Exceeded.Comp1"
    
    pred.varwise.contrib.labels <- do.call(rbind, V.add.var.labels(rownames(pred.varwise.contrib)))
    colnames(pred.varwise.contrib.labels) <- c("Pred.Var.Label", "Pred.Value.Labels")
    pred.varwise.contrib <- cbind.data.frame(pred.varwise.contrib,
                                             pred.varwise.contrib.thresholdexceeded,
                                             pred.varwise.contrib.labels)
    
    resp.varwise.contrib <-
      as.data.frame(tapply(
        resp.compwise.contrib[, 1],
        var.from.disjunc.names(resp.compwise.contrib),
        sum
      ))
    colnames(resp.varwise.contrib) <- "Resp.Contrib.Compwise1"
    resp.varwise.contrib$Resp.Varwise.Threshold <- 
      table(var.from.disjunc.names(resp.contrib[,1,drop=F])) / sum(table(var.from.disjunc.names(resp.contrib[,1,drop=F]))) * 100
    resp.varwise.contrib.thresholdexceeded <- as.data.frame(resp.varwise.contrib$Resp.Contrib.Compwise1 >= resp.varwise.contrib$Resp.Varwise.Threshold)
    colnames(resp.varwise.contrib.thresholdexceeded) <- "Resp.Threshold.Exceeded.Comp1"
    
    resp.varwise.contrib.labels <- do.call(rbind, V.add.var.labels(rownames(resp.varwise.contrib)))
    colnames(resp.varwise.contrib.labels) <- c("Resp.Var.Label", "Resp.Value.Labels")
    resp.varwise.contrib <- cbind.data.frame(resp.varwise.contrib,
                                             resp.varwise.contrib.thresholdexceeded,
                                             resp.varwise.contrib.labels)
    
    pred.compwise <- cbind.data.frame(pred.factorscores[, 1],
                                      pred.contrib[, 1] * 100,
                                      Pred.Cosines = x$TExPosition.Data$ri)
    colnames(pred.compwise) <-
      c("Pred.FactorScore.Comp1", "Pred.Contrib.to.Comp1")
    resp.compwise <- cbind.data.frame(resp.factorscores[, 1],
                                      resp.contrib[, 1] * 100,
                                      Resp.Cosines = TExPosition.Data$rj)
    colnames(resp.compwise) <-
      c("Resp.FactorScore.Comp1", "Resp.Contrib.to.Comp1")
  }
  
  if (length(comp.contrib) > 1) {
    pred.compwise.contrib <- pred.contrib %*% diag(comp.contrib)
    resp.compwise.contrib <- resp.contrib %*% diag(comp.contrib)
    
    # browser()
    
    pred.varwise.contrib.percomp <-
      as.data.frame(apply(
        pred.contrib,
        2,
        FUN = function(x) {
          return(tapply(x, var.from.disjunc.names(pred.compwise.contrib), sum))
        }
      ))
    colnames(pred.varwise.contrib.percomp) <-
      c(paste(
        "Pred.Contrib.Per.Comp",
        1:ncol(pred.varwise.contrib.percomp),
        sep = ""
      ))
    pred.varwise.contrib <-
      as.data.frame(apply(
        pred.compwise.contrib,
        2,
        FUN = function(x) {
          return(tapply(x, var.from.disjunc.names(pred.compwise.contrib), sum))
        }
      ))
    colnames(pred.varwise.contrib) <-
      c(paste(
        "Pred.Contrib.Compwise",
        1:ncol(pred.varwise.contrib),
        sep = ""
      ))
    pred.varwise.contrib$Pred.Varwise.Contrib.Total <- apply(pred.varwise.contrib, 1, sum)
    pred.varwise.contrib$Pred.Varwise.Threshold <- 
      table(var.from.disjunc.names(pred.contrib)) / sum(table(var.from.disjunc.names(pred.contrib))) * 100
    pred.varwise.contrib.thresholdexceeded <- pred.varwise.contrib.percomp * 100 >= pred.varwise.contrib$Pred.Varwise.Threshold
    colnames(pred.varwise.contrib.thresholdexceeded) <- c(paste("Pred.Threshold.Exceeded.Comp",
                                                                1:ncol(pred.varwise.contrib.percomp),
                                                                sep = ""))
    
    # browser()
    
    pred.varwise.contrib.labels <- do.call(rbind, V.add.var.labels(rownames(pred.varwise.contrib)))
    colnames(pred.varwise.contrib.labels) <- c("Pred.Var.Label", "Pred.Value.Labels")
    
    pred.varwise.contrib <-
      cbind.data.frame(pred.varwise.contrib.percomp * 100, 
                       pred.varwise.contrib,
                       pred.varwise.contrib.thresholdexceeded,
                       pred.varwise.contrib.labels)
    
    resp.varwise.contrib.percomp <-
      as.data.frame(apply(
        resp.contrib,
        2,
        FUN = function(x) {
          return(tapply(x, var.from.disjunc.names(resp.compwise.contrib), sum))
        }
      ))
    colnames(resp.varwise.contrib.percomp) <-
      c(paste(
        "Resp.Contrib.Per.Comp",
        1:ncol(resp.varwise.contrib.percomp),
        sep = ""
      ))
    resp.varwise.contrib <-
      as.data.frame(apply(
        resp.compwise.contrib,
        2,
        FUN = function(x) {
          return(tapply(x, var.from.disjunc.names(resp.compwise.contrib), sum))
        }
      ))
    colnames(resp.varwise.contrib) <-
      c(paste(
        "Resp.Contrib.Compwise",
        1:ncol(resp.varwise.contrib),
        sep = ""
      ))
    resp.varwise.contrib$Resp.Varwise.Contrib.Total <- apply(resp.varwise.contrib, 1, sum)
    resp.varwise.contrib$Resp.Varwise.Threshold <- 
      table(var.from.disjunc.names(resp.contrib)) / sum(table(var.from.disjunc.names(resp.contrib))) * 100
    resp.varwise.contrib.thresholdexceeded <- resp.varwise.contrib.percomp * 100 >= resp.varwise.contrib$Resp.Varwise.Threshold
    colnames(resp.varwise.contrib.thresholdexceeded) <- c(paste("Resp.Threshold.Exceeded.Comp",
                                                                1:ncol(resp.varwise.contrib.percomp),
                                                                sep = ""))
    
    resp.varwise.contrib.labels <- do.call(rbind, V.add.var.labels(rownames(resp.varwise.contrib)))
    colnames(resp.varwise.contrib.labels) <- c("Resp.Var.Label", "Resp.Value.Labels")
    
    resp.varwise.contrib <-
      cbind.data.frame(resp.varwise.contrib.percomp * 100, 
                       resp.varwise.contrib,
                       resp.varwise.contrib.thresholdexceeded,
                       resp.varwise.contrib.labels)
    
    
    pred.compwise <- cbind.data.frame(
      pred.factorscores,
      pred.contrib * 100,
      pred.compwise.contrib,
      apply(pred.compwise.contrib, 1, sum),
      x$TExPosition.Data$ri
    )
    colnames(pred.compwise) <- c(
      paste("Pred.FactorScore.Comp", 1:ncol(pred.factorscores), sep = ""),
      paste("Pred.Contrib.to.Comp", 1:ncol(pred.contrib), sep = ""),
      paste(
        "Pred.Compwise.Contrib",
        1:ncol(pred.compwise.contrib),
        sep = ""
      ),
      "Pred.Compwise.Contrib.Total",
      paste("Pred.Cosine.Comp", 1:ncol(pred.contrib), sep = "")
    )
    resp.compwise <- cbind.data.frame(
      resp.factorscores,
      resp.contrib * 100,
      resp.compwise.contrib,
      apply(resp.compwise.contrib, 1, sum),
      x$TExPosition.Data$rj
    )
    colnames(resp.compwise) <- c(
      paste("Resp.FactorScore.Comp", 1:ncol(resp.factorscores), sep = ""),
      paste("Resp.Contrib.to.Comp", 1:ncol(resp.contrib), sep = ""),
      paste(
        "Resp.Compwise.Contrib",
        1:ncol(resp.compwise.contrib),
        sep = ""
      ),
      "Resp.Compwise.Contrib.Total",
      paste("Resp.Cosine.Comp", 1:ncol(pred.contrib), sep = "")
    )
  }
  
  result <- list(
    pred.compwise = pred.compwise,
    resp.compwise = resp.compwise,
    comp.variance.contrib = comp.contrib,
    pred.varwise.contrib = pred.varwise.contrib,
    resp.varwise.contrib = resp.varwise.contrib
  )
  return(result)
}

plot.var.direction <- function(x, up = "triangle filled", down = "triangle down filled", other = "diamond filled") {
  if(str_ends(x, "\\+")) {
    out <- up
  } else if (str_ends(x, "\\-")) {
    out <- down 
  } else {
    out <- other
  }
  return(out)
}
V.plot.var.direction <- Vectorize(plot.var.direction, SIMPLIFY = F)

plot.PLSCA.Comp1only <- function(x, cutoff = 5, usethreshold = T, title = "") {
  pred.compwise <- x$pred.compwise
  pred.compwise <- cbind.data.frame(varnames = var.from.disjunc.names(pred.compwise),
                                    varnames.bifactor = rownames(pred.compwise),
                                    FactorScore.Comp1 = pred.compwise$Pred.FactorScore.Comp1,
                                    Contrib.to.Comp1 = pred.compwise$Pred.Contrib.to.Comp1)
  
  pred.varwise.contrib <- x$pred.varwise.contrib
  if(usethreshold == T) {
    pred.varwise.contrib$Pred.Contrib.Compwise1.GT.Cutoff <- pred.varwise.contrib$Pred.Threshold.Exceeded.Comp1
  } else {
    pred.varwise.contrib$Pred.Contrib.Compwise1.GT.Cutoff <- 
      pred.varwise.contrib$Pred.Contrib.Compwise1 > cutoff
  }
  
  plot.pred.compwise <- pred.compwise[which(
    pred.compwise$varnames %in%
      rownames(pred.varwise.contrib[which(pred.varwise.contrib$Pred.Contrib.Compwise1.GT.Cutoff == T),])),]
  
  resp.compwise <- x$resp.compwise
  resp.compwise <- cbind.data.frame(varnames = var.from.disjunc.names(resp.compwise),
                                    varnames.bifactor = rownames(resp.compwise),
                                    FactorScore.Comp1 = resp.compwise$Resp.FactorScore.Comp1,
                                    Contrib.to.Comp1 = resp.compwise$Resp.Contrib.to.Comp1)
  
  plot.pred.compwise$varnames.sortbytotalcontrib <- 
    factor(plot.pred.compwise$varnames,
           levels = c(levels(resp.compwise$varnames), 
                      rownames(pred.varwise.contrib)[order(
                        pred.varwise.contrib$Pred.Contrib.Compwise1, 
                        decreasing = T)]))
  
  resp.compwise$varnames.sortbytotalcontrib <-
    factor(resp.compwise$varnames,
           levels = levels(plot.pred.compwise$varnames.sortbytotalcontrib))
  
  plot.data <- rbind.data.frame(plot.pred.compwise, resp.compwise)
  plot.data.labels <- do.call(rbind, V.add.var.labels(as.character(plot.data$varnames.sortbytotalcontrib)))
  colnames(plot.data.labels) <- c("Var.Label", "Value.Labels")
  plot.data <- cbind.data.frame(plot.data, plot.data.labels)
  
  
  plot.ggplot <-
    ggplot() + geom_vline(xintercept = 0) +
    geom_point(data = plot.data,
               aes(x = FactorScore.Comp1,
                   y = varnames.sortbytotalcontrib,
                   label = varnames.bifactor,
                   label1 = Var.Label,
                   label2 = Value.Labels,
                   color = varnames,
                   size = Contrib.to.Comp1)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          legend.position = "none") +
    labs(title = title,
         y = "Variables")
  
  plot.colors.levels <- levels(droplevels(plot.data$varnames.sortbytotalcontrib))
  plot.colors <- rainbow_hcl(length(plot.colors.levels))
  names(plot.colors) <- plot.colors.levels
  plot.colors[which(names(plot.colors) %in% levels(droplevels(resp.compwise$varnames.sortbytotalcontrib)))] <- "#000000"
  plot.colors.scale <- scale_color_manual(name = "varnames.sortbytotalcontrib", values = plot.colors)
  
  plot.ggplot <- plot.ggplot + plot.colors.scale
  
  return(plot.ggplot)
}

plot.PLSCA.Comp1and2 <- function(x, cutoff = 5, usethreshold = T, topNpred = NULL, title = "", 
                                 xlab = paste0("Component 1: ", substr(as.character(x$comp.variance.contrib[1]), 1, 5), "% of total variance"), 
                                 ylab = paste0("Component 2: ", substr(as.character(x$comp.variance.contrib[2]), 1, 5), "% of total variance")) {
  pred.compwise <- x$pred.compwise
  pred.compwise <- cbind.data.frame(varnames = var.from.disjunc.names(pred.compwise),
                                    varnames.bifactor = rownames(pred.compwise),
                                    FactorScore.Comp1 = pred.compwise$Pred.FactorScore.Comp1,
                                    FactorScore.Comp2 = pred.compwise$Pred.FactorScore.Comp2,
                                    Contrib.to.Comp1 = pred.compwise$Pred.Contrib.to.Comp1,
                                    Contrib.to.Comp2 = pred.compwise$Pred.Contrib.to.Comp2,
                                    Compwise.Contrib.Total = pred.compwise$Pred.Compwise.Contrib.Total)
  # pred.compwise.labels <- do.call(rbind, V.add.var.labels(pred.compwise$varnames))
  # colnames(pred.compwise.labels) <- c("Var.Label", "Value.Labels")
  # pred.compwise <- cbind.data.frame(pred.compwise, pred.compwise.labels)
  
  pred.varwise.contrib <- x$pred.varwise.contrib
  
  if(usethreshold == T) {
    pred.varwise.contrib$Pred.Contrib.Per.Comp1or2.GT.Cutoff <-
      pred.varwise.contrib$Pred.Threshold.Exceeded.Comp1 | 
      pred.varwise.contrib$Pred.Threshold.Exceeded.Comp2
  } else {
    pred.varwise.contrib$Pred.Contrib.Per.Comp1or2.GT.Cutoff <- 
      pred.varwise.contrib$Pred.Contrib.Per.Comp1 > cutoff | 
      pred.varwise.contrib$Pred.Contrib.Per.Comp2 > cutoff
  }
  
  # browser()
  
  plot.pred.compwise <- pred.compwise[which(
    pred.compwise$varnames %in%
      rownames(pred.varwise.contrib[which(pred.varwise.contrib$Pred.Contrib.Per.Comp1or2.GT.Cutoff == T),])),]
  
  resp.compwise <- x$resp.compwise
  resp.compwise <- cbind.data.frame(varnames = var.from.disjunc.names(resp.compwise),
                                    varnames.bifactor = rownames(resp.compwise),
                                    FactorScore.Comp1 = resp.compwise$Resp.FactorScore.Comp1,
                                    FactorScore.Comp2 = resp.compwise$Resp.FactorScore.Comp2,
                                    Contrib.to.Comp1 = resp.compwise$Resp.Contrib.to.Comp1,
                                    Contrib.to.Comp2 = resp.compwise$Resp.Contrib.to.Comp2,
                                    Compwise.Contrib.Total = resp.compwise$Resp.Compwise.Contrib.Total)
  # resp.compwise.labels <- do.call(rbind, V.add.var.labels(resp.compwise$varnames))
  # colnames(resp.compwise.labels) <- c("Var.Label", "Value.Labels")
  # resp.compwise <- cbind.data.frame(resp.compwise, resp.compwise.labels)
  
  # plot.resp.compwise <- resp.compwise
  
  resp.varwise.contrib <- x$resp.varwise.contrib
  
  # browser()
  
  
  plot.pred.compwise$varnames.sortbytotalcontrib <- 
    factor(plot.pred.compwise$varnames,
           levels = c(rownames(resp.varwise.contrib)[order(
                        resp.varwise.contrib$Resp.Varwise.Contrib.Total, 
                        decreasing = T)], 
                      rownames(pred.varwise.contrib)[order(
                        pred.varwise.contrib$Pred.Varwise.Contrib.Total, 
                        decreasing = T)]))
  plot.pred.compwise$PredResp <- "Pred"
  
  resp.compwise$varnames.sortbytotalcontrib <-
    factor(resp.compwise$varnames,
           levels = levels(plot.pred.compwise$varnames.sortbytotalcontrib))
  resp.compwise$PredResp <- "Resp"
  
  
  # browser()
  
  
  plot.data <- rbind.data.frame(plot.pred.compwise, resp.compwise)
  
  if(!is.null(topNpred)) {
    plot.data <- 
      plot.data[which(
        plot.data$varnames.sortbytotalcontrib %in% 
          levels(plot.data$varnames.sortbytotalcontrib)[1:(
            length(unique(resp.compwise$varnames)) + topNpred)]), ]
  }
  
  plot.data.labels <- do.call(rbind, V.add.var.labels(as.character(plot.data$varnames.sortbytotalcontrib)))
  colnames(plot.data.labels) <- c("Var.Label", "Value.Labels")
  
  plot.directions <- do.call(rbind, V.plot.var.direction(as.character(plot.data$varnames.bifactor), up = "+", down = "-", other = "0"))
  colnames(plot.directions) <- c("Var.Direction")
  
  plot.data <- cbind.data.frame(plot.data, plot.data.labels, plot.directions)
  
  
  # browser()
  
  
  plot.ggplot <- 
    ggplot() + 
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
    geom_point(data = plot.data,
               aes(x = FactorScore.Comp1, 
                   y = FactorScore.Comp2,
                   fill = varnames.sortbytotalcontrib,
                   color = varnames.sortbytotalcontrib,
                   size = Compwise.Contrib.Total,
                   shape = Var.Direction,
                   label = varnames.bifactor,
                   label1 = Var.Label,
                   label2 = Value.Labels)) +
    geom_point(data = subset(plot.data, PredResp == "Pred"),
               aes(x = FactorScore.Comp1,
                   y = FactorScore.Comp2,
                   fill = varnames.sortbytotalcontrib,
                   color = varnames.sortbytotalcontrib,
                   size = Compwise.Contrib.Total,
                   shape = Var.Direction,
                   label = varnames.bifactor,
                   label1 = Var.Label,
                   label2 = Value.Labels)) +
    geom_point(data = subset(plot.data, PredResp == "Resp"),
               aes(x = FactorScore.Comp1, 
                   y = FactorScore.Comp2,
                   fill = varnames.sortbytotalcontrib,
                   color = varnames.sortbytotalcontrib,
                   size = Compwise.Contrib.Total,
                   shape = Var.Direction,
                   label = varnames.bifactor,
                   label1 = Var.Label,
                   label2 = Value.Labels)) + coord_fixed(ratio = 1)
  
  # browser()
  
  plot.shape <- c(25, 24, 23)
  names(plot.shape) <- levels(droplevels(as.factor(plot.data$Var.Direction)))
  plot.shape.scale <- scale_shape_manual(values = plot.shape)
  
  plot.colors.levels <- levels(droplevels(as.factor(plot.data$varnames)))
  plot.colors <- qualitative_hcl(length(plot.colors.levels))
  names(plot.colors) <- plot.colors.levels
  
  plot.fill <- plot.colors
  names(plot.colors) <- plot.colors.levels
  
  plot.colors[which(names(plot.colors) %in% levels(droplevels(as.factor(resp.compwise$varnames.sortbytotalcontrib))))] <- "#000000"
  
  plot.fill[which(names(plot.fill) %in% levels(droplevels(as.factor(resp.compwise$varnames.sortbytotalcontrib))))] <-
    sequential_hcl(length(levels(droplevels(as.factor(resp.compwise$varnames.sortbytotalcontrib)))), palette = "Viridis")
  
  plot.colors.scale <- scale_color_manual(values = plot.colors)
  plot.fill.scale <- scale_fill_manual(values = plot.fill)
  
  plot.ggplot <- plot.ggplot + plot.colors.scale + plot.fill.scale + plot.shape.scale + 
    theme_classic() +
    labs(x = xlab,
         y = ylab,
         title = title, 
         subtitle = "Responses (outlined shapes) and Predictors, ordered by decreasing contribution to variance") +
    theme(axis.line = element_blank(),
          legend.position = "right")
  
  plot.out <- ggplotly(plot.ggplot)
  
  # browser()
  
  legendgroup <- vector(mode = "list", length = length(plot.out$x$data))
  for (i in 3:length(plot.out$x$data)) {
    legendtext <- plot.out$x$data[[i]]$legendgroup
    legendgroup[[i]] <- substr(legendtext, 2, nchar(legendtext)-3)
    plot.out$x$data[[i]]$legendgroup <- legendgroup[[i]]
  }
  
  # browser()
  
  legenduniq <- unique(unlist(legendgroup))
  legendshow <- vector(mode = "integer", length = length(legenduniq))
  for (i in 3:length(plot.out$x$data)) {
    if(length(plot.out$x$data[[i]]$legendgroup) == 0) {
      next
    }
    if(legendshow[which(legenduniq %in% plot.out$x$data[[i]]$legendgroup)] == 0) {
      legendshow[which(legenduniq %in% plot.out$x$data[[i]]$legendgroup)] <- 1
      plot.out$x$data[[i]]$showlegend <- TRUE
      plot.out$x$data[[i]]$name <- plot.out$x$data[[i]]$legendgroup
    } else {
      legendshow[which(legenduniq %in% plot.out$x$data[[i]]$legendgroup)] <- 
        legendshow[which(legenduniq %in% plot.out$x$data[[i]]$legendgroup)] + 1
      plot.out$x$data[[i]]$showlegend <- FALSE
    }
  }
  
  # browser()
  
  plot.out$x$data[[1]]$showlegend <- TRUE
  plot.out$x$data[[1]]$name <- "Component 1 axis"
  plot.out$x$data[[2]]$showlegend <- TRUE
  plot.out$x$data[[2]]$name <- "Component 2 axis"
  
  plot.out$x$layout$legend$traceorder <- "grouped"
  
  return(plot.out)
}

run.PLSCA <- function(pred, 
                      resp, 
                      title = NULL, 
                      topNpred = 50, 
                      filename = NULL, 
                      saveplot = F,
                      savexlsx = F) {
  
  result <- tepPLSCA(pred, resp, make_data1_nominal = F, make_data2_nominal = F, graphs = F)
  contrib <- varwise.contrib(result)
  plot.out <- plot.PLSCA.Comp1and2(x = contrib, 
                                   title = title,
                                   topNpred = topNpred)
  
  if(saveplot == T) {
    saveWidget(plot.out, file = paste0(getwd(), "/figures/plot_", filename,".html"))
  }
  
  # if(savexlsx == T) {
  #   export.to.xlsx(contrib, file = paste0(getwd(), "/output/plsca_", filename, ".xlsx"))
  # }
  
  return(list(result = result,
              contrib = contrib,
              plot = plot.out))
  
}

# plsca_res <- tepPLSCA(pred_mat, resp_mat, make_data1_nominal = F, make_data2_nominal = F, graphs = F)
# plsca_contrib <- varwise.contrib(plsca_res)
# plsca_plot <- plot.PLSCA.Comp1and2(plsca_contrib, title = "Test", topNpred = 50)

plsca_vax <- run.PLSCA(
  pred = mcd_data %>% select(starts_with("VAXQr")) %>% as.matrix(),
  resp = mcd_data %>% select(!starts_with("VAXQr")) %>% as.matrix(),
  title = "Vaccine behaviours by item",
  topNpred = 50,
  filename = "vaccine_results",
  saveplot = T,
  savexlsx = T
)


plsca_panas <- run.PLSCA(
  pred = mcd_data %>% select(!starts_with("PANAS")) %>% as.matrix(),
  resp = mcd_data %>% select(starts_with("PANAS")) %>% as.matrix(),
  title = "Positive and Negative Affect Scale (PANAS)",
  topNpred = 50,
  filename = "panas_results",
  saveplot = T,
  savexlsx = T
)



message("./scripts/04_outliers.R was executed")
