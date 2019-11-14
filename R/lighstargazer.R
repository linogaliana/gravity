#' Strip regression objects to reduce memory needs
#' 
#' Remove elements that are more related
#'  to prediction than model to reduce
#'  memory needs to store a glm output
#'  
#' 
#' @param object A stripped regression by
#'  \link{strip} function. 
#'  Accepted classes are \link[stats]{glm} or
#'  \link[pscl]{zeroinfl}
#'  
#' @return A stargazer object
#' 
#' @export

lightstargazer <- function(light_model, ...){
  UseMethod("lightstargazer")
}


lightstargazer.zeroinfl <- function(light_model,
                                modelname = "\\textsc{Low-income}",
                                modellabel = "Outcome",
                                modeltype = c("count","zero")){
  
  if (!inherits(light_model$object, "zeroinfl"))
    stop("light_model should be a zeroinfl object")
  
  modeltype <- match.arg(modeltype)
  
  if (modeltype == "count"){
    coeff_part <- light_model$summary$coefficients$count
  } else{
    coeff_part <- light_model$summary$coefficients$zero
  }
  
  text_coeff <- paste0(round(coeff_part[,'Estimate'],3),
                       sapply(coeff_part[,'Pr(>|z|)'], signif_stars))
  text_sd <- paste0("(",round(coeff_part[,'Std. Error'],3),")")
  text_coeff <- cbind("variable" = rownames(coeff_part),text_coeff, text_sd)
  
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)
  
  label <- c("Constant",
             "Population in home cell (log)",
             "Population in destination cell (log)",
             "Employment in home cell (log)",
             "Employment in destination cell (log)",
             "$p_{j}^{D1}$ in destination cell (tax data)",
             "$p_{j}^{D9}$ in destination cell (tax data)",
             "Distance (suburbs $\\to$ suburbs)",
             "Distance (center $\\to$ suburbs)",
             "Distance (suburbs $\\to$ center)",
             "Distance (center $\\to$ center)"
  )
  
  if (modeltype == "count"){
    label <- c(label,"$\\theta$ (log)")
  }
  
  text_coeff <- cbind('label' = label, text_coeff)
  
  
  template_stargazer2 <- template_stargazer
  lapply(1:length(text_coeff[,'variable']), function(ligne)({
    idx <- which(grepl(pattern = text_coeff[,'variable'][ligne], template_stargazer2, fixed = TRUE))
    if (length(idx)==0){
      print("nothing to change")
    }else{
      template_stargazer2[idx:(idx+1)] <<- c(
        paste0(" ",
               text_coeff[,'label'][ligne],
               " & ",
               text_coeff[,'text_coeff'][ligne],
               " \\\\ "
        ),
        paste0(
          "  & ",
          text_coeff[,'text_sd'][ligne],
          " \\\\ "
        )
      )
    }
  }))
  idx <- which(grepl("depvarname", template_stargazer2))
  template_stargazer2 <- c(
    template_stargazer2[1:(idx-1)],
    gsub("depvarname", modelname, template_stargazer2[idx]),
    gsub("depvarname", modellabel, template_stargazer2[idx]),
    template_stargazer2[(idx+1):length(template_stargazer2)]
  )
  
  # STATS -------
  
  template_stargazer2[grepl("Observations", template_stargazer2)] <- sprintf("Observations & %s \\\\ ",
                                                                             format(light_model$summary$n, big.mark = ","))
  template_stargazer2[grepl("Bayesian information criterion", template_stargazer2)] <- sprintf("Bayesian information criterion & %s \\\\ ",
                                                                                               format(BIC_zeroinfl(light_model$object), big.mark = ","))
  template_stargazer2[grepl("Log Likelihood", template_stargazer2)] <- sprintf("Log Likelihood & %s \\\\ ",
                                                                               format(light_model$summary$loglik, big.mark = ","))
  
  
  # Inverse observation and BIC
  idx <- which(grepl("Observations", template_stargazer2))
  template_stargazer2[c(idx-1,idx)]  <- template_stargazer2[c(idx,idx-1)]
  
  # Add deviation parameter for negative binomial models
  if (modeltype == "count"){
    idx <- which(grepl("Log Likelihood", template_stargazer2))  
    template_stargazer2 <- c(
      template_stargazer2[1:idx],
      sprintf("$\\theta$ & %s \\\\ ", round(light_model$object$theta,3)),
      template_stargazer2[(idx+1):length(template_stargazer2)]
    )
    
  }
  
  return(template_stargazer2)
}


lightstargazer.glm <- function(light_model,
                               modelname = "\\textsc{Low-income}",
                               modellabel = "Outcome",
                               modeltype = c("outcome","selection"),
                               omit = "residential_D1_destinationTRUE"){
  
  if (inherits(light_model$object, "zeroinfl"))
    stop("Use lightstargazer_pscl for zeroinfl object")
  
  modeltype <- match.arg(modeltype)
  
  coeff_part <- light_model$summary$coefficients
  
  if (as.character(light_model$object$call[1]) == "lm"){
    pvaluesvar <- "Pr(>|t|)"
  } else{
    pvaluesvar <- "Pr(>|z|)"
  }
  text_coeff <- paste0(round(coeff_part[,'Estimate'],3),
                       sapply(coeff_part[,pvaluesvar], signif_stars))
  text_sd <- paste0("(",round(coeff_part[,'Std. Error'],3),")")
  text_coeff <- cbind("variable" = rownames(coeff_part),text_coeff, text_sd)
  
  if (!is.null(omit)) text_coeff <- text_coeff[!grepl(omit, text_coeff[,'variable']),]
  
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)
  
  
  label <- c("Constant",
             "Population in home cell (log)",
             "Population in destination cell (log)",
             "Employment in home cell (log)",
             "Employment in destination cell (log)",
             "$p_{j}^{D1}$ in destination cell (tax data)",
             "$p_{j}^{D9}$ in destination cell (tax data)",
             "Distance (suburbs $\\to$ suburbs)",
             "Distance (center $\\to$ suburbs)",
             "Distance (suburbs $\\to$ center)",
             "Distance (center $\\to$ center)"
  )
  
  # if (as.character(light_model$object$call[1]) %in% c("glm.nb","MASS::glm.nb")){
  #   label <- c(label,"$\\theta$ (log)")
  # }
  
  text_coeff <- cbind('label' = label, text_coeff)
  
  
  template_stargazer2 <- template_stargazer
  lapply(1:length(text_coeff[,'variable']), function(ligne)({
    idx <- which(grepl(pattern = text_coeff[,'variable'][ligne], template_stargazer2, fixed = TRUE))
    if (length(idx)==0){
      print("nothing to change")
    }else{
      template_stargazer2[idx:(idx+1)] <<- c(
        paste0(" ",
               text_coeff[,'label'][ligne],
               " & ",
               text_coeff[,'text_coeff'][ligne],
               " \\\\ "
        ),
        paste0(
          "  & ",
          text_coeff[,'text_sd'][ligne],
          " \\\\ "
        )
      )
    }
  }))
  idx <- which(grepl("depvarname", template_stargazer2))
  template_stargazer2 <- c(
    template_stargazer2[1:(idx-1)],
    gsub("depvarname", modelname, template_stargazer2[idx]),
    gsub("depvarname", modellabel, template_stargazer2[idx]),
    template_stargazer2[(idx+1):length(template_stargazer2)]
  )
  
  # STATS -------
  
  template_stargazer2[grepl("Observations", template_stargazer2)] <- sprintf("Observations & %s \\\\ ",
                                                                             format(light_model$summary$n, big.mark = ","))
  if (as.character(light_model$object$call[1]) == "lm"){
    template_stargazer2[grepl("Bayesian information criterion", template_stargazer2)] <- sprintf("Bayesian information criterion & %s \\\\ ",
                                                                                                 "")
    template_stargazer2[grepl("Log Likelihood", template_stargazer2)] <- sprintf("Log Likelihood & %s \\\\ ",
                                                                                 "")
    
  } else{
    template_stargazer2[grepl("Bayesian information criterion", template_stargazer2)] <- sprintf("Bayesian information criterion & %s \\\\ ",
                                                                                                 format(BIC_strip_glm(light_model), big.mark = ","))
    template_stargazer2[grepl("Log Likelihood", template_stargazer2)] <- sprintf("Log Likelihood & %s \\\\ ",
                                                                                 format(as.numeric(logLik(light_model$object)), big.mark = ","))
  }
  
  
  # Inverse observation and BIC
  idx <- which(grepl("Observations", template_stargazer2))
  template_stargazer2[c(idx-1,idx)]  <- template_stargazer2[c(idx,idx-1)]
  
  # Add deviation parameter for negative binomial models
  if (as.character(light_model$object$call[1]) %in% c("glm.nb","MASS::glm.nb")){
    idx <- which(grepl("Log Likelihood", template_stargazer2))  
    template_stargazer2 <- c(
      template_stargazer2[1:idx],
      sprintf("$\\theta$ & %s \\\\ ", round(light_model$object$theta,3)),
      template_stargazer2[(idx+1):length(template_stargazer2)]
    )
    if (as.character(light_model$object$call[1]) == "lm"){
      idx <- which(grepl("Log Likelihood", template_stargazer2))  
      template_stargazer2 <- c(
        template_stargazer2[1:idx],
        sprintf("$R^2$ & %s \\\\ ", round(light_model$summary$r.squared,3)),
        template_stargazer2[(idx+1):length(template_stargazer2)]
      )
    }
    
  }
  
  return(template_stargazer2)
}


lightstargazer.heckman <- function(light_model,
                                   modelname = "\\textsc{Low-income}",
                                   modellabel = "Outcome",
                                   modeltype = c("outcome","selection"),
                                   omit = "residential_D1_destinationTRUE",
                                   label = NULL){
  
  # if (inherits(light_model$object, "zeroinfl"))
  #   stop("Use lightstargazer_pscl for zeroinfl object")
  
  modeltype <- match.arg(modeltype)
  
  coeff_part <- light_model$summary$estimate
  
  idx <- which(grepl("(Intercept)", rownames(coeff_part)))
  if (modeltype == "selection"){
    coeff_part <- coeff_part[light_model$summary$param$index$betaS,]
  } else{
    coeff_part <- coeff_part[light_model$summary$param$index$betaO,]
  }
  
  pvaluesvar <- "Pr(>|t|)"
  
  text_coeff <- paste0(round(coeff_part[,'Estimate'],3),
                       sapply(coeff_part[,pvaluesvar], signif_stars))
  text_sd <- paste0("(",round(coeff_part[,'Std. Error'],3),")")
  text_coeff <- cbind("variable" = rownames(coeff_part),text_coeff, text_sd)
  
  if (!is.null(omit)) text_coeff <- text_coeff[!grepl(paste(omit, collapse = "|"), text_coeff[,'variable']),]
  
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)
  
  
  if (is.null(label)) label <- c("Constant",
                                 "Population in home cell (log)",
                                 "Population in destination cell (log)",
                                 "Employment in home cell (log)",
                                 "Employment in destination cell (log)",
                                 "$p_{j}^{D1}$ in destination cell (tax data)",
                                 "$p_{j}^{D9}$ in destination cell (tax data)",
                                 "Distance (suburbs $\\to$ suburbs)",
                                 "Distance (center $\\to$ suburbs)",
                                 "Distance (suburbs $\\to$ center)",
                                 "Distance (center $\\to$ center)"
  )
  
  
  if (modeltype == "outcome"){
    label <- c(label,"invMillsRatio","sigma","rho")
  }
  
  text_coeff <- cbind('label' = label, text_coeff)
  
  template_stargazer2 <- template_stargazer
  lapply(1:length(text_coeff[,'variable']), function(ligne)({
    idx <- which(grepl(pattern = text_coeff[,'variable'][ligne], template_stargazer2, fixed = TRUE))
    if (length(idx)==0){
      print("nothing to change")
    }else{
      template_stargazer2[idx:(idx+1)] <<- c(
        paste0(" ",
               text_coeff[,'label'][ligne],
               " & ",
               text_coeff[,'text_coeff'][ligne],
               " \\\\ "
        ),
        paste0(
          "  & ",
          text_coeff[,'text_sd'][ligne],
          " \\\\ "
        )
      )
    }
  }))
  idx <- which(grepl("depvarname", template_stargazer2))
  template_stargazer2 <- c(
    template_stargazer2[1:(idx-1)],
    gsub("depvarname", modelname, template_stargazer2[idx]),
    gsub("depvarname", modellabel, template_stargazer2[idx]),
    template_stargazer2[(idx+1):length(template_stargazer2)]
  )
  
  # STATS -------
  
  template_stargazer2[grepl("Observations", template_stargazer2)] <- sprintf("Observations & %s \\\\ ",
                                                                             format(light_model$summary$param$nObs, big.mark = ","))
  template_stargazer2[grepl("Bayesian information criterion", template_stargazer2)] <- sprintf("Bayesian information criterion & %s \\\\ ",
                                                                                               "")
  template_stargazer2[grepl("Log Likelihood", template_stargazer2)] <- sprintf("Log Likelihood & %s \\\\ ",
                                                                               "")
  
  
  
  # Inverse observation and BIC
  idx <- which(grepl("Observations", template_stargazer2))
  template_stargazer2[c(idx-1,idx)]  <- template_stargazer2[c(idx,idx-1)]
  
  return(template_stargazer2)
}