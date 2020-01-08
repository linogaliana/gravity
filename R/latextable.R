#' Wrapper to extract coefficients from a list of models
#' 
#' @param idx Index
#' @inheritParams light_table
#' @return List with coefficients ready to be merged
#'  to produce a latex table

extract_coeff <- function(idx, model_list, modeltype){
  
  type <- modeltype[idx]
  
  if (!(inherits(model_list[[idx]], "zeroinfl") |
        (inherits(model_list[[idx]], "negbin")))){
    mod <- model_list[[idx]]
    text_coeff <- paste0(round(mod$coefficients[,'Estimate'],3),
                         sapply(mod$coefficients[,'Pr(>|t|)'], signif_stars))
    text_sd <- paste0("(",round(mod$coefficients[,'Std. Error'],3),")")
    text_coeff <- cbind("variable" = rownames(mod$coefficients),text_coeff, text_sd)
    text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                    fixed = TRUE)
    return(text_coeff)  
  }
  
  mod <- model_list[[idx]]
  if (inherits(model_list[[idx]], "zeroinfl")){
    if ((is.na(type)) || (type == "selection")){
      clist <- mod$coefficients$zero
    } else{
      clist <- mod$coefficients$count
    }
  } else{
    clist <- mod$coefficients
  }
  
  text_coeff <- paste0(round(clist[,'Estimate'],3),
                       sapply(clist[,'Pr(>|z|)'], signif_stars))
  text_sd <- paste0("(",round(clist[,'Std. Error'],3),")")
  text_coeff <- cbind("variable" = rownames(clist),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)
  
  return(text_coeff)  
  
}


#' Produce latex tables from stripped objects to reduce memory needs
#' 
#' @param model_list List of models
#' @param modeltype Character vectors indicating whether
#'  we should use selection or outcome model. Ignored if
#'  model is not zeroinfl
#' @param title Table caption
#' @param label Table label
#' @param dep.var.labels Label for dependent variables
#' @param column.labels
#' @param covariate.labels A character vector of labels for
#'  columns in regression tables.
#'  Their layout, in terms of the number of columns
#'  associated with each label, is given by the
#'  argument `column.separate`.
#' @param column.separate A numeric vector that specifies how
#'  column.labels should be laid out across regression table
#'  columns. A value of `c(2, 1, 3)`, for instance, will apply
#'  the first label to the two first columns, the second label
#'  to the third column, and the third label will apply to
#'  the following three columns (i.e., columns
#'  number four, five and six). If the argument's value is `NULL`
#'  or the regression table contains more columns than are
#'  referred to in `column.separate`, a value of `1`
#'  is assumed for each *excess* column label.
#' @param add.lines Rows to add in model statistics part
#' @param notes Notes that should be added at the end
#' @param omit List of variables that should be removed from
#'  the table
#'  
#' This function is designed to produce `latex` tables with
#'  stripped objects (see \link{strip}). It follows
#'  \link[stargazer]{stargazer} standards but proposes a
#'  simplified framework. Customization is limiteds
#'  
#'  @return A character vector.
#'  
#'  @examples data("bioChemists", package = "pscl")
#' 
#' fm_zip    <- pscl::zeroinfl(art ~ . | ., data = bioChemists)
#' fm_zip2   <- pscl::zeroinfl(art ~ 1 | ., data = bioChemists)
#' glm_model <- glm(art ~ . , data = bioChemists)
#' fm_zip3   <- pscl::zeroinfl(art ~ 1 | ., data = bioChemists,
#'                             dist = "negbin")
#' fm_zip5   <- MASS::glm.nb(art ~ 1, data = bioChemists)
#' 
#' model_list <- lapply(list(fm_zip, fm_zip2,
#'                           glm_model, fm_zip3,
#'                           fm_zip5), gravity::strip)
#' 
#' 
#' model_list <- list(
#'   light_model$object,
#'   light_model2$object,
#'   light_model3$object,
#'   light_model4$object,
#'   light_model5$object
#' )
#' 
#' cat(
#'   gravity:::light_table(model_list = model_list,
#'                         covariate.labels = c("x1","x2")),
#'   sep = "\n"
#' )
#' 
#' @export

light_table <- function(
  model_list,
  modeltype = rep("outcome", length(model_list)),
  title = "Title",
  label = "label",
  dep.var.labels = "Label dep.var.labels",
  column.labels = rep("bla", length(model_list)),
  covariate.labels = NULL,
  column.separate = NULL,
  add.lines = "rows to add",
  notes = "notes to add",
  omit = ""){
  
  ncols_models <- length(model_list)
  
  coeff_data <- lapply(seq_len(ncols_models), extract_coeff, model_list, modeltype)
  
  
  # PART I : HEAD ------- 
  
  header <- sprintf("\\begin{table}[!htbp] \\centering 
  \\caption{%s} 
  \\label{%s}", title, label)
  
  
  tabular_header <- sprintf(
    "\\begin{tabular}{@{\\extracolsep{5pt}}l%s} 
  \\\\[-1.8ex]\\hline 
  \\hline \\\\[-1.8ex]",
    paste(rep("c",ncols_models), collapse = "")
  )
  
  
  table_total <- c(header,tabular_header)
  
  depvar_header <- sprintf("
   & \\multicolumn{%s}{c}{\\textit{Dependent variable:}} \\\\ 
\\cline{2-%s}
\\\\[-1.8ex] & \\multicolumn{%s}{c}{%s} \\\\
  ", ncols_models, ncols_models+1,
                           ncols_models, dep.var.labels)
  
  table_total <- c(table_total,depvar_header)
  
  if (!is.null(column.labels)){
    colvar_header <- paste(c("",column.labels[1:ncols_models]), collapse = " & ")
  } else{
    colvar_header <- ""
  }
  
  if (length(ncols_models)>1){
    colvar_header <- c(
      colvar_header,
      paste0(" \\\\[-1.8ex] & ",
             paste(paste0("(",seq_len(ncols_models), ")"), collapse = " & ")
      )
    )
  }
  
  colvar_header <- paste0(colvar_header, " \\\\")
  
  
  table_total <- c(table_total, colvar_header, "\\hline \\\\[-1.8ex] ")
  
  # PART II : BODY -------
  
  arrange_coeff <- function(text_coeff){
    
    if (is.null(nrow(text_coeff))){
      text_coeff <- rbind(text_coeff, "")
    }
    
    text_coeff <- cbind(text_coeff, "text_zempty" = "")
    
    body_table <- reshape2::melt(
      data.frame(text_coeff), id.vars = "variable",
      variable.name = "obj"
    )
    
    body_table <- body_table[order(body_table$variable,
                                   body_table$obj), ]
    body_table$variable <- as.character(body_table$variable)
    
    body_table$value <- paste0(" & ", body_table$value)
    
    return(body_table)
  }
  
  
  coeff_body <- lapply(coeff_data, arrange_coeff)
  
  coeff_body <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("variable","obj"), all = TRUE),
                       coeff_body)
  coeff_body[is.na(coeff_body)] <- " & "
  
  
  # PUT CONSTANT IN LAST POSITION
  constant_idx <- which(coeff_body[,'variable'] == "(Intercept)")
  
  if (!is.null(constant_idx)){
    rows <- seq_len(nrow(coeff_body))
    rows <- rows[-constant_idx]
    coeff_body <- coeff_body[c(rows, constant_idx),]
  }
  
  if (omit != ""){
    coeff_body <- coeff_body[!(coeff_body[,'variable'] %in% omit),]
  }
  
  coeff_body <- coeff_body[!(coeff_body[,'variable'] == "Log(theta)"),]
  
  list_variables <- unique(coeff_body[,'variable'])
  
  rows_sd <- grep("\\(.*?\\)", coeff_body$value.x)
  coeff_body$variable[sort(c(rows_sd,rows_sd+1))] <- ""
  
  coeff_body <- coeff_body[,!(names(coeff_body) %in% "obj")]
  body_table <- apply(coeff_body, 1, paste, collapse="")
  
  body_table <- paste0(body_table, "\\\\")
  
  
  if (!is.null(covariate.labels)){
    n_replace <- min(length(list_variables), length(covariate.labels))
    body_table <- stringr::str_replace(
      string = body_table,
      pattern = list_variables[1:n_replace],
      replacement = covariate.labels[1:n_replace]
    )
  }
  
  
  table_total <- c(table_total, body_table, "\\hline \\\\[-1.8ex] ")
  
  
  
  # PART III: STATISTICS -----
  
  statsdf <- lapply(model_list, function(mod){
    
    if (inherits(mod,"zeroinfl")){
      llk <- mod$loglik
      bic <- BIC(mod)
    } else{
      llk <- logLik(mod)
      k <- attributes(llk)$df
      bic <- -2*as.numeric(llk) + k*log(mod$n)
      llk <- as.numeric(llk)
    }
    
    df <- data.frame(
      stat = c("Observations",
               "Log likelihood",
               "Log likelihood (by obs.)",
               "Bayesian information criterion"),
      order = seq_len(4),
      val = as.character(
        c(format(mod$n, digits = 0),
          format(llk, digits = 1L, nsmall = 1L),
          format(llk/mod$n, digits = 3L, nsmall = 3L),
          format(bic, digits = 1L, nsmall = 1L)
        )
      )
    )
    
    if ((inherits(mod,"zeroinfl") && mod$dist == "negbin") || (inherits(mod,"negbin"))){
      
      df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                             order = 0,
                             val = as.character(
                               format(1/mod$theta, digits = 1L, nsmall = 1L))
      ), df)
    }
    
    return(df)
  })
  
  statsdf <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("stat","order"), all = TRUE),
                    statsdf)
  statsdf <- statsdf[order(statsdf$order),]
  statsdf <- statsdf[, names(statsdf) != "order"]
  statsdf[, ] <- lapply(statsdf[, ], as.character)
  statsdf[is.na(statsdf)] <- ""
  statsdf <- apply(statsdf, 1, paste, collapse = " & ")
  stats_table <- paste0(statsdf, " \\\\")
  
  
  table_total <- c(table_total, stats_table, 
                   "\\hline ",
                   "\\hline \\\\[-1.8ex] ")
  
  
  # PART IV: FOOTER -----
  
  foot_table <- sprintf(
    "\\textit{Note:}  & \\multicolumn{%s}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\ ",
    ncols_models
  )
  
  if (add.lines != ""){
    foot_table <- c(foot_table,
                    sprintf(
                      " & \\multicolumn{%s}{r}{%s} \\\\ ",
                      ncols_models,
                      add.lines
                    ))
  }
  
  foot_table <- c(foot_table, "\\end{tabular} ", "\\end{table} ")
  
  
  table_total <- c(table_total, foot_table)
  
  
  # TO DO: INCLURE THETA
  # # Add deviation parameter for negative binomial models
  # if (modeltype == "count"){
  #   idx <- which(grepl("Log Likelihood", template_stargazer2))  
  #   template_stargazer2 <- c(
  #     template_stargazer2[1:idx],
  #     sprintf("$\\theta$ & %s \\\\ ", round(light_model$object$theta,3)),
  #     template_stargazer2[(idx+1):length(template_stargazer2)]
  #   )
  #   
  # }
  
  
  # - should be $-$
  table_total <- gsub(pattern = "-", replacement = "$-$",
                      x = table_total)
  
  
  return(table_total)
}
