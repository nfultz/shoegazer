.onAttach <- 
function(libname, pkgname) {
  packageStartupMessage("\nPlease cite as: \n")
  packageStartupMessage(" Hlavac, Marek (2015). stargazer: Well-Formatted Regression and Summary Statistics Tables.")
  packageStartupMessage(" R package version 5.2. http://CRAN.R-project.org/package=stargazer \n")
}

.stargazer.wrap <-
  function(..., type, title, style, summary, out, out.header, covariate.labels, column.labels, column.separate, 
           dep.var.caption, dep.var.labels, dep.var.labels.include, align, coef, se, t, p, t.auto, 
           p.auto, ci, ci.custom, ci.level, ci.separator, add.lines, apply.coef, apply.se, apply.t, apply.p, apply.ci,
           colnames,
           column.sep.width, decimal.mark, df, digit.separate, digit.separator, digits, digits.extra, 
           flip, float, 
           float.env, font.size, header, initial.zero, intercept.bottom, intercept.top, keep, keep.stat, 
           label, model.names, model.numbers, multicolumn, no.space, notes, notes.align, notes.append, 
           notes.label, object.names, omit, omit.labels, omit.stat, omit.summary.stat, omit.table.layout,
           omit.yes.no, order, ord.intercepts, perl, report, rownames,
           rq.se, selection.equation, single.row, star.char, star.cutoffs, suppress.errors, 
           table.layout, table.placement, 
           zero.component, summary.logical, summary.stat, nobs, mean.sd, min.max, median, iqr) {
  
  
############## TEXT AND html MODE ##############

  
  

###########################################

    ## invisible output
    invisible.output <- NULL
    latex.code <- NULL
    text.out <- NULL
    
    ## error handling
    error.present <- "\n"
    
	
    # get object names --- !!! CHECK ORDER
    object.names.string <- deparse(substitute(list(...))) ### for further processing to extract object names
    .global.object.names.all <- .get.object.names(object.names.string)
  
	
    # get objects
    list.of.objects <- list(...)
    objects <- as.list(.get.objects(list.of.objects))
    how.many.objects <- length(objects)
	
  
    # should we include a summary statistics table when given a data frame
    .global.summary <- rep(TRUE, times=how.many.objects)
    
    ## check if argument input is ok
    .format.rownames <- TRUE
    .format.colnames <- TRUE
  
    # flip the table?
    .format.flip <- flip
  
    if (how.many.objects < 1) { error.present <- c(error.present, "% Error: At least one object is required.\n") }
    else {
      
      # identify objects
      for (i in seq(1:how.many.objects)) {
        
        if (is.data.frame(objects[[i]])) {
          obj.rownames <- rownames(objects[[i]])
          if (is.null(obj.rownames)) { .format.rownames <- FALSE }
        }
        else if ((is.matrix(objects[[i]])) && (class(objects[[i]]) != "coeftest")) { 
          
          .global.summary[i] <- FALSE   # content output default for matrices
          
          obj.rownames <- rownames(objects[[i]])
          obj.colnames <- colnames(objects[[i]])
          
          if (is.null(obj.rownames)) { 
            if (.format.flip == FALSE) { .format.rownames <- FALSE }
            else { .format.colnames <- FALSE }
            obj.rownames <- as.character(c(1:nrow(objects[[i]])))
          }
          if (is.null(obj.colnames)) { 
            if (.format.flip == FALSE) { .format.colnames <- FALSE }
            else { .format.rownames <- FALSE }
            obj.colnames <- as.character(c(1:ncol(objects[[i]])))
          }
          
          objects[[i]] <- as.data.frame(objects[[i]])
          colnames(objects[[i]]) <- obj.colnames
        }
        else if (is.vector(objects[[i]])) {
          
          .global.summary[i] <- FALSE   # content output default for vectors
          
          obj.names <- names(objects[[i]])
          
          if (is.null(obj.names)) { 
            .format.colnames <- FALSE
            .format.rownames <- FALSE
            obj.names <- as.character(c(1:length(objects[[i]])))
          }
          
          objects[[i]] <- as.data.frame(t(objects[[i]]))
          names(objects[[i]]) <- obj.names
        
          if (.format.flip == TRUE) { .format.colnames <- FALSE } 
          else { .format.rownames <- FALSE }
        }
          
        if (!is.data.frame(objects[[i]])) {
        
          # if zelig$result relevant, identify this automatically
          if (class(objects[[i]]) %in% c("coeftest","lmerMod","glmerMod","nlmerMod","fGARCH")) {  # use this to eliminate lmer, glmer, nlmer
            if (.model.identify(objects[[i]])=="unknown") { error.present <- c(error.present, "% Error: Unrecognized object type.\n",i) }
          }
          else {
            if (!is.null(objects[[i]]$zelig.call)) {
              if (!is.null(objects[[i]]$formula)) { formula <- objects[[i]]$formula }
              objects[[i]] <- objects[[i]]$result          
              if (!is.null(formula)) { objects[[i]]$formula2 <- formula }
            }
        
            ###
            if (is.atomic(objects[[i]]) & (!is.null(objects[[i]]))) { error.present <- c(error.present, "% Error: Unrecognized object type.\n") }
            else if (.model.identify(objects[[i]])=="unknown") { error.present <- c(error.present, "% Error: Unrecognized object type.\n") }
            else if (.model.identify(objects[[i]])=="unsupported zelig") { error.present <- c(error.present, "% Error: Unsupported 'zelig' model.\n") }
          }  
        }
      }
    
    }
  
    if (!is.character(type)) { error.present <- c(error.present, "% Error: Argument 'type' must be of type 'character.'\n") }
    if (length(type) != 1) { error.present <- c(error.present, "% Error: Argument 'type' must be of length 1.'\n") }
    if (is.character(type)) {
      if (!(tolower(type) %in% c("latex", "text", "html"))) {
        error.present <- c(error.present, "% Error: 'style' must be either 'latex' (default), 'html' or 'text.'\n")
      }
    }
    
    if (!is.character(title)) { error.present <- c(error.present, "% Error: Argument 'title' must be of type 'character.'\n") }
    
    if (!is.character(style)) { error.present <- c(error.present, "% Error: Argument 'style' must be of type 'character.'\n") }
    if (length(style) != 1) { error.present <- c(error.present, "% Error: Argument 'style' must be of length 1.'\n") }
    if (is.character(style)) {
      if (!(tolower(style) %in% c("all","all2","default","commadefault","aer","ajps","ajs","asq","asr","apsr","demography","io","jpam","qje"))) {
        error.present <- c(error.present, "% Error: 'style' not recognized'\n")
      }
    }
    
    if ((!is.logical(summary)) & (!is.null(summary))) { error.present <- c(error.present, "% Error: Argument 'summary' must be NULL, or of type 'logical' (TRUE/FALSE) \n") }
    
    if ((!is.character(out)) & (!is.null(out))) { error.present <- c(error.present, "% Error: Argument 'out' must be NULL (default), or a vector of type 'character.' \n") }
    if (!is.logical(out.header)) { error.present <- c(error.present, "% Error: Argument 'out.header' be of type 'logical' (TRUE/FALSE) \n") }
  
    if ((!is.numeric(column.separate)) & (!is.null(column.separate))) { error.present <- c(error.present, "% Error: Argument 'column.separate' must be NULL (default), a vector of type 'numeric.'\n") }
  
    if ((!is.character(column.labels)) & (!is.null(column.labels))) { error.present <- c(error.present, "% Error: Argument 'column.labels' must be NULL (default), or a vector of type 'character.'\n") }
    if ((!is.character(covariate.labels)) & (!is.null(covariate.labels))) { error.present <- c(error.present, "% Error: Argument 'covariate.labels' must be NULL (default), or a vector of type 'character.'\n") }
    if ((!is.character(dep.var.labels)) & (!is.null(dep.var.labels))) { error.present <- c(error.present, "% Error: Argument 'dep.var.labels' must be NULL (default), or a vector of type 'character.'\n") }
    
    if ((!is.logical(dep.var.labels.include)) & (!is.null(dep.var.labels.include))) { error.present <- c(error.present, "% Error: Argument 'dep.var.labels.include' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(dep.var.labels.include) != 1) & (!is.null(dep.var.labels.include))) { error.present <- c(error.present, "% Error: Argument 'dep.var.labels.include' must be of length 1.'\n") }
    
    if ((!is.character(dep.var.caption)) & (!is.null(dep.var.caption))) { error.present <- c(error.present, "% Error: Argument 'dep.var.caption must be NULL (default), or of type 'character.'\n") }
    if ((length(dep.var.caption) != 1) & (!is.null(dep.var.caption))) { error.present <- c(error.present, "% Error: Argument 'dep.var.caption' must be of length 1.'\n") }  
  
    coef <- .turn.into.list(coef); se <- .turn.into.list(se)
    t <- .turn.into.list(t); p <- .turn.into.list(p)
    
    if ((!.is.list.numeric(coef)))  { error.present <- c(error.present, "% Error: Argument 'coef' must be NULL (default), or a list of numeric vectors.\n") }
    if ((!.is.list.numeric(se)))  { error.present <- c(error.present, "% Error: Argument 'se' must be NULL (default), or a list of numeric vectors.\n") }
    if ((!.is.list.numeric(t)))  { error.present <- c(error.present, "% Error: Argument 't' must be NULL (default), or a list of numeric vectors.\n") }
    if ((!.is.list.numeric(p)))  { error.present <- c(error.present, "% Error: Argument 'p' must be NULL (default), or a list of numeric vectors.\n") }
  
    if (!is.logical(t.auto)) { error.present <- c(error.present, "% Error: Argument 't.auto' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(t.auto) != 1) { error.present <- c(error.present, "% Error: Argument 't.auto' must be of length 1.'\n") }

    if (!is.logical(p.auto)) { error.present <- c(error.present, "% Error: Argument 't.auto' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(p.auto) != 1) { error.present <- c(error.present, "% Error: Argument 't.auto' must be of length 1.'\n") }

    if (!is.logical(align)) { error.present <- c(error.present, "% Error: Argument 'align' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(align) != 1) { error.present <- c(error.present, "% Error: Argument 'align' must be of length 1.'\n") }

    if (!is.logical(ci)) { error.present <- c(error.present, "% Error: Argument 'ci' must be of type 'logical' (TRUE/FALSE) \n") }
    
    ci.custom <- .turn.into.list(ci.custom)
    if ((!.is.list.numeric.matrix(ci.custom)))  { error.present <- c(error.present, "% Error: Argument 'ci.custom' must be NULL (default), or a list of numeric matrices. \n") }
    else if (!is.null(ci.custom)) {
      l <- length(ci.custom)
      
      bad.dimension <- FALSE
      for (i in 1:l) {
        if (!is.null(ci.custom[[i]])) {
          if (ncol(ci.custom[[i]]) != 2 ) { bad.dimension <- TRUE }
        }
      }  
      if (bad.dimension) { error.present <- c(error.present, "% Error: The numeric matrix in 'ci.custom' must have two columns (lower bound and upper bound, respectively). \n") }
    }
    
    if (!is.numeric(ci.level)) { error.present <- c(error.present, "% Error: Argument 'ci.level' must be of type 'numeric.' \n") }
  
    if ((!is.character(ci.separator)) & (!is.null(ci.separator))) { error.present <- c(error.present, "% Error: Argument 'ci.separator' must be NULL (default), or of type 'character.'\n") }
    if ((length(ci.separator) != 1) & (!is.null(ci.separator))) { error.present <- c(error.present, "% Error: Argument 'ci.separator' must be of length 1.'\n") }
  
    add.lines <- .turn.into.list(add.lines)
    if ((!is.list(add.lines)) & (!is.null(add.lines))) { error.present <- c(error.present, "% Error: Argument 'add.lines' must be NULL (default), or a list of vectors. \n") }
    if (!is.null(add.lines)) {
      if (length(add.lines) < 1) { error.present <- c(error.present, "% Error: The list in argument 'add.lines' must be of length 1 or more. \n") }
      if (!all(unlist(lapply(add.lines, is.vector)))) { error.present <- c(error.present, "% Error: Argument 'add.lines' must be NULL (default), or a list of vectors. \n") }
    }
  
    if ((!is.function(apply.coef)) & (!is.null(apply.coef))) { error.present <- c(error.present, "% Error: Argument 'apply.coef' must be NULL (default), or a function.'\n") }
    if ((!is.function(apply.se)) & (!is.null(apply.se))) { error.present <- c(error.present, "% Error: Argument 'apply.se' must be NULL (default), or a function.'\n") }
    if ((!is.function(apply.t)) & (!is.null(apply.t))) { error.present <- c(error.present, "% Error: Argument 'apply.t' must be NULL (default), or a function.'\n") }
    if ((!is.function(apply.p)) & (!is.null(apply.p))) { error.present <- c(error.present, "% Error: Argument 'apply.p' must be NULL (default), or a function.'\n") }
    if ((!is.function(apply.ci)) & (!is.null(apply.ci))) { error.present <- c(error.present, "% Error: Argument 'apply.ci' must be NULL (default), or a function.'\n") }

    if (!is.character(column.sep.width)) { error.present <- c(error.present, "% Error: Argument 'column.sep.width' must be of type 'character.'\n") }
    if (length(column.sep.width) != 1)  { error.present <- c(error.present, "% Error: Argument 'column.sep.width' must be of length 1.'\n") }
  
    if ((!is.character(decimal.mark)) & (!is.null(decimal.mark))) { error.present <- c(error.present, "% Error: Argument 'decimal.mark' must be NULL (default), or of type 'character.'\n") }
    if ((length(decimal.mark) != 1) & (!is.null(decimal.mark))) { error.present <- c(error.present, "% Error: Argument 'decimal.mark' must be of length 1.'\n") }
  
    if (!is.logical(df)) { error.present <- c(error.present, "% Error: Argument 'df' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(df) != 1) { error.present <- c(error.present, "% Error: Argument 'df' must be of length 1.'\n") }
    
    if ((!is.numeric(digit.separate)) & (!is.null(digit.separate)) & (!is.character(digit.separate))) { error.present <- c(error.present, "% Error: Argument 'digit.separate' must be NULL (default), a vector of type 'numeric,' or of type 'character.' \n") }
    if (is.character(digit.separate)) {
      if (!(digit.separate %in% c("lakh","japan","china"))) { error.present <- c(error.present, "% Error: If argument 'digit.separate' is of type character, it must be one of \"lakh\"/\"china\"/\"japan\".\n") }
    }
    
    if ((!is.character(digit.separator)) & (!is.null(digit.separator))) { error.present <- c(error.present, "% Error: Argument 'digit.separator' must be NULL (default), or of type 'character.'\n") }
    if ((length(digit.separator) != 1) & (!is.null(digit.separator))) { error.present <- c(error.present, "% Error: Argument 'digit.separator' must be of length 1.'\n") }
    
    if ((!is.numeric(digits)) & (!is.null(digits))) { 
      if (!is.na(digits)) { error.present <- c(error.present, "% Error: Argument 'digits' must be NULL (default), or of type 'numeric.'\n") }
    }
    if ((length(digits) != 1) & (!is.null(digits))) { 
      if (!is.na(digits)) { error.present <- c(error.present, "% Error: Argument 'digits' must be of length 1.'\n") }
    }
    if (!is.null(digits)) {
      if (!is.na(digits)) {
        if ((digits<0) & (is.numeric(digits))) { error.present <- c(error.present, "% Error: Argument 'digits' must be >= 0.'\n") }
      }
    }
    
    if ((!is.numeric(digits.extra)) & (!is.null(digits.extra))) { error.present <- c(error.present, "% Error: Argument 'digits.extra' must be NULL (default), or of type 'numeric.'\n") }
    if ((length(digits.extra) != 1) & (!is.null(digits.extra))) { error.present <- c(error.present, "% Error: Argument 'digits.extra' must be of length 1.'\n") }
    if (!is.null(digits.extra)) {
      if ((digits.extra<0) & (is.numeric(digits.extra))) { error.present <- c(error.present, "% Error: Argument 'digits.extra' must be >= 0.'\n") }
    }
  
    if (!is.logical(flip)) { error.present <- c(error.present, "% Error: Argument 'flip' must be of type 'logical' (TRUE/FALSE) \n") }
    if ((length(flip) != 1) & (!is.null(flip))) { error.present <- c(error.present, "% Error: Argument 'flip' must be of length 1.'\n") }
  
    if (!is.logical(float)) { error.present <- c(error.present, "% Error: Argument 'float' must be of type 'logical' (TRUE/FALSE) \n") }
    if ((length(float) != 1) & (!is.null(float))) { error.present <- c(error.present, "% Error: Argument 'float' must be of length 1.'\n") }
    
    if (!(float.env %in% c("table","table*","sidewaystable"))) { error.present <- c(error.present, "% Error: Argument 'float.env' must be one of \"table\", \"table*\" or \"sidewaystable\".\n") }  
    if (length(float.env) != 1) { error.present <- c(error.present, "% Error: Argument 'float.env' must be of length 1.'\n") }

    if (!is.null(font.size)) {
      if (!(font.size %in% c("tiny","scriptsize","footnotesize","small","normalsize","large","Large","LARGE","huge","Huge"))) { error.present <- c(error.present, "% Error: Argument 'font.size' must be NULL (default), or one of the available font sizes. See documentation.") }  
    }
    if ((length(font.size) != 1) & (!is.null(font.size))) { error.present <- c(error.present, "% Error: Argument 'font.size' must be of length 1.'\n") }
  
    if (!is.logical(header)) { error.present <- c(error.present, "% Error: Argument 'header' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(header) != 1) { error.present <- c(error.present, "% Error: Argument 'header' must be of length 1.'\n") }
      
    if ((!is.logical(initial.zero)) & (!is.null(initial.zero))) { error.present <- c(error.present, "% Error: Argument 'initial.zero' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(initial.zero) != 1) & (!is.null(initial.zero))) { error.present <- c(error.present, "% Error: Argument 'initial.zero' must be of length 1.'\n") }
  
    if (!is.logical(intercept.bottom)) { error.present <- c(error.present, "% Error: Argument 'intercept.bottom' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(intercept.bottom) != 1) { error.present <- c(error.present, "% Error: Argument 'intercept.bottom' must be of length 1.'\n") }
      
    if (!is.logical(intercept.top)) { error.present <- c(error.present, "% Error: Argument 'intercept.top' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(intercept.top) != 1) { error.present <- c(error.present, "% Error: Argument 'intercept.top' must be of length 1.'\n") }
  
    if (intercept.top & intercept.bottom) { error.present <- c(error.present, "% Error: Arguments 'intercept.bottom' and 'intercept.top' cannot both be TRUE. \n")}
  
    if ((!is.character(keep)) & (!is.numeric(keep)) & (!is.null(keep))) { error.present <- c(error.present, "% Error: Argument 'keep' must be NULL (default; all variables kept), or a vector of type 'character' or 'numeric.'\n") }
  
    if ((!is.character(keep.stat)) & (!is.null(keep.stat))) { error.present <- c(error.present, "% Error: Argument 'keep.stat' must be NULL (default), or a vector of type 'character.'\n") }
    keep.stat.acceptable <- c("all","n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho(se)*","Mills(se)*","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")     # list of statistic codes that are acceptable
    if (is.character(keep.stat)) {
      is.acceptable <- unique(tolower(keep.stat) %in% keep.stat.acceptable)
      if (length(is.acceptable)>1) { is.acceptable <- FALSE }
      if (!is.acceptable) { error.present <- c(error.present, "% Error: Unknown statistic in 'keep.stat' argument.\n") }
    } 
      
    if (!is.character(label)) { error.present <- c(error.present, "% Error: Argument 'label' must be of type 'character.'\n") }
  
    if ((!is.logical(model.names)) & (!is.null(model.names))) { error.present <- c(error.present, "% Error: Argument 'model.names' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(model.names) != 1) & (!is.null(model.names))) { error.present <- c(error.present, "% Error: Argument 'model.names' must be of length 1.'\n") }
    
    if ((!is.logical(model.numbers)) & (!is.null(model.numbers))) { error.present <- c(error.present, "% Error: Argument 'model.numbers' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(model.numbers) != 1) & (!is.null(model.numbers))) { error.present <- c(error.present, "% Error: Argument 'model.numbers' must be of length 1.'\n") }
  
    if (!is.logical(multicolumn)) { error.present <- c(error.present, "% Error: Argument 'multicolumn' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(multicolumn) != 1) { error.present <- c(error.present, "% Error: Argument 'multicolumn' must be of length 1.'\n") }
  
    if ((!is.logical(no.space)) & (!is.null(no.space))) { error.present <- c(error.present, "% Error: Argument 'no.space' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(no.space) != 1) & (!is.null(no.space))) { error.present <- c(error.present, "% Error: Argument 'no.space' must be of length 1.'\n") }
    
    if ((!is.character(notes)) & (!is.null(notes))) { error.present <- c(error.present, "% Error: Argument 'notes' must be NULL (default), or a vector of type 'character.'\n") }
    
    if (!is.null(notes.align)) {
      if (!(tolower(notes.align) %in% c("l","c","r"))) { error.present <- c(error.present, "% Error: Argument 'notes.align' must be NULL (default), or \"l\"/\"c\"/\"r\".\n") }  
    }
    if ((length(notes.align) != 1) & (!is.null(notes.align))) { error.present <- c(error.present, "% Error: Argument 'notes.align' must be of length 1.'\n") }
  
    if (!is.logical(notes.append)) { error.present <- c(error.present, "% Error: Argument 'notes.append' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(notes.append) != 1) { error.present <- c(error.present, "% Error: Argument 'notes.append' must be of length 1.'\n") }
  
    if ((!is.character(notes.label)) & (!is.null(notes.label))) { error.present <- c(error.present, "% Error: Argument 'notes.label' must be NULL (default), or of type 'character.'\n") }
    if ((length(notes.label) != 1) & (!is.null(notes.label))) { error.present <- c(error.present, "% Error: Argument 'notes.label' must be of length 1.'\n") }
  
    if (!is.logical(object.names)) { error.present <- c(error.present, "% Error: Argument 'object.names' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(object.names) != 1) { error.present <- c(error.present, "% Error: Argument 'object.names' must be of length 1.'\n") }
    
    if ((!is.character(omit)) & (!is.numeric(omit)) & (!is.null(omit))) { error.present <- c(error.present, "% Error: Argument 'omit' must be NULL (default; no omissions), or a vector of type 'character' or 'numeric.'\n") }
    if ((!is.character(omit.labels)) & (!is.null(omit.labels))) { error.present <- c(error.present, "% Error: Argument 'omit' must be NULL (default; no omissions), or a vector of type 'character.'\n") }
    if (!is.null(omit.labels)) {
      if (length(omit) != length(omit.labels)) { error.present <- c(error.present, "% Error: Arguments 'omit.labels' must be NULL (default; no omissions), or equal in length to 'omit.labels'.'\n") }
    }
  
    if ((!is.character(omit.stat)) & (!is.null(omit.stat))) { error.present <- c(error.present, "% Error: Argument 'omit.stat' must be NULL (default), or a vector of type 'character.'\n") }
    omit.stat.acceptable <- c("all","n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho(se)*","Mills(se)*","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")     # list of statistic codes that are acceptable
    if (is.character(omit.stat)) {
      is.acceptable <- unique(tolower(omit.stat) %in% omit.stat.acceptable)
      if (length(is.acceptable)>1) { is.acceptable <- FALSE }
      if (!is.acceptable) { error.present <- c(error.present, "% Error: Unknown statistic in 'omit.stat' argument.\n") }
    } 
  
    if ((!is.character(omit.summary.stat)) & (!is.null(omit.summary.stat))) { error.present <- c(error.present, "% Error: Argument 'omit.summary.stat' must be NULL (default), or a vector of type 'character.'\n") }
    omit.summary.stat.acceptable <- c("n","mean","sd","min","p25","median","p75","max")
    if (is.character(omit.summary.stat)) {
      is.acceptable <- unique(tolower(omit.summary.stat) %in% omit.summary.stat.acceptable)
      if (length(is.acceptable)>1) { is.acceptable <- FALSE }
      if (!is.acceptable) { error.present <- c(error.present, "% Error: Unknown statistic in 'omit.summary.stat' argument.\n") }
    } 
  
    if ((!is.character(omit.yes.no)) & (!is.null(omit.yes.no))) { error.present <- c(error.present, "% Error: Argument 'omit.yes.no' must be a vector of type 'character.'\n") }
    if ((length(omit.yes.no) != 2) & (!is.null(omit.yes.no))) { error.present <- c(error.present, "% Error: Argument 'omit.yes.no' must be of length 2.'\n") }
  
    if ((!is.character(order)) & (!is.numeric(order)) & (!is.null(order))) { error.present <- c(error.present, "% Error: Argument 'order' must be NULL (default; no omissions), or a vector of type 'character' or 'numeric.'\n") }
    
    if (!is.logical(ord.intercepts)) { error.present <- c(error.present, "% Error: Argument 'ord.intercepts' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(ord.intercepts) != 1) { error.present <- c(error.present, "% Error: Argument 'ord.intercepts' must be of length 1.'\n") }
    
    if (!is.logical(perl)) { error.present <- c(error.present, "% Error: Argument 'perl' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(perl) != 1) { error.present <- c(error.present, "% Error: Argument 'perl' must be of length 1.'\n") }
  
    if (!(is.logical(colnames)) & (!is.null(colnames))) { error.present <- c(error.present, "% Error: Argument 'colnames' must be NULL, or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(colnames) != 1) & (!is.null(colnames))) { error.present <- c(error.present, "% Error: Argument 'colnames' must be of length 1.'\n") }
  
    if (!(is.logical(rownames)) & (!is.null(rownames))) { error.present <- c(error.present, "% Error: Argument 'rownames' must be NULL, or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(rownames) != 1) & (!is.null(rownames))) { error.present <- c(error.present, "% Error: Argument 'rownames' must be of length 1.'\n") }
  
    if (!is.character(rq.se)) { error.present <- c(error.present, "% Error: Argument 'rq.se' must be of type 'character.' \n") }
    if (length(rq.se) != 1) { error.present <- c(error.present, "% Error: Argument 'rq.se' must be of length 1.'\n") }
    if (is.character(rq.se)) {
      if (!(rq.se %in% c("iid", "nid", "ker", "boot"))) { error.present <- c(error.present, "% Error: Argument 'rq.se' must be one of: 'iid', 'nid', 'ker' or 'boot.' \n") }
    }
  
    if (!is.logical(selection.equation)) { error.present <- c(error.present, "% Error: Argument 'selection.equation' must be of type 'logical' (TRUE/FALSE) \n") }
    if ((length(selection.equation) != 1) & (!is.null(selection.equation))) { error.present <- c(error.present, "% Error: Argument 'selection.equation' must be of length 1.'\n") }
  
    if (!is.logical(single.row)) { error.present <- c(error.present, "% Error: Argument 'single.row' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(single.row) != 1) { error.present <- c(error.present, "% Error: Argument 'single.row' must be of length 1.'\n") }
  
    if ((!is.character(star.char)) & (!is.null(star.char))) { error.present <- c(error.present, "% Error: Argument 'star.char' must be NULL (default), or of type 'character.'\n") }
    if ((!(length(star.char) >= 1)) & (!is.null(star.char))) { error.present <- c(error.present, "% Error: Argument 'star.char' must be at least of length 1.'\n") }
    
    if (!is.null(star.cutoffs)) {
      if (sum(is.na(star.cutoffs)) != length(star.cutoffs)) {
        if (!is.numeric(star.cutoffs)) { error.present <- c(error.present, "% Error: Argument 'star.cutoffs' must be NULL (default), or a vector of type 'numeric.'\n") }
      }
      if ( !(length(star.cutoffs) >= 1) & (!is.null(star.cutoffs))) { error.present <- c(error.present, "% Error: Argument 'star.cutoffs' must be a vector with at least one element.\n") }
      if (sum(star.cutoffs[!is.na(star.cutoffs)] == sort(star.cutoffs, decreasing = TRUE, na.last=NA)) != length(star.cutoffs[!is.na(star.cutoffs)])) { error.present <- c(error.present, "% Error: The elements of 'star.cutoffs' must be in weakly decreasing order.\n") }
    }
  
    if ((!is.character(summary.stat)) & (!is.null(summary.stat))) { error.present <- c(error.present, "% Error: Argument 'summary.stat' must be NULL (default), or a vector of type 'character.'\n") }
    summary.stat.acceptable <- c("n","mean","sd","min","p25","median","p75","max")     # list of statistic codes that are acceptable
    if (is.character(summary.stat)) {
      is.acceptable <- unique(tolower(summary.stat) %in% summary.stat.acceptable)
      if (length(is.acceptable)>1) { is.acceptable <- FALSE }
      if (!is.acceptable) { error.present <- c(error.present, "% Error: Unknown statistic in 'summary.stat' argument.\n") }
    } 
  
    if ((!is.character(table.layout)) & (!is.null(table.layout))) { error.present <- c(error.present, "% Error: Argument 'table.layout' must be of type 'character.'\n") }
    if ((length(table.layout) != 1) & (!is.null(table.layout)))  { error.present <- c(error.present, "% Error: Argument 'table.layout' must be of length 1.'\n") }
    if (is.character(table.layout) & (length(table.layout)==1)) {   # test if report only contains allowed letters
      layout.error <- FALSE
      for (i in 1:nchar(table.layout)) {
        ch <- substring(table.layout,i,i)
        if (!(ch %in% c("=","-","!","l","d","m","c","#","b","t","o","a","s","n"))) (layout.error <- TRUE)
      }
      if (layout.error) { error.present <- c(error.present, "% Error: Invalid characters in 'table.layout'. See package documentation. \n") }
    }  
  
    if ((!is.character(omit.table.layout)) & (!is.null(omit.table.layout))) { error.present <- c(error.present, "% Error: Argument 'omit.table.layout' must be of type 'character.'\n") }
    if ((length(omit.table.layout) != 1) & (!is.null(omit.table.layout)))  { error.present <- c(error.present, "% Error: Argument 'omit.table.layout' must be of length 1.'\n") }
    if (is.character(omit.table.layout) & (length(omit.table.layout)==1)) {   # test if report only contains allowed letters
      layout.error <- FALSE
      for (i in 1:nchar(omit.table.layout)) {
        ch <- substring(omit.table.layout,i,i)
        if (!(ch %in% c("=","-","!","l","d","m","c","#","b","t","o","a","s","n"))) (layout.error <- TRUE)
      }
      if (layout.error) { error.present <- c(error.present, "% Error: Invalid characters in 'omit.table.layout'. See package documentation. \n") }
    }  
  
    if (!is.character(table.placement)) { error.present <- c(error.present, "% Error: Argument 'table.placement' must be of type 'character.'\n") }
    if (length(table.placement) != 1)  { error.present <- c(error.present, "% Error: Argument 'table.placement' must be of length 1.'\n") }
    if (is.character(table.placement) & (length(table.placement)==1)) {   # test if table.placement only contains allowed letters
      tp.error <- FALSE
      for (i in 1:nchar(table.placement)) {
        ch <- substring(table.placement,i,i)
        if (!(ch %in% c("h","t","b","p","!","H"))) (tp.error <- TRUE)
      }
      if (tp.error) { error.present <- c(error.present, "% Error: Argument 'table.placement' can only consist of \"h\",\"t\",\"b\",\"p\",\"!\",\"H\".\n") }
    }
  
    if ((!is.character(report)) & (!is.null(report))) { error.present <- c(error.present, "% Error: Argument 'report' must be of type 'character.'\n") }
    if ((length(report) != 1) & (!is.null(report)))  { error.present <- c(error.present, "% Error: Argument 'report' must be of length 1.'\n") }
    if (is.character(report) & (length(report)==1)) {   # test if report only contains allowed letters
      report.error <- FALSE
      for (i in 1:nchar(report)) {
        ch <- substring(report,i,i)
        if (!(ch %in% c("v","c","s","t","p","*"))) (report.error <- TRUE)
      }
      if (report.error) { error.present <- c(error.present, "% Error: Argument 'report' can only consist of \"v\",\"c\",\"s\",\"t\",\"p\",\"*\".\n") }
    }  
  
    if (!is.logical(zero.component)) { error.present <- c(error.present, "% Error: Argument 'zero.component' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(zero.component) != 1) { error.present <- c(error.present, "% Error: Argument 'zero.component' must be of length 1.'\n") }
    
    if (!is.logical(summary.logical)) { error.present <- c(error.present, "% Error: Argument 'summary.logical' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(summary.logical) != 1) { error.present <- c(error.present, "% Error: Argument 'summary.logical' must be of length 1.'\n") }

    if (!is.logical(nobs)) { error.present <- c(error.present, "% Error: Argument 'nobs' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(nobs) != 1) { error.present <- c(error.present, "% Error: Argument 'nobs' must be of length 1.'\n") }
    
    if (!is.logical(mean.sd)) { error.present <- c(error.present, "% Error: Argument 'mean.sd' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(mean.sd) != 1) { error.present <- c(error.present, "% Error: Argument 'mean.sd' must be of length 1.'\n") }
    
    if (!is.logical(min.max)) { error.present <- c(error.present, "% Error: Argument 'min.max' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(min.max) != 1) { error.present <- c(error.present, "% Error: Argument 'min.max' must be of length 1.'\n") }
    
    if (!is.logical(median)) { error.present <- c(error.present, "% Error: Argument 'median' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(median) != 1) { error.present <- c(error.present, "% Error: Argument 'median' must be of length 1.'\n") }
    
    if (!is.logical(iqr)) { error.present <- c(error.present, "% Error: Argument 'iqr' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(iqr) != 1) { error.present <- c(error.present, "% Error: Argument 'iqr' must be of length 1.'\n") }
    
    ## decide what style to use here: start with all settings, and then make adjustment based on desired journal

    # initialize pseudo-global variables at NULL
    .summary.object <- NULL
    .global.dependent.variables.written <- NULL
    .global.coefficients <- NULL
    .format.model.left <- NULL
    .format.model.right <- NULL
    .which.variable.label <- NULL
    .return.value <- NULL
    .publish.horizontal.line <- NULL
    .table.part.published <- NULL
    .format.omit.table <- NULL

    # info about the package and author
    .global.package.name <- "stargazer"
    .global.package.version <- "5.2"
    .global.package.author.name <- "Marek Hlavac"
    .global.package.author.affiliation <- "Harvard University"
    .global.package.author.email <- "hlavac at fas.harvard.edu"
    
    # statistics (.global variables)
    .global.formulas.rhs <- NULL
    .global.models <- NULL
    .global.dependent.variables <- NULL
    .global.coefficient.variables <- NULL
    .global.coef.vars.by.model <- NULL  ## list of coefficient variables by model - to be used by omit, omit.labels, etc
    .global.std.errors <- NULL
    .global.ci.lb <- NULL
    .global.ci.rb <- NULL
    .global.t.stats <- NULL
    .global.p.values <- NULL
    .global.N <- NULL
    .global.LL <- NULL
    .global.R2 <- NULL
    .global.mills <- NULL
    .global.max.R2 <- NULL # maximum possible R2 
    .global.adj.R2 <- NULL
    .global.AIC <- NULL
    .global.BIC <- NULL
    .global.scale <- NULL   # estimated scale parameter (gee)
    .global.UBRE <- NULL    # UBRE score (GAM)
    .global.sigma2 <- NULL  # sigma2 from arima
    .global.theta <- NULL   # theta from negative binomial
    .global.rho <- NULL
  
    .global.sel.equation <- NULL # selection equation, as opposed to default outcome equation, in heckit and 
    .global.zero.component <- NULL # zero, as opposed to count, component in hurdle and zeroinfl
    
    # with degrees of freedom
    .global.SER <- NULL   # residual standard error; standard error of the regression
    .global.F.stat <- NULL # F-statistic for the regression
    .global.chi.stat <- NULL  # chi-squared statistic
    .global.wald.stat <- NULL # Wald test statistic (for coxph)
    .global.lr.stat <- NULL  # LR test statistic (for coxph)
    .global.logrank.stat <- NULL # Score (logrank) test (for coxph)
    .global.null.deviance <- NULL 
    .global.residual.deviance <- NULL
    
    # intercept strings
    .global.intercept.strings <- c("(Intercept)", "(intercept)","Intercept")
    
    # .formatting: Default
    .format.space.size <- "-1.8ex"
    
    .format.dependent.variable.text <- "\\textit{Dependent variable:}"
    .format.dependent.variable.text.underline <- TRUE
    .format.dependent.variable.text.on <- TRUE
    
    .format.dep.var.labels <- NULL
    .format.covariate.labels <- NULL
    .format.add.lines <- NULL
    
    .format.dependent.variables.text <- ""
    .format.underline.dependent.variables <- TRUE
    .format.dependent.variables.left <- ""
    .format.dependent.variables.right <- ""
    .format.dependent.variables.capitalize <- FALSE
    
    .format.ordered.intercepts <- TRUE
  
    # column labels
    .format.column.left <- ""
    .format.column.right <- ""
    
    # model numbers
    .format.model.numbers <- TRUE
        
    # common headers for multiple columns?
    .format.multicolumn <- TRUE
    
    # names for models
    .format.model.names.include <- TRUE
    .format.model.names <- NULL
    .format.model.names <- cbind(c("aov","ANOVA",""), c("arima","ARIMA",""), c("Arima","ARIMA",""), c("blogit","bivariate","logistic"))
    .format.model.names <- cbind(.format.model.names, c("bprobit","bivariate","probit"), c("betareg", "beta",""), c("chopit","compound hierarchical","ordered probit"))
    .format.model.names <- cbind(.format.model.names, c("clm","cumulative","link"), c("censReg", "censored", "regression"), c("cloglog.net","network compl.","log log"), c("clogit","conditional","logistic"), c("coxph","Cox","prop. hazards"))
    .format.model.names <- cbind(.format.model.names, c("dynlm","dynamic","linear"), c("lagsarlm","spatial","autoregressive"), c("errorsarlm","spatial","error"))
    .format.model.names <- cbind(.format.model.names, c("ei.dynamic","Quinn dynamic","ecological inference"), c("ei.hier","$2 \times 2$ hierarchical","ecological inference"))
    .format.model.names <- cbind(.format.model.names, c("ei.RxC","hierarchical multinominal-Dirichlet","ecological inference"), c("exp","exponential",""), c("ergm","exponential family","random graph"))
    .format.model.names <- cbind(.format.model.names, c("factor.bayes","Bayesian","factor analysis"), c("factor.mix","mixed data","factor analysis"))
    .format.model.names <- cbind(.format.model.names, c("factor.ord","ordinal data","factor analysis"), c("fGARCH","GARCH",""), c("gamma","gamma",""))
    .format.model.names <- cbind(.format.model.names, c("gamma.gee","gamma generalized","estimating equation"), c("gamma.mixed","mixed effects","gamma"))
    .format.model.names <- cbind(.format.model.names, c("gamma.net","network","gamma"), c("gamma.survey","survey-weighted","gamma"), c("glmrob","robust","GLM"), c("gls","generalized","least squares"))
    .format.model.names <- cbind(.format.model.names, c("gmm","GMM",""), c("rem.dyad", "relational", "event (dyadic)"))
    .format.model.names <- cbind(.format.model.names, c("irt1d","IRT","(1-dim.)"), c("irtkd","IRT","(k-dim.)"))
    .format.model.names <- cbind(.format.model.names, c("logit","logistic",""), c("logit.bayes","Bayesian","logistic"))
    .format.model.names <- cbind(.format.model.names, c("logit.gam","GAM","(logistic)"), c("logit.gee","logistic generalized","estimating equation"))
    .format.model.names <- cbind(.format.model.names, c("logit.mixed","mixed effects","logistic"), c("logit.net","network","logistic"))
    .format.model.names <- cbind(.format.model.names, c("logit.survey","survey-weighted","logistic"), c("lognorm","log-normal",""))
    .format.model.names <- cbind(.format.model.names, c("lmer","linear","mixed-effects"), c("glmer","generalized linear","mixed-effects"), c("nlmer","non-linear","mixed-effects"))
    .format.model.names <- cbind(.format.model.names, c("ls","OLS",""), c("ls.mixed","mixed effect","linear"), c("lme","linear","mixed effects"), c("lmrob","MM-type","linear"))
    .format.model.names <- cbind(.format.model.names, c("ls.net","network","least squares"), c("mlogit","multinomial","logistic"), c("mnlogit","multinomial","logit"))
    .format.model.names <- cbind(.format.model.names, c("mlogit.bayes","Bayesian","multinomial logistic"), c("negbin","negative","binomial"), c("normal","normal",""))
    .format.model.names <- cbind(.format.model.names, c("multinom","multinomial log-linear","(neural networks)"), c("nlme","non-linear","mixed effects"))
    .format.model.names <- cbind(.format.model.names, c("normal.bayes","Bayesian","normal"), c("normal.gam","GAM","(continuous)"))
    .format.model.names <- cbind(.format.model.names, c("normal.gee","normal generalized","estimating equation"), c("normal.net","network","normal"))
    .format.model.names <- cbind(.format.model.names, c("normal.survey","survey-weighted","normal"), c("ologit","ordered","logistic"))
    .format.model.names <- cbind(.format.model.names, c("oprobit","ordered","probit"), c("oprobit.bayes","Bayesian","ordered probit"))
    .format.model.names <- cbind(.format.model.names, c("pmg","mean","groups"), c("poisson","Poisson",""), c("poisson.bayes","Bayesian","Poisson"))
    .format.model.names <- cbind(.format.model.names, c("poisson.gam","GAM","(count)"), c("poisson.mixed","mixed effects","Poisson"))
    .format.model.names <- cbind(.format.model.names, c("poisson.survey","survey-weighted","Poisson"), c("poisson.gee","Poisson generalized","estimation equation"))
    .format.model.names <- cbind(.format.model.names, c("probit","probit",""), c("probit.bayes","Bayesian","probit"))
    .format.model.names <- cbind(.format.model.names, c("probit.gam","GAM","(probit)"), c("probit.gee","probit generalized","estimating equation"))
    .format.model.names <- cbind(.format.model.names, c("probit.mixed","mixed effects","probit"), c("probit.net","network","probit"))
    .format.model.names <- cbind(.format.model.names, c("probit.survey","survey-weighted","probit"), c("relogit","rare events","logistic"))
    .format.model.names <- cbind(.format.model.names, c("rq","quantile","regression"))
    .format.model.names <- cbind(.format.model.names, c("rlm","robust","linear"), c("sur","SUR",""), c("threesls","3SLS",""))
    .format.model.names <- cbind(.format.model.names, c("tobit","Tobit",""), c("tobit(AER)","Tobit",""), c("tobit.bayes","Bayesian","Tobit"))
    .format.model.names <- cbind(.format.model.names, c("twosls","2SLS",""), c("weibull","Weibull",""))
    .format.model.names <- cbind(.format.model.names, c("zeroinfl","zero-inflated","count data"), c("hurdle","hurdle",""))
    .format.model.names <- cbind(.format.model.names, c("plm","panel","linear"), c("pgmm","panel","GMM"), c("ivreg","instrumental","variable"))
    .format.model.names <- cbind(.format.model.names, c("coxreg","Cox",""), c("mlreg","ML","prop. hazards"), c("weibreg","Weibull",""))
    .format.model.names <- cbind(.format.model.names, c("aftreg","accelerated"," failure time"), c("phreg","parametric","prop. hazards"))
    .format.model.names <- cbind(.format.model.names, c("bj","Buckley-James",""), c("cph","Cox",""), c("Gls","generalized","least squares"), c("lrm","logistic",""))
    .format.model.names <- cbind(.format.model.names, c("ols","OLS",""), c("psm","parametric","survival"), c("Rq","quantile","regression"))
    .format.model.names <- cbind(.format.model.names, c("hetglm","heteroskedastic","GLM"), c("coeftest","coefficient","test"))
    .format.model.names <- cbind(.format.model.names, c("heckit","Heckman","selection"), c("selection","selection",""))
    .format.model.names <- cbind(.format.model.names, c("probit.ss","probit",""), c("binaryChoice","binary","choice"))
    .format.model.names <- cbind(.format.model.names, c("brglm","GLM","(bias reduction)"), c("maBina","binary model","(marginal effect)"))
    .format.model.names <- cbind(.format.model.names, c("mclogit","mixed","conditional logit"))
  
    # if you use, say, glm() that does not correspond to one of the pre-defined models, put this as family and link
    .format.model.function <- TRUE
    .format.model.family <- ""
    .format.model.dist <- ""
    .format.model.link <- "link = "
    
    ## names for journal/output styles
    # economics
    .journal.style.names <- cbind(c("aer","American Economic Review"), c("qje","Quarterly Journal of Economics"), c("econometrica","Econometrica"))
    .journal.style.names <- cbind(.journal.style.names, c("jpe","Journal of Political Economy"), c("jel","Journal of Economic Literature"))
    .journal.style.names <- cbind(.journal.style.names, c("jep","Journal of Economic Perspestives"))
    
    .format.coefficient.variables.capitalize <- FALSE
    .format.coefficient.variables.left <- ""
    .format.coefficient.variables.right <- ""
    .format.coefficient.table.parts <- c("variable name","coefficient*","standard error"," ")
    
    ## .formatting of numeric output
    # keep initial zeros?
    .format.initial.zero <- TRUE
    # if all zeros, keep going until you find a non-zero digit
    .format.until.nonzero.digit <- TRUE
    .format.max.extra.digits <- 2
    
    ## threshholds for the stars
    .format.stars <- "*"
    .format.cutoffs <- c(0.1, 0.05, 0.01)
    
    .format.std.errors.left <- "("
    .format.std.errors.right <- ")"
    
    .format.p.values.left <- "p = "
    .format.p.values.right <- ""
    
    .format.t.stats.left <- "t = "
    .format.t.stats.right <- ""
    
    .format.models.text <- ""
    .format.models.left <- "\\textit{"
    .format.models.right <- "}"
    .format.underline.models <- FALSE
    .format.models.skip.if.one <- TRUE # skip models section if only one model in table?
    .format.object.names <- FALSE
    
    .format.numbers.text <- ""
    .format.numbers.left <- "("
    .format.numbers.right <- ")"
    .format.numbers.roman <- FALSE
    
    .format.digit.separator.where <- c(3)    # how 'often' to separate digits (e.g., thousands separator = 3)
    .format.digit.separator <- ","
    .format.ci.separator <- ", "
    .format.round.digits <- 3
    # for decimal comma use: .format.decimal.character <- "{,}"
    .format.decimal.character <- "."
    .format.dec.mark.align <- FALSE
  
    # degrees of freedom - report or not?
    .format.df <- TRUE
    
    .format.table.parts <- c("=!","dependent variable label","dependent variables","models","colums","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","scale","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*(p)","chi2(df)*(p)","Wald(df)*(p)","LR(df)*(p)","logrank(df)*(p)","null deviance(df)","residual deviance(df)","=!","notes")
    
    .format.omit.regexp <- NULL
    .format.omit.labels <- NULL
    .format.omit.yes <- "Yes"
    .format.omit.no <- "No"
  
    .format.keep.regexp <- NULL
    
    .format.N <- "Observations"
    .format.LL <- "Log Likelihood"
    .format.R2 <- "R$^{2}$"
    .format.max.R2 <- "Max. Possible R$^{2}$"
    .format.adj.R2 <- "Adjusted R$^{2}$"
    .format.scale <- "Scale Parameter"
    .format.UBRE <- "UBRE"
    .format.rho <- "$\\rho$"
    .format.mills <- "Inverse Mills Ratio"
    .format.AIC <- "Akaike Inf. Crit."
    .format.BIC <- "Bayesian Inf. Crit."
    .format.sigma2 <- "$\\sigma^{2}$"
    .format.theta <- "$\\theta$"
    
    .format.SER <- "Residual Std. Error"
    .format.F.stat <- "F Statistic"
    .format.chi.stat <- "$\\chi^{2}$"
    .format.wald.stat <- "Wald Test"
    .format.lr.stat <- "LR Test"
    .format.logrank.stat <- "Score (Logrank) Test"
    .format.null.deviance <- "Null Deviance"
    .format.residual.deviance <- "Residual Deviance"
    
    .format.df.left <- "(df = "
    .format.df.right <- ")"
    .format.df.separator <- "; "
    .format.intelligent.df <- TRUE
    
    # this is for se, tstat, p.values at the bottom of the table, by statistics
    .format.se.left <- " ("
    .format.se.right <- ")"
    .format.tstat.left <- " (z = "
    .format.tstat.right <- ")"
    .format.p.value.left <- " (p = "
    .format.p.value.right <- ")"
    
    .format.intercept.name <- "Constant"
    .format.intercept.bottom <- TRUE
    .format.note <- "\\textit{Note:} "
    .format.note.alignment <- "r"
    .format.note.content <- c("$^{*}$p$<$[0.*]; $^{**}$p$<$[0.**]; $^{***}$p$<$[0.***]")
    
    #### summary statistic table
    .format.s.statistics.names <- cbind(c("n","N"), c("nmiss","missing"), c("mean","Mean"), c("sd","St. Dev."), c("median","Median"), c("min","Min"), c("max","Max"), c("mad","Median Abs. Dev."), c("p","Pctl(!)"))
    .format.s.stat.parts <- c("=!","stat names","-","statistics1","-!","notes")
    .format.s.statistics.list <- c("n","mean","sd","min","p25","median","p75","max")
    
    .format.s.statistics.names.left <- ""
    .format.s.statistics.names.right <- ""
    .format.s.statistics.names.label <- "Statistic"
    
    .format.s.coefficient.variables.capitalize <- FALSE
    .format.s.coefficient.variables.left <- ""
    .format.s.coefficient.variables.right <- ""
    
    .format.s.round.digits <- 3
    
    .format.s.note <- ""
    .format.s.note.alignment <- "l"
    .format.s.note.content <- NULL

    ####
    .adjust.settings.style(style)
    
    # continue only if no errors
    if(length(error.present) != 1) { 
      return(invisible(
        if(suppress.errors) "" else error.present  
      ))
    }
      
    # summary statistic table or regular table of data frame contents
    if (!is.null(summary)) { 
      
      # make sure summary is as long as the number of objects
      if (length(summary) > how.many.objects) { summary <- summary[1:how.many.objects] }
      if (length(summary) < how.many.objects) { length(summary) <- how.many.objects }
      
      # fill in values of summary, if NA keep deafult
      for (i in 1:how.many.objects) {
        if (!is.na(summary[i])) {
          .global.summary[i] <- summary[i]
        }
        else if (i > 1) {  # if NA fill in previous value of summary
          .global.summary[i] <- summary[i-1]
        }
      }
    }
    
    
    ## use formatting arguments
    
    # header with name, version, etc.
    .format.header <- header
    
    # no empty lines? single row for coefficient and std.error/CI?
    .format.single.row <- single.row
    if (.format.single.row == TRUE) { .format.no.space <- TRUE }
    else { .format.no.space <- FALSE }
    if (!is.null(no.space)) { .format.no.space <- no.space }
    
    # font size
    .format.font.size <- font.size
    
    # floating, floating environment, etc.
    .format.floating <- float
    .format.floating.environment <- float.env
    .format.table.placement <- table.placement
    .format.column.sep.width <- column.sep.width
    
    # if not case-sensitive, transfer to lower case
    if (!is.null(digit.separate)) { digit.separate <- tolower(digit.separate) }
    
    # report df?
    .format.df <- df
    if (.format.df == FALSE) {
      .format.table.parts <- gsub("(df)", "", .format.table.parts, fixed=TRUE)
    }
    
    # column, dependent variable and covariate labels
    .format.column.labels <- column.labels
    .format.column.separate <- column.separate
    .format.covariate.labels <- covariate.labels
    .format.dep.var.labels <- dep.var.labels
    .format.add.lines <- add.lines
    
    if (dep.var.labels.include == FALSE) {
      .format.table.parts <- .format.table.parts[.format.table.parts!="dependent variables"] 
    }
    
    if (!is.null(dep.var.caption)) {
      if (dep.var.caption == "") {
        .format.table.parts <- .format.table.parts[.format.table.parts!="dependent variable label"]
      }
      else {
        .format.dependent.variable.text <- dep.var.caption
      }
    }
    
    # confidence intervals
    .format.ci <- ci
    .format.ci.level <- ci.level
    if (!is.null(ci.separator)) { .format.ci.separator <- ci.separator }
    if (!is.null(ci.custom)) { .format.ci <- TRUE }
    
    # omit
    .format.omit.regexp <- omit
    .format.omit.index <- omit
    if (is.character(omit)) { .format.omit.index <- NULL }
    if (is.numeric(omit)) { .format.omit.regexp <- NULL }
    
    .format.omit.labels <- omit.labels
    if (!is.null(omit.yes.no)) { 
      .format.omit.yes <- omit.yes.no[1]
      .format.omit.no <- omit.yes.no[2]
    }
    
    # keep
    .format.keep.regexp <- keep
    .format.keep.index <- keep
    if (is.character(keep)) { .format.keep.index <- NULL }
    if (is.numeric(keep)) { .format.keep.regexp <- NULL }
    
    # remove omitted statistics from table parts
    if (!is.null(omit.stat)) {
      .lower.omit.stat <- tolower(omit.stat)    # make it all lower-case
      if ("all" %in% .lower.omit.stat) { .lower.omit.stat <- omit.stat.acceptable }
      if ("n" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="N"] }
      if ("rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="R-squared"] }
      if ("adj.rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="adjusted R-squared"] }
      if ("max.rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="max R-squared"] }
      if ("ll" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="log likelihood"] }
      if ("scale" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="scale"] }
      if ("sigma2" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="sigma2"] }        
      if ("theta" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,5)!="theta"] }
      if ("aic" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="AIC"] }
      if ("bic" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="BIC"] }
      if ("ubre" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="UBRE"] }
      if ("rho" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,3)!="rho"] }
      if ("mills" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,5)!="Mills"] }
      if ("ser" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,3)!="SER"] }
      if ("f" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,11)!="F statistic"] }
      if ("chi2" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,4)!="chi2"] }
      if ("wald" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,4)!="Wald"] }
      if ("lr" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,2)!="LR"] }
      if ("logrank" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,7)!="logrank"] }
      if ("null.dev" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,13)!="null deviance"] }
      if ("res.dev" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,17)!="residual deviance"] }
    }
    
    # keep statistics in the table
    if (!is.null(keep.stat)) {
      .lower.keep.stat <- tolower(keep.stat)    # make it all lower-case
      
      # do this by omitting everything except what you keep
      .lower.omit.stat <- c("n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho","Mills","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")
      .lower.omit.stat <- .lower.omit.stat[!(.lower.omit.stat %in% .lower.keep.stat) ]
      
      if ("n" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="N"] }
      if ("rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="R-squared"] }
      if ("adj.rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="adjusted R-squared"] }
      if ("max.rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="max R-squared"] }
      if ("ll" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="log likelihood"] }
      if ("scale" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="scale"] }
      if ("sigma2" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="sigma2"] }        
      if ("theta" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,5)!="theta"] }
      if ("aic" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="AIC"] }
      if ("bic" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="BIC"] }
      if ("ubre" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="UBRE"] }
      if ("rho" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,3)!="rho"] }
      if ("mills" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,5)!="Mills"] }
      if ("ser" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,3)!="SER"] }
      if ("f" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,11)!="F statistic"] }
      if ("chi2" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,4)!="chi2"] }
      if ("wald" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,4)!="Wald"] }
      if ("lr" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,2)!="LR"] }
      if ("logrank" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,7)!="logrank"] }
      if ("null.dev" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,13)!="null deviance"] }
      if ("res.dev" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,17)!="residual deviance"] }
    }
    
    # keep statistics in table parts
    if (!is.null(keep.stat)) {
      .lower.keep.stat <- tolower(keep.stat)    # make it all lower-case
      keep.stat.acceptable <- c("all","n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho(se)*","Mills(se)*","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")     # list of statistic codes that are acceptable
      remove.stats <- keep.stat.acceptable[!(keep.stat.acceptable %in% .lower.keep.stat)]
      .format.table.parts <- .format.table.parts[!(.format.table.parts %in% remove.stats)]
    }
    
    # digits, initial.zeros, decimal characters
    if (!is.null(decimal.mark)) { .format.decimal.character <- decimal.mark }
    if (!is.null(align)) { .format.dec.mark.align <- align }
    if (!is.null(digit.separator)) { .format.digit.separator <- digit.separator }
    if (!is.null(initial.zero)) { .format.initial.zero <- initial.zero }
    
    if (!is.null(digit.separate)) { 
      if (digit.separate=="lakh") { .format.digit.separator.where <- c(3,2) }  # lakhs 
      else if ((digit.separate=="china") | (digit.separate=="japan")) { .format.digit.separator.where <- 4 }
      else { .format.digit.separator.where <- digit.separate}
    }
    
    if (!is.null(digits)) { 
      .format.round.digits <- digits 
      .format.s.round.digits <- digits
    }
    
    if (!is.null(digits.extra)) { 
      .format.max.extra.digits <- digits.extra
      if (digits.extra>=1) { .format.until.nonzero.digit <- TRUE }
      else ( .format.until.nonzero.digit <- FALSE )
    }
    
    # intercept top and bottom
    if (!is.null(intercept.top)) { .format.intercept.top <- intercept.top }
    if (!is.null(intercept.bottom)) { .format.intercept.bottom <- intercept.bottom }
    
    # model names, numbers and multicolumn
    if (!is.null(model.names)) { 
      .format.model.names.include <- model.names 
      if (model.names == TRUE) { .format.models.skip.if.one <- FALSE }
    }    
    if (!is.null(model.numbers)) { .format.model.numbers <- model.numbers }
    .format.multicolumn <- multicolumn
    
    # object names
    .format.object.names <- object.names
    
    # report coefs, std errs, t, p?
    if (!is.null(report)) {
      .format.coefficient.table.parts <- NULL
      for (i in 1:nchar(report)) {
        component.letter <- substr(report, i, i)
        if (component.letter == "v") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "variable name") }
        if (component.letter == "c") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "coefficient") }
        if (component.letter == "s") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "standard error") }
        if (component.letter == "t") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "t-stat") }
        if (component.letter == "p") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "p-value") }
        if ((component.letter == "*") & (i > 1)) { 
          l <- length(.format.coefficient.table.parts)
          if ((.format.coefficient.table.parts[l] != "variable name") & (substr(report,i-1,i-1) != "*")) {
            .format.coefficient.table.parts[l] <- paste(.format.coefficient.table.parts[l],"*",sep="")
          }
        }
      }
      .format.coefficient.table.parts <- append(.format.coefficient.table.parts, " ")
    }
    
    
    # significance stars
    if (!is.null(star.cutoffs)) { 
      # assign cutoff values
      .format.cutoffs <- star.cutoffs
    }
    
    if (!is.null(star.char)) { 
      .format.stars <- star.char
    }
    
    for (i in 1:length(.format.cutoffs)) {
      if (is.na(.format.stars[i])) {
        .format.stars[i] <- paste(rep(.format.stars[1], i), sep="", collapse="")
      }  
    }
    .format.stars <- .format.stars[1:length(.format.cutoffs)]
    
    # selection equation
    .global.sel.equation <- selection.equation
    
    # colnames and rownames
    if (!is.null(rownames)) { .format.rownames <- rownames }
    if (!is.null(colnames)) { .format.colnames <- colnames }
    
    # zero vs. count component
    .global.zero.component <- zero.component
    
    # notes
    
    replace.dec.mark <- function(s) { return (gsub(".", .format.decimal.character, s, fixed=TRUE))}
    
    # replace star cutoffs in the notes section
    for (i in 1:length(.format.cutoffs)) {
      if (!is.na(.format.stars[i])) {
        star.string <- paste(rep("*", i), sep="", collapse="")
        .format.note.content <- gsub(paste("[.",star.string,"]",sep=""), replace.dec.mark(gsub("^[0]+", "",.format.cutoffs[i])), .format.note.content, fixed=TRUE)  
        .format.note.content <- gsub(paste("[0.",star.string,"]",sep=""), replace.dec.mark(.format.cutoffs[i]), .format.note.content, fixed=TRUE)
        .format.note.content <- gsub(paste("[",star.string,"]",sep=""), replace.dec.mark(.format.cutoffs[i]*100), .format.note.content, fixed=TRUE)        
      }
    }
    
    
    if (!is.null(notes)) { 
      if (notes.append == TRUE) {
        .format.note.content <- c(.format.note.content, notes)
        .format.s.note.content <- c(.format.s.note.content, notes)
      }
      else {
        .format.note.content <- notes
        .format.s.note.content <- notes
      }
    }
    if (!is.null(notes.align)) { 
      .format.note.alignment <- notes.align 
      .format.s.note.alignment <- notes.align
    }
    
    if (!is.null(notes.label)) { 
      .format.note <- notes.label
      .format.s.note <- notes.label
    }    
    
    # ordered probit/logit, etc. - report intercepts?
    .format.ordered.intercepts <- ord.intercepts
    
    # perl-compatible regular expressions?
    .format.perl <- perl
    
    # standard error for quantile regression
    .format.rq.se <- rq.se
    
    # report logical variables in summary statistics tables?
    .format.summ.logical <- summary.logical
    
    # summary statistics - what statistics to report - !!! this needs to come before summary.stat and omit.summary.stat
    if (!nobs) { .format.s.statistics.list <- .format.s.statistics.list[.format.s.statistics.list!="n"] }
    if (!mean.sd) { .format.s.statistics.list <- .format.s.statistics.list[(.format.s.statistics.list!="mean")&(.format.s.statistics.list!="sd")]}
    if (!min.max) { .format.s.statistics.list <- .format.s.statistics.list[(.format.s.statistics.list!="min")&(.format.s.statistics.list!="max")]}
    if (!median) { .format.s.statistics.list <- .format.s.statistics.list[.format.s.statistics.list!="median"] }
    if (!iqr) { .format.s.statistics.list <- .format.s.statistics.list[(.format.s.statistics.list!="p25")&(.format.s.statistics.list!="p75")]}
    
    # keep summary statistics
    if (!is.null(summary.stat)) {
      .lower.keep.summary.stat <- tolower(summary.stat)    # make it all lower-case
      .format.s.statistics.list <- .lower.keep.summary.stat
    } 
    
    # remove omitted statistics from table parts
    if (!is.null(omit.summary.stat)) {
      .lower.omit.summary.stat <- tolower(omit.summary.stat)    # make it all lower-case
      .format.s.statistics.list <- .format.s.statistics.list[!(.format.s.statistics.list %in% .lower.omit.summary.stat)]
    }
    
    # table layout
    .format.table.parts.nonstat <- c("=","-","-!","=!","dependent variable label",
                                     "dependent variables","models","columns","numbers",
                                     "objects","coefficients","omit","additional","notes")  
    # these are the non-model statistics parts of the table
    
    if (!is.null(table.layout)) {
      .format.table.parts.new <- NULL
      for (i in 1:nchar(table.layout)) {
        component.letter <- substr(table.layout, i, i)
        if (component.letter == "=") { .format.table.parts.new <- append(.format.table.parts.new, "=") }
        if (component.letter == "-") { .format.table.parts.new <- append(.format.table.parts.new, "-") }
        if ((component.letter == "!") & (i > 1)) { 
          if (.format.table.parts.new[i-1] %in% c("-","=")) {
            .format.table.parts.new[i-1] <- paste(.format.table.parts.new[i-1], "!", sep="")
          }
        }
        if (component.letter == "l") { .format.table.parts.new <- append(.format.table.parts.new, "dependent variable label") }
        if (component.letter == "d") { .format.table.parts.new <- append(.format.table.parts.new, "dependent variables") }
        if (component.letter == "m") { 
          .format.table.parts.new <- append(.format.table.parts.new, "models") 
          .format.model.names.include <- TRUE 
        }
        if (component.letter == "c") { .format.table.parts.new <- append(.format.table.parts.new, "columns") }
        if (component.letter == "#") { 
          .format.table.parts.new <- append(.format.table.parts.new, "numbers")
          .format.model.numbers <- TRUE
        }  
        if (component.letter == "b") { 
          .format.table.parts.new <- append(.format.table.parts.new, "objects") 
          .format.object.names <- TRUE
        }  
        if (component.letter == "t") { .format.table.parts.new <- append(.format.table.parts.new, "coefficients") }  
        if (component.letter == "o") { .format.table.parts.new <- append(.format.table.parts.new, "omit") }  
        if (component.letter == "a") { .format.table.parts.new <- append(.format.table.parts.new, "additional") }  
        if (component.letter == "n") { .format.table.parts.new <- append(.format.table.parts.new, "notes") }  
        if (component.letter == "s") { 
          .format.table.parts.new <- append(.format.table.parts.new, 
                                            .format.table.parts[!(.format.table.parts %in% .format.table.parts.nonstat)]) 
        }
        
      }
      .format.table.parts <- .format.table.parts.new
    }
    
    # now omit table parts
    if (!is.null(omit.table.layout)) {
      for (i in 1:nchar(omit.table.layout)) {
        component.letter <- substr(omit.table.layout, i, i)
        if (component.letter == "=") { .format.table.parts <- .format.table.parts[.format.table.parts!="="] }
        if (component.letter == "-") { .format.table.parts <- .format.table.parts[.format.table.parts!="-"] }
        if ((component.letter == "!") & (i > 1)) {
          if (substr(omit.table.layout, i-1, i-1) == "=") { .format.table.parts <- .format.table.parts[.format.table.parts!="=!"] }
          if (substr(omit.table.layout, i-1, i-1) == "-") { .format.table.parts <- .format.table.parts[.format.table.parts!="-!"] }
        }
        if (component.letter == "l") { .format.table.parts <- .format.table.parts[.format.table.parts!="dependent variable label"] }
        if (component.letter == "d") { .format.table.parts <- .format.table.parts[.format.table.parts!="dependent variables"] }
        if (component.letter == "m") { .format.table.parts <- .format.table.parts[.format.table.parts!="models"] }
        if (component.letter == "c") { .format.table.parts <- .format.table.parts[.format.table.parts!="columns"] }
        if (component.letter == "#") { .format.table.parts <- .format.table.parts[.format.table.parts!="numbers"] }
        if (component.letter == "b") { .format.table.parts <- .format.table.parts[.format.table.parts!="objects"] }
        if (component.letter == "t") { .format.table.parts <- .format.table.parts[.format.table.parts!="coefficients"] }
        if (component.letter == "o") { .format.table.parts <- .format.table.parts[.format.table.parts!="omit"] }
        if (component.letter == "a") { .format.table.parts <- .format.table.parts[.format.table.parts!="additional"] }
        if (component.letter == "n") { .format.table.parts <- .format.table.parts[.format.table.parts!="notes"] }
        if (component.letter == "s") { .format.table.parts <- .format.table.parts[.format.table.parts %in% .format.table.parts.nonstat] }          
      }
    }
    
    
    # intelligent division of regression tables vs. summary statistics tables
    regression.table.objects <- NULL
    number.of.table <- 0
    title.table <- NULL
    label.table <- NULL
    for (i in seq(1:how.many.objects)) {
      if (is.data.frame(objects[[i]])==TRUE) {
        if (!is.null(regression.table.objects)) { 
          number.of.table <- number.of.table + 1    # allows for multiple table titles and labels
          
          if (!is.na(title[number.of.table])) { .format.title <- title[number.of.table] }
          else { .format.title <- title[length(title)] }
          
          if (!is.na(label[number.of.table])) { .format.label <- label[number.of.table] }
          else { .format.label <- label[length(label)] }
          
          if (type == "latex") {
            do.call(.stargazer.reg.table, as.list(objects[regression.table.objects]))  
            invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(do.call(.stargazer.reg.table, as.list(objects[regression.table.objects])),file=NULL)) )
          }
          else if ((type == "text") | (type == "html") | (type == "mmd") ) {
            latex.code <- c(latex.code, invisible(capture.output(do.call(.stargazer.reg.table, as.list(objects[regression.table.objects])),file=NULL)) )
          }
        }
        
        number.of.table <- number.of.table + 1
        if (!is.na(title[number.of.table])) { .format.title <- title[number.of.table] }
        else { .format.title <- title[length(title)] }
        
        if (!is.na(label[number.of.table])) { .format.label <- label[number.of.table] }
        else { .format.label <- label[length(label)] }
        
        if (.global.summary[i]==TRUE) {
          if (type == "latex") {
            .stargazer.summ.stat.table(objects[[i]])
            invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(.stargazer.summ.stat.table(objects[[i]]),file=NULL)) )
          }
          else if ((type == "text") | (type == "html") | (type == "mmd")) {
            latex.code <- c(latex.code, invisible(capture.output(.stargazer.summ.stat.table(objects[[i]]),file=NULL)) )
          }
        }
        else {
          if (type == "latex") {
            .stargazer.data.frame.table(objects[[i]])
            invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(.stargazer.data.frame.table(objects[[i]]),file=NULL)) )
          }
          else if ((type == "text") | (type == "html") | (type == "mmd")) {
            latex.code <- c(latex.code, invisible(capture.output(.stargazer.data.frame.table(objects[[i]]),file=NULL)) )
          }
        }
        regression.table.objects <- NULL
      }
      else {
        regression.table.objects <- c(regression.table.objects, i)
        .global.object.names <- .global.object.names.all[regression.table.objects]
      }
    }
    
    if (!is.null(regression.table.objects)) {	
      number.of.table <- number.of.table + 1
      if (!is.na(title[number.of.table])) { .format.title <- title[number.of.table] }
      else { .format.title <- title[length(title)] }
      
      if (!is.na(label[number.of.table])) { .format.label <- label[number.of.table] }
      else { .format.label <- label[length(label)] }
      
      if (type == "latex") {
        do.call(.stargazer.reg.table, as.list(objects[regression.table.objects]))  
        invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(do.call(.stargazer.reg.table, as.list(objects[regression.table.objects])),file=NULL)) )
      }
      else if ((type == "text") | (type == "html") | (type == "mmd")) {
        latex.code <- c(latex.code, invisible(capture.output(do.call(.stargazer.reg.table, as.list(objects[regression.table.objects])),file=NULL)) )
      }
    }
    
    # don't do text output or file outputs if there are errors
    if (type == "text") {
      .text.output(latex.code)
      invisible.output <- invisible(capture.output(.text.output(latex.code)))
    }
    else if (type == "html") {
      .html.output(latex.code)
      invisible.output <- invisible(capture.output(.html.output(latex.code)))
    }
    else if (type == "mmd") {
      .mmd.output(latex.code)
      invisible.output <- invisible(capture.output(.mmd.output(latex.code)))
    }
    
    if (length(out) >= 1) { 
      text.out <- invisible(capture.output(.text.output(latex.code)))
      html.out <- invisible(capture.output(.html.output(latex.code)))
      .output.file(out, latex.code, text.out, html.out, type, out.header) 
    }


    return(invisible(invisible.output))
}

