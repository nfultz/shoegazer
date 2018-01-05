.onAttach <- 
function(libname, pkgname) {
  packageStartupMessage("\nPlease cite as: \n")
  packageStartupMessage(" Hlavac, Marek (2015). stargazer: Well-Formatted Regression and Summary Statistics Tables.")
  packageStartupMessage(" R package version 5.2. http://CRAN.R-project.org/package=stargazer \n")
}

stargazer <-
  function(..., type = "latex", title="", style="default", summary=NULL, out=NULL, out.header=FALSE,
           column.labels=NULL, column.separate = NULL, covariate.labels=NULL, dep.var.caption=NULL, 
           dep.var.labels=NULL, dep.var.labels.include=TRUE, align=FALSE, coef=NULL, se=NULL, t=NULL, 
           p=NULL,  t.auto=TRUE, p.auto=TRUE, ci=FALSE, ci.custom=NULL, ci.level=0.95, ci.separator=NULL, 
           add.lines=NULL, apply.coef=NULL, apply.se=NULL, apply.t=NULL, apply.p=NULL, apply.ci=NULL, 
           colnames = NULL, column.sep.width = "5pt", 
           decimal.mark=NULL, df=TRUE, digit.separate=NULL, digit.separator=NULL, digits=NULL, digits.extra=NULL, 
           flip=FALSE,
           float=TRUE, float.env="table", font.size=NULL, header=TRUE, initial.zero=NULL, intercept.bottom=TRUE, 
           intercept.top=FALSE, keep=NULL, keep.stat=NULL, label="", model.names=NULL, model.numbers=NULL, 
           multicolumn=TRUE, no.space=NULL, notes=NULL, notes.align=NULL, notes.append=TRUE, notes.label=NULL, 
           object.names=FALSE,
           omit=NULL, omit.labels=NULL, omit.stat=NULL, omit.summary.stat=NULL, omit.table.layout=NULL,
           omit.yes.no=c("Yes","No"), order=NULL, ord.intercepts=FALSE, 
           perl=FALSE, report=NULL, rownames = NULL,
           rq.se = "nid", selection.equation=FALSE, single.row=FALSE, star.char=NULL, 
           star.cutoffs=NULL, suppress.errors=FALSE, table.layout=NULL, table.placement = "!htbp", zero.component=FALSE, 
           summary.logical=TRUE, summary.stat=NULL, nobs=TRUE, mean.sd=TRUE, min.max=TRUE, median=FALSE, 
           iqr=FALSE) {
  
    o <- options(warn=-1) 
    on.exit(options(o))  

###########################################

    ## invisible output
    invisible.output <- NULL
    latex.code <- NULL
    text.out <- NULL
    
    ## error handling
    error.present <- "\n"
    
	  fmt <- list()
	  gbl <- list()
    
    
    # get object names --- !!! CHECK ORDER
    object.names.string <- deparse(substitute(list(...))) ### for further processing to extract object names
    gbl$object.names.all <- .get.object.names(object.names.string)
  
	
    # get objects
    list.of.objects <- list(...)
    objects <- as.list(.get.objects(list.of.objects))
    how.many.objects <- length(objects)
	
  
    # should we include a summary statistics table when given a data frame
    gbl$summary <- rep(TRUE, times=how.many.objects)
    
    ## check if argument input is ok
    fmt$rownames <- TRUE
    fmt$colnames <- TRUE
  
    # flip the table?
    fmt$flip <- flip

    coef <- .turn.into.list(coef); 
    se <- .turn.into.list(se)
    t <- .turn.into.list(t); 
    p <- .turn.into.list(p)
    ci.custom <- .turn.into.list(ci.custom)
    add.lines <- .turn.into.list(add.lines)
    
    gbl["coef"] <- list(coef)
    gbl["se"] <- list(se)
    gbl["t"] <- list(t)
    gbl["p"] <- list(p)
    gbl["ci.custom"] <- list(ci.custom)
    gbl["add.lines"] <- list(add.lines)
    gbl["order"] <- list(order)
    
    gbl["apply.coef"] <- list(apply.coef)
    gbl["apply.se"]   <- list(apply.se)
    gbl["apply.t"]    <- list(apply.t)
    gbl["apply.p"]    <- list(apply.p)
    gbl["apply.ci"]   <- list(apply.ci)
    
              
    if (how.many.objects < 1) { error.present <- c(error.present, "% Error: At least one object is required.\n") }
    else {
      
      # identify objects
      for (i in seq(1:how.many.objects)) {
        
        if (is.data.frame(objects[[i]])) {
          obj.rownames <- rownames(objects[[i]])
          if (is.null(obj.rownames)) { fmt$rownames <- FALSE }
        }
        else if ((is.matrix(objects[[i]])) && (class(objects[[i]]) != "coeftest")) { 
          
          gbl$summary[i] <- FALSE   # content output default for matrices
          
          obj.rownames <- rownames(objects[[i]])
          obj.colnames <- colnames(objects[[i]])
          
          if (is.null(obj.rownames)) { 
            if (fmt$flip == FALSE) { fmt$rownames <- FALSE }
            else { fmt$colnames <- FALSE }
            obj.rownames <- as.character(c(1:nrow(objects[[i]])))
          }
          if (is.null(obj.colnames)) { 
            if (fmt$flip == FALSE) { fmt$colnames <- FALSE }
            else { fmt$rownames <- FALSE }
            obj.colnames <- as.character(c(1:ncol(objects[[i]])))
          }
          
          objects[[i]] <- as.data.frame(objects[[i]])
          colnames(objects[[i]]) <- obj.colnames
        }
        else if (is.vector(objects[[i]])) {
          
          gbl$summary[i] <- FALSE   # content output default for vectors
          
          obj.names <- names(objects[[i]])
          
          if (is.null(obj.names)) { 
            fmt$colnames <- FALSE
            fmt$rownames <- FALSE
            obj.names <- as.character(c(1:length(objects[[i]])))
          }
          
          objects[[i]] <- as.data.frame(t(objects[[i]]))
          names(objects[[i]]) <- obj.names
        
          if (fmt$flip == TRUE) { fmt$colnames <- FALSE } 
          else { fmt$rownames <- FALSE }
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
    gbl$dependent.variables.written <- NULL
    gbl$coefficients <- NULL
    fmt$model.left <- NULL
    fmt$model.right <- NULL
    .which.variable.label <- NULL
    .return.value <- NULL
    .publish.horizontal.line <- NULL
    .table.part.published <- NULL
    fmt$omit.table <- NULL

    # info about the package and author
    gbl$package.name <- "stargazer"
    gbl$package.version <- "5.2"
    gbl$package.author.name <- "Marek Hlavac"
    gbl$package.author.affiliation <- "Harvard University"
    gbl$package.author.email <- "hlavac at fas.harvard.edu"
    
    # statistics (.global variables)
    gbl$formulas.rhs <- NULL
    gbl$models <- NULL
    gbl$dependent.variables <- NULL
    gbl$coefficient.variables <- NULL
    gbl$coef.vars.by.model <- NULL  ## list of coefficient variables by model - to be used by omit, omit.labels, etc
    gbl$std.errors <- NULL
    gbl$ci.lb <- NULL
    gbl$ci.rb <- NULL
    gbl$t.stats <- NULL
    gbl$p.values <- NULL
    gbl$N <- NULL
    gbl$LL <- NULL
    gbl$R2 <- NULL
    gbl$mills <- NULL
    gbl$max.R2 <- NULL # maximum possible R2 
    gbl$adj.R2 <- NULL
    gbl$AIC <- NULL
    gbl$BIC <- NULL
    gbl$scale <- NULL   # estimated scale parameter (gee)
    gbl$UBRE <- NULL    # UBRE score (GAM)
    gbl$sigma2 <- NULL  # sigma2 from arima
    gbl$theta <- NULL   # theta from negative binomial
    gbl$rho <- NULL
  
    gbl$sel.equation <- NULL # selection equation, as opposed to default outcome equation, in heckit and 
    gbl$zero.component <- NULL # zero, as opposed to count, component in hurdle and zeroinfl
    
    # with degrees of freedom
    gbl$SER <- NULL   # residual standard error; standard error of the regression
    gbl$F.stat <- NULL # F-statistic for the regression
    gbl$chi.stat <- NULL  # chi-squared statistic
    gbl$wald.stat <- NULL # Wald test statistic (for coxph)
    gbl$lr.stat <- NULL  # LR test statistic (for coxph)
    gbl$logrank.stat <- NULL # Score (logrank) test (for coxph)
    gbl$null.deviance <- NULL 
    gbl$residual.deviance <- NULL
    
    # intercept strings
    gbl$intercept.strings <- c("(Intercept)", "(intercept)","Intercept")
    
    gbl$t.auto <- t.auto
    gbl$p.auto <- p.auto
    
    
    # .formatting: Default
    fmt$space.size <- "-1.8ex"
    
    fmt$dependent.variable.text <- "\\textit{Dependent variable:}"
    fmt$dependent.variable.text.underline <- TRUE
    fmt$dependent.variable.text.on <- TRUE
    
    fmt$dep.var.labels <- NULL
    fmt$covariate.labels <- NULL
    fmt$add.lines <- NULL
    
    fmt$dependent.variables.text <- ""
    fmt$underline.dependent.variables <- TRUE
    fmt$dependent.variables.left <- ""
    fmt$dependent.variables.right <- ""
    fmt$dependent.variables.capitalize <- FALSE
    
    fmt$ordered.intercepts <- TRUE
  
    # column labels
    fmt$column.left <- ""
    fmt$column.right <- ""
    
    # model numbers
    fmt$model.numbers <- TRUE
        
    # common headers for multiple columns?
    fmt$multicolumn <- TRUE
    
    # names for models
    fmt$model.names.include <- TRUE

    # if you use, say, glm() that does not correspond to one of the pre-defined models, put this as family and link
    fmt$model.function <- TRUE
    fmt$model.family <- ""
    fmt$model.dist <- ""
    fmt$model.link <- "link = "
    
    fmt$coefficient.variables.capitalize <- FALSE
    fmt$coefficient.variables.left <- ""
    fmt$coefficient.variables.right <- ""
    fmt$coefficient.table.parts <- c("variable name","coefficient*","standard error"," ")
    
    ## .formatting of numeric output
    # keep initial zeros?
    fmt$initial.zero <- TRUE
    # if all zeros, keep going until you find a non-zero digit
    fmt$until.nonzero.digit <- TRUE
    fmt$max.extra.digits <- 2
    
    ## threshholds for the stars
    fmt$stars <- "*"
    fmt$cutoffs <- c(0.1, 0.05, 0.01)
    
    fmt$std.errors.left <- "("
    fmt$std.errors.right <- ")"
    
    fmt$p.values.left <- "p = "
    fmt$p.values.right <- ""
    
    fmt$t.stats.left <- "t = "
    fmt$t.stats.right <- ""
    
    fmt$models.text <- ""
    fmt$models.left <- "\\textit{"
    fmt$models.right <- "}"
    fmt$underline.models <- FALSE
    fmt$models.skip.if.one <- TRUE # skip models section if only one model in table?
    fmt$object.names <- FALSE
    
    fmt$numbers.text <- ""
    fmt$numbers.left <- "("
    fmt$numbers.right <- ")"
    fmt$numbers.roman <- FALSE
    
    fmt$digit.separator.where <- c(3)    # how 'often' to separate digits (e.g., thousands separator = 3)
    fmt$digit.separator <- ","
    fmt$ci.separator <- ", "
    fmt$round.digits <- 3
    # for decimal comma use: fmt$decimal.character <- "{,}"
    fmt$decimal.character <- "."
    fmt$dec.mark.align <- FALSE
  
    # degrees of freedom - report or not?
    fmt$df <- TRUE
    
    fmt$table.parts <- c("=!","dependent variable label","dependent variables","models","colums","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","scale","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*(p)","chi2(df)*(p)","Wald(df)*(p)","LR(df)*(p)","logrank(df)*(p)","null deviance(df)","residual deviance(df)","=!","notes")
    
    fmt$omit.regexp <- NULL
    fmt$omit.labels <- NULL
    fmt$omit.yes <- "Yes"
    fmt$omit.no <- "No"
  
    fmt$keep.regexp <- NULL
    
    fmt$N <- "Observations"
    fmt$LL <- "Log Likelihood"
    fmt$R2 <- "R$^{2}$"
    fmt$max.R2 <- "Max. Possible R$^{2}$"
    fmt$adj.R2 <- "Adjusted R$^{2}$"
    fmt$scale <- "Scale Parameter"
    fmt$UBRE <- "UBRE"
    fmt$rho <- "$\\rho$"
    fmt$mills <- "Inverse Mills Ratio"
    fmt$AIC <- "Akaike Inf. Crit."
    fmt$BIC <- "Bayesian Inf. Crit."
    fmt$sigma2 <- "$\\sigma^{2}$"
    fmt$theta <- "$\\theta$"
    
    fmt$SER <- "Residual Std. Error"
    fmt$F.stat <- "F Statistic"
    fmt$chi.stat <- "$\\chi^{2}$"
    fmt$wald.stat <- "Wald Test"
    fmt$lr.stat <- "LR Test"
    fmt$logrank.stat <- "Score (Logrank) Test"
    fmt$null.deviance <- "Null Deviance"
    fmt$residual.deviance <- "Residual Deviance"
    
    fmt$df.left <- "(df = "
    fmt$df.right <- ")"
    fmt$df.separator <- "; "
    fmt$intelligent.df <- TRUE
    
    # this is for se, tstat, p.values at the bottom of the table, by statistics
    fmt$se.left <- " ("
    fmt$se.right <- ")"
    fmt$tstat.left <- " (z = "
    fmt$tstat.right <- ")"
    fmt$p.value.left <- " (p = "
    fmt$p.value.right <- ")"
    
    fmt$intercept.name <- "Constant"
    fmt$intercept.bottom <- TRUE
    fmt$note <- "\\textit{Note:} "
    fmt$note.alignment <- "r"
    fmt$note.content <- c("$^{*}$p$<$[0.*]; $^{**}$p$<$[0.**]; $^{***}$p$<$[0.***]")
    
    #### summary statistic table
    fmt$s.statistics.names <- cbind(c("n","N"), c("nmiss","missing"), c("mean","Mean"), c("sd","St. Dev."), c("median","Median"), c("min","Min"), c("max","Max"), c("mad","Median Abs. Dev."), c("p","Pctl(!)"))
    fmt$s.stat.parts <- c("=!","stat names","-","statistics1","-!","notes")
    fmt$s.statistics.list <- c("n","mean","sd","min","p25","median","p75","max")
    
    fmt$s.statistics.names.left <- ""
    fmt$s.statistics.names.right <- ""
    fmt$s.statistics.names.label <- "Statistic"
    
    fmt$s.coefficient.variables.capitalize <- FALSE
    fmt$s.coefficient.variables.left <- ""
    fmt$s.coefficient.variables.right <- ""
    
    fmt$s.round.digits <- 3
    
    fmt$s.note <- ""
    fmt$s.note.alignment <- "l"
    fmt$s.note.content <- NULL

    ####
    fmt <- .adjust.settings.style(style, fmt)
    
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
          gbl$summary[i] <- summary[i]
        }
        else if (i > 1) {  # if NA fill in previous value of summary
          gbl$summary[i] <- summary[i-1]
        }
      }
    }
    
    
    ## use formatting arguments
    
    # header with name, version, etc.
    fmt$header <- header
    
    # no empty lines? single row for coefficient and std.error/CI?
    fmt$single.row <- single.row
    if (fmt$single.row == TRUE) { fmt$no.space <- TRUE }
    else { fmt$no.space <- FALSE }
    if (!is.null(no.space)) { fmt$no.space <- no.space }
    
    # font size
    fmt$font.size <- font.size
    
    # floating, floating environment, etc.
    fmt$floating <- float
    fmt$floating.environment <- float.env
    fmt$table.placement <- table.placement
    fmt$column.sep.width <- column.sep.width
    
    # if not case-sensitive, transfer to lower case
    if (!is.null(digit.separate)) { digit.separate <- tolower(digit.separate) }
    
    # report df?
    fmt$df <- df
    if (fmt$df == FALSE) {
      fmt$table.parts <- gsub("(df)", "", fmt$table.parts, fixed=TRUE)
    }
    
    # column, dependent variable and covariate labels
    fmt$column.labels <- column.labels
    fmt$column.separate <- column.separate
    fmt$covariate.labels <- covariate.labels
    fmt$dep.var.labels <- dep.var.labels
    fmt$add.lines <- add.lines
    
    if (dep.var.labels.include == FALSE) {
      fmt$table.parts <- fmt$table.parts[fmt$table.parts!="dependent variables"] 
    }
    
    if (!is.null(dep.var.caption)) {
      if (dep.var.caption == "") {
        fmt$table.parts <- fmt$table.parts[fmt$table.parts!="dependent variable label"]
      }
      else {
        fmt$dependent.variable.text <- dep.var.caption
      }
    }
    
    # confidence intervals
    fmt$ci <- ci
    fmt$ci.level <- ci.level
    if (!is.null(ci.separator)) { fmt$ci.separator <- ci.separator }
    if (!is.null(ci.custom)) { fmt$ci <- TRUE }
    
    # omit
    fmt$omit.regexp <- omit
    fmt$omit.index <- omit
    if (is.character(omit)) { fmt$omit.index <- NULL }
    if (is.numeric(omit)) { fmt$omit.regexp <- NULL }
    
    fmt$omit.labels <- omit.labels
    if (!is.null(omit.yes.no)) { 
      fmt$omit.yes <- omit.yes.no[1]
      fmt$omit.no <- omit.yes.no[2]
    }
    
    # keep
    fmt$keep.regexp <- keep
    fmt$keep.index <- keep
    if (is.character(keep)) { fmt$keep.index <- NULL }
    if (is.numeric(keep)) { fmt$keep.regexp <- NULL }
    
    # remove omitted statistics from table parts
    if (!is.null(omit.stat)) {
      .lower.omit.stat <- tolower(omit.stat)    # make it all lower-case
      if ("all" %in% .lower.omit.stat) { .lower.omit.stat <- omit.stat.acceptable }
      if ("n" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="N"] }
      if ("rsq" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="R-squared"] }
      if ("adj.rsq" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="adjusted R-squared"] }
      if ("max.rsq" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="max R-squared"] }
      if ("ll" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="log likelihood"] }
      if ("scale" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="scale"] }
      if ("sigma2" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="sigma2"] }        
      if ("theta" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,5)!="theta"] }
      if ("aic" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="AIC"] }
      if ("bic" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="BIC"] }
      if ("ubre" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="UBRE"] }
      if ("rho" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,3)!="rho"] }
      if ("mills" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,5)!="Mills"] }
      if ("ser" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,3)!="SER"] }
      if ("f" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,11)!="F statistic"] }
      if ("chi2" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,4)!="chi2"] }
      if ("wald" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,4)!="Wald"] }
      if ("lr" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,2)!="LR"] }
      if ("logrank" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,7)!="logrank"] }
      if ("null.dev" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,13)!="null deviance"] }
      if ("res.dev" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,17)!="residual deviance"] }
    }
    
    # keep statistics in the table
    if (!is.null(keep.stat)) {
      .lower.keep.stat <- tolower(keep.stat)    # make it all lower-case
      
      # do this by omitting everything except what you keep
      .lower.omit.stat <- c("n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho","Mills","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")
      .lower.omit.stat <- .lower.omit.stat[!(.lower.omit.stat %in% .lower.keep.stat) ]
      
      if ("n" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="N"] }
      if ("rsq" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="R-squared"] }
      if ("adj.rsq" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="adjusted R-squared"] }
      if ("max.rsq" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="max R-squared"] }
      if ("ll" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="log likelihood"] }
      if ("scale" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="scale"] }
      if ("sigma2" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="sigma2"] }        
      if ("theta" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,5)!="theta"] }
      if ("aic" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="AIC"] }
      if ("bic" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="BIC"] }
      if ("ubre" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="UBRE"] }
      if ("rho" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,3)!="rho"] }
      if ("mills" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,5)!="Mills"] }
      if ("ser" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,3)!="SER"] }
      if ("f" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,11)!="F statistic"] }
      if ("chi2" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,4)!="chi2"] }
      if ("wald" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,4)!="Wald"] }
      if ("lr" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,2)!="LR"] }
      if ("logrank" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,7)!="logrank"] }
      if ("null.dev" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,13)!="null deviance"] }
      if ("res.dev" %in% .lower.omit.stat) { fmt$table.parts <- fmt$table.parts[substr(fmt$table.parts,1,17)!="residual deviance"] }
    }
    
    # keep statistics in table parts
    if (!is.null(keep.stat)) {
      .lower.keep.stat <- tolower(keep.stat)    # make it all lower-case
      keep.stat.acceptable <- c("all","n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho(se)*","Mills(se)*","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")     # list of statistic codes that are acceptable
      remove.stats <- keep.stat.acceptable[!(keep.stat.acceptable %in% .lower.keep.stat)]
      fmt$table.parts <- fmt$table.parts[!(fmt$table.parts %in% remove.stats)]
    }
    
    # digits, initial.zeros, decimal characters
    if (!is.null(decimal.mark)) { fmt$decimal.character <- decimal.mark }
    if (!is.null(align)) { fmt$dec.mark.align <- align }
    if (!is.null(digit.separator)) { fmt$digit.separator <- digit.separator }
    if (!is.null(initial.zero)) { fmt$initial.zero <- initial.zero }
    
    if (!is.null(digit.separate)) { 
      if (digit.separate=="lakh") { fmt$digit.separator.where <- c(3,2) }  # lakhs 
      else if ((digit.separate=="china") | (digit.separate=="japan")) { fmt$digit.separator.where <- 4 }
      else { 
        ## first deal with digit separator
        
        fmt$digit.separator.where <- digit.separate
        fmt$digit.separator.where[fmt$digit.separator.where <= 0] <- -1
      }
    }
    
    if (!is.null(digits)) { 
      fmt$round.digits <- digits 
      fmt$s.round.digits <- digits
    }
    
    if (!is.null(digits.extra)) { 
      fmt$max.extra.digits <- digits.extra
      if (digits.extra>=1) { fmt$until.nonzero.digit <- TRUE }
      else ( fmt$until.nonzero.digit <- FALSE )
    }
    
    # intercept top and bottom
    if (!is.null(intercept.top)) { fmt$intercept.top <- intercept.top }
    if (!is.null(intercept.bottom)) { fmt$intercept.bottom <- intercept.bottom }
    
    # model names, numbers and multicolumn
    if (!is.null(model.names)) { 
      fmt$model.names.include <- model.names 
      if (model.names == TRUE) { fmt$models.skip.if.one <- FALSE }
    }    
    if (!is.null(model.numbers)) { fmt$model.numbers <- model.numbers }
    fmt$multicolumn <- multicolumn
    
    # object names
    fmt$object.names <- object.names
    
    # report coefs, std errs, t, p?
    if (!is.null(report)) {
      fmt$coefficient.table.parts <- NULL
      for (i in 1:nchar(report)) {
        component.letter <- substr(report, i, i)
        if (component.letter == "v") { fmt$coefficient.table.parts <- append(fmt$coefficient.table.parts, "variable name") }
        if (component.letter == "c") { fmt$coefficient.table.parts <- append(fmt$coefficient.table.parts, "coefficient") }
        if (component.letter == "s") { fmt$coefficient.table.parts <- append(fmt$coefficient.table.parts, "standard error") }
        if (component.letter == "t") { fmt$coefficient.table.parts <- append(fmt$coefficient.table.parts, "t-stat") }
        if (component.letter == "p") { fmt$coefficient.table.parts <- append(fmt$coefficient.table.parts, "p-value") }
        if ((component.letter == "*") & (i > 1)) { 
          l <- length(fmt$coefficient.table.parts)
          if ((fmt$coefficient.table.parts[l] != "variable name") & (substr(report,i-1,i-1) != "*")) {
            fmt$coefficient.table.parts[l] <- paste(fmt$coefficient.table.parts[l],"*",sep="")
          }
        }
      }
      fmt$coefficient.table.parts <- append(fmt$coefficient.table.parts, " ")
    }
    
    
    # significance stars
    if (!is.null(star.cutoffs)) { 
      # assign cutoff values
      fmt$cutoffs <- star.cutoffs
    }
    
    if (!is.null(star.char)) { 
      fmt$stars <- star.char
    }
    
    for (i in 1:length(fmt$cutoffs)) {
      if (is.na(fmt$stars[i])) {
        fmt$stars[i] <- paste(rep(fmt$stars[1], i), sep="", collapse="")
      }  
    }
    fmt$stars <- fmt$stars[1:length(fmt$cutoffs)]
    
    # selection equation
    gbl$sel.equation <- selection.equation
    
    # colnames and rownames
    if (!is.null(rownames)) { fmt$rownames <- rownames }
    if (!is.null(colnames)) { fmt$colnames <- colnames }
    
    # zero vs. count component
    gbl$zero.component <- zero.component
    
    # notes
    
    
    # replace star cutoffs in the notes section
    for (i in 1:length(fmt$cutoffs)) {
      if (!is.na(fmt$stars[i])) {
        star.string <- paste(rep("*", i), sep="", collapse="")
        fmt$note.content <- gsub(paste("[.",star.string,"]",sep=""), replace.dec.mark(gsub("^[0]+", "",fmt$cutoffs[i]), fmt), fmt$note.content, fixed=TRUE)  
        fmt$note.content <- gsub(paste("[0.",star.string,"]",sep=""), replace.dec.mark(fmt$cutoffs[i], fmt), fmt$note.content, fixed=TRUE)
        fmt$note.content <- gsub(paste("[",star.string,"]",sep=""), replace.dec.mark(fmt$cutoffs[i]*100, fmt), fmt$note.content, fixed=TRUE)        
      }
    }
    
    
    if (!is.null(notes)) { 
      if (notes.append == TRUE) {
        fmt$note.content <- c(fmt$note.content, notes)
        fmt$s.note.content <- c(fmt$s.note.content, notes)
      }
      else {
        fmt$note.content <- notes
        fmt$s.note.content <- notes
      }
    }
    if (!is.null(notes.align)) { 
      fmt$note.alignment <- notes.align 
      fmt$s.note.alignment <- notes.align
    }
    
    if (!is.null(notes.label)) { 
      fmt$note <- notes.label
      fmt$s.note <- notes.label
    }    
    
    # ordered probit/logit, etc. - report intercepts?
    fmt$ordered.intercepts <- ord.intercepts
    
    # perl-compatible regular expressions?
    fmt$perl <- perl
    
    # standard error for quantile regression
    fmt$rq.se <- rq.se
    
    # report logical variables in summary statistics tables?
    fmt$summ.logical <- summary.logical
    
    # summary statistics - what statistics to report - !!! this needs to come before summary.stat and omit.summary.stat
    if (!nobs) { fmt$s.statistics.list <- fmt$s.statistics.list[fmt$s.statistics.list!="n"] }
    if (!mean.sd) { fmt$s.statistics.list <- fmt$s.statistics.list[(fmt$s.statistics.list!="mean")&(fmt$s.statistics.list!="sd")]}
    if (!min.max) { fmt$s.statistics.list <- fmt$s.statistics.list[(fmt$s.statistics.list!="min")&(fmt$s.statistics.list!="max")]}
    if (!median) { fmt$s.statistics.list <- fmt$s.statistics.list[fmt$s.statistics.list!="median"] }
    if (!iqr) { fmt$s.statistics.list <- fmt$s.statistics.list[(fmt$s.statistics.list!="p25")&(fmt$s.statistics.list!="p75")]}
    
    # keep summary statistics
    if (!is.null(summary.stat)) {
      .lower.keep.summary.stat <- tolower(summary.stat)    # make it all lower-case
      fmt$s.statistics.list <- .lower.keep.summary.stat
    } 
    
    # remove omitted statistics from table parts
    if (!is.null(omit.summary.stat)) {
      .lower.omit.summary.stat <- tolower(omit.summary.stat)    # make it all lower-case
      fmt$s.statistics.list <- fmt$s.statistics.list[!(fmt$s.statistics.list %in% .lower.omit.summary.stat)]
    }
    
    # table layout
    fmt$table.parts.nonstat <- c("=","-","-!","=!","dependent variable label",
                                     "dependent variables","models","columns","numbers",
                                     "objects","coefficients","omit","additional","notes")  
    # these are the non-model statistics parts of the table
    
    if (!is.null(table.layout)) {
      fmt$table.parts.new <- NULL
      for (i in 1:nchar(table.layout)) {
        component.letter <- substr(table.layout, i, i)
        if (component.letter == "=") { fmt$table.parts.new <- append(fmt$table.parts.new, "=") }
        if (component.letter == "-") { fmt$table.parts.new <- append(fmt$table.parts.new, "-") }
        if ((component.letter == "!") & (i > 1)) { 
          if (fmt$table.parts.new[i-1] %in% c("-","=")) {
            fmt$table.parts.new[i-1] <- paste(fmt$table.parts.new[i-1], "!", sep="")
          }
        }
        if (component.letter == "l") { fmt$table.parts.new <- append(fmt$table.parts.new, "dependent variable label") }
        if (component.letter == "d") { fmt$table.parts.new <- append(fmt$table.parts.new, "dependent variables") }
        if (component.letter == "m") { 
          fmt$table.parts.new <- append(fmt$table.parts.new, "models") 
          fmt$model.names.include <- TRUE 
        }
        if (component.letter == "c") { fmt$table.parts.new <- append(fmt$table.parts.new, "columns") }
        if (component.letter == "#") { 
          fmt$table.parts.new <- append(fmt$table.parts.new, "numbers")
          fmt$model.numbers <- TRUE
        }  
        if (component.letter == "b") { 
          fmt$table.parts.new <- append(fmt$table.parts.new, "objects") 
          fmt$object.names <- TRUE
        }  
        if (component.letter == "t") { fmt$table.parts.new <- append(fmt$table.parts.new, "coefficients") }  
        if (component.letter == "o") { fmt$table.parts.new <- append(fmt$table.parts.new, "omit") }  
        if (component.letter == "a") { fmt$table.parts.new <- append(fmt$table.parts.new, "additional") }  
        if (component.letter == "n") { fmt$table.parts.new <- append(fmt$table.parts.new, "notes") }  
        if (component.letter == "s") { 
          fmt$table.parts.new <- append(fmt$table.parts.new, 
                                            fmt$table.parts[!(fmt$table.parts %in% fmt$table.parts.nonstat)]) 
        }
        
      }
      fmt$table.parts <- fmt$table.parts.new
    }
    
    # now omit table parts
    if (!is.null(omit.table.layout)) {
      for (i in 1:nchar(omit.table.layout)) {
        component.letter <- substr(omit.table.layout, i, i)
        if (component.letter == "=") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="="] }
        if (component.letter == "-") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="-"] }
        if ((component.letter == "!") & (i > 1)) {
          if (substr(omit.table.layout, i-1, i-1) == "=") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="=!"] }
          if (substr(omit.table.layout, i-1, i-1) == "-") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="-!"] }
        }
        if (component.letter == "l") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="dependent variable label"] }
        if (component.letter == "d") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="dependent variables"] }
        if (component.letter == "m") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="models"] }
        if (component.letter == "c") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="columns"] }
        if (component.letter == "#") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="numbers"] }
        if (component.letter == "b") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="objects"] }
        if (component.letter == "t") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="coefficients"] }
        if (component.letter == "o") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="omit"] }
        if (component.letter == "a") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="additional"] }
        if (component.letter == "n") { fmt$table.parts <- fmt$table.parts[fmt$table.parts!="notes"] }
        if (component.letter == "s") { fmt$table.parts <- fmt$table.parts[fmt$table.parts %in% fmt$table.parts.nonstat] }          
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
          
          if (!is.na(title[number.of.table])) { fmt$title <- title[number.of.table] }
          else { fmt$title <- title[length(title)] }
          
          if (!is.na(label[number.of.table])) { fmt$label <- label[number.of.table] }
          else { fmt$label <- label[length(label)] }
          
          if (type == "latex") {
            do.call(.stargazer.reg.table, c(fmt=fmt, gbl=gbl, as.list(objects[regression.table.objects]))  )
            invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(
              do.call(.stargazer.reg.table, c(fmt=fmt, gbl=gbl, as.list(objects[regression.table.objects])))
              ,file=NULL)) )
          }
          else if ((type == "text") | (type == "html") | (type == "mmd") ) {
            latex.code <- c(latex.code, invisible(capture.output(do.call(.stargazer.reg.table, c(fmt=fmt, gbl=gbl, as.list(objects[regression.table.objects]))),file=NULL)) )
          }
        }
        
        number.of.table <- number.of.table + 1
        if (!is.na(title[number.of.table])) { fmt$title <- title[number.of.table] }
        else { fmt$title <- title[length(title)] }
        
        if (!is.na(label[number.of.table])) { fmt$label <- label[number.of.table] }
        else { fmt$label <- label[length(label)] }
        
        if (gbl$summary[i]==TRUE) {
          if (type == "latex") {
            .stargazer.summ.stat.table(objects[[i]], fmt, gbl)
            invisible.output <- latex.code <- c(invisible.output, 
                                                invisible(capture.output(.stargazer.summ.stat.table(objects[[i]], fmt, gbl),file=NULL)) )
          }
          else if ((type == "text") | (type == "html") | (type == "mmd")) {
            latex.code <- c(latex.code, invisible(capture.output(.stargazer.summ.stat.table(objects[[i]], fmt, gbl),file=NULL)) )
          }
        }
        else {
          if (type == "latex") {
            .stargazer.data.frame.table(objects[[i]], fmt, gbl)
            invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(.stargazer.data.frame.table(objects[[i]], fmt, gbl),file=NULL)) )
          }
          else if ((type == "text") | (type == "html") | (type == "mmd")) {
            latex.code <- c(latex.code, invisible(capture.output(.stargazer.data.frame.table(objects[[i]], fmt, gbl),file=NULL)) )
          }
        }
        regression.table.objects <- NULL
      }
      else {
        regression.table.objects <- c(regression.table.objects, i)
        gbl$object.names <- gbl$object.names.all[regression.table.objects]
      }
    }
    
    if (!is.null(regression.table.objects)) {	
      number.of.table <- number.of.table + 1
      if (!is.na(title[number.of.table])) { fmt$title <- title[number.of.table] }
      else { fmt$title <- title[length(title)] }
      
      if (!is.na(label[number.of.table])) { fmt$label <- label[number.of.table] }
      else { fmt$label <- label[length(label)] }
      
      if (type == "latex") {
        do.call(".stargazer.reg.table", c(as.list(objects[regression.table.objects]), fmt=list(fmt), gbl=list(gbl)))  
        invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(
          do.call(.stargazer.reg.table, c(as.list(objects[regression.table.objects]), fmt=list(fmt), gbl=list(gbl)))
                  ,file=NULL)) 
          )
      }
      else if ((type == "text") | (type == "html") | (type == "mmd")) {
        latex.code <- c(latex.code, invisible(capture.output(
          do.call(.stargazer.reg.table, c(fmt=list(fmt), gbl=list(gbl), as.list(objects[regression.table.objects])))
          ,file=NULL)) )
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

