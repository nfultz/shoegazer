
.new.table <-
  function(object.name, user.coef=NULL, user.se=NULL, user.t=NULL, user.p=NULL, auto.t=TRUE, auto.p=TRUE, user.ci.lb=NULL, user.ci.rb=NULL, gbl) {
    
    if (class(object.name)[1] == "Glm") {
      .summary.object <<- summary.glm(object.name)
    }
    else if (!(.model.identify(object.name) %in% c("aftreg", "coxreg","phreg","weibreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq"))) {
      .summary.object <<- summary(object.name)
    }
    else {
      .summary.object <<- object.name
    }
    
    if (.model.identify(object.name) == "rq") {
      .summary.object <<- suppressMessages(summary(object.name, se=fmt$rq.se))
    }
    
    model.num.total <- 1   # model number for multinom, etc.
    if (.model.identify(object.name) == "multinom") {
      if (!is.null(nrow(.summary.object$coefficients))) {
        model.num.total <-  nrow(.summary.object$coefficients)
      }
    }
    
    # set to null
    
    gbl$models <- NULL
    
    gbl$dependent.variables <- NULL
    gbl$dependent.variables.written <- NULL
    
    gbl$coefficient.variables <- NULL
    gbl$coef.vars.by.model <- NULL
    gbl$coefficients <- NULL
    gbl$std.errors <- NULL
    gbl$ci.lb <- NULL
    gbl$ci.rb <- NULL
    
    gbl$t.stats <- NULL
    gbl$p.values <- NULL
    
    gbl$N <- NULL
    gbl$LL <- NULL
    gbl$R2 <- NULL
    gbl$max.R2 <- NULL
    gbl$adj.R2 <- NULL
    gbl$AIC <- NULL
    gbl$BIC <- NULL
    gbl$scale <- NULL
    gbl$UBRE <- NULL
    gbl$sigma2 <- NULL
    gbl$theta <- NULL
    gbl$rho <- NULL
    gbl$mills <- NULL
    
    gbl$SER <- NULL
    gbl$F.stat <- NULL
    gbl$chi.stat <- NULL
    gbl$wald.stat <- NULL
    gbl$lr.stat <- NULL
    gbl$logrank.stat <- NULL
    gbl$null.deviance <- NULL
    gbl$residual.deviance <- NULL
    
    for (model.num in 1:model.num.total) {
      
      gbl$models <- c(gbl$models, suppressMessages(as.vector(.model.identify(object.name))))
      
      gbl$dependent.variables <- c(gbl$dependent.variables, suppressMessages(.dependent.variable(object.name, model.num)))
      gbl$dependent.variables.written <- c(gbl$dependent.variables.written, suppressMessages(.dependent.variable.written(object.name, model.num)))
      gbl$coefficient.variables <- suppressMessages(.coefficient.variables(object.name))
      
      gbl$coef.vars.by.model <-  suppressMessages(cbind(gbl$coef.vars.by.model, gbl$coefficient.variables))
      
      get.coef <- suppressMessages(.get.coefficients(object.name, user.coef, model.num=model.num))
      get.se <- suppressMessages(.get.standard.errors(object.name, user.se, model.num=model.num))
      
      gbl$coefficients <- cbind(gbl$coefficients, get.coef)
      gbl$std.errors <- cbind(gbl$std.errors, get.se)
      
      gbl$ci.lb <- suppressMessages(cbind(gbl$ci.lb, .get.ci.lb(object.name, user.ci.lb, model.num=model.num)))
      gbl$ci.rb <- suppressMessages(cbind(gbl$ci.rb, .get.ci.rb(object.name, user.ci.rb, model.num=model.num))) 
      
      feed.coef <- NA; feed.se <- NA
      if (!is.null(get.coef)) { feed.coef <- get.coef }
      if (!is.null(get.se)) { feed.se <- get.se }
      if (!is.null(user.coef)) { feed.coef <- user.coef }   # feed user-defined coefficients, if available
      if (!is.null(user.se)) { feed.se <- user.se }   # feed user-defined std errors, if available
      
      gbl$t.stats <- suppressMessages(cbind(gbl$t.stats, .get.t.stats(object.name, user.t, auto.t, feed.coef, feed.se, user.coef, user.se, model.num=model.num)))
      gbl$p.values <- suppressMessages(cbind(gbl$p.values, .get.p.values(object.name, user.p, auto.p, feed.coef, feed.se, user.coef, user.se, model.num=model.num)))
      
      
      gbl$N <- c(gbl$N, suppressMessages(.number.observations(object.name)))
      gbl$LL <- c(gbl$LL, suppressMessages(.log.likelihood(object.name)))
      gbl$R2 <- c(gbl$R2, suppressMessages(.r.squared(object.name)))
      gbl$max.R2 <- c(gbl$max.R2, suppressMessages(.max.r.squared(object.name)))
      gbl$adj.R2 <- c(gbl$adj.R2, suppressMessages(.adj.r.squared(object.name)))
      gbl$AIC <- c(gbl$AIC, suppressMessages(.AIC(object.name)))
      gbl$BIC <- c(gbl$BIC, suppressMessages(.BIC(object.name)))
      gbl$scale <- c(gbl$scale, suppressMessages(.get.scale(object.name)))
      gbl$UBRE <- c(gbl$UBRE, suppressMessages(.gcv.UBRE(object.name)))
      gbl$sigma2 <- c(gbl$sigma2, suppressMessages(.get.sigma2(object.name)))
      
      gbl$rho <- cbind(suppressMessages(.get.rho(object.name)))
      gbl$mills <- cbind(suppressMessages(.get.mills(object.name)))
      gbl$theta <- cbind(suppressMessages(.get.theta(object.name)))
      gbl$SER <- cbind(suppressMessages(.SER(object.name)))
      gbl$F.stat <- cbind(suppressMessages(.F.stat(object.name)))
      gbl$chi.stat <- cbind(suppressMessages(.chi.stat(object.name)))
      gbl$wald.stat <- cbind(suppressMessages(.wald.stat(object.name)))
      gbl$lr.stat <- cbind(suppressMessages(.lr.stat(object.name)))
      gbl$logrank.stat <- cbind(suppressMessages(.logrank.stat(object.name)))
      gbl$null.deviance <- cbind(suppressMessages(.null.deviance(object.name)))
      gbl$residual.deviance <- cbind(suppressMessages(.residual.deviance(object.name)))
    }
    
    gbl
  }



.table.empty.line <-
  function(fmt, gbl) {
    if (fmt$no.space == FALSE) {
      cat(" ")
      for (i in seq(1:length(gbl$models))) {
        cat("& ")
      }
      cat("\\\\ \n")
    }
  }

.table.enter.coefficients <-
  function(which.variable) {
    
    if (which.variable > length(gbl$coefficients)) {
      return();
    }
    
    local.coefficient.var.name <- gbl$coefficient.variables[which.variable]
    
    #skip all of this if omitted based on regular expression
    omitted <- FALSE
    
    if (!is.null(fmt$omit.regexp)) {
      for (i in seq(1:length(fmt$omit.regexp))) {
        if (length(grep(fmt$omit.regexp[i], local.coefficient.var.name, perl=fmt$perl, fixed=FALSE))!=0) { omitted <- TRUE	}
      }
    }
    
    if (!is.null(fmt$keep.regexp)) {
      omitted <- TRUE
      for (i in seq(1:length(fmt$keep.regexp))) {
        if (length(grep(fmt$keep.regexp[i], local.coefficient.var.name, perl=fmt$perl, fixed=FALSE))!=0) { omitted <- FALSE	}
      }
    }
    
    if (!is.null(fmt$omit.index)) {
      for (i in seq(1:length(fmt$omit.index))) {
        if (fmt$omit.index[i] == which.variable) { omitted <- TRUE }
      }
    }
    
    if (!is.null(fmt$keep.index)) {
      omitted <- TRUE
      for (i in seq(1:length(fmt$keep.index))) {
        if (fmt$keep.index[i] == which.variable) { omitted <- FALSE }
      }
    }
    
    if (omitted == FALSE) {
      
      .which.variable.label <<- .which.variable.label + 1
      
      # remove final -TRUE (added by Zelig) from dummy variables
      if (substr(local.coefficient.var.name, nchar(local.coefficient.var.name)-3, nchar(local.coefficient.var.name)) == "TRUE") {
        
        ### only remove TRUE if added by Zelig, rather than pre-existing in the formula name
        if (length(grep(local.coefficient.var.name, gbl$formulas.rhs,fixed=TRUE))==0) {    
          local.coefficient.var.name <- substr(local.coefficient.var.name, 1, nchar(local.coefficient.var.name)-4)
        }
      }
      
      # remove everything before and including he last dollar sign from variable name
      temp <- strsplit(local.coefficient.var.name,"$",fixed=TRUE)
      local.coefficient.var.name <- temp[[1]][length(temp[[1]])]
      
      # if underscore or ^ in variable name, then insert an escape \ before it
      local.coefficient.var.name <- .remove.special.chars(local.coefficient.var.name)
      
      if (length(fmt$coefficient.table.parts)>=1) {
        for (i in seq(1:length(fmt$coefficient.table.parts))) {
          .coefficient.table.part(part=fmt$coefficient.table.parts[i], which.variable, variable.name=local.coefficient.var.name)
        }
      }
    }
  }

.table.header <-
  function(fmt, gbl) {
    .floating.header(fmt)
    
    #
    .formatting.alignment <- paste("@{\\extracolsep{",fmt$column.sep.width,"}}l", sep="")
    for (i in seq(1:length(gbl$models))) {
      if (fmt$dec.mark.align==FALSE) {
        .formatting.alignment <- paste(.formatting.alignment, "c", sep="")
      }
      else {
        .formatting.alignment <- paste(.formatting.alignment, "D{", fmt$decimal.character,"}{", fmt$decimal.character,"}{-", fmt$round.digits,"} ", sep="")
      }
    }
    #
    
    cat("\\begin{tabular}{",.formatting.alignment,"} \n",sep="")
  }

.table.info.comment <-
  function(fmt, gbl) {
    cat("\n")
    if (fmt$header==TRUE) {
      cat("% Table created by ", gbl$package.name, " v.", gbl$package.version, " by ", gbl$package.author.name, ", ", gbl$package.author.affiliation, ". E-mail: ", gbl$package.author.email, "\n", sep="")  
      cat("% Date and time:", format(Sys.time(), "%a, %b %d, %Y - %X"))
      cat("\n")
      
      required.latex.packages <- NULL
      if (fmt$dec.mark.align==TRUE) { required.latex.packages <- c(required.latex.packages, "dcolumn") }
      if (fmt$floating.environment=="sidewaystable") { required.latex.packages <- c(required.latex.packages, "rotating") }
      
      if (!is.null(required.latex.packages)) {
        cat("% Requires LaTeX packages: ")
        for (i in 1:length(required.latex.packages)){
          cat(required.latex.packages[i]," ", sep="")
        }
        cat("\n")
      }
    }
  }

.table.insert.space <-
  function(fmt) {
    cat("\\\\[",fmt$space.size,"]",sep="")
  }


.adjust.settings.style <-
  function(what.style, fmt) {
    style <- tolower(what.style)
    
    if (style == "all") {
      fmt$table.parts <- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*(p)", "SER(df)","F statistic(df)*(p)","chi2(df)*(p)","Wald(df)*(p)","LR(df)*(p)","logrank(df)*(p)","AIC","BIC","UBRE","rho(se)*(p)","Mills(se)*(p)","residual deviance(df)*","null deviance(df)*","=!","notes")  
      fmt$coefficient.table.parts <- c("variable name","coefficient*","standard error","t-stat","p-value")  
    }
    
    else if (style == "all2") {
      fmt$table.parts <- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*(p)", "SER(df)","F statistic(df)*(p)","chi2(df)*(p)","Wald(df)*(p)","LR(df)*(p)","logrank(df)*(p)","AIC","BIC","UBRE","rho(se)*(p)","Mills(se)*(p)","residual deviance(df)*","null deviance(df)*","=!","notes")  
      fmt$coefficient.table.parts <- c("variable name","coefficient*","standard error")  
    }
    
    # aer = American Economic Review
    else if (style == "aer") {
      fmt$table.parts <- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","-!","notes")
      fmt$models.skip.if.one <- TRUE
      fmt$dependent.variable.text.on <- FALSE
      
      fmt$until.nonzero.digit <- FALSE
      fmt$max.extra.digits <- 0    
      
      fmt$model.left <- ""
      fmt$model.right <- ""
      
      fmt$note <- "\\textit{Notes:}"
      fmt$note.alignment <- "l"
      fmt$note.content <- c("$^{***}$Significant at the [***] percent level.","$^{**}$Significant at the [**] percent level.","$^{*}$Significant at the [*] percent level.")
    }
    
    # ajps = American Journal of Political Science
    else if (style == "ajps") {
      fmt$table.parts <- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      fmt$models.skip.if.one <- TRUE
      fmt$dependent.variable.text.on <- FALSE
      fmt$digit.separator <- ""
      fmt$dependent.variables.left <- "\\textbf{"
      fmt$dependent.variables.right <- "}"
      fmt$column.left <- "\\textbf{"
      fmt$column.right <- "}"
      fmt$models.left <- "\\textbf{"
      fmt$models.right <- "}"
      fmt$numbers.left <- "\\textbf{Model "
      fmt$numbers.right <- "}"
      fmt$coefficient.table.parts <- c("variable name","coefficient*","standard error") 
      fmt$N <- "N"
      fmt$AIC <- "AIC"
      fmt$BIC <- "BIC"
      fmt$chi.stat <- "Chi-square"
      fmt$R2 <- "R-squared"
      fmt$adj.R2 <- "Adj. R-squared"
      fmt$max.R2 <- "Max. R-squared"
      fmt$note <- ""
      fmt$note.content <- c("$^{***}$p $<$ [.***]; $^{**}$p $<$ [.**]; $^{*}$p $<$ [.*]")
      fmt$note.alignment <- "l"
      fmt$s.stat.parts <- c("-!","stat names","-","statistics1","-!","notes")
    }  
    
    # ajs = American Journal of Sociology
    else if (style == "ajs") {
      fmt$table.parts <- c(" ","=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","-!","notes")
      fmt$models.skip.if.one <- TRUE
      fmt$dependent.variables.capitalize <- TRUE
      fmt$dependent.variable.text.on <- FALSE
      
      fmt$numbers.left <- ""
      fmt$numbers.right <- ""
      
      fmt$until.nonzero.digit <- FALSE
      fmt$max.extra.digits <- 0    
      
      fmt$model.left <- ""
      fmt$model.right <- ""
      
      fmt$note <- "\\textit{Notes:}"
      fmt$note.alignment <- "l"
      fmt$note.content <- c("$^{*}$P $<$ [.*]","$^{**}$P $<$ [.**]","$^{***}$P $<$ [.***]")
      fmt$cutoffs <- c(0.05, 0.01, 0.001)
      
      fmt$initial.zero <- FALSE
    }
    
    # apsr = American Political Science Review
    else if (style == "apsr") {
      fmt$table.parts <- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      fmt$models.skip.if.one <- TRUE
      fmt$dependent.variable.text.on <- FALSE
      
      fmt$models.left <- ""
      fmt$models.right <- ""
      fmt$coefficient.table.parts <- c("variable name","coefficient*","standard error")
      fmt$N <- "N"
      fmt$AIC <- "AIC"
      fmt$BIC <- "BIC"
      fmt$chi.stat <- "chi$^{2}$"
      fmt$note <- ""
      fmt$note.content <- c("$^{*}$p $<$ [.*]; $^{**}$p $<$ [.**]; $^{***}$p $<$ [.***]")
      fmt$note.alignment <- "l"
      fmt$s.stat.parts <- c("-!","stat names","-","statistics1","-!","notes")
    }
    
    # asq = Administrative Science Quarterly
    else if (style == "asq") {
      fmt$table.parts <- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      fmt$models.skip.if.one <- TRUE
      fmt$dependent.variable.text.on <- FALSE
      
      fmt$digit.separator <- ""
      fmt$dependent.variables.left <- "\\textbf{"
      fmt$dependent.variables.right <- "}"
      fmt$column.left <- "\\textbf{"
      fmt$column.right <- "}"
      fmt$models.left <- "\\textbf{"
      fmt$models.right <- "}"
      fmt$numbers.left <- "\\textbf{Model "
      fmt$numbers.right <- "}"
      fmt$coefficient.table.parts <- c("variable name","coefficient*","standard error") 
      fmt$AIC <- "AIC"
      fmt$BIC <- "BIC"
      fmt$chi.stat <- "Chi-square"
      fmt$R2 <- "R-squared"
      fmt$adj.R2 <- "Adj. R-squared"
      fmt$max.R2 <- "Max. R-squared"
      fmt$note <- ""
      fmt$note.content <- c("$^{\\bullet}$p $<$ [.*]; $^{\\bullet\\bullet}$p $<$ [.**]; $^{\\bullet\\bullet\\bullet}$p $<$ [.***]")
      fmt$note.alignment <- "l"
      fmt$s.stat.parts <- c("-!","stat names","-","statistics1","-!","notes")
      fmt$stars <- "\\bullet"
    }  
    
    # asr = American Sociological Review
    else if (style == "asr") {
      fmt$table.parts <- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      fmt$models.skip.if.one <- TRUE
      fmt$dependent.variable.text.on <- FALSE
      
      fmt$models.left <- ""
      fmt$models.right <- ""
      fmt$coefficient.table.parts <- c("variable name","coefficient*")
      fmt$N <- "\\textit{N}"
      fmt$AIC <- "AIC"
      fmt$BIC <- "BIC"
      fmt$chi.stat <- "chi$^{2}$"
      fmt$note <- ""
      fmt$note.content <- c("$^{*}$p $<$ [.*]; $^{**}$p $<$ [.**]; $^{***}$p $<$ [.***]")
      fmt$cutoffs <- c(0.05, 0.01, 0.001)
      fmt$note.alignment <- "l"
      fmt$s.stat.parts <- c("-!","stat names","-","statistics1","-!","notes")
    }
    
    # "demography" = Demography
    else if (style == "demography") {
      fmt$table.parts <- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      fmt$models.skip.if.one <- TRUE
      fmt$dependent.variable.text.on <- FALSE
      
      fmt$models.left <- ""
      fmt$models.right <- ""
      fmt$numbers.left <- "Model "
      fmt$numbers.right <- ""
      fmt$coefficient.table.parts <- c("variable name","coefficient*","standard error")
      fmt$N <- "\\textit{N}"
      fmt$AIC <- "AIC"
      fmt$BIC <- "BIC"
      fmt$chi.stat <- "Chi-Square"
      fmt$note <- ""
      fmt$note.content <- c("$^{*}$p $<$ [.*]; $^{**}$p $<$ [.**]; $^{***}$p $<$ [.***]")
      fmt$cutoffs <- c(0.05, 0.01, 0.001)
      fmt$note.alignment <- "l"
      fmt$s.stat.parts <- c("-!","stat names","-","statistics1","-!","notes")
    }
    
    # io = International Organization
    else if (style == "io") {
      fmt$table.parts <- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      fmt$models.skip.if.one <- TRUE
      fmt$dependent.variable.text.on <- FALSE
      
      fmt$coefficient.table.parts <- c("variable name","coefficient*","standard error")
      fmt$coefficient.variables.capitalize <- TRUE
      fmt$s.coefficient.variables.capitalize <- TRUE
      fmt$intercept.name <- "Constant"
      fmt$N <- "\\textit{Observations}"
      fmt$AIC <- "\\textit{Akaike information criterion}"
      fmt$BIC <- "\\textit{Bayesian information criterion}"
      fmt$chi.stat <- "\\textit{Chi-square}"
      fmt$logrank.stat <- "\\textit{Score (logrank) test}"
      fmt$lr.stat <- "\\textit{LR test}"
      fmt$max.R2 <- "\\textit{Maximum R-squared}"
      fmt$R2 <- "\\textit{R-squared}"
      fmt$adj.R2 <- "\\textit{Adjusted R-squared}"
      fmt$UBRE <- "\\textit{UBRE}"
      fmt$F.stat <- "\\textit{F statistic}"
      fmt$LL <- "\\textit{Log likelihood}"
      fmt$SER <- "\\textit{Residual standard error}"
      fmt$null.deviance <- "\\textit{Null deviance}"
      fmt$residual.deviance <- "\\textit{Residual deviance}"
      fmt$scale <- "\\textit{Scale}"
      fmt$wald.stat <- "\\textit{Wald test}"
      fmt$note <- "\\textit{Notes:}"
      fmt$note.content <- c("$^{***}$p $<$ [.***]; $^{**}$p $<$ [.**]; $^{*}$p $<$ [.*]")
      fmt$note.alignment <- "l"
      fmt$s.stat.parts <- c("-!","stat names","-","statistics1","-!","notes")
    }
    
    
    # jpam = Journal of Policy Analysis and Management
    else if (style == "jpam") {
      fmt$table.parts <- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      fmt$models.skip.if.one <- TRUE
      fmt$dependent.variable.text.on <- FALSE
      
      fmt$models.left <- ""
      fmt$models.right <- ""
      fmt$numbers.left <- "Model "
      fmt$numbers.right <- ""
      fmt$numbers.roman <- TRUE
      fmt$coefficient.table.parts <- c("variable name","coefficient*","standard error")
      fmt$intercept.bottom <- FALSE
      fmt$intercept.top <- TRUE
      fmt$N <- "N"
      fmt$AIC <- "AIC"
      fmt$BIC <- "BIC"
      fmt$note <- "\\textit{Note:}"
      fmt$note.content <- c("$^{***}$p $<$ [.***]; $^{**}$p $<$ [.**]; $^{*}$p $<$ [.*]")
      fmt$note.alignment <- "l"
      fmt$s.stat.parts <- c("-!","stat names","-","statistics1","-!","notes")
      fmt$s.statistics.names <- cbind(c("n","N"), c("nmiss","missing"), c("mean","Mean"), c("sd","SD"), c("median","Median"), c("min","Minimum"), c("max","Maximum"), c("mad","Median Abs. Dev."), c("p","Percentile(!)"))
      
    }
    
    # "qje" = Quarterly Journal of Economics
    else if (style=="qje") {
      fmt$table.parts <- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")    
      fmt$dependent.variable.text.on <- FALSE
      fmt$s.stat.parts <- c("-!","stat names","=","statistics1","=!","notes")
      fmt$N <- "\\textit{N}"
      fmt$note <- "\\textit{Notes:}"
      fmt$note.content <- c("$^{***}$Significant at the [***] percent level.", "$^{**}$Significant at the [**] percent level.", "$^{*}$Significant at the [*] percent level.") 
    }
    
    # find style based on journal ("default" or other)
    else if (style=="commadefault") {
      fmt$table.parts <- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")
      fmt$digit.separator <- " "
      fmt$decimal.character <- ","
    }
    
    else if (style=="default") {
      fmt$table.parts <- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")
    }
    
    fmt
  }

.set.font.size <- 
  function(fmt) {
    if (!is.null(fmt$font.size)) {
      cat("\\", fmt$font.size," \n", sep="")
    }
  }

.floating.header <-
  function(fmt) {
    if (fmt$floating==TRUE) {
      cat("\\begin{", fmt$floating.environment,"}[", fmt$table.placement,"] \\centering \n",sep="")
      cat("  \\caption{", fmt$title, "} \n",sep="")   
      cat("  \\label{", fmt$label, "} \n",sep="")
      .set.font.size(fmt)
    }
    else if (!is.null(fmt$font.size)) { # set font size using begingroup
      cat("\\begingroup \n", sep="")
      .set.font.size(fmt)
    }
  }



.print.table.statistic <-
  function(gbl.var.name, fmt.var.name, decimal.digits=fmt$round.digits, part.string="", part.number=NULL, type.se=FALSE, fmt, gbl) {
    
    # default values
    report.df <- FALSE
    report.p.value <- FALSE
    significance.stars <- FALSE
    report.se <- FALSE
    report.tstat <- FALSE
    intelligent.df <- fmt$intelligent.df
    force.math <- FALSE
    
    # reporting of df, p-value, significance stars, standard errors, t-stats
    if (length(grep("(df)", part.string,fixed=TRUE))!=0) { report.df <- TRUE } 
    if (length(grep("(se)", part.string,fixed=TRUE))!=0) { report.se <- TRUE }
    if (length(grep("(t)", part.string,fixed=TRUE))!=0) { report.tstat <- TRUE }
    if (length(grep("(p)", part.string,fixed=TRUE))!=0) { report.p.value <- TRUE } 
    if (length(grep("*", part.string,fixed=TRUE))!=0) { significance.stars <- TRUE } 
    
    
    # first for vectors (statistics without, say, degrees of freedom)
    if (is.vector(gbl.var.name) == TRUE) {
      if (sum(!is.na(gbl.var.name))!=0) {
        cat (fmt.var.name)
        for (i in seq(1:length(gbl$models))) {
          if (!is.na(gbl.var.name[i])) { 
            if (fmt$dec.mark.align == TRUE) {
              cat(" & \\multicolumn{1}{c}{",.iround(gbl.var.name[i], decimal.digits, fmt=fmt),"}", sep="")
            }
            else {
              cat(" & ",.iround(gbl.var.name[i], decimal.digits, fmt=fmt), sep="")
            }
          }
          else { cat(" & ", sep="") }
        }
        cat(" \\\\ \n")
        .table.part.published[part.number] <<- TRUE
      }
    }
    else if ((is.matrix(gbl.var.name) == TRUE) & (type.se == FALSE)) {     # for statistics that have degrees of freedom
      if (sum(!is.na(as.vector(gbl.var.name["statistic",])))!=0) {
        
        # intelligent df reporting (figure out whether only report it on left side, or also)
        report.df.left.column <- FALSE
        
        # whittle down unique values
        df.all.together <- NULL
        for (i in seq(1:length(gbl$models))) {
          df.string <- ""
          for (j in seq(1:(nrow(gbl.var.name)- 2))) {
            df.string <- paste(df.string,";",as.character(gbl.var.name[paste("df",as.character(j),sep=""),i]),sep="")
          }
          df.all.together <- append(df.all.together, df.string)
        }
        # remove.na.r
        df.all.together.no.NA <- NULL
        for (i in seq(1:length(df.all.together))) {
          if (substr(df.all.together[i],1,3)!=";NA") { df.all.together.no.NA <- c(df.all.together.no.NA, df.all.together[i]) }
        }
        df.all.together.no.NA.unique <- sort(unique(df.all.together.no.NA))
        
        # put df on the left if only one unique df in the table, and not just one column w/ given df
        if (intelligent.df == TRUE) {
          if ((length(df.all.together.no.NA.unique)==1) & (length(df.all.together.no.NA)>=2)) { report.df.left.column <- TRUE }				
        }
        
        # write down the line	
        cat (fmt.var.name)
        
        # report df on left side w/ intelligent reporting
        if (report.df.left.column == TRUE) {
          if (report.df == TRUE) {
            
            cat(" ",fmt$df.left,sep="")
            df.list <- unlist(strsplit(df.all.together.no.NA.unique[1],";"))
            
            for (i in seq(from=2, to=length(df.list))) {
              if (i>=3) { cat(fmt$df.separator) }
              cat(df.list[i],sep="")
            }
            cat(fmt$df.right,sep="")
          }
        }
        
        # now, go column by column
        for (i in seq(1:length(gbl$models))) {
          if (!is.na(gbl.var.name["statistic",i])) {
            
            if (fmt$dec.mark.align==TRUE) {
              cat(" & \\multicolumn{1}{c}{",.iround(gbl.var.name["statistic",i], decimal.digits, fmt=fmt), sep="") 
              force.math <- TRUE
            }
            else {
              cat(" & ",.iround(gbl.var.name["statistic",i], decimal.digits, fmt=fmt), sep="")
            }
            
            # significance stars
            if ((significance.stars == TRUE) & (!is.na(gbl.var.name["p-value",i]))) { .enter.significance.stars(gbl.var.name["p-value",i], force.math, fmt=fmt) }
            
            
            # degrees of freedom - only report by statistics if not in the left column already
            if (report.df.left.column == FALSE) {
              if ((report.df == TRUE) & (!is.na(gbl.var.name["df1",i]))) {
                cat(" ",fmt$df.left,sep="")
                for (j in seq(1:(nrow(gbl.var.name)- 2))) {
                  if (!is.na(gbl.var.name[paste("df",as.character(j),sep=""),i])) {
                    if (j>=2) { cat(fmt$df.separator) }
                    cat(gbl.var.name[paste("df",as.character(j),sep=""),i],sep="")
                  }
                }
                cat(fmt$df.right,sep="")
              }
            }
            
            # p-values
            if ((report.p.value == TRUE) & (!is.na(gbl.var.name["p-value",i]))) {
              cat(" ",fmt$p.value.left,sep="")
              if (!is.na(gbl.var.name[paste("df",as.character(j),sep=""),i])) { 
                cat(.iround(gbl.var.name["p-value",i],fmt$round.digits, round.up.positive=TRUE),sep="") 
              }
              cat(fmt$p.value.right,sep="")
            }
            
            if (fmt$dec.mark.align==TRUE) {
              cat("}")  
            }
            else {
              cat("")
            }
            
          }
          else { cat(" & ", sep="") }
        }
        cat(" \\\\ \n")			
        .table.part.published[part.number] <<- TRUE
      }
    }
    else if ((is.matrix(gbl.var.name) == TRUE) & (type.se == TRUE)) {       # for statistics that have a standard error
      if (sum(!is.na(as.vector(gbl.var.name["statistic",])))!=0) {
        
        # write down the line	
        cat (fmt.var.name)
        
        # now, go column by column
        for (i in seq(1:length(gbl$models))) {
          if (!is.na(gbl.var.name["statistic",i])) { 
            
            if (fmt$dec.mark.align == TRUE) {
              cat(" & \\multicolumn{1}{c}{",.iround(gbl.var.name["statistic",i], decimal.digits), sep="")  
            }
            else {
              cat(" & ",.iround(gbl.var.name["statistic",i], decimal.digits), sep="")
            }
            
            
            # significance stars
            if ((significance.stars == TRUE) & (!is.na(gbl.var.name["p-value",i]))) { .enter.significance.stars(gbl.var.name["p-value",i], force.math) }
            
            # standard errors
            if ((report.se == TRUE) & (!is.na(gbl.var.name["se",i]))) { cat(" ",fmt$se.left,.iround(gbl.var.name["se",i], decimal.digits),fmt$se.right,sep="") }
            
            # t-statistics
            if ((report.tstat == TRUE) & (!is.na(gbl.var.name["tstat",i]))) { cat(" ",fmt$tstat.left, .iround(gbl.var.name["tstat",i], decimal.digits),fmt$tstat.right,sep="") }
            
            # p-values
            if ((report.p.value == TRUE) & (!is.na(gbl.var.name["p-value",i]))) { cat(" ",fmt$p.value.left,.iround(gbl.var.name["p-value",i], decimal.digits),fmt$p.value.right,sep="") }
            
            if (fmt$dec.mark.align == TRUE) {
              cat("}")
            }
            else {
              cat("")
            }
          }
          else { cat(" & ", sep="") }
        }
        cat(" \\\\ \n")			
        .table.part.published[part.number] <<- TRUE
      }
    }
  }

.publish.table <-
  function(fmt, gbl) {
    
    .table.info.comment(fmt, gbl)
    
    # table header
    
    .table.header(fmt, gbl)
    .table.insert.space(fmt)
    
    .table.part.published <<- as.vector(rep(NA, times=length(fmt$table.parts)))    # to keep track what has been published (to deal intelligently with horizontal lines)
    .publish.horizontal.line <<- TRUE   # should non-compulsory horizontal lines be published? (yes, if something else published since the previous line)
    
    if (length(fmt$table.parts)>=1) {
      for (i in seq(1:length(fmt$table.parts))) {
        .publish.table.part(part=fmt$table.parts[i], which.part.number=i, fmt, gbl)
        
        if (.table.part.published[i]==TRUE) { .publish.horizontal.line <<- TRUE }
        if ((fmt$table.parts[i]=="-") | (fmt$table.parts[i]=="-!") | (fmt$table.parts[i]=="=") | (fmt$table.parts[i]=="=!")) { .publish.horizontal.line <<- FALSE }
      }
    }
    
    cat("\\end{tabular} \n")
    if (fmt$floating == TRUE) { cat("\\end{", fmt$floating.environment,"} \n", sep="") }
    else if (!is.null(fmt$font.size)) {
      cat("\\endgroup \n",sep="")
    }
    
  }

.publish.table.part <-
  function(part, which.part.number, fmt, gbl) {
    
    .table.part.published[which.part.number] <<- FALSE
    
    # dependent variable label line
    if (part=="dependent variable label") {
      if (fmt$dependent.variable.text.on == TRUE) { 
        cat(" & \\multicolumn{",length(gbl$models),"}{c}{",fmt$dependent.variable.text, "} \\\\ \n", sep="")
        if (fmt$dependent.variable.text.underline == TRUE) { cat("\\cline{2-",length(gbl$models)+1,"} \n", sep="") }
      }
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # dependent variables
    else if (part=="dependent variables") {
      .table.insert.space(fmt)
      cat(fmt$dependent.variables.text)
      how.many.columns <- 0
      label.counter <- 0
      
      for (i in seq(1:length(gbl$models))) {
        if (is.null(fmt$dep.var.labels)) { fmt$dep.var.labels <- NA }
        how.many.columns <- how.many.columns + 1
        
        # write down if next column has different dependent variable, or if end of columns
        different.dependent.variable <- FALSE
        if (i == length(gbl$models)) {different.dependent.variable <- TRUE}
        else if ((as.character(gbl$dependent.variables[i])) != (as.character(gbl$dependent.variables[i+1])))  {different.dependent.variable <- TRUE}
        
        if (fmt$multicolumn==FALSE) { different.dependent.variable <- TRUE }
        
        if (different.dependent.variable == TRUE) {
          label.counter <- label.counter + 1 
          if (how.many.columns == 1) {
            if (fmt$dec.mark.align==TRUE) {
              if (is.na(fmt$dep.var.labels[label.counter])) {
                if (fmt$dependent.variables.capitalize == TRUE) { cat(" & \\multicolumn{1}{c}{",fmt$dependent.variables.left,toupper(as.character(gbl$dependent.variables.written[i])),fmt$dependent.variables.right,"}", sep="") }
                else { cat(" & \\multicolumn{1}{c}{",fmt$dependent.variables.left,as.character(gbl$dependent.variables.written[i]),fmt$dependent.variables.right,"}", sep="") }
              }
              else { cat(" & \\multicolumn{1}{c}{",fmt$dependent.variables.left,fmt$dep.var.labels[label.counter],fmt$dependent.variables.right,"}", sep="") }
            }
            else {
              if (is.na(fmt$dep.var.labels[label.counter])) {
                if (fmt$dependent.variables.capitalize == TRUE) { cat(" & ",fmt$dependent.variables.left,toupper(as.character(gbl$dependent.variables.written[i])),fmt$dependent.variables.right, sep="") }
                else { cat(" & ",fmt$dependent.variables.left,as.character(gbl$dependent.variables.written[i]),fmt$dependent.variables.right, sep="") }
              }
              else { cat(" & ",fmt$dependent.variables.left,fmt$dep.var.labels[label.counter],fmt$dependent.variables.right, sep="") }
            }
          }
          else {
            if (is.na(fmt$dep.var.labels[label.counter])) {
              if (fmt$dependent.variables.capitalize == TRUE) {cat(" & \\multicolumn{",how.many.columns,"}{c}{",fmt$dependent.variables.left,toupper(as.character(gbl$dependent.variables.written[i])),fmt$dependent.variables.right,"}", sep="")}
              else {cat(" & \\multicolumn{",how.many.columns,"}{c}{",fmt$dependent.variables.left,as.character(gbl$dependent.variables.written[i]),fmt$dependent.variables.right,"}", sep="")}
            }
            else {cat(" & \\multicolumn{",how.many.columns,"}{c}{",fmt$dependent.variables.left,fmt$dep.var.labels[label.counter],fmt$dependent.variables.right,"}", sep="")}
          }
          
          how.many.columns <- 0
        }
      }
      cat(" \\\\ \n")
      
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # models
    else if (part=="models")  {
      if ((fmt$model.names.include==TRUE) & ((fmt$models.skip.if.one == FALSE) | ((fmt$models.skip.if.one == TRUE) & (length(unique(gbl$models))>=2)))) {
        
        renamedgbl <- list()
        
        .table.insert.space(fmt)
        cat(fmt$models.text)
        
        # rename models based on .formatting preferences
        renamedgbl$models <- as.matrix(rbind(gbl$models, rep("", times=length(gbl$models))))
        for (i in seq(1:length(gbl$models))) {
          for (j in seq(1:ncol(fmt.model.names))) {
            model.strsplit <- unlist(strsplit(gbl$models[i], split="#"))
            if (gbl$models[i]==fmt.model.names[1,j]) { 
              renamedgbl$models[1,i] <- fmt.model.names[2,j] 
              renamedgbl$models[2,i] <- fmt.model.names[3,j]
            }
            else if ((model.strsplit[1]=="glm()") | (model.strsplit[1]=="svyglm()") | (model.strsplit[1]=="gee()") | (model.strsplit[1]=="gam()")) {
              if ( fmt$model.function == TRUE ) { renamedgbl$models[1,i] <- paste(substr(model.strsplit[1],1,nchar(model.strsplit[1])-2),": ", fmt$model.family, model.strsplit[2], sep="") }
              else { renamedgbl$models[1,i] <- paste(fmt$model.family, model.strsplit[2], sep="")}
              
              renamedgbl$models[2,i] <- paste(fmt$model.link, model.strsplit[3], sep="")
            }
            else if ((model.strsplit[1]=="survreg()") | (model.strsplit[1]=="polr()")) {
              if ( fmt$model.function == TRUE ) { renamedgbl$models[1,i] <- paste(substr(model.strsplit[1],1,nchar(model.strsplit[1])-2),": ", fmt$model.dist, model.strsplit[2], sep="") }
              else { renamedgbl$models[1,i] <- paste(fmt$model.dist, model.strsplit[2], sep="")}
              renamedgbl$models[2,i] <- ""
            }
          }
        }
        
        if (sum(renamedgbl$models[2,]==rep("", times=length(gbl$models)))==length(gbl$models)) { how.many.model.rows <- 1}
        else { how.many.model.rows <- 2 }
        
        for (row in seq(from=1, to=how.many.model.rows)) {
          how.many.columns <- 0
          for (i in seq(1:length(gbl$models))) {
            how.many.columns <- how.many.columns + 1
            
            # write down if next column has different dependent variable, or if end of columns
            different.model <- FALSE
            if (i == length(gbl$models)) {different.model <- TRUE}
            else if ((as.character(gbl$models[i])) != (as.character(gbl$models[i+1]))) {different.model <- TRUE}
            else if ((as.character(gbl$dependent.variables[i])) != (as.character(gbl$dependent.variables[i+1]))) {different.model <- TRUE}   # subsume models under dependent variables
            
            if (fmt$multicolumn==FALSE) { different.model <- TRUE }
            
            if (different.model == TRUE) {
              if (how.many.columns == 1) {
                if (fmt$dec.mark.align == TRUE) {  
                  cat(" & \\multicolumn{1}{c}{",fmt$models.left,as.character(renamedgbl$models[row,i]),fmt$models.right,"}", sep="")
                }
                else {
                  cat(" & ",fmt$models.left,as.character(renamedgbl$models[row,i]),fmt$models.right, sep="")
                }
              }
              else {cat(" & \\multicolumn{",how.many.columns,"}{c}{",fmt$models.left,as.character(renamedgbl$models[row,i]),fmt$models.right,"}", sep="")}
              
              how.many.columns <- 0
            }
          }
          cat(" \\\\ \n")	
        }
        
        
        # underline models
        if (fmt$underline.models == TRUE) {
          how.many.columns <- 0
          for (i in seq(1:length(gbl$models))) {
            how.many.columns <- how.many.columns + 1
            
            # underline if next column has different dependent variable, or if end of columns
            different.model <- FALSE
            if (i == length(gbl$models)) {different.model <- TRUE}
            else if ((as.character(gbl$models[i])) != (as.character(gbl$models[i+1])))  {different.model <- TRUE}
            else if ((as.character(gbl$dependent.variables[i])) != (as.character(gbl$dependent.variables[i+1]))) {different.model <- TRUE}   # subsume models under dependent variables
            
            if (different.model== TRUE) {
              cat("\\cline{",(i-how.many.columns+1)+1,"-",i+1,"} ",sep="")
              
              how.many.columns <- 0
            }
          }
          cat("\n")
        }
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # column labels
    else if (part=="columns") {
      if (!is.null(fmt$column.labels)) {
        
        if (is.null(fmt$column.separate)) { fmt$column.separate <- 1 }
        
        # adjust column.separate to have the same number of columns as the table
        models.in.table <- length(gbl$models)
        models.in.col <- 0   
        for (i in seq(1:length(fmt$column.separate))) {       # count up how many models in column.separate
          models.in.col <- models.in.col + fmt$column.separate[i]
        }
        
        excess <- models.in.table - models.in.col
        
        # if too few column labels, add ones to column.separate
        if (excess > 0) {
          last.index <- length(fmt$column.separate)
          for (i in seq(1:excess)) {
            fmt$column.separate[last.index + i] <- 1
          }
        }
        
        # if too many column labels, then cut down
        if (excess < 0) {
          
          col.total <- 0
          newfmt$column.separate <- NULL
          
          for(i in seq(1:length(fmt$column.separate))) {
            col.total <- col.total + fmt$column.separate[i]
            if (col.total > models.in.table) {
              newfmt$column.separate[i] <- fmt$column.separate[i] - (col.total - models.in.table)
              if (newfmt$column.separate[i] == 0) { newfmt$column.separate <- newfmt$column.separate[-i] }
              break
            }
            else {
              newfmt$column.separate[i] <- fmt$column.separate[i]
            }
          }
          
          fmt$column.separate <- newfmt$column.separate
          
        }
        
        # output column labels
        col.position <- 1
        for (i in seq(1:length(fmt$column.separate))) {
          if (is.null(fmt$column.labels[col.position])) { fmt$column.labels[col.position] <- "" }
          if (is.na(fmt$column.labels[col.position])) { fmt$column.labels[col.position] <- "" }
          if (fmt$column.separate[i]==1) {
            if (fmt$dec.mark.align==TRUE) {
              cat(" & \\multicolumn{1}{c}{",fmt$column.left,fmt$column.labels[col.position],fmt$column.right,"}", sep="") 
            }
            else {
              cat(" & ",fmt$column.left,fmt$column.labels[col.position],fmt$column.right, sep="") 
            }
          }
          else {
            cat(" & \\multicolumn{",fmt$column.separate[i],"}{c}{",fmt$column.left,fmt$column.labels[col.position],fmt$column.right,"}", sep="") 
          }
          col.position <- col.position + 1
        }
        cat(" \\\\ \n")  
      }
    }
    
    # numbers
    else if (part=="numbers") {
      if ((fmt$model.numbers == TRUE) & (length(gbl$models)>1)) {
        .table.insert.space(fmt)
        cat(fmt$numbers.text)
        for (i in seq(1:length(gbl$models))) {
          if (fmt$dec.mark.align==TRUE) {
            if (fmt$numbers.roman == TRUE) { cat(" & \\multicolumn{1}{c}{",fmt$numbers.left,.roman.numeral(i),fmt$numbers.right,"}", sep="") }
            else { cat(" & \\multicolumn{1}{c}{",fmt$numbers.left,i,fmt$numbers.right,"}", sep="") }
          }
          else {
            if (fmt$numbers.roman == TRUE) { cat(" & ",fmt$numbers.left,.roman.numeral(i),fmt$numbers.right, sep="") }
            else { cat(" & ",fmt$numbers.left,i,fmt$numbers.right, sep="") }
          }
          
        }
        cat("\\\\ \n")
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # numbers
    else if (part=="objects") {
      if (fmt$object.names == TRUE) {
        .table.insert.space()
        for (i in seq(1:length(gbl$models))) {
          if (fmt$dec.mark.align==TRUE) {
            cat(" & \\multicolumn{1}{c}{",gbl$object.names[i],"}", sep="")
          }
          else {
            cat(" & ",gbl$object.names[i], sep="")
          }
        }
        cat("\\\\ \n")
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    ## coefficients
    else if (part=="coefficients") { 		
      .which.variable.label <<- 0
      if (is.null(fmt$covariate.labels)) { fmt$covariate.labels <- NA }
      
      # then, enter the coefficients
      
      for (i in seq(1:length(gbl$coefficient.variables))) { .table.enter.coefficients(i) }
      
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # number of observations
    else if (part=="N") { .print.table.statistic(gbl.var.name=gbl$N, fmt.var.name=fmt$N, decimal.digits=0, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # fixed effects table
    else if (part=="omit") {
      if ((!is.null(fmt$omit.regexp)) & (!is.null(fmt$omit.labels))) {
        fmt$omit.table <- matrix(fmt$omit.no, nrow=length(fmt$omit.regexp), ncol=length(gbl$models)) 
        for (i in seq(1:length(gbl$models))) {
          for (j in seq(1:length(fmt$omit.regexp))) {
            for (k in seq(1:length(gbl$coef.vars.by.model[,i]))) {
              relevant.coef.var <- gbl$coef.vars.by.model[k,i]
              if (length(grep(fmt$omit.regexp[j], relevant.coef.var, perl=fmt$perl, fixed=FALSE))!=0) {
                fmt$omit.table[j,i] <- fmt$omit.yes
              }
            }
          }
        }
        for (i in seq(1:length(fmt$omit.regexp))) {
          cat (fmt$omit.labels[i])
          for (j in seq(1:length(gbl$models))) {
            if (fmt$dec.mark.align == TRUE) {
              cat(" & \\multicolumn{1}{c}{",fmt$omit.table[i,j],"}", sep="")
            }
            else {
              cat(" & ",fmt$omit.table[i,j], sep="")
            }
          }
          cat(" \\\\ \n")
        }
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # R-squared
    else if (part=="R-squared") {	.print.table.statistic(gbl.var.name=gbl$R2, fmt.var.name=fmt$R2, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # max R-squared
    else if (part=="max R-squared") {	.print.table.statistic(gbl.var.name=gbl$max.R2, fmt.var.name=fmt$max.R2, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # adjusted R-squared
    else if (part=="adjusted R-squared") { .print.table.statistic(gbl.var.name=gbl$adj.R2, fmt.var.name=fmt$adj.R2, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # log likelihood
    else if (part=="log likelihood") { .print.table.statistic(gbl.var.name=gbl$LL, fmt.var.name=fmt$LL, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # Akaike Information Criterion (AIC)
    else if (part=="AIC") { .print.table.statistic(gbl.var.name=gbl$AIC, fmt.var.name=fmt$AIC, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # Bayesian Information Criterion (BIC)
    else if (part=="BIC") { .print.table.statistic(gbl.var.name=gbl$BIC, fmt.var.name=fmt$BIC, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # Scale Parameter
    else if (part=="scale") { .print.table.statistic(gbl.var.name=gbl$scale, fmt.var.name=fmt$scale, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # UBRE
    else if (part=="UBRE") { .print.table.statistic(gbl.var.name=gbl$UBRE, fmt.var.name=fmt$UBRE, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # sigma2
    else if (part=="sigma2") { .print.table.statistic(gbl.var.name=gbl$sigma2, fmt.var.name=fmt$sigma2, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    ## with degrees of freedom
    
    # residual standard error (sigma); standard error of the regression
    else if (substr(part,1,nchar("SER"))=="SER") { .print.table.statistic(gbl.var.name=gbl$SER, fmt.var.name=fmt$SER, part.string=part, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # F-statistic
    else if (substr(part,1,nchar("F statistic"))=="F statistic") { .print.table.statistic(gbl.var.name=gbl$F.stat, fmt.var.name=fmt$F.stat, part.string=part, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # theta
    else if (substr(part,1,nchar("theta"))=="theta") { .print.table.statistic(gbl.var.name=gbl$theta, fmt.var.name=fmt$theta, part.string=part, part.number=which.part.number, type.se=TRUE, fmt=fmt, gbl=gbl) }
    
    # rho
    else if (substr(part,1,nchar("rho"))=="rho") { .print.table.statistic(gbl.var.name=gbl$rho, fmt.var.name=fmt$rho, part.string=part, part.number=which.part.number, type.se=TRUE, fmt=fmt, gbl=gbl) }
    
    # Inverse Mills ratio
    else if (substr(part,1,nchar("Mills"))=="Mills") { .print.table.statistic(gbl.var.name=gbl$mills, fmt.var.name=fmt$mills, part.string=part, part.number=which.part.number, type.se=TRUE, fmt=fmt, gbl=gbl) }
    
    
    # Chi-squared
    else if (substr(part,1,nchar("chi2"))=="chi2") { .print.table.statistic(gbl.var.name=gbl$chi.stat, fmt.var.name=fmt$chi.stat, part.string=part, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # Wald Test
    else if (substr(part,1,nchar("Wald"))=="Wald") { .print.table.statistic(gbl.var.name=gbl$wald.stat, fmt.var.name=fmt$wald.stat, part.string=part, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # LR Test
    else if (substr(part,1,nchar("LR"))=="LR") { .print.table.statistic(gbl.var.name=gbl$lr.stat, fmt.var.name=fmt$lr.stat, part.string=part, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # Score (Logrank) Test
    else if (substr(part,1,nchar("logrank"))=="logrank") { .print.table.statistic(gbl.var.name=gbl$logrank.stat, fmt.var.name=fmt$logrank.stat, part.string=part, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # null deviance
    else if (substr(part,1,nchar("null deviance"))=="null deviance") { .print.table.statistic(gbl.var.name=gbl$null.deviance, fmt.var.name=fmt$null.deviance, part.string=part, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    # residual deviance
    else if (substr(part,1,nchar("residual deviance"))=="residual deviance") { .print.table.statistic(gbl.var.name=gbl$residual.deviance, fmt.var.name=fmt$residual.deviance, part.string=part, part.number=which.part.number, fmt=fmt, gbl=gbl) }
    
    ##
    
    # single horizontal line, no matter what
    else if (part=="-!") {
      cat("\\hline ")
      .table.insert.space(fmt)
      cat(" \n") 
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # single horizontal line, optional
    else if (part=="-") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline ")
        .table.insert.space(fmt)
        cat(" \n") 
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # double horizontal line, no matter what
    else if (part=="=!") {
      cat("\\hline \n") 
      cat("\\hline ")
      .table.insert.space(fmt)
      cat(" \n")
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # double horizontal line
    else if (part=="=") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline \n") 
        cat("\\hline ")
        .table.insert.space(fmt)
        cat(" \n") 
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # notes
    else if (part=="notes") {
      if (fmt$note != "") { cat(fmt$note) }
      for (i in seq(1:length(fmt$note.content))) {
        
        fmt$note.content[i] <- fmt$note.content[i]
        
        # print individual notes
        if (fmt$note == "") { cat("\\multicolumn{",length(gbl$models)+1,"}{",fmt$note.alignment,"}{",fmt$note.content[i],"} \\\\ \n", sep="") }
        else { cat(" & \\multicolumn{",length(gbl$models),"}{",fmt$note.alignment,"}{",fmt$note.content[i],"} \\\\ \n", sep="") }
      }
      .table.part.published[which.part.number] <<- TRUE
    }	
    
    # empty line
    else if (part==" ") {
      .table.empty.line(fmt);
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # additional lines
    else if (part=="additional") { fmt <- .print.additional.lines(part.number=which.part.number, fmt=fmt) }
  }


.stargazer.reg.table <-
  function(..., fmt, gbl) {
    
    list.of.models <- as.list(list(...))
    how.many.models <- length(list.of.models)
    
    # find how many models user wants to customize
    # max.user <- max(length(coef),length(se),length(t),length(p),length(ci.custom))
    for(x in c("coef", "se", "t", "p", "ci.custom")){
      x <- gbl[[x]] 
      length(x) <- how.many.models
      gbl[x] <- list(x)
      ## this is to not accidentally erase the list elements when they are NULL
    }
    # length(gbl$coef) <- length(gbl$se) <- length(gbl$t) <- length(gbl$p) <- length(gbl$ci.custom) <- how.many.models
    
    if (how.many.models >= 1) {
      suppressMessages(
        gbl <- .new.table(list.of.models[[1]], user.coef=gbl$coef[[1]], user.se=gbl$se[[1]], user.t=gbl$t[[1]], user.p=gbl$p[[1]], auto.t=gbl$t.auto, auto.p=gbl$p.auto, user.ci.lb=gbl$ci.custom[[1]][,1], user.ci.rb=gbl$ci.custom[[1]][,2], gbl = gbl)
      )
      if (how.many.models >= 2) {
        for (i in seq(from = 2,to = how.many.models)) { 
          #if (i <= max.user) {
          suppressMessages(
            gbl <- .add.model(list.of.models[[i]], user.coef=gbl$coef[[i]], user.se=gbl$se[[i]], user.t=gbl$t[[i]], user.p=gbl$p[[i]], auto.t=gbl$t.auto, auto.p=gbl$p.auto, user.ci.lb=gbl$ci.custom[[i]][,1], user.ci.rb=gbl$ci.custom[[i]][,2], fmt, gbl)
            ) 
          #}
          #else {
          #  suppressMessages(.add.model(list.of.models[[i]], user.coef=NULL, user.se=NULL, user.t=NULL, user.p=NULL, auto.t=t.auto, auto.p=p.auto, user.ci.lb=NULL, user.ci.rb=NULL))
          #}
        }
      }
      gbl <- .apply(gbl)
      gbl <- .order.reg.table(fmt, gbl)
      suppressMessages(.publish.table(fmt, gbl))
    }
  } 


.data.frame.table.header <-
  function(object) {
    .floating.header()
    
    .formatting.alignment <- paste("@{\\extracolsep{",fmt$column.sep.width,"}} ", sep="")
    for (i in seq(1:(length(names(object))))) {
      if (fmt$dec.mark.align == FALSE) {
        .formatting.alignment <- paste(.formatting.alignment, "c", sep="")
      }
      else {
        .formatting.alignment <- paste(.formatting.alignment, "D{", fmt$decimal.character,"}{", fmt$decimal.character,"}{-", fmt$s.round.digits,"} ", sep="")
      }
    }
    #
    
    cat("\\begin{tabular}{",.formatting.alignment,"} \n",sep="")
  }

.stargazer.data.frame.table <-
  function(object) {  
    
    # flip objects
    if (fmt$flip == TRUE) { 
      
      # keep row- and column names
      obj.rownames <- rownames(object)
      obj.colnames <- colnames(object)
      
      object <- as.data.frame(t(object))
      
      colnames(object) <- obj.rownames
      rownames(object) <- obj.colnames
    }
    
    if ((nrow(object) < 1) | (ncol(object) < 1)) {
      cat("% Error: Data frame must have at least one row and one column.\n")
    }
    else {
      object <- .order.data.frame(object, order)
      
      .table.info.comment()
      
      #create table header
      .data.frame.table.header(object)
      .table.insert.space()
      
      .table.part.published <<- as.vector(rep(NA, times=length(fmt$s.stat.parts)))    # to keep track what has been published (to deal intelligently with horizontal lines)
      .publish.horizontal.line <<- TRUE   # should non-compulsory horizontal lines be published? (yes, if something else published since the previous line)
      
      if (length(fmt$s.stat.parts)>=1) {
        for (i in seq(1:length(fmt$s.stat.parts))) {
          .data.frame.table.part(object,fmt$s.stat.parts[i], which.part.number = i)
          
          if (.table.part.published[i]==TRUE) { .publish.horizontal.line <<- TRUE }
          if ((fmt$s.stat.parts[i]=="-") | (fmt$s.stat.parts[i]=="-!") | (fmt$s.stat.parts[i]=="=") | (fmt$s.stat.parts[i]=="=!")) { .publish.horizontal.line <<- FALSE }
        }
      }
      
      cat("\\end{tabular} \n")
      if (fmt$floating == TRUE) { cat("\\end{", fmt$floating.environment,"} \n", sep="") }
      else if (!is.null(fmt$font.size)) {
        cat("\\endgroup \n",sep="")
      }
    }
  }

.data.frame.table.part <-
  function(object, part, which.part.number) {
    
    .table.part.published[which.part.number] <<- FALSE
    
    if ((part=="stat names") & (fmt$colnames==TRUE)) {
      
      x.which <- 0
      
      if (is.null(fmt$covariate.labels)) { fmt$covariate.labels <- NA }
      
      for (x in seq(1:length(names(object)))) {
        
        omitted <- FALSE
        
        if (!is.null(fmt$omit.regexp)) {
          for (j in seq(1:length(fmt$omit.regexp))) {
            if (length(grep(fmt$omit.regexp[j], names(object)[x], perl=fmt$perl, fixed=FALSE))!=0) { omitted <- TRUE  }
          }
        }
        
        if (!is.null(fmt$keep.regexp)) {
          omitted <- TRUE
          for (j in seq(1:length(fmt$keep.regexp))) {
            if (length(grep(fmt$keep.regexp[j], names(object)[x], perl=fmt$perl, fixed=FALSE))!=0) { omitted <- FALSE  }
          }
        }
        
        if (!is.null(fmt$omit.index)) {
          for (j in seq(1:length(fmt$omit.index))) {
            if (fmt$omit.index[j] == x) { omitted <- TRUE }
          }
        }
        
        if (!is.null(fmt$keep.index)) {
          omitted <- TRUE
          for (j in seq(1:length(fmt$keep.index))) {
            if (fmt$keep.index[j] == x) { omitted <- FALSE }
          }
        }
        
        if (omitted == FALSE) {
          
          x.which <- x.which + 1
          
          if (x >= 2) { cat(" & ", sep="")}
          
          # if underscore or ^ in variable name, then insert an escape \ before it
          name.printed <- .remove.special.chars(names(object)[x])
          
          if (is.na(fmt$covariate.labels[x.which])) {
            if (fmt$coefficient.variables.capitalize == TRUE) { name.printed <- toupper(name.printed) }
          }
          else { name.printed <- fmt$covariate.labels[x.which] }
          
          
          if (fmt$dec.mark.align==TRUE) {
            cat("\\multicolumn{1}{c}{",fmt$s.coefficient.variables.left, name.printed,fmt$s.coefficient.variables.right,"}", sep="")  
          }
          else {
            cat(fmt$s.coefficient.variables.left, name.printed,fmt$s.coefficient.variables.right, sep="")  
          }
        }
        
      }
      
      cat(" \\\\ \n")
      .table.part.published[which.part.number] <<- TRUE
    }
    
    if (substr(part,1,10)=="statistics") {
      for (y in seq(1:nrow(object))) {
        for (x in seq(1:length(names(object)))) {
          
          omitted <- FALSE
          
          if (!is.null(fmt$omit.regexp)) {
            for (j in seq(1:length(fmt$omit.regexp))) {
              if (length(grep(fmt$omit.regexp[j], names(object)[x], perl=fmt$perl, fixed=FALSE))!=0) { omitted <- TRUE  }
            }
          }
          
          if (!is.null(fmt$keep.regexp)) {
            omitted <- TRUE
            for (j in seq(1:length(fmt$keep.regexp))) {
              if (length(grep(fmt$keep.regexp[j], names(object)[x], perl=fmt$perl, fixed=FALSE))!=0) { omitted <- FALSE  }
            }
          }
          
          if (!is.null(fmt$omit.index)) {
            for (j in seq(1:length(fmt$omit.index))) {
              if (fmt$omit.index[j] == x) { omitted <- TRUE }
            }
          }
          
          if (!is.null(fmt$keep.index)) {
            omitted <- TRUE
            for (j in seq(1:length(fmt$keep.index))) {
              if (fmt$keep.index[j] == x) { omitted <- FALSE }
            }
          }
          
          
          if (omitted == FALSE) {     
            if (x >= 2) { cat(" & ", sep="") }
            
            .how.much.to.round <- fmt$round.digits
            if (is.numeric(object[y,x])) {
              
              if (.is.all.integers(object[y,x])) { .how.much.to.round <- 0 }
              
              rounded.object <- .iround(object[y,x], .how.much.to.round)
              
              if (fmt$dec.mark.align==TRUE) {
                cat(rounded.object, sep="")  
              }
              else {
                cat("$", rounded.object, "$",sep="")  
              }
            }
            else {
              adjusted.object <- .remove.special.chars(object[y, x])
              if (is.na(adjusted.object)) { adjusted.object <- "" }
              
              if (fmt$dec.mark.align==TRUE) {
                cat("\\multicolumn{1}{c}{", adjusted.object, "}", sep="")  
              }
              else {
                cat(adjusted.object, sep="")  
              }
              
            }
            
          }
          
          
          
        }
        # add empty lines
        how.many.empty.lines <- as.numeric(substr(part,11,nchar(part)))
        if (is.na(how.many.empty.lines)) { how.many.empty.lines <- 1 } 
        
        for (j in seq(1:how.many.empty.lines)) {
          cat(" \\\\ \n") 
        }
      }
      .table.part.published[which.part.number] <<- TRUE
    }
    
    
    # notes
    else if ((part=="notes") & (!is.null(fmt$s.note.content))) {
      if (fmt$s.note != "") cat(fmt$s.note)
      for (i in seq(1:length(fmt$s.note.content))) {
        fmt$s.note.content[i] <- fmt$s.note.content[i]
        if (fmt$s.note == "") { cat("\\multicolumn{",length(names(object)),"}{",fmt$s.note.alignment,"}{",fmt$s.note.content[i],"} \\\\ \n", sep="") }
        else { cat(" & \\multicolumn{",length(names(object)),"}{",fmt$s.note.alignment,"}{",fmt$s.note.content[i],"} \\\\ \n", sep="") }
      }
      .table.part.published[which.part.number] <<- TRUE
    }	
    
    # empty line
    else if (part==" ") {
      .table.empty.line()
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # horizontal line
    else if (part=="-!") {
      cat("\\hline ")
      .table.insert.space()
      cat(" \n")
      .table.part.published[which.part.number] <<- TRUE
    }
    
    else if (part=="-") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline ")
        .table.insert.space()
        cat(" \n")
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # double horizontal line
    else if (part=="=!") {
      cat("\\hline \n") 
      cat("\\hline ")
      .table.insert.space()
      cat(" \n")
      .table.part.published[which.part.number] <<- TRUE
    }
    
    else if (part=="=") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline \n") 
        cat("\\hline ")
        .table.insert.space()
        cat(" \n")
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
  }

.stargazer.summ.stat.table <-
  function(object, fmt, gbl) {
    
    if (length(names(object)) < 1) {
      cat("% Error: Data frame columns do not have any names.\n")
    }
    else if ((nrow(object) < 1) | (ncol(object) < 1)) {
      cat("% Error: Data frame must have at least one row and one column.\n")
    }
    else {
      
      object <- .order.data.frame(object, order, summary=T, fmt$rownames, fmt$perl)
      
      .table.info.comment(fmt, gbl)
      
      # create table header
      .summ.stat.table.header(object, fmt)
      .table.insert.space(fmt)
      
      for (i in seq(1:length(fmt$s.stat.parts))) {
        .summ.stat.table.part(object,fmt$s.stat.parts[i], fmt)
      }
      
      cat("\\end{tabular} \n")
      if (fmt$floating == TRUE) { cat("\\end{", fmt$floating.environment,"} \n", sep="") }
      else if (!is.null(fmt$font.size)) {
        cat("\\endgroup \n",sep="")
      }
    }
  }

.summ.stat.publish.statistic <-
  function(object, which.variable, which.statistic, fmt) {
    
    if ((is.numeric(object[,which.variable]) == TRUE) | ((is.logical(object[,which.variable])) & (fmt$summ.logical==TRUE)))  {
      
      if ((is.logical(object[,which.variable])) & (fmt$summ.logical==TRUE)) {
        temp.var <- rep(NA, time=length(object[,which.variable]))
        temp.var[object[,which.variable]==TRUE] <- 1
        temp.var[object[,which.variable]==FALSE] <- 0
      }
      else {
        temp.var <- object[,which.variable]
      }
      
      which.statistic <- tolower(which.statistic)
      if (which.statistic == "n") {
        return(.iround(sum(!is.na(temp.var)), 0, fmt=fmt))
      }
      else if (which.statistic == "nmiss") {
        return(.iround(sum(is.na(temp.var)), 0, fmt=fmt))
      }
      else if (which.statistic == "mean") {
        return(.iround(mean(temp.var, na.rm=TRUE), fmt$s.round.digits, fmt=fmt))
      }
      else if (which.statistic == "median") {
        median.value <- median(temp.var, na.rm=TRUE)
        
        if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- fmt$s.round.digits }
        else { 
          if (.is.all.integers(median.value) == TRUE) { how.much.to.round <- 0 }
          else { how.much.to.round <- 1 }
        }
        
        return(.iround(median.value, how.much.to.round, fmt=fmt))
      }
      else if (which.statistic == "sd") {
        return(.iround(sd(temp.var, na.rm=TRUE), fmt$s.round.digits, fmt=fmt))
      }
      else if (which.statistic == "min") {
        if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- fmt$s.round.digits }
        else { how.much.to.round <- 0 }
        
        return(.iround(min(temp.var, na.rm=TRUE), how.much.to.round, fmt=fmt))
      }
      else if (which.statistic == "max") {
        if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- fmt$s.round.digits }
        else { how.much.to.round <- 0 }
        
        return(.iround(max(temp.var, na.rm=TRUE), how.much.to.round, fmt=fmt))
      }
      else if (which.statistic == "mad") {
        return(.iround(mad(temp.var, na.rm=TRUE), fmt$s.round.digits, fmt=fmt))
      }
      else if (substr(which.statistic,1,1) == "p") {
        
        percentile.value <- quantile(temp.var, as.numeric(substr(which.statistic,2,nchar(which.statistic))) / 100, na.rm=TRUE)
        
        if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- fmt$s.round.digits }
        else { 
          if (.is.all.integers(percentile.value) == TRUE) { how.much.to.round <- 0 }
          else { how.much.to.round <- 1 }
        }
        
        return(.iround(percentile.value, how.much.to.round, fmt=fmt))
      }
    }
    else { return(NA) }
  }

.summ.stat.table.header <-
  function(object, fmt) {
    .floating.header(fmt)
    
    #
    .formatting.alignment <- paste("@{\\extracolsep{",fmt$column.sep.width,"}}l", sep="")
    
    if (fmt$flip == FALSE) { width <- length(fmt$s.statistics.list) }
    else { width <- length(.summ.stat.included(object)) }
    
    for (i in seq(1:width)) {
      if (fmt$dec.mark.align == FALSE) {
        .formatting.alignment <- paste(.formatting.alignment, "c", sep="")
      }
      else {
        .formatting.alignment <- paste(.formatting.alignment, "D{", fmt$decimal.character,"}{", fmt$decimal.character,"}{-", fmt$s.round.digits,"} ", sep="")
      }
    }
    #
    
    cat("\\begin{tabular}{",.formatting.alignment,"} \n",sep="")
  }

# figure out which variables are included --> returns indices of included variables
.summ.stat.included <- 
  function(object, fmt) {
    
    included <- NULL
    
    for (i in seq(1:length(names(object)))) {
      
      # skip all of this if omitted based on regular expression
      omitted <- FALSE
      
      if ((is.numeric(object[,i]) == TRUE) | (is.logical(object[,i]) & (fmt$summ.logical==TRUE))) {
        
        # also omit if all missing values
        if (!any(!is.na(object[,i]))) { omitted <- TRUE }
        
        if (!is.null(fmt$omit.regexp)) {
          for (j in seq(1:length(fmt$omit.regexp))) {
            if (length(grep(fmt$omit.regexp[j], names(object)[i], perl=fmt$perl, fixed=FALSE))!=0) { omitted <- TRUE  }
          }
        }
        
        if (!is.null(fmt$keep.regexp)) {
          omitted <- TRUE
          for (j in seq(1:length(fmt$keep.regexp))) {
            if (length(grep(fmt$keep.regexp[j], names(object)[i], perl=fmt$perl, fixed=FALSE))!=0) { omitted <- FALSE  }
          }
        }
        
        if (!is.null(fmt$omit.index)) {
          for (j in seq(1:length(fmt$omit.index))) {
            if (fmt$omit.index[j] == i) { omitted <- TRUE }
          }
        }
        
        if (!is.null(fmt$keep.index)) {
          omitted <- TRUE
          for (j in seq(1:length(fmt$keep.index))) {
            if (fmt$keep.index[j] == i) { omitted <- FALSE }
          }
        }
      }
      else { omitted <- TRUE }
      
      if (omitted == FALSE) { included <- c(included, i) }
    }
    
    return(included)
    
  }

.summ.stat.table.part <-
  function(object, part, fmt) {
    
    included <- .summ.stat.included(object, fmt)
    
    # with summary statistics, always publish horizontal line
    .publish.horizontal.line <<- TRUE
    
    if (part=="stat names") {
      cat(fmt$s.statistics.names.label, sep="")
      
      if (fmt$flip == FALSE) {
        if (length(fmt$s.statistics.list)>=1) {
          for (i in seq(1:length(fmt$s.statistics.list))) {
            for (j in seq(1:ncol(fmt$s.statistics.names))) {
              if ((substr(fmt$s.statistics.list[i],1,1)=="p") & (substr(fmt$s.statistics.list[i],1,1)==fmt$s.statistics.names[1,j])) {
                cat(" & \\multicolumn{1}{c}{", fmt$s.statistics.names.left, sub("!", substr(fmt$s.statistics.list[i],2,nchar(fmt$s.statistics.list[i])), fmt$s.statistics.names[2,j], ignore.case =FALSE, fixed=TRUE), fmt$s.statistics.names.right,"}", sep="")
              }
              else if (fmt$s.statistics.list[i]==fmt$s.statistics.names[1,j]) {
                cat(" & \\multicolumn{1}{c}{", fmt$s.statistics.names.left, fmt$s.statistics.names[2,j], fmt$s.statistics.names.right, "}", sep="")
              }
            }
          }
        }
      }
      else {   # flipped summary statistic table
        
        if (is.null(fmt$covariate.labels)) { fmt$covariate.labels <- NA }
        
        i.label <- 0
        
        for (i in included) {
          
          i.label <- i.label + 1
          
          # if underscore in variable name, then insert an escape \ before it
          name.printed <- .remove.special.chars(names(object)[i])
          
          cat(" & ")
          if (is.na(fmt$covariate.labels[i.label])) { 
            if ( fmt$s.coefficient.variables.capitalize == TRUE) { cat(fmt$s.coefficient.variables.left, toupper(name.printed), fmt$s.coefficient.variables.right, sep="") }
            else { cat(fmt$s.coefficient.variables.left, name.printed, fmt$s.coefficient.variables.right, sep="") }
          }
          else { cat(fmt$s.coefficient.variables.left, fmt$covariate.labels[i.label], fmt$s.coefficient.variables.right, sep="") }        
        }
        
      }
      
      cat(" \\\\ \n")
    }
    
    if (substr(part,1,10)=="statistics") {
      if (is.null(fmt$covariate.labels)) { fmt$covariate.labels <- NA }
      
      
      if (fmt$flip == FALSE) {
        
        i.label <- 0
        for (i in included) {
          i.label <- i.label + 1
          
          # if underscore in variable name, then insert an escape \ before it
          name.printed <- .remove.special.chars(names(object)[i])
          
          if (is.na(fmt$covariate.labels[i.label])) { 
            if ( fmt$s.coefficient.variables.capitalize == TRUE) { cat(fmt$s.coefficient.variables.left, toupper(name.printed), fmt$s.coefficient.variables.right, sep="") }
            else { cat(fmt$s.coefficient.variables.left, name.printed, fmt$s.coefficient.variables.right, sep="") }
          }
          else { cat(fmt$s.coefficient.variables.left, fmt$covariate.labels[i.label], fmt$s.coefficient.variables.right, sep="") }
          
          if (length(fmt$s.statistics.list)>=1) {
            for (j in seq(1:length(fmt$s.statistics.list))) {
              
              # if aligning decimal marks, need to use multicolumn for anything w/o decimal mark
              if (fmt$dec.mark.align == FALSE) {   # not aligning
                cat(" & ", .summ.stat.publish.statistic(object, i, fmt$s.statistics.list[j], fmt), sep="")  
              }
              else {     # aligning
                if (.is.all.integers(.summ.stat.publish.statistic(object, i, fmt$s.statistics.list[j], fmt))) {
                  cat(" & \\multicolumn{1}{c}{", .summ.stat.publish.statistic(object, i, fmt$s.statistics.list[j], fmt),"}", sep="")
                }
                else {
                  cat(" & ", .summ.stat.publish.statistic(object, i, fmt$s.statistics.list[j], fmt), sep="")
                }
              }	    
            }
          }
          
          # add empty lines
          how.many.empty.lines <- as.numeric(substr(part,11,nchar(part)))
          if (is.na(how.many.empty.lines)) { how.many.empty.lines <- 1 } 
          
          for (j in seq(1:how.many.empty.lines)) {
            cat(" \\\\ \n")
          }
        }
      }
      else {   # flipped
        if (length(fmt$s.statistics.list)>=1) {
          for (i in seq(1:length(fmt$s.statistics.list))) {
            for (j in seq(1:ncol(fmt$s.statistics.names))) {
              if ((substr(fmt$s.statistics.list[i],1,1)=="p") & (substr(fmt$s.statistics.list[i],1,1)==fmt$s.statistics.names[1,j])) {
                cat(fmt$s.statistics.names.left, sub("!", substr(fmt$s.statistics.list[i],2,nchar(fmt$s.statistics.list[i])), fmt$s.statistics.names[2,j], ignore.case =FALSE, fixed=TRUE), fmt$s.statistics.names.right, sep="")
              }
              else if (fmt$s.statistics.list[i]==fmt$s.statistics.names[1,j]) {
                cat(fmt$s.statistics.names.left, fmt$s.statistics.names[2,j], fmt$s.statistics.names.right, sep="")
              }
            }
            for (j in included) {
              # if aligning decimal marks, need to use multicolumn for anything w/o decimal mark
              if (fmt$dec.mark.align == FALSE) {   # not aligning
                cat(" & ", .summ.stat.publish.statistic(object, j, fmt$s.statistics.list[i], fmt), sep="")  
              }
              else {     # aligning
                if (.is.all.integers(.summ.stat.publish.statistic(object, j, fmt$s.statistics.list[i], fmt))) {
                  cat(" & \\multicolumn{1}{c}{", .summ.stat.publish.statistic(object, j, fmt$s.statistics.list[i], fmt),"}", sep="")
                }
                else {
                  cat(" & ", .summ.stat.publish.statistic(object, j, fmt$s.statistics.list[i], fmt), sep="")
                }
              } 
            }
            # add empty lines
            how.many.empty.lines <- as.numeric(substr(part,11,nchar(part)))
            if (is.na(how.many.empty.lines)) { how.many.empty.lines <- 1 } 
            
            for (k in seq(1:how.many.empty.lines)) {
              cat(" \\\\ \n")
            }
          }
        }
      }
    }
    
    # notes
    else if ((part=="notes") & (!is.null(fmt$s.note.content))) {
      if (fmt$s.note != "") cat(fmt$s.note)
      
      if (fmt$s.note=="") { offset <- 1 }
      else { offset <- 0 }
      
      if (fmt$flip == FALSE) { width <- length(fmt$s.statistics.list)+ offset }
      else { width <- length(included) + offset }
      
      for (i in seq(1:length(fmt$s.note.content))) {
        fmt$s.note.content[i] <- fmt$s.note.content[i]
        if (fmt$s.note == "") { cat("\\multicolumn{",width,"}{",fmt$s.note.alignment,"}{",fmt$s.note.content[i],"} \\\\ \n", sep="") }
        else { cat(" & \\multicolumn{",width,"}{",fmt$s.note.alignment,"}{",fmt$s.note.content[i],"} \\\\ \n", sep="") }
      }
    }	
    
    
    # empty line
    else if (part==" ") {
      .table.empty.line(fmt, gbl)
    }
    
    # horizontal line
    else if (part=="-!") {
      cat("\\hline ")
      .table.insert.space(fmt)
      cat(" \n")
    }
    
    else if (part=="-") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline ")
        .table.insert.space(fmt)
        cat(" \n")
      }
    }
    
    # double horizontal line
    else if (part=="=!") {
      cat("\\hline \n") 
      cat("\\hline ")
      .table.insert.space(fmt)
      cat(" \n")
    }
    
    else if (part=="=") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline \n") 
        cat("\\hline ")
        .table.insert.space(fmt)
        cat(" \n")
      }
    }
  }
