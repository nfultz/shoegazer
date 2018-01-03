
.new.table <-
  function(object.name, user.coef=NULL, user.se=NULL, user.t=NULL, user.p=NULL, auto.t=TRUE, auto.p=TRUE, user.ci.lb=NULL, user.ci.rb=NULL) {
    
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
      .summary.object <<- suppressMessages(summary(object.name, se=.format.rq.se))
    }
    
    model.num.total <- 1   # model number for multinom, etc.
    if (.model.identify(object.name) == "multinom") {
      if (!is.null(nrow(.summary.object$coefficients))) {
        model.num.total <-  nrow(.summary.object$coefficients)
      }
    }
    
    # set to null
    
    .global.models <<- NULL
    
    .global.dependent.variables <<- NULL
    .global.dependent.variables.written <<- NULL
    
    .global.coefficient.variables <<- NULL
    .global.coef.vars.by.model <<- NULL
    .global.coefficients <<- NULL
    .global.std.errors <<- NULL
    .global.ci.lb <<- NULL
    .global.ci.rb <<- NULL
    
    .global.t.stats <<- NULL
    .global.p.values <<- NULL
    
    .global.N <<- NULL
    .global.LL <<- NULL
    .global.R2 <<- NULL
    .global.max.R2 <<- NULL
    .global.adj.R2 <<- NULL
    .global.AIC <<- NULL
    .global.BIC <<- NULL
    .global.scale <<- NULL
    .global.UBRE <<- NULL
    .global.sigma2 <<- NULL
    .global.theta <<- NULL
    .global.rho <<- NULL
    .global.mills <<- NULL
    
    .global.SER <<- NULL
    .global.F.stat <<- NULL
    .global.chi.stat <<- NULL
    .global.wald.stat <<- NULL
    .global.lr.stat <<- NULL
    .global.logrank.stat <<- NULL
    .global.null.deviance <<- NULL
    .global.residual.deviance <<- NULL
    
    for (model.num in 1:model.num.total) {
      
      .global.models <<- c(.global.models, suppressMessages(as.vector(.model.identify(object.name))))
      
      .global.dependent.variables <<- c(.global.dependent.variables, suppressMessages(.dependent.variable(object.name, model.num)))
      .global.dependent.variables.written <<- c(.global.dependent.variables.written, suppressMessages(.dependent.variable.written(object.name, model.num)))
      .global.coefficient.variables <<- suppressMessages(.coefficient.variables(object.name))
      
      .global.coef.vars.by.model <<-  suppressMessages(cbind(.global.coef.vars.by.model, .global.coefficient.variables))
      
      get.coef <- suppressMessages(.get.coefficients(object.name, user.coef, model.num=model.num))
      get.se <- suppressMessages(.get.standard.errors(object.name, user.se, model.num=model.num))
      
      .global.coefficients <<- cbind(.global.coefficients, get.coef)
      .global.std.errors <<- cbind(.global.std.errors, get.se)
      
      .global.ci.lb <<- suppressMessages(cbind(.global.ci.lb, .get.ci.lb(object.name, user.ci.lb, model.num=model.num)))
      .global.ci.rb <<- suppressMessages(cbind(.global.ci.rb, .get.ci.rb(object.name, user.ci.rb, model.num=model.num))) 
      
      feed.coef <- NA; feed.se <- NA
      if (!is.null(get.coef)) { feed.coef <- get.coef }
      if (!is.null(get.se)) { feed.se <- get.se }
      if (!is.null(user.coef)) { feed.coef <- user.coef }   # feed user-defined coefficients, if available
      if (!is.null(user.se)) { feed.se <- user.se }   # feed user-defined std errors, if available
      
      .global.t.stats <<- suppressMessages(cbind(.global.t.stats, .get.t.stats(object.name, user.t, auto.t, feed.coef, feed.se, user.coef, user.se, model.num=model.num)))
      .global.p.values <<- suppressMessages(cbind(.global.p.values, .get.p.values(object.name, user.p, auto.p, feed.coef, feed.se, user.coef, user.se, model.num=model.num)))
      
      
      .global.N <<- c(.global.N, suppressMessages(.number.observations(object.name)))
      .global.LL <<- c(.global.LL, suppressMessages(.log.likelihood(object.name)))
      .global.R2 <<- c(.global.R2, suppressMessages(.r.squared(object.name)))
      .global.max.R2 <<- c(.global.max.R2, suppressMessages(.max.r.squared(object.name)))
      .global.adj.R2 <<- c(.global.adj.R2, suppressMessages(.adj.r.squared(object.name)))
      .global.AIC <<- c(.global.AIC, suppressMessages(.AIC(object.name)))
      .global.BIC <<- c(.global.BIC, suppressMessages(.BIC(object.name)))
      .global.scale <<- c(.global.scale, suppressMessages(.get.scale(object.name)))
      .global.UBRE <<- c(.global.UBRE, suppressMessages(.gcv.UBRE(object.name)))
      .global.sigma2 <<- c(.global.sigma2, suppressMessages(.get.sigma2(object.name)))
      
      .global.rho <<- cbind(suppressMessages(.get.rho(object.name)))
      .global.mills <<- cbind(suppressMessages(.get.mills(object.name)))
      .global.theta <<- cbind(suppressMessages(.get.theta(object.name)))
      .global.SER <<- cbind(suppressMessages(.SER(object.name)))
      .global.F.stat <<- cbind(suppressMessages(.F.stat(object.name)))
      .global.chi.stat <<- cbind(suppressMessages(.chi.stat(object.name)))
      .global.wald.stat <<- cbind(suppressMessages(.wald.stat(object.name)))
      .global.lr.stat <<- cbind(suppressMessages(.lr.stat(object.name)))
      .global.logrank.stat <<- cbind(suppressMessages(.logrank.stat(object.name)))
      .global.null.deviance <<- cbind(suppressMessages(.null.deviance(object.name)))
      .global.residual.deviance <<- cbind(suppressMessages(.residual.deviance(object.name)))
    }
    
  }



.table.empty.line <-
  function() {
    if (.format.no.space == FALSE) {
      cat(" ")
      for (i in seq(1:length(.global.models))) {
        cat("& ")
      }
      cat("\\\\ \n")
    }
  }

.table.enter.coefficients <-
  function(which.variable) {
    
    if (which.variable > length(.global.coefficients)) {
      return();
    }
    
    local.coefficient.var.name <- .global.coefficient.variables[which.variable]
    
    #skip all of this if omitted based on regular expression
    omitted <- FALSE
    
    if (!is.null(.format.omit.regexp)) {
      for (i in seq(1:length(.format.omit.regexp))) {
        if (length(grep(.format.omit.regexp[i], local.coefficient.var.name, perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE	}
      }
    }
    
    if (!is.null(.format.keep.regexp)) {
      omitted <- TRUE
      for (i in seq(1:length(.format.keep.regexp))) {
        if (length(grep(.format.keep.regexp[i], local.coefficient.var.name, perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE	}
      }
    }
    
    if (!is.null(.format.omit.index)) {
      for (i in seq(1:length(.format.omit.index))) {
        if (.format.omit.index[i] == which.variable) { omitted <- TRUE }
      }
    }
    
    if (!is.null(.format.keep.index)) {
      omitted <- TRUE
      for (i in seq(1:length(.format.keep.index))) {
        if (.format.keep.index[i] == which.variable) { omitted <- FALSE }
      }
    }
    
    if (omitted == FALSE) {
      
      .which.variable.label <<- .which.variable.label + 1
      
      # remove final -TRUE (added by Zelig) from dummy variables
      if (substr(local.coefficient.var.name, nchar(local.coefficient.var.name)-3, nchar(local.coefficient.var.name)) == "TRUE") {
        
        ### only remove TRUE if added by Zelig, rather than pre-existing in the formula name
        if (length(grep(local.coefficient.var.name, .global.formulas.rhs,fixed=TRUE))==0) {    
          local.coefficient.var.name <- substr(local.coefficient.var.name, 1, nchar(local.coefficient.var.name)-4)
        }
      }
      
      # remove everything before and including he last dollar sign from variable name
      temp <- strsplit(local.coefficient.var.name,"$",fixed=TRUE)
      local.coefficient.var.name <- temp[[1]][length(temp[[1]])]
      
      # if underscore or ^ in variable name, then insert an escape \ before it
      local.coefficient.var.name <- .remove.special.chars(local.coefficient.var.name)
      
      if (length(.format.coefficient.table.parts)>=1) {
        for (i in seq(1:length(.format.coefficient.table.parts))) {
          .coefficient.table.part(part=.format.coefficient.table.parts[i], which.variable, variable.name=local.coefficient.var.name)
        }
      }
    }
  }

.table.header <-
  function() {
    .floating.header()
    
    #
    .formatting.alignment <- paste("@{\\extracolsep{",.format.column.sep.width,"}}l", sep="")
    for (i in seq(1:length(.global.models))) {
      if (.format.dec.mark.align==FALSE) {
        .formatting.alignment <- paste(.formatting.alignment, "c", sep="")
      }
      else {
        .formatting.alignment <- paste(.formatting.alignment, "D{", .format.decimal.character,"}{", .format.decimal.character,"}{-", .format.round.digits,"} ", sep="")
      }
    }
    #
    
    cat("\\begin{tabular}{",.formatting.alignment,"} \n",sep="")
  }

.table.info.comment <-
  function() {
    cat("\n")
    if (.format.header==TRUE) {
      cat("% Table created by ", .global.package.name, " v.", .global.package.version, " by ", .global.package.author.name, ", ", .global.package.author.affiliation, ". E-mail: ", .global.package.author.email, "\n", sep="")  
      cat("% Date and time:", format(Sys.time(), "%a, %b %d, %Y - %X"))
      cat("\n")
      
      required.latex.packages <- NULL
      if (.format.dec.mark.align==TRUE) { required.latex.packages <- c(required.latex.packages, "dcolumn") }
      if (.format.floating.environment=="sidewaystable") { required.latex.packages <- c(required.latex.packages, "rotating") }
      
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
  function() {
    cat("\\\\[",.format.space.size,"]",sep="")
  }


.adjust.settings.style <-
  function(what.style) {
    style <- tolower(what.style)
    
    if (style == "all") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*(p)", "SER(df)","F statistic(df)*(p)","chi2(df)*(p)","Wald(df)*(p)","LR(df)*(p)","logrank(df)*(p)","AIC","BIC","UBRE","rho(se)*(p)","Mills(se)*(p)","residual deviance(df)*","null deviance(df)*","=!","notes")  
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error","t-stat","p-value")  
    }
    
    else if (style == "all2") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*(p)", "SER(df)","F statistic(df)*(p)","chi2(df)*(p)","Wald(df)*(p)","LR(df)*(p)","logrank(df)*(p)","AIC","BIC","UBRE","rho(se)*(p)","Mills(se)*(p)","residual deviance(df)*","null deviance(df)*","=!","notes")  
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")  
    }
    
    # aer = American Economic Review
    else if (style == "aer") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","-!","notes")
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.until.nonzero.digit <<- FALSE
      .format.max.extra.digits <<- 0    
      
      .format.model.left <<- ""
      .format.model.right <<- ""
      
      .format.note <<- "\\textit{Notes:}"
      .format.note.alignment <<- "l"
      .format.note.content <<- c("$^{***}$Significant at the [***] percent level.","$^{**}$Significant at the [**] percent level.","$^{*}$Significant at the [*] percent level.")
    }
    
    # ajps = American Journal of Political Science
    else if (style == "ajps") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      .format.digit.separator <<- ""
      .format.dependent.variables.left <<- "\\textbf{"
      .format.dependent.variables.right <<- "}"
      .format.column.left <<- "\\textbf{"
      .format.column.right <<- "}"
      .format.models.left <<- "\\textbf{"
      .format.models.right <<- "}"
      .format.numbers.left <<- "\\textbf{Model "
      .format.numbers.right <<- "}"
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error") 
      .format.N <<- "N"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "Chi-square"
      .format.R2 <<- "R-squared"
      .format.adj.R2 <<- "Adj. R-squared"
      .format.max.R2 <<- "Max. R-squared"
      .format.note <<- ""
      .format.note.content <<- c("$^{***}$p $<$ [.***]; $^{**}$p $<$ [.**]; $^{*}$p $<$ [.*]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }  
    
    # ajs = American Journal of Sociology
    else if (style == "ajs") {
      .format.table.parts <<- c(" ","=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","-!","notes")
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variables.capitalize <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.numbers.left <<- ""
      .format.numbers.right <<- ""
      
      .format.until.nonzero.digit <<- FALSE
      .format.max.extra.digits <<- 0    
      
      .format.model.left <<- ""
      .format.model.right <<- ""
      
      .format.note <<- "\\textit{Notes:}"
      .format.note.alignment <<- "l"
      .format.note.content <<- c("$^{*}$P $<$ [.*]","$^{**}$P $<$ [.**]","$^{***}$P $<$ [.***]")
      .format.cutoffs <<- c(0.05, 0.01, 0.001)
      
      .format.initial.zero <<- FALSE
    }
    
    # apsr = American Political Science Review
    else if (style == "apsr") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.models.left <<- ""
      .format.models.right <<- ""
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")
      .format.N <<- "N"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "chi$^{2}$"
      .format.note <<- ""
      .format.note.content <<- c("$^{*}$p $<$ [.*]; $^{**}$p $<$ [.**]; $^{***}$p $<$ [.***]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }
    
    # asq = Administrative Science Quarterly
    else if (style == "asq") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.digit.separator <<- ""
      .format.dependent.variables.left <<- "\\textbf{"
      .format.dependent.variables.right <<- "}"
      .format.column.left <<- "\\textbf{"
      .format.column.right <<- "}"
      .format.models.left <<- "\\textbf{"
      .format.models.right <<- "}"
      .format.numbers.left <<- "\\textbf{Model "
      .format.numbers.right <<- "}"
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error") 
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "Chi-square"
      .format.R2 <<- "R-squared"
      .format.adj.R2 <<- "Adj. R-squared"
      .format.max.R2 <<- "Max. R-squared"
      .format.note <<- ""
      .format.note.content <<- c("$^{\\bullet}$p $<$ [.*]; $^{\\bullet\\bullet}$p $<$ [.**]; $^{\\bullet\\bullet\\bullet}$p $<$ [.***]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
      .format.stars <<- "\\bullet"
    }  
    
    # asr = American Sociological Review
    else if (style == "asr") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.models.left <<- ""
      .format.models.right <<- ""
      .format.coefficient.table.parts <<- c("variable name","coefficient*")
      .format.N <<- "\\textit{N}"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "chi$^{2}$"
      .format.note <<- ""
      .format.note.content <<- c("$^{*}$p $<$ [.*]; $^{**}$p $<$ [.**]; $^{***}$p $<$ [.***]")
      .format.cutoffs <<- c(0.05, 0.01, 0.001)
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }
    
    # "demography" = Demography
    else if (style == "demography") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.models.left <<- ""
      .format.models.right <<- ""
      .format.numbers.left <<- "Model "
      .format.numbers.right <<- ""
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")
      .format.N <<- "\\textit{N}"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "Chi-Square"
      .format.note <<- ""
      .format.note.content <<- c("$^{*}$p $<$ [.*]; $^{**}$p $<$ [.**]; $^{***}$p $<$ [.***]")
      .format.cutoffs <<- c(0.05, 0.01, 0.001)
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }
    
    # io = International Organization
    else if (style == "io") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")
      .format.coefficient.variables.capitalize <<- TRUE
      .format.s.coefficient.variables.capitalize <<- TRUE
      .format.intercept.name <<- "Constant"
      .format.N <<- "\\textit{Observations}"
      .format.AIC <<- "\\textit{Akaike information criterion}"
      .format.BIC <<- "\\textit{Bayesian information criterion}"
      .format.chi.stat <<- "\\textit{Chi-square}"
      .format.logrank.stat <<- "\\textit{Score (logrank) test}"
      .format.lr.stat <<- "\\textit{LR test}"
      .format.max.R2 <<- "\\textit{Maximum R-squared}"
      .format.R2 <<- "\\textit{R-squared}"
      .format.adj.R2 <<- "\\textit{Adjusted R-squared}"
      .format.UBRE <<- "\\textit{UBRE}"
      .format.F.stat <<- "\\textit{F statistic}"
      .format.LL <<- "\\textit{Log likelihood}"
      .format.SER <<- "\\textit{Residual standard error}"
      .format.null.deviance <<- "\\textit{Null deviance}"
      .format.residual.deviance <<- "\\textit{Residual deviance}"
      .format.scale <<- "\\textit{Scale}"
      .format.wald.stat <<- "\\textit{Wald test}"
      .format.note <<- "\\textit{Notes:}"
      .format.note.content <<- c("$^{***}$p $<$ [.***]; $^{**}$p $<$ [.**]; $^{*}$p $<$ [.*]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }
    
    
    # jpam = Journal of Policy Analysis and Management
    else if (style == "jpam") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.models.left <<- ""
      .format.models.right <<- ""
      .format.numbers.left <<- "Model "
      .format.numbers.right <<- ""
      .format.numbers.roman <<- TRUE
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")
      .format.intercept.bottom <<- FALSE
      .format.intercept.top <<- TRUE
      .format.N <<- "N"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.note <<- "\\textit{Note:}"
      .format.note.content <<- c("$^{***}$p $<$ [.***]; $^{**}$p $<$ [.**]; $^{*}$p $<$ [.*]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
      .format.s.statistics.names <<- cbind(c("n","N"), c("nmiss","missing"), c("mean","Mean"), c("sd","SD"), c("median","Median"), c("min","Minimum"), c("max","Maximum"), c("mad","Median Abs. Dev."), c("p","Percentile(!)"))
      
    }
    
    # "qje" = Quarterly Journal of Economics
    else if (style=="qje") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")    
      .format.dependent.variable.text.on <<- FALSE
      .format.s.stat.parts <<- c("-!","stat names","=","statistics1","=!","notes")
      .format.N <<- "\\textit{N}"
      .format.note <<- "\\textit{Notes:}"
      .format.note.content <<- c("$^{***}$Significant at the [***] percent level.", "$^{**}$Significant at the [**] percent level.", "$^{*}$Significant at the [*] percent level.") 
    }
    
    # find style based on journal ("default" or other)
    else if (style=="commadefault") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")
      .format.digit.separator <<- " "
      .format.decimal.character <<- ","
    }
    
    else if (style=="default") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")
    }
  }

.set.font.size <- 
  function() {
    if (!is.null(.format.font.size)) {
      cat("\\", .format.font.size," \n", sep="")
    }
  }

.floating.header <-
  function() {
    if (.format.floating==TRUE) {
      cat("\\begin{", .format.floating.environment,"}[", .format.table.placement,"] \\centering \n",sep="")
      cat("  \\caption{", .format.title, "} \n",sep="")   
      cat("  \\label{", .format.label, "} \n",sep="")
      .set.font.size()
    }
    else if (!is.null(.format.font.size)) { # set font size using begingroup
      cat("\\begingroup \n", sep="")
      .set.font.size()
    }
  }



.print.table.statistic <-
  function(.global.var.name, .format.var.name, decimal.digits=.format.round.digits, part.string="", part.number=NULL, type.se=FALSE) {
    
    # default values
    report.df <- FALSE
    report.p.value <- FALSE
    significance.stars <- FALSE
    report.se <- FALSE
    report.tstat <- FALSE
    intelligent.df <- .format.intelligent.df
    force.math <- FALSE
    
    # reporting of df, p-value, significance stars, standard errors, t-stats
    if (length(grep("(df)", part.string,fixed=TRUE))!=0) { report.df <- TRUE } 
    if (length(grep("(se)", part.string,fixed=TRUE))!=0) { report.se <- TRUE }
    if (length(grep("(t)", part.string,fixed=TRUE))!=0) { report.tstat <- TRUE }
    if (length(grep("(p)", part.string,fixed=TRUE))!=0) { report.p.value <- TRUE } 
    if (length(grep("*", part.string,fixed=TRUE))!=0) { significance.stars <- TRUE } 
    
    
    # first for vectors (statistics without, say, degrees of freedom)
    if (is.vector(.global.var.name) == TRUE) {
      if (sum(!is.na(.global.var.name))!=0) {
        cat (.format.var.name)
        for (i in seq(1:length(.global.models))) {
          if (!is.na(.global.var.name[i])) { 
            if (.format.dec.mark.align == TRUE) {
              cat(" & \\multicolumn{1}{c}{",.iround(.global.var.name[i], decimal.digits),"}", sep="")
            }
            else {
              cat(" & ",.iround(.global.var.name[i], decimal.digits), sep="")
            }
          }
          else { cat(" & ", sep="") }
        }
        cat(" \\\\ \n")
        .table.part.published[part.number] <<- TRUE
      }
    }
    else if ((is.matrix(.global.var.name) == TRUE) & (type.se == FALSE)) {     # for statistics that have degrees of freedom
      if (sum(!is.na(as.vector(.global.var.name["statistic",])))!=0) {
        
        # intelligent df reporting (figure out whether only report it on left side, or also)
        report.df.left.column <- FALSE
        
        # whittle down unique values
        df.all.together <- NULL
        for (i in seq(1:length(.global.models))) {
          df.string <- ""
          for (j in seq(1:(nrow(.global.var.name)- 2))) {
            df.string <- paste(df.string,";",as.character(.global.var.name[paste("df",as.character(j),sep=""),i]),sep="")
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
        cat (.format.var.name)
        
        # report df on left side w/ intelligent reporting
        if (report.df.left.column == TRUE) {
          if (report.df == TRUE) {
            
            cat(" ",.format.df.left,sep="")
            df.list <- unlist(strsplit(df.all.together.no.NA.unique[1],";"))
            
            for (i in seq(from=2, to=length(df.list))) {
              if (i>=3) { cat(.format.df.separator) }
              cat(df.list[i],sep="")
            }
            cat(.format.df.right,sep="")
          }
        }
        
        # now, go column by column
        for (i in seq(1:length(.global.models))) {
          if (!is.na(.global.var.name["statistic",i])) {
            
            if (.format.dec.mark.align==TRUE) {
              cat(" & \\multicolumn{1}{c}{",.iround(.global.var.name["statistic",i], decimal.digits), sep="") 
              force.math <- TRUE
            }
            else {
              cat(" & ",.iround(.global.var.name["statistic",i], decimal.digits), sep="")
            }
            
            # significance stars
            if ((significance.stars == TRUE) & (!is.na(.global.var.name["p-value",i]))) { .enter.significance.stars(.global.var.name["p-value",i], force.math) }
            
            
            # degrees of freedom - only report by statistics if not in the left column already
            if (report.df.left.column == FALSE) {
              if ((report.df == TRUE) & (!is.na(.global.var.name["df1",i]))) {
                cat(" ",.format.df.left,sep="")
                for (j in seq(1:(nrow(.global.var.name)- 2))) {
                  if (!is.na(.global.var.name[paste("df",as.character(j),sep=""),i])) {
                    if (j>=2) { cat(.format.df.separator) }
                    cat(.global.var.name[paste("df",as.character(j),sep=""),i],sep="")
                  }
                }
                cat(.format.df.right,sep="")
              }
            }
            
            # p-values
            if ((report.p.value == TRUE) & (!is.na(.global.var.name["p-value",i]))) {
              cat(" ",.format.p.value.left,sep="")
              if (!is.na(.global.var.name[paste("df",as.character(j),sep=""),i])) { 
                cat(.iround(.global.var.name["p-value",i],.format.round.digits, round.up.positive=TRUE),sep="") 
              }
              cat(.format.p.value.right,sep="")
            }
            
            if (.format.dec.mark.align==TRUE) {
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
    else if ((is.matrix(.global.var.name) == TRUE) & (type.se == TRUE)) {       # for statistics that have a standard error
      if (sum(!is.na(as.vector(.global.var.name["statistic",])))!=0) {
        
        # write down the line	
        cat (.format.var.name)
        
        # now, go column by column
        for (i in seq(1:length(.global.models))) {
          if (!is.na(.global.var.name["statistic",i])) { 
            
            if (.format.dec.mark.align == TRUE) {
              cat(" & \\multicolumn{1}{c}{",.iround(.global.var.name["statistic",i], decimal.digits), sep="")  
            }
            else {
              cat(" & ",.iround(.global.var.name["statistic",i], decimal.digits), sep="")
            }
            
            
            # significance stars
            if ((significance.stars == TRUE) & (!is.na(.global.var.name["p-value",i]))) { .enter.significance.stars(.global.var.name["p-value",i], force.math) }
            
            # standard errors
            if ((report.se == TRUE) & (!is.na(.global.var.name["se",i]))) { cat(" ",.format.se.left,.iround(.global.var.name["se",i], decimal.digits),.format.se.right,sep="") }
            
            # t-statistics
            if ((report.tstat == TRUE) & (!is.na(.global.var.name["tstat",i]))) { cat(" ",.format.tstat.left, .iround(.global.var.name["tstat",i], decimal.digits),.format.tstat.right,sep="") }
            
            # p-values
            if ((report.p.value == TRUE) & (!is.na(.global.var.name["p-value",i]))) { cat(" ",.format.p.value.left,.iround(.global.var.name["p-value",i], decimal.digits),.format.p.value.right,sep="") }
            
            if (.format.dec.mark.align == TRUE) {
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
  function() {
    
    .table.info.comment()
    
    # table header
    
    .table.header()
    .table.insert.space()
    
    .table.part.published <<- as.vector(rep(NA, times=length(.format.table.parts)))    # to keep track what has been published (to deal intelligently with horizontal lines)
    .publish.horizontal.line <<- TRUE   # should non-compulsory horizontal lines be published? (yes, if something else published since the previous line)
    
    if (length(.format.table.parts)>=1) {
      for (i in seq(1:length(.format.table.parts))) {
        .publish.table.part(part=.format.table.parts[i], which.part.number=i)
        
        if (.table.part.published[i]==TRUE) { .publish.horizontal.line <<- TRUE }
        if ((.format.table.parts[i]=="-") | (.format.table.parts[i]=="-!") | (.format.table.parts[i]=="=") | (.format.table.parts[i]=="=!")) { .publish.horizontal.line <<- FALSE }
      }
    }
    
    cat("\\end{tabular} \n")
    if (.format.floating == TRUE) { cat("\\end{", .format.floating.environment,"} \n", sep="") }
    else if (!is.null(.format.font.size)) {
      cat("\\endgroup \n",sep="")
    }
    
  }

.publish.table.part <-
  function(part, which.part.number) {
    
    .table.part.published[which.part.number] <<- FALSE
    
    # dependent variable label line
    if (part=="dependent variable label") {
      if (.format.dependent.variable.text.on == TRUE) { 
        cat(" & \\multicolumn{",length(.global.models),"}{c}{",.format.dependent.variable.text, "} \\\\ \n", sep="")
        if (.format.dependent.variable.text.underline == TRUE) { cat("\\cline{2-",length(.global.models)+1,"} \n", sep="") }
      }
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # dependent variables
    else if (part=="dependent variables") {
      .table.insert.space()
      cat(.format.dependent.variables.text)
      how.many.columns <- 0
      label.counter <- 0
      
      for (i in seq(1:length(.global.models))) {
        if (is.null(.format.dep.var.labels)) { .format.dep.var.labels <<- NA }
        how.many.columns <- how.many.columns + 1
        
        # write down if next column has different dependent variable, or if end of columns
        different.dependent.variable <- FALSE
        if (i == length(.global.models)) {different.dependent.variable <- TRUE}
        else if ((as.character(.global.dependent.variables[i])) != (as.character(.global.dependent.variables[i+1])))  {different.dependent.variable <- TRUE}
        
        if (.format.multicolumn==FALSE) { different.dependent.variable <- TRUE }
        
        if (different.dependent.variable == TRUE) {
          label.counter <- label.counter + 1 
          if (how.many.columns == 1) {
            if (.format.dec.mark.align==TRUE) {
              if (is.na(.format.dep.var.labels[label.counter])) {
                if (.format.dependent.variables.capitalize == TRUE) { cat(" & \\multicolumn{1}{c}{",.format.dependent.variables.left,toupper(as.character(.global.dependent.variables.written[i])),.format.dependent.variables.right,"}", sep="") }
                else { cat(" & \\multicolumn{1}{c}{",.format.dependent.variables.left,as.character(.global.dependent.variables.written[i]),.format.dependent.variables.right,"}", sep="") }
              }
              else { cat(" & \\multicolumn{1}{c}{",.format.dependent.variables.left,.format.dep.var.labels[label.counter],.format.dependent.variables.right,"}", sep="") }
            }
            else {
              if (is.na(.format.dep.var.labels[label.counter])) {
                if (.format.dependent.variables.capitalize == TRUE) { cat(" & ",.format.dependent.variables.left,toupper(as.character(.global.dependent.variables.written[i])),.format.dependent.variables.right, sep="") }
                else { cat(" & ",.format.dependent.variables.left,as.character(.global.dependent.variables.written[i]),.format.dependent.variables.right, sep="") }
              }
              else { cat(" & ",.format.dependent.variables.left,.format.dep.var.labels[label.counter],.format.dependent.variables.right, sep="") }
            }
          }
          else {
            if (is.na(.format.dep.var.labels[label.counter])) {
              if (.format.dependent.variables.capitalize == TRUE) {cat(" & \\multicolumn{",how.many.columns,"}{c}{",.format.dependent.variables.left,toupper(as.character(.global.dependent.variables.written[i])),.format.dependent.variables.right,"}", sep="")}
              else {cat(" & \\multicolumn{",how.many.columns,"}{c}{",.format.dependent.variables.left,as.character(.global.dependent.variables.written[i]),.format.dependent.variables.right,"}", sep="")}
            }
            else {cat(" & \\multicolumn{",how.many.columns,"}{c}{",.format.dependent.variables.left,.format.dep.var.labels[label.counter],.format.dependent.variables.right,"}", sep="")}
          }
          
          how.many.columns <- 0
        }
      }
      cat(" \\\\ \n")
      
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # models
    else if (part=="models")  {
      if ((.format.model.names.include==TRUE) & ((.format.models.skip.if.one == FALSE) | ((.format.models.skip.if.one == TRUE) & (length(unique(.global.models))>=2)))) {
        
        .table.insert.space()
        cat(.format.models.text)
        
        # rename models based on .formatting preferences
        renamed.global.models <- as.matrix(rbind(.global.models, rep("", times=length(.global.models))))
        for (i in seq(1:length(.global.models))) {
          for (j in seq(1:ncol(.format.model.names))) {
            model.strsplit <- unlist(strsplit(.global.models[i], split="#"))
            if (.global.models[i]==.format.model.names[1,j]) { 
              renamed.global.models[1,i] <- .format.model.names[2,j] 
              renamed.global.models[2,i] <- .format.model.names[3,j]
            }
            else if ((model.strsplit[1]=="glm()") | (model.strsplit[1]=="svyglm()") | (model.strsplit[1]=="gee()") | (model.strsplit[1]=="gam()")) {
              if ( .format.model.function == TRUE ) { renamed.global.models[1,i] <- paste(substr(model.strsplit[1],1,nchar(model.strsplit[1])-2),": ", .format.model.family, model.strsplit[2], sep="") }
              else { renamed.global.models[1,i] <- paste(.format.model.family, model.strsplit[2], sep="")}
              
              renamed.global.models[2,i] <- paste(.format.model.link, model.strsplit[3], sep="")
            }
            else if ((model.strsplit[1]=="survreg()") | (model.strsplit[1]=="polr()")) {
              if ( .format.model.function == TRUE ) { renamed.global.models[1,i] <- paste(substr(model.strsplit[1],1,nchar(model.strsplit[1])-2),": ", .format.model.dist, model.strsplit[2], sep="") }
              else { renamed.global.models[1,i] <- paste(.format.model.dist, model.strsplit[2], sep="")}
              renamed.global.models[2,i] <- ""
            }
          }
        }
        
        if (sum(renamed.global.models[2,]==rep("", times=length(.global.models)))==length(.global.models)) { how.many.model.rows <- 1}
        else { how.many.model.rows <- 2 }
        
        for (row in seq(from=1, to=how.many.model.rows)) {
          how.many.columns <- 0
          for (i in seq(1:length(.global.models))) {
            how.many.columns <- how.many.columns + 1
            
            # write down if next column has different dependent variable, or if end of columns
            different.model <- FALSE
            if (i == length(.global.models)) {different.model <- TRUE}
            else if ((as.character(.global.models[i])) != (as.character(.global.models[i+1]))) {different.model <- TRUE}
            else if ((as.character(.global.dependent.variables[i])) != (as.character(.global.dependent.variables[i+1]))) {different.model <- TRUE}   # subsume models under dependent variables
            
            if (.format.multicolumn==FALSE) { different.model <- TRUE }
            
            if (different.model == TRUE) {
              if (how.many.columns == 1) {
                if (.format.dec.mark.align == TRUE) {  
                  cat(" & \\multicolumn{1}{c}{",.format.models.left,as.character(renamed.global.models[row,i]),.format.models.right,"}", sep="")
                }
                else {
                  cat(" & ",.format.models.left,as.character(renamed.global.models[row,i]),.format.models.right, sep="")
                }
              }
              else {cat(" & \\multicolumn{",how.many.columns,"}{c}{",.format.models.left,as.character(renamed.global.models[row,i]),.format.models.right,"}", sep="")}
              
              how.many.columns <- 0
            }
          }
          cat(" \\\\ \n")	
        }
        
        
        # underline models
        if (.format.underline.models == TRUE) {
          how.many.columns <- 0
          for (i in seq(1:length(.global.models))) {
            how.many.columns <- how.many.columns + 1
            
            # underline if next column has different dependent variable, or if end of columns
            different.model <- FALSE
            if (i == length(.global.models)) {different.model <- TRUE}
            else if ((as.character(.global.models[i])) != (as.character(.global.models[i+1])))  {different.model <- TRUE}
            else if ((as.character(.global.dependent.variables[i])) != (as.character(.global.dependent.variables[i+1]))) {different.model <- TRUE}   # subsume models under dependent variables
            
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
      if (!is.null(.format.column.labels)) {
        
        if (is.null(.format.column.separate)) { .format.column.separate <- 1 }
        
        # adjust column.separate to have the same number of columns as the table
        models.in.table <- length(.global.models)
        models.in.col <- 0   
        for (i in seq(1:length(.format.column.separate))) {       # count up how many models in column.separate
          models.in.col <- models.in.col + .format.column.separate[i]
        }
        
        excess <- models.in.table - models.in.col
        
        # if too few column labels, add ones to column.separate
        if (excess > 0) {
          last.index <- length(.format.column.separate)
          for (i in seq(1:excess)) {
            .format.column.separate[last.index + i] <- 1
          }
        }
        
        # if too many column labels, then cut down
        if (excess < 0) {
          
          col.total <- 0
          new.format.column.separate <- NULL
          
          for(i in seq(1:length(.format.column.separate))) {
            col.total <- col.total + .format.column.separate[i]
            if (col.total > models.in.table) {
              new.format.column.separate[i] <- .format.column.separate[i] - (col.total - models.in.table)
              if (new.format.column.separate[i] == 0) { new.format.column.separate <- new.format.column.separate[-i] }
              break
            }
            else {
              new.format.column.separate[i] <- .format.column.separate[i]
            }
          }
          
          .format.column.separate <- new.format.column.separate
          
        }
        
        # output column labels
        col.position <- 1
        for (i in seq(1:length(.format.column.separate))) {
          if (is.null(.format.column.labels[col.position])) { .format.column.labels[col.position] <- "" }
          if (is.na(.format.column.labels[col.position])) { .format.column.labels[col.position] <- "" }
          if (.format.column.separate[i]==1) {
            if (.format.dec.mark.align==TRUE) {
              cat(" & \\multicolumn{1}{c}{",.format.column.left,.format.column.labels[col.position],.format.column.right,"}", sep="") 
            }
            else {
              cat(" & ",.format.column.left,.format.column.labels[col.position],.format.column.right, sep="") 
            }
          }
          else {
            cat(" & \\multicolumn{",.format.column.separate[i],"}{c}{",.format.column.left,.format.column.labels[col.position],.format.column.right,"}", sep="") 
          }
          col.position <- col.position + 1
        }
        cat(" \\\\ \n")  
      }
    }
    
    # numbers
    else if (part=="numbers") {
      if ((.format.model.numbers == TRUE) & (length(.global.models)>1)) {
        .table.insert.space()
        cat(.format.numbers.text)
        for (i in seq(1:length(.global.models))) {
          if (.format.dec.mark.align==TRUE) {
            if (.format.numbers.roman == TRUE) { cat(" & \\multicolumn{1}{c}{",.format.numbers.left,.roman.numeral(i),.format.numbers.right,"}", sep="") }
            else { cat(" & \\multicolumn{1}{c}{",.format.numbers.left,i,.format.numbers.right,"}", sep="") }
          }
          else {
            if (.format.numbers.roman == TRUE) { cat(" & ",.format.numbers.left,.roman.numeral(i),.format.numbers.right, sep="") }
            else { cat(" & ",.format.numbers.left,i,.format.numbers.right, sep="") }
          }
          
        }
        cat("\\\\ \n")
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # numbers
    else if (part=="objects") {
      if (.format.object.names == TRUE) {
        .table.insert.space()
        for (i in seq(1:length(.global.models))) {
          if (.format.dec.mark.align==TRUE) {
            cat(" & \\multicolumn{1}{c}{",.global.object.names[i],"}", sep="")
          }
          else {
            cat(" & ",.global.object.names[i], sep="")
          }
        }
        cat("\\\\ \n")
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    ## coefficients
    else if (part=="coefficients") { 		
      .which.variable.label <<- 0
      if (is.null(.format.covariate.labels)) { .format.covariate.labels <<- NA }
      
      # then, enter the coefficients
      
      for (i in seq(1:length(.global.coefficient.variables))) { .table.enter.coefficients(i) }
      
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # number of observations
    else if (part=="N") { .print.table.statistic(.global.var.name=.global.N, .format.var.name=.format.N, decimal.digits=0, part.number=which.part.number) }
    
    # fixed effects table
    else if (part=="omit") {
      if ((!is.null(.format.omit.regexp)) & (!is.null(.format.omit.labels))) {
        .format.omit.table <<- matrix(.format.omit.no, nrow=length(.format.omit.regexp), ncol=length(.global.models)) 
        for (i in seq(1:length(.global.models))) {
          for (j in seq(1:length(.format.omit.regexp))) {
            for (k in seq(1:length(.global.coef.vars.by.model[,i]))) {
              relevant.coef.var <- .global.coef.vars.by.model[k,i]
              if (length(grep(.format.omit.regexp[j], relevant.coef.var, perl=.format.perl, fixed=FALSE))!=0) {
                .format.omit.table[j,i] <<- .format.omit.yes
              }
            }
          }
        }
        for (i in seq(1:length(.format.omit.regexp))) {
          cat (.format.omit.labels[i])
          for (j in seq(1:length(.global.models))) {
            if (.format.dec.mark.align == TRUE) {
              cat(" & \\multicolumn{1}{c}{",.format.omit.table[i,j],"}", sep="")
            }
            else {
              cat(" & ",.format.omit.table[i,j], sep="")
            }
          }
          cat(" \\\\ \n")
        }
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # R-squared
    else if (part=="R-squared") {	.print.table.statistic(.global.var.name=.global.R2, .format.var.name=.format.R2, part.number=which.part.number) }
    
    # max R-squared
    else if (part=="max R-squared") {	.print.table.statistic(.global.var.name=.global.max.R2, .format.var.name=.format.max.R2, part.number=which.part.number) }
    
    # adjusted R-squared
    else if (part=="adjusted R-squared") { .print.table.statistic(.global.var.name=.global.adj.R2, .format.var.name=.format.adj.R2, part.number=which.part.number) }
    
    # log likelihood
    else if (part=="log likelihood") { .print.table.statistic(.global.var.name=.global.LL, .format.var.name=.format.LL, part.number=which.part.number) }
    
    # Akaike Information Criterion (AIC)
    else if (part=="AIC") { .print.table.statistic(.global.var.name=.global.AIC, .format.var.name=.format.AIC, part.number=which.part.number) }
    
    # Bayesian Information Criterion (BIC)
    else if (part=="BIC") { .print.table.statistic(.global.var.name=.global.BIC, .format.var.name=.format.BIC, part.number=which.part.number) }
    
    # Scale Parameter
    else if (part=="scale") { .print.table.statistic(.global.var.name=.global.scale, .format.var.name=.format.scale, part.number=which.part.number) }
    
    # UBRE
    else if (part=="UBRE") { .print.table.statistic(.global.var.name=.global.UBRE, .format.var.name=.format.UBRE, part.number=which.part.number) }
    
    # sigma2
    else if (part=="sigma2") { .print.table.statistic(.global.var.name=.global.sigma2, .format.var.name=.format.sigma2, part.number=which.part.number) }
    
    ## with degrees of freedom
    
    # residual standard error (sigma); standard error of the regression
    else if (substr(part,1,nchar("SER"))=="SER") { .print.table.statistic(.global.var.name=.global.SER, .format.var.name=.format.SER, part.string=part, part.number=which.part.number) }
    
    # F-statistic
    else if (substr(part,1,nchar("F statistic"))=="F statistic") { .print.table.statistic(.global.var.name=.global.F.stat, .format.var.name=.format.F.stat, part.string=part, part.number=which.part.number) }
    
    # theta
    else if (substr(part,1,nchar("theta"))=="theta") { .print.table.statistic(.global.var.name=.global.theta, .format.var.name=.format.theta, part.string=part, part.number=which.part.number, type.se=TRUE) }
    
    # rho
    else if (substr(part,1,nchar("rho"))=="rho") { .print.table.statistic(.global.var.name=.global.rho, .format.var.name=.format.rho, part.string=part, part.number=which.part.number, type.se=TRUE) }
    
    # Inverse Mills ratio
    else if (substr(part,1,nchar("Mills"))=="Mills") { .print.table.statistic(.global.var.name=.global.mills, .format.var.name=.format.mills, part.string=part, part.number=which.part.number, type.se=TRUE) }
    
    
    # Chi-squared
    else if (substr(part,1,nchar("chi2"))=="chi2") { .print.table.statistic(.global.var.name=.global.chi.stat, .format.var.name=.format.chi.stat, part.string=part, part.number=which.part.number) }
    
    # Wald Test
    else if (substr(part,1,nchar("Wald"))=="Wald") { .print.table.statistic(.global.var.name=.global.wald.stat, .format.var.name=.format.wald.stat, part.string=part, part.number=which.part.number) }
    
    # LR Test
    else if (substr(part,1,nchar("LR"))=="LR") { .print.table.statistic(.global.var.name=.global.lr.stat, .format.var.name=.format.lr.stat, part.string=part, part.number=which.part.number) }
    
    # Score (Logrank) Test
    else if (substr(part,1,nchar("logrank"))=="logrank") { .print.table.statistic(.global.var.name=.global.logrank.stat, .format.var.name=.format.logrank.stat, part.string=part, part.number=which.part.number) }
    
    # null deviance
    else if (substr(part,1,nchar("null deviance"))=="null deviance") { .print.table.statistic(.global.var.name=.global.null.deviance, .format.var.name=.format.null.deviance, part.string=part, part.number=which.part.number) }
    
    # residual deviance
    else if (substr(part,1,nchar("residual deviance"))=="residual deviance") { .print.table.statistic(.global.var.name=.global.residual.deviance, .format.var.name=.format.residual.deviance, part.string=part, part.number=which.part.number) }
    
    ##
    
    # single horizontal line, no matter what
    else if (part=="-!") {
      cat("\\hline ")
      .table.insert.space()
      cat(" \n") 
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # single horizontal line, optional
    else if (part=="-") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline ")
        .table.insert.space()
        cat(" \n") 
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # double horizontal line, no matter what
    else if (part=="=!") {
      cat("\\hline \n") 
      cat("\\hline ")
      .table.insert.space()
      cat(" \n")
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # double horizontal line
    else if (part=="=") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline \n") 
        cat("\\hline ")
        .table.insert.space()
        cat(" \n") 
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # notes
    else if (part=="notes") {
      if (.format.note != "") { cat(.format.note) }
      for (i in seq(1:length(.format.note.content))) {
        
        .format.note.content[i] <- .format.note.content[i]
        
        # print individual notes
        if (.format.note == "") { cat("\\multicolumn{",length(.global.models)+1,"}{",.format.note.alignment,"}{",.format.note.content[i],"} \\\\ \n", sep="") }
        else { cat(" & \\multicolumn{",length(.global.models),"}{",.format.note.alignment,"}{",.format.note.content[i],"} \\\\ \n", sep="") }
      }
      .table.part.published[which.part.number] <<- TRUE
    }	
    
    # empty line
    else if (part==" ") {
      .table.empty.line();
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # additional lines
    else if (part=="additional") { .print.additional.lines(part.number=which.part.number) }
  }


.stargazer.reg.table <-
  function(...) {
    
    list.of.models <- as.list(list(...))
    how.many.models <- length(list.of.models)
    
    # find how many models user wants to customize
    # max.user <- max(length(coef),length(se),length(t),length(p),length(ci.custom))
    length(coef) <<- length(se) <<- length(t) <<- length(p) <<- length(ci.custom) <<- how.many.models
    
    if (how.many.models >= 1) {
      suppressMessages(.new.table(list.of.models[[1]], user.coef=coef[[1]], user.se=se[[1]], user.t=t[[1]], user.p=p[[1]], auto.t=t.auto, auto.p=p.auto, user.ci.lb=ci.custom[[1]][,1], user.ci.rb=ci.custom[[1]][,2]))
      if (how.many.models >= 2) {
        for (i in seq(from = 2,to = how.many.models)) { 
          #if (i <= max.user) {
          suppressMessages(.add.model(list.of.models[[i]], user.coef=coef[[i]], user.se=se[[i]], user.t=t[[i]], user.p=p[[i]], auto.t=t.auto, auto.p=p.auto, user.ci.lb=ci.custom[[i]][,1], user.ci.rb=ci.custom[[i]][,2])) 
          #}
          #else {
          #  suppressMessages(.add.model(list.of.models[[i]], user.coef=NULL, user.se=NULL, user.t=NULL, user.p=NULL, auto.t=t.auto, auto.p=p.auto, user.ci.lb=NULL, user.ci.rb=NULL))
          #}
        }
      }
      .apply(auto.t=t.auto, auto.p=p.auto)
      .order.reg.table(order)
      suppressMessages(.publish.table())
    }
  } 


.data.frame.table.header <-
  function(object) {
    .floating.header()
    
    .formatting.alignment <- paste("@{\\extracolsep{",.format.column.sep.width,"}} ", sep="")
    for (i in seq(1:(length(names(object))))) {
      if (.format.dec.mark.align == FALSE) {
        .formatting.alignment <- paste(.formatting.alignment, "c", sep="")
      }
      else {
        .formatting.alignment <- paste(.formatting.alignment, "D{", .format.decimal.character,"}{", .format.decimal.character,"}{-", .format.s.round.digits,"} ", sep="")
      }
    }
    #
    
    cat("\\begin{tabular}{",.formatting.alignment,"} \n",sep="")
  }

.stargazer.data.frame.table <-
  function(object) {  
    
    # flip objects
    if (.format.flip == TRUE) { 
      
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
      
      .table.part.published <<- as.vector(rep(NA, times=length(.format.s.stat.parts)))    # to keep track what has been published (to deal intelligently with horizontal lines)
      .publish.horizontal.line <<- TRUE   # should non-compulsory horizontal lines be published? (yes, if something else published since the previous line)
      
      if (length(.format.s.stat.parts)>=1) {
        for (i in seq(1:length(.format.s.stat.parts))) {
          .data.frame.table.part(object,.format.s.stat.parts[i], which.part.number = i)
          
          if (.table.part.published[i]==TRUE) { .publish.horizontal.line <<- TRUE }
          if ((.format.s.stat.parts[i]=="-") | (.format.s.stat.parts[i]=="-!") | (.format.s.stat.parts[i]=="=") | (.format.s.stat.parts[i]=="=!")) { .publish.horizontal.line <<- FALSE }
        }
      }
      
      cat("\\end{tabular} \n")
      if (.format.floating == TRUE) { cat("\\end{", .format.floating.environment,"} \n", sep="") }
      else if (!is.null(.format.font.size)) {
        cat("\\endgroup \n",sep="")
      }
    }
  }

.data.frame.table.part <-
  function(object, part, which.part.number) {
    
    .table.part.published[which.part.number] <<- FALSE
    
    if ((part=="stat names") & (.format.colnames==TRUE)) {
      
      x.which <- 0
      
      if (is.null(.format.covariate.labels)) { .format.covariate.labels <<- NA }
      
      for (x in seq(1:length(names(object)))) {
        
        omitted <- FALSE
        
        if (!is.null(.format.omit.regexp)) {
          for (j in seq(1:length(.format.omit.regexp))) {
            if (length(grep(.format.omit.regexp[j], names(object)[x], perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE  }
          }
        }
        
        if (!is.null(.format.keep.regexp)) {
          omitted <- TRUE
          for (j in seq(1:length(.format.keep.regexp))) {
            if (length(grep(.format.keep.regexp[j], names(object)[x], perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE  }
          }
        }
        
        if (!is.null(.format.omit.index)) {
          for (j in seq(1:length(.format.omit.index))) {
            if (.format.omit.index[j] == x) { omitted <- TRUE }
          }
        }
        
        if (!is.null(.format.keep.index)) {
          omitted <- TRUE
          for (j in seq(1:length(.format.keep.index))) {
            if (.format.keep.index[j] == x) { omitted <- FALSE }
          }
        }
        
        if (omitted == FALSE) {
          
          x.which <- x.which + 1
          
          if (x >= 2) { cat(" & ", sep="")}
          
          # if underscore or ^ in variable name, then insert an escape \ before it
          name.printed <- .remove.special.chars(names(object)[x])
          
          if (is.na(.format.covariate.labels[x.which])) {
            if (.format.coefficient.variables.capitalize == TRUE) { name.printed <- toupper(name.printed) }
          }
          else { name.printed <- .format.covariate.labels[x.which] }
          
          
          if (.format.dec.mark.align==TRUE) {
            cat("\\multicolumn{1}{c}{",.format.s.coefficient.variables.left, name.printed,.format.s.coefficient.variables.right,"}", sep="")  
          }
          else {
            cat(.format.s.coefficient.variables.left, name.printed,.format.s.coefficient.variables.right, sep="")  
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
          
          if (!is.null(.format.omit.regexp)) {
            for (j in seq(1:length(.format.omit.regexp))) {
              if (length(grep(.format.omit.regexp[j], names(object)[x], perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE  }
            }
          }
          
          if (!is.null(.format.keep.regexp)) {
            omitted <- TRUE
            for (j in seq(1:length(.format.keep.regexp))) {
              if (length(grep(.format.keep.regexp[j], names(object)[x], perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE  }
            }
          }
          
          if (!is.null(.format.omit.index)) {
            for (j in seq(1:length(.format.omit.index))) {
              if (.format.omit.index[j] == x) { omitted <- TRUE }
            }
          }
          
          if (!is.null(.format.keep.index)) {
            omitted <- TRUE
            for (j in seq(1:length(.format.keep.index))) {
              if (.format.keep.index[j] == x) { omitted <- FALSE }
            }
          }
          
          
          if (omitted == FALSE) {     
            if (x >= 2) { cat(" & ", sep="") }
            
            .how.much.to.round <- .format.round.digits
            if (is.numeric(object[y,x])) {
              
              if (.is.all.integers(object[y,x])) { .how.much.to.round <- 0 }
              
              rounded.object <- .iround(object[y,x], .how.much.to.round)
              
              if (.format.dec.mark.align==TRUE) {
                cat(rounded.object, sep="")  
              }
              else {
                cat("$", rounded.object, "$",sep="")  
              }
            }
            else {
              adjusted.object <- .remove.special.chars(object[y, x])
              if (is.na(adjusted.object)) { adjusted.object <- "" }
              
              if (.format.dec.mark.align==TRUE) {
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
    else if ((part=="notes") & (!is.null(.format.s.note.content))) {
      if (.format.s.note != "") cat(.format.s.note)
      for (i in seq(1:length(.format.s.note.content))) {
        .format.s.note.content[i] <- .format.s.note.content[i]
        if (.format.s.note == "") { cat("\\multicolumn{",length(names(object)),"}{",.format.s.note.alignment,"}{",.format.s.note.content[i],"} \\\\ \n", sep="") }
        else { cat(" & \\multicolumn{",length(names(object)),"}{",.format.s.note.alignment,"}{",.format.s.note.content[i],"} \\\\ \n", sep="") }
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
  function(object) {
    
    if (length(names(object)) < 1) {
      cat("% Error: Data frame columns do not have any names.\n")
    }
    else if ((nrow(object) < 1) | (ncol(object) < 1)) {
      cat("% Error: Data frame must have at least one row and one column.\n")
    }
    else {
      
      object <- .order.data.frame(object, order, summary=T)
      
      .table.info.comment()
      
      # create table header
      .summ.stat.table.header(object)
      .table.insert.space()
      
      for (i in seq(1:length(.format.s.stat.parts))) {
        .summ.stat.table.part(object,.format.s.stat.parts[i])
      }
      
      cat("\\end{tabular} \n")
      if (.format.floating == TRUE) { cat("\\end{", .format.floating.environment,"} \n", sep="") }
      else if (!is.null(.format.font.size)) {
        cat("\\endgroup \n",sep="")
      }
    }
  }

.summ.stat.publish.statistic <-
  function(object, which.variable, which.statistic) {
    
    if ((is.numeric(object[,which.variable]) == TRUE) | ((is.logical(object[,which.variable])) & (.format.summ.logical==TRUE)))  {
      
      if ((is.logical(object[,which.variable])) & (.format.summ.logical==TRUE)) {
        temp.var <- rep(NA, time=length(object[,which.variable]))
        temp.var[object[,which.variable]==TRUE] <- 1
        temp.var[object[,which.variable]==FALSE] <- 0
      }
      else {
        temp.var <- object[,which.variable]
      }
      
      which.statistic <- tolower(which.statistic)
      if (which.statistic == "n") {
        return(.iround(sum(!is.na(temp.var)), 0))
      }
      else if (which.statistic == "nmiss") {
        return(.iround(sum(is.na(temp.var)), 0))
      }
      else if (which.statistic == "mean") {
        return(.iround(mean(temp.var, na.rm=TRUE), .format.s.round.digits))
      }
      else if (which.statistic == "median") {
        median.value <- median(temp.var, na.rm=TRUE)
        
        if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- .format.s.round.digits }
        else { 
          if (.is.all.integers(median.value) == TRUE) { how.much.to.round <- 0 }
          else { how.much.to.round <- 1 }
        }
        
        return(.iround(median.value, how.much.to.round))
      }
      else if (which.statistic == "sd") {
        return(.iround(sd(temp.var, na.rm=TRUE), .format.s.round.digits))
      }
      else if (which.statistic == "min") {
        if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- .format.s.round.digits }
        else { how.much.to.round <- 0 }
        
        return(.iround(min(temp.var, na.rm=TRUE), how.much.to.round))
      }
      else if (which.statistic == "max") {
        if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- .format.s.round.digits }
        else { how.much.to.round <- 0 }
        
        return(.iround(max(temp.var, na.rm=TRUE), how.much.to.round))
      }
      else if (which.statistic == "mad") {
        return(.iround(mad(temp.var, na.rm=TRUE), .format.s.round.digits))
      }
      else if (substr(which.statistic,1,1) == "p") {
        
        percentile.value <- quantile(temp.var, as.numeric(substr(which.statistic,2,nchar(which.statistic))) / 100, na.rm=TRUE)
        
        if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- .format.s.round.digits }
        else { 
          if (.is.all.integers(percentile.value) == TRUE) { how.much.to.round <- 0 }
          else { how.much.to.round <- 1 }
        }
        
        return(.iround(percentile.value, how.much.to.round))
      }
    }
    else { return(NA) }
  }

.summ.stat.table.header <-
  function(object) {
    .floating.header()
    
    #
    .formatting.alignment <- paste("@{\\extracolsep{",.format.column.sep.width,"}}l", sep="")
    
    if (.format.flip == FALSE) { width <- length(.format.s.statistics.list) }
    else { width <- length(.summ.stat.included(object)) }
    
    for (i in seq(1:width)) {
      if (.format.dec.mark.align == FALSE) {
        .formatting.alignment <- paste(.formatting.alignment, "c", sep="")
      }
      else {
        .formatting.alignment <- paste(.formatting.alignment, "D{", .format.decimal.character,"}{", .format.decimal.character,"}{-", .format.s.round.digits,"} ", sep="")
      }
    }
    #
    
    cat("\\begin{tabular}{",.formatting.alignment,"} \n",sep="")
  }

# figure out which variables are included --> returns indices of included variables
.summ.stat.included <- 
  function(object) {
    
    included <- NULL
    
    for (i in seq(1:length(names(object)))) {
      
      # skip all of this if omitted based on regular expression
      omitted <- FALSE
      
      if ((is.numeric(object[,i]) == TRUE) | (is.logical(object[,i]) & (.format.summ.logical==TRUE))) {
        
        # also omit if all missing values
        if (!any(!is.na(object[,i]))) { omitted <- TRUE }
        
        if (!is.null(.format.omit.regexp)) {
          for (j in seq(1:length(.format.omit.regexp))) {
            if (length(grep(.format.omit.regexp[j], names(object)[i], perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE  }
          }
        }
        
        if (!is.null(.format.keep.regexp)) {
          omitted <- TRUE
          for (j in seq(1:length(.format.keep.regexp))) {
            if (length(grep(.format.keep.regexp[j], names(object)[i], perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE  }
          }
        }
        
        if (!is.null(.format.omit.index)) {
          for (j in seq(1:length(.format.omit.index))) {
            if (.format.omit.index[j] == i) { omitted <- TRUE }
          }
        }
        
        if (!is.null(.format.keep.index)) {
          omitted <- TRUE
          for (j in seq(1:length(.format.keep.index))) {
            if (.format.keep.index[j] == i) { omitted <- FALSE }
          }
        }
      }
      else { omitted <- TRUE }
      
      if (omitted == FALSE) { included <- c(included, i) }
    }
    
    return(included)
    
  }

.summ.stat.table.part <-
  function(object, part) {
    
    included <- .summ.stat.included(object)
    
    # with summary statistics, always publish horizontal line
    .publish.horizontal.line <<- TRUE
    
    if (part=="stat names") {
      cat(.format.s.statistics.names.label, sep="")
      
      if (.format.flip == FALSE) {
        if (length(.format.s.statistics.list)>=1) {
          for (i in seq(1:length(.format.s.statistics.list))) {
            for (j in seq(1:ncol(.format.s.statistics.names))) {
              if ((substr(.format.s.statistics.list[i],1,1)=="p") & (substr(.format.s.statistics.list[i],1,1)==.format.s.statistics.names[1,j])) {
                cat(" & \\multicolumn{1}{c}{", .format.s.statistics.names.left, sub("!", substr(.format.s.statistics.list[i],2,nchar(.format.s.statistics.list[i])), .format.s.statistics.names[2,j], ignore.case =FALSE, fixed=TRUE), .format.s.statistics.names.right,"}", sep="")
              }
              else if (.format.s.statistics.list[i]==.format.s.statistics.names[1,j]) {
                cat(" & \\multicolumn{1}{c}{", .format.s.statistics.names.left, .format.s.statistics.names[2,j], .format.s.statistics.names.right, "}", sep="")
              }
            }
          }
        }
      }
      else {   # flipped summary statistic table
        
        if (is.null(.format.covariate.labels)) { .format.covariate.labels <<- NA }
        
        i.label <- 0
        
        for (i in included) {
          
          i.label <- i.label + 1
          
          # if underscore in variable name, then insert an escape \ before it
          name.printed <- .remove.special.chars(names(object)[i])
          
          cat(" & ")
          if (is.na(.format.covariate.labels[i.label])) { 
            if ( .format.s.coefficient.variables.capitalize == TRUE) { cat(.format.s.coefficient.variables.left, toupper(name.printed), .format.s.coefficient.variables.right, sep="") }
            else { cat(.format.s.coefficient.variables.left, name.printed, .format.s.coefficient.variables.right, sep="") }
          }
          else { cat(.format.s.coefficient.variables.left, .format.covariate.labels[i.label], .format.s.coefficient.variables.right, sep="") }        
        }
        
      }
      
      cat(" \\\\ \n")
    }
    
    if (substr(part,1,10)=="statistics") {
      if (is.null(.format.covariate.labels)) { .format.covariate.labels <<- NA }
      
      
      if (.format.flip == FALSE) {
        
        i.label <- 0
        for (i in included) {
          i.label <- i.label + 1
          
          # if underscore in variable name, then insert an escape \ before it
          name.printed <- .remove.special.chars(names(object)[i])
          
          if (is.na(.format.covariate.labels[i.label])) { 
            if ( .format.s.coefficient.variables.capitalize == TRUE) { cat(.format.s.coefficient.variables.left, toupper(name.printed), .format.s.coefficient.variables.right, sep="") }
            else { cat(.format.s.coefficient.variables.left, name.printed, .format.s.coefficient.variables.right, sep="") }
          }
          else { cat(.format.s.coefficient.variables.left, .format.covariate.labels[i.label], .format.s.coefficient.variables.right, sep="") }
          
          if (length(.format.s.statistics.list)>=1) {
            for (j in seq(1:length(.format.s.statistics.list))) {
              
              # if aligning decimal marks, need to use multicolumn for anything w/o decimal mark
              if (.format.dec.mark.align == FALSE) {   # not aligning
                cat(" & ", .summ.stat.publish.statistic(object, i, .format.s.statistics.list[j]), sep="")  
              }
              else {     # aligning
                if (.is.all.integers(.summ.stat.publish.statistic(object, i, .format.s.statistics.list[j]))) {
                  cat(" & \\multicolumn{1}{c}{", .summ.stat.publish.statistic(object, i, .format.s.statistics.list[j]),"}", sep="")
                }
                else {
                  cat(" & ", .summ.stat.publish.statistic(object, i, .format.s.statistics.list[j]), sep="")
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
        if (length(.format.s.statistics.list)>=1) {
          for (i in seq(1:length(.format.s.statistics.list))) {
            for (j in seq(1:ncol(.format.s.statistics.names))) {
              if ((substr(.format.s.statistics.list[i],1,1)=="p") & (substr(.format.s.statistics.list[i],1,1)==.format.s.statistics.names[1,j])) {
                cat(.format.s.statistics.names.left, sub("!", substr(.format.s.statistics.list[i],2,nchar(.format.s.statistics.list[i])), .format.s.statistics.names[2,j], ignore.case =FALSE, fixed=TRUE), .format.s.statistics.names.right, sep="")
              }
              else if (.format.s.statistics.list[i]==.format.s.statistics.names[1,j]) {
                cat(.format.s.statistics.names.left, .format.s.statistics.names[2,j], .format.s.statistics.names.right, sep="")
              }
            }
            for (j in included) {
              # if aligning decimal marks, need to use multicolumn for anything w/o decimal mark
              if (.format.dec.mark.align == FALSE) {   # not aligning
                cat(" & ", .summ.stat.publish.statistic(object, j, .format.s.statistics.list[i]), sep="")  
              }
              else {     # aligning
                if (.is.all.integers(.summ.stat.publish.statistic(object, j, .format.s.statistics.list[i]))) {
                  cat(" & \\multicolumn{1}{c}{", .summ.stat.publish.statistic(object, j, .format.s.statistics.list[i]),"}", sep="")
                }
                else {
                  cat(" & ", .summ.stat.publish.statistic(object, j, .format.s.statistics.list[i]), sep="")
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
    else if ((part=="notes") & (!is.null(.format.s.note.content))) {
      if (.format.s.note != "") cat(.format.s.note)
      
      if (.format.s.note=="") { offset <- 1 }
      else { offset <- 0 }
      
      if (.format.flip == FALSE) { width <- length(.format.s.statistics.list)+ offset }
      else { width <- length(included) + offset }
      
      for (i in seq(1:length(.format.s.note.content))) {
        .format.s.note.content[i] <- .format.s.note.content[i]
        if (.format.s.note == "") { cat("\\multicolumn{",width,"}{",.format.s.note.alignment,"}{",.format.s.note.content[i],"} \\\\ \n", sep="") }
        else { cat(" & \\multicolumn{",width,"}{",.format.s.note.alignment,"}{",.format.s.note.content[i],"} \\\\ \n", sep="") }
      }
    }	
    
    
    # empty line
    else if (part==" ") {
      .table.empty.line()
    }
    
    # horizontal line
    else if (part=="-!") {
      cat("\\hline ")
      .table.insert.space()
      cat(" \n")
    }
    
    else if (part=="-") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline ")
        .table.insert.space()
        cat(" \n")
      }
    }
    
    # double horizontal line
    else if (part=="=!") {
      cat("\\hline \n") 
      cat("\\hline ")
      .table.insert.space()
      cat(" \n")
    }
    
    else if (part=="=") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline \n") 
        cat("\\hline ")
        .table.insert.space()
        cat(" \n")
      }
    }
  }
