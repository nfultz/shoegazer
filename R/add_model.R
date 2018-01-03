.add.model <-
  function(object.name, user.coef=NULL, user.se=NULL, user.t=NULL, user.p=NULL, auto.t=TRUE, auto.p=TRUE, user.ci.lb=NULL, user.ci.rb=NULL) {
    
    if (class(object.name)[1] == "Glm") {
      .summary.object <<- summary.glm(object.name)
    }
    else if (!(.model.identify(object.name) %in% c("aftreg", "coxreg","phreg","weibreg", "Glm", "bj", "cph", "lrm", "ols", "psm", "Rq"))) {
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
    
    for (model.num in 1:model.num.total) {
      
      .global.models <<- append(.global.models, .model.identify(object.name))
      
      .global.dependent.variables <<- append(.global.dependent.variables, .dependent.variable(object.name, model.num))
      .global.dependent.variables.written <<- append(.global.dependent.variables.written, .dependent.variable.written(object.name, model.num))
      
      .global.N <<- append(.global.N, .number.observations(object.name))
      .global.LL <<- append(.global.LL, .log.likelihood(object.name))
      .global.R2 <<- append(.global.R2, .r.squared(object.name))
      .global.max.R2 <<- append(.global.max.R2, .max.r.squared(object.name))
      .global.adj.R2 <<- append(.global.adj.R2, .adj.r.squared(object.name))
      .global.AIC <<- append(.global.AIC, .AIC(object.name))
      .global.BIC <<- append(.global.BIC, .BIC(object.name))
      .global.scale <<- append(.global.scale, .get.scale(object.name))
      .global.UBRE <<- append(.global.UBRE, .gcv.UBRE(object.name))
      .global.sigma2 <<- append(.global.sigma2, .get.sigma2(object.name))
      
      
      .global.rho <<- cbind(.global.rho, .get.rho(object.name))
      .global.mills <<- cbind(.global.mills, .get.mills(object.name))
      .global.theta <<- cbind(.global.theta, .get.theta(object.name))
      .global.SER <<- cbind(.global.SER, .SER(object.name))
      .global.F.stat <<- cbind(.global.F.stat, .F.stat(object.name))
      .global.chi.stat <<- cbind(.global.chi.stat, .chi.stat(object.name))
      .global.wald.stat <<- cbind(.global.wald.stat, .wald.stat(object.name))
      .global.lr.stat <<- cbind(.global.lr.stat, .lr.stat(object.name))
      .global.logrank.stat <<- cbind(.global.logrank.stat, .logrank.stat(object.name))
      .global.null.deviance <<- cbind(.global.null.deviance, .null.deviance(object.name))
      .global.residual.deviance <<- cbind(.global.residual.deviance, .residual.deviance(object.name))
      
      max.length <- length(.global.coefficient.variables)+length(.coefficient.variables(object.name))
      
      # add RHS variables and coefficients
      coef.var <- .coefficient.variables(object.name)
      .global.coef.vars.by.model <<-  cbind(.global.coef.vars.by.model, coef.var)
      
      temp.gcv <- rep(NA,each=1,times=max.length)
      
      temp.gcv[1:length(.global.coefficient.variables)] <- .global.coefficient.variables
      
      how.many.gcv <- length(.global.coefficient.variables)
      
      # try to find variable
      position <- 0
      for (i in seq(1:length(coef.var))) {
        
        found <- FALSE
        
        for (j in seq(1:length(.global.coefficient.variables))) {
          if (coef.var[i] == .global.coefficient.variables[j]) {
            found <- TRUE
            for (k in 1:how.many.gcv) {
              if (coef.var[i]==temp.gcv[k]) {
                position <- k
              }
            }
          }
        }
        
        
        # If variable was found, no need to add it
        if (found == FALSE) {
          
          # append new variable to list of regressors
          while ((position < how.many.gcv) & (!(temp.gcv[position+1] %in% coef.var))) {
            position <- position + 1
          }
          
          temp.gcv <- append(temp.gcv, coef.var[i], after=position)
          how.many.gcv <- how.many.gcv + 1
          position <- position + 1
        }
        
      }
      
      .global.coefficient.variables <<- temp.gcv[1:how.many.gcv]
      
      # build up coefficients from scratch
      temp.coefficients <- temp.std.errors <- temp.ci.lb <- temp.ci.rb <- temp.t.stats <- temp.p.values <- matrix(data = NA, nrow = length(.global.coefficient.variables), ncol = ncol(.global.coefficients)+1)
      rownames(temp.coefficients) <- rownames(temp.std.errors) <- rownames(temp.ci.lb) <- rownames(temp.ci.rb) <- rownames(temp.t.stats) <- rownames(temp.p.values) <- .global.coefficient.variables
      
      # fill in from previous iteration of .global coefficients
      which.variable <- 0
      for (row in .global.coefficient.variables) {
        
        which.variable <- which.variable + 1
        
        row.i <- .rename.intercept(row)   # row with intercept renamed to get the omit and keep right
        
        ### if omitted variable, then advance to the next iteration of the loop --- !!! do this also for index
        #skip all of this if omitted based on regular expression
        omitted <- FALSE
        
        if (!is.null(.format.omit.regexp)) {
          for (i in seq(1:length(.format.omit.regexp))) {
            if (length(grep(.format.omit.regexp[i], row.i, perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE	}
          }
        }
        
        if (!is.null(.format.keep.regexp)) {
          omitted <- TRUE
          for (i in seq(1:length(.format.keep.regexp))) {
            if (length(grep(.format.keep.regexp[i], row.i, perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE	}
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
        
        if (omitted == TRUE) { next }
        
        
        ###
        
        for (col in seq(1:ncol(.global.coefficients))) {
          if (sum(as.vector(rownames(.global.coefficients[,col, drop=FALSE])==row))!=0) { 
            if (!is.null(.global.coefficients)) { temp.coefficients[row, col] <- .global.coefficients[row, col] }
            if (!is.null(.global.std.errors)) { temp.std.errors[row, col] <- .global.std.errors[row, col] }
            if (!is.null(.global.ci.lb)) { temp.ci.lb[row, col] <- .global.ci.lb[row, col] }
            if (!is.null(.global.ci.rb)) { temp.ci.rb[row, col] <- .global.ci.rb[row, col] }
            if (!is.null(.global.t.stats)) { temp.t.stats[row, col] <- .global.t.stats[row, col] }
            if (!is.null(.global.p.values)) { temp.p.values[row, col] <- .global.p.values[row, col] }
          }
        }
        
        feed.coef <- NA; feed.se <- NA
        # coefficients and standard errors
        if (!is.null(.get.coefficients(object.name, user.coef, model.num=model.num)[row])) { 
          temp.coefficients[row, ncol(temp.coefficients)] <- .get.coefficients(object.name, user.coef, model.num=model.num)[row] 
          feed.coef <- temp.coefficients[, ncol(temp.coefficients)]
        }
        if (!is.null(.get.standard.errors(object.name, user.se, model.num=model.num)[row])) { 
          temp.std.errors[row, ncol(temp.std.errors)] <- .get.standard.errors(object.name, user.se, model.num=model.num)[row] 
          feed.se <- temp.std.errors[, ncol(temp.std.errors)]
        }
        
        # confidence interval, left and right bound
        if (!is.null(.get.ci.lb(object.name, user.ci.lb, model.num=model.num)[row])) { temp.ci.lb[row, ncol(temp.ci.lb)] <- .get.ci.lb(object.name, user.ci.lb, model.num=model.num)[row] }
        if (!is.null(.get.ci.rb(object.name, user.ci.rb, model.num=model.num)[row])) { temp.ci.rb[row, ncol(temp.ci.rb)] <- .get.ci.rb(object.name, user.ci.rb, model.num=model.num)[row] }
        
        # t-stats and p-values
        #if (!is.null(user.coef)) { feed.coef <- user.coef }   # feed user-defined coefficients, if available - check that this does not mess up multinom
        #if (!is.null(user.se)) { feed.se <- user.se }   # feed user-defined std errors, if available
        if (!is.null(.get.t.stats(object.name, user.t, auto.t, feed.coef, feed.se, user.coef, user.se, model.num=model.num)[row])) { temp.t.stats[row, ncol(temp.std.errors)] <- .get.t.stats(object.name, user.t, auto.t, feed.coef, feed.se, user.coef, user.se, model.num=model.num)[row] }
        if (!is.null(.get.p.values(object.name, user.p, auto.p, feed.coef, feed.se, user.coef, user.se, model.num=model.num)[row])) { temp.p.values[row, ncol(temp.std.errors)] <- .get.p.values(object.name, user.p, auto.p, feed.coef, feed.se, user.coef, user.se, model.num=model.num)[row] }
      }
      
      if (!is.null(temp.coefficients)) { .global.coefficients <<- temp.coefficients }
      if (!is.null(temp.std.errors)) { .global.std.errors <<- temp.std.errors }
      if (!is.null(temp.ci.lb)) { .global.ci.lb <<- temp.ci.lb }
      if (!is.null(temp.ci.rb)) { .global.ci.rb <<- temp.ci.rb }
      if (!is.null(temp.t.stats)) { .global.t.stats <<- temp.t.stats }
      if (!is.null(temp.p.values)) { .global.p.values <<- temp.p.values }
      
    }
    
  } 
