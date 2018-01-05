.get.summary.object <- function(object.name) {
  if (class(object.name)[1] == "Glm") {
    .summary.object<- summary.glm(object.name)
  }
  else if (!(.model.identify(object.name) %in% c("aftreg", "coxreg","phreg","weibreg", "Glm", "bj", "cph", "lrm", "ols", "psm", "Rq"))) {
    .summary.object<- summary(object.name)
  }
  else {
    .summary.object<- object.name
  }
  
  if (.model.identify(object.name) == "rq") {
    .summary.object<- suppressMessages(summary(object.name, se=fmt$rq.se))
  }
  
  .summary.object
}

.add.model <-
  function(object.name, user.coef=NULL, user.se=NULL, user.t=NULL, user.p=NULL, auto.t=TRUE, auto.p=TRUE, user.ci.lb=NULL, user.ci.rb=NULL, fmt, gbl) {
    
    .summary.object <- .get.summary.object(object.name)
    
    model.num.total <- 1   # model number for multinom, etc.
    if (.model.identify(object.name) == "multinom") {
      if (!is.null(nrow(.summary.object$coefficients))) {
        model.num.total <-  nrow(.summary.object$coefficients)
      }
    }
    
    for (model.num in 1:model.num.total) {
      
      gbl$models<- append(gbl$models, .model.identify(object.name))
      
      gbl$dependent.variables <- append(gbl$dependent.variables, .dependent.variable(object.name, model.num))
      gbl$dependent.variables.written <- append(gbl$dependent.variables.written, .dependent.variable.written(object.name, model.num))
      
      gbl$N <- append(gbl$N, .number.observations(object.name))
      gbl$LL <- append(gbl$LL, .log.likelihood(object.name))
      gbl$R2 <- append(gbl$R2, .r.squared(object.name))
      gbl$max.R2 <- append(gbl$max.R2, .max.r.squared(object.name))
      gbl$adj.R2 <- append(gbl$adj.R2, .adj.r.squared(object.name))
      gbl$AIC <- append(gbl$AIC, .AIC(object.name))
      gbl$BIC <- append(gbl$BIC, .BIC(object.name))
      gbl$scale <- append(gbl$scale, .get.scale(object.name))
      gbl$UBRE <- append(gbl$UBRE, .gcv.UBRE(object.name))
      gbl$sigma2 <- append(gbl$sigma2, .get.sigma2(object.name))
      
      
      gbl$rho <- cbind(gbl$rho, .get.rho(object.name))
      gbl$mills <- cbind(gbl$mills, .get.mills(object.name))
      gbl$theta <- cbind(gbl$theta, .get.theta(object.name))
      gbl$SER <- cbind(gbl$SER, .SER(object.name))
      gbl$F.stat <- cbind(gbl$F.stat, .F.stat(object.name))
      gbl$chi.stat <- cbind(gbl$chi.stat, .chi.stat(object.name))
      gbl$wald.stat <- cbind(gbl$wald.stat, .wald.stat(object.name))
      gbl$lr.stat <- cbind(gbl$lr.stat, .lr.stat(object.name))
      gbl$logrank.stat <- cbind(gbl$logrank.stat, .logrank.stat(object.name))
      gbl$null.deviance <- cbind(gbl$null.deviance, .null.deviance(object.name, .summary.object))
      gbl$residual.deviance <- cbind(gbl$residual.deviance, .residual.deviance(object.name))
      
      max.length <- length(gbl$coefficient.variables)+length(.coefficient.variables(object.name))
      
      # add RHS variables and coefficients
      coef.var <- .coefficient.variables(object.name)
      gbl$coef.vars.by.model <-  cbind(gbl$coef.vars.by.model, coef.var)
      
      temp.gcv <- rep(NA,each=1,times=max.length)
      
      temp.gcv[1:length(gbl$coefficient.variables)] <- gbl$coefficient.variables
      
      how.many.gcv <- length(gbl$coefficient.variables)
      
      # try to find variable
      position <- 0
      for (i in seq(1:length(coef.var))) {
        
        found <- FALSE
        
        for (j in seq(1:length(gbl$coefficient.variables))) {
          if (coef.var[i] == gbl$coefficient.variables[j]) {
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
      
      gbl$coefficient.variables <- temp.gcv[1:how.many.gcv]
      
      # build up coefficients from scratch
      temp.coefficients <- temp.std.errors <- temp.ci.lb <- temp.ci.rb <- temp.t.stats <- temp.p.values <- matrix(data = NA, nrow = length(gbl$coefficient.variables), ncol = ncol(gbl$coefficients)+1)
      rownames(temp.coefficients) <- rownames(temp.std.errors) <- rownames(temp.ci.lb) <- rownames(temp.ci.rb) <- rownames(temp.t.stats) <- rownames(temp.p.values) <- gbl$coefficient.variables
      
      # fill in from previous iteration of .global coefficients
      which.variable <- 0
      for (row in gbl$coefficient.variables) {
        
        which.variable <- which.variable + 1
        
        row.i <- .rename.intercept(row, gbl, fmt)   # row with intercept renamed to get the omit and keep right
        
        ### if omitted variable, then advance to the next iteration of the loop --- !!! do this also for index
        #skip all of this if omitted based on regular expression
        omitted <- FALSE
        
        if (!is.null(fmt$omit.regexp)) {
          for (i in seq(1:length(fmt$omit.regexp))) {
            if (length(grep(fmt$omit.regexp[i], row.i, perl=fmt$perl, fixed=FALSE))!=0) { omitted <- TRUE	}
          }
        }
        
        if (!is.null(fmt$keep.regexp)) {
          omitted <- TRUE
          for (i in seq(1:length(fmt$keep.regexp))) {
            if (length(grep(fmt$keep.regexp[i], row.i, perl=fmt$perl, fixed=FALSE))!=0) { omitted <- FALSE	}
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
        
        if (omitted == TRUE) { next }
        
        
        ###
        
        for (col in seq(1:ncol(gbl$coefficients))) {
          if (sum(as.vector(rownames(gbl$coefficients[,col, drop=FALSE])==row))!=0) { 
            if (!is.null(gbl$coefficients)) { temp.coefficients[row, col] <- gbl$coefficients[row, col] }
            if (!is.null(gbl$std.errors)) { temp.std.errors[row, col] <- gbl$std.errors[row, col] }
            if (!is.null(gbl$ci.lb)) { temp.ci.lb[row, col] <- gbl$ci.lb[row, col] }
            if (!is.null(gbl$ci.rb)) { temp.ci.rb[row, col] <- gbl$ci.rb[row, col] }
            if (!is.null(gbl$t.stats)) { temp.t.stats[row, col] <- gbl$t.stats[row, col] }
            if (!is.null(gbl$p.values)) { temp.p.values[row, col] <- gbl$p.values[row, col] }
          }
        }
        
        feed.coef <- NA; feed.se <- NA
        # coefficients and standard errors
        if (!is.null(.get.coefficients(object.name, user.coef, model.num=model.num, .summary.object)[row])) { 
          temp.coefficients[row, ncol(temp.coefficients)] <- .get.coefficients(object.name, user.coef, model.num=model.num, .summary.object)[row] 
          feed.coef <- temp.coefficients[, ncol(temp.coefficients)]
        }
        if (!is.null(.get.standard.errors(object.name, user.se, model.num=model.num, .summary.object)[row])) { 
          temp.std.errors[row, ncol(temp.std.errors)] <- .get.standard.errors(object.name, user.se, model.num=model.num, .summary.object)[row] 
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
      
      if (!is.null(temp.coefficients)) { gbl$coefficients <- temp.coefficients }
      if (!is.null(temp.std.errors)) { gbl$std.errors <- temp.std.errors }
      if (!is.null(temp.ci.lb)) { gbl$ci.lb <- temp.ci.lb }
      if (!is.null(temp.ci.rb)) { gbl$ci.rb <- temp.ci.rb }
      if (!is.null(temp.t.stats)) { gbl$t.stats <- temp.t.stats }
      if (!is.null(temp.p.values)) { gbl$p.values <- temp.p.values }
      
    }
    
    gbl
  } 
