.adj.r.squared <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","Arima","coeftest","maBina", "lmer", "glmer", "nlmer", "Gls"))) {
      if (model.name %in% c("heckit")) {
        return(.summary.object$rSquared$R2adj)
      }
      if (model.name %in% c("felm")) {
        return(.summary.object$r2adj)
      }
      if (!is.null(suppressMessages(.summary.object$adj.r.squared))) {
        return(as.vector(suppressMessages(.summary.object$adj.r.squared)))
      }
      else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
        return(as.vector(.summary.object$r.sq))
      }
      else if (model.name %in% c("plm")) {
        return(as.vector(.summary.object$r.squared["adjrsq"]))
      }
      else if (model.name %in% c("ols")) {
        n <- nobs(object.name)
        p <- length(object.name$coefficients[names(object.name$coefficients)!="Intercept"])
        r2 <- object.name$stats["R2"]
        adj.r2 <- 1-(1-r2)*((n-1) / (n-p-1))
        return(as.vector(adj.r2))
      }
    }
    return(NA)
  }



.F.stat <-
  function(object.name) {
    F.stat.output <- as.vector(rep(NA,times=4))
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH", "Arima", "maBina","coeftest", "lmer", "glmer", "nlmer", "Gls"))) {
      if (model.name %in% c("plm")) {
        F.stat.value <- .summary.object$fstatistic$statistic
        df.numerator <- .summary.object$fstatistic$parameter["df1"]
        df.denominator <- .summary.object$fstatistic$parameter["df2"]
        F.stat.p.value <- .summary.object$fstatistic$p.value
        
        F.stat.output <- as.vector(c(F.stat.value, df.numerator, df.denominator, F.stat.p.value))
      }
      else if (!is.null(suppressMessages(.summary.object$fstatistic["value"]))) {
        F.stat.value <- .summary.object$fstatistic["value"]
        df.numerator <- .summary.object$fstatistic["numdf"]
        df.denominator <- .summary.object$fstatistic["dendf"]
        F.stat.p.value <- pf(F.stat.value, df.numerator, df.denominator, lower.tail=FALSE)
        
        F.stat.output <- as.vector(c(F.stat.value, df.numerator, df.denominator, F.stat.p.value))
      }
    }
    
    names(F.stat.output) <- c("statistic","df1","df2","p-value")
    return(cbind(F.stat.output))
  }

.gcv.UBRE <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH", "Arima", "maBina", "coeftest", "lmer", "Gls", "glmer", "nlmer"))) {
      if (!is.null(object.name$gcv.ubre)) {
        return(as.vector(object.name$gcv.ubre))
      }
    }
    return(NA)
  }




.wald.stat <-
  function(object.name) {
    wald.output <- as.vector(rep(NA,times=3))
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina","coeftest", "Gls", "ivreg","lmer","glmer","nlmer"))) {
      if (!is.null(.summary.object$waldtest)) {
        wald.value <- suppressMessages(.summary.object$waldtest[1])
        df.value <- suppressMessages(.summary.object$waldtest[2])
        wald.p.value <- suppressMessages(.summary.object$waldtest[3])
        wald.output <- as.vector(c(wald.value, df.value, wald.p.value))
      }
      else if (model.name %in% c("tobit(AER)")) {
        wald.value <- .summary.object$wald
        df.value <- .summary.object$df - .summary.object$idf
        wald.p.value <- pchisq(wald.value, df.value, lower.tail=FALSE)
        wald.output <- as.vector(c(wald.value, df.value, wald.p.value))
        
      }
      else if (model.name %in% c("lagsarlm", "errorsarlm")) {
        wald.value <- as.vector(.summary.object$Wald1$statistic)
        df.value <- as.vector(.summary.object$Wald1$parameter)
        wald.p.value <- as.vector(.summary.object$Wald1$p.value)
        wald.output <- as.vector(c(wald.value, df.value, wald.p.value))
      }
      
    }
    
    names(wald.output) <- c("statistic","df1","p-value")
    return(cbind(wald.output))
  }

.get.coefficients.1 <-
  function(object.name, user.given=NULL, model.num=1, .summary.object) {
    
    if (!is.null(user.given)) { 
      
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { user.given <- as.vector(user.given[model.num,]) }
      }
      
      return(user.given) 
    }
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.survey",
                          "cloglog.net", "gamma.net", "logit.net", "probit.net", "brglm", "glm()", "Glm()", "svyglm()", "plm", "pgmm", "ivreg", "lmrob", "glmrob", "dynlm", "gmm", "mclogit")) {
      return(.summary.object$coefficients[,"Estimate"])
    }
    if (model.name %in% c("Arima")) {
      return(object.name$coef)
    }
    if (model.name %in% c("censReg")) {
      return(.summary.object$estimate[,1])
    }
    if (model.name %in% c("mnlogit")) {
      return(.summary.object$CoefTable[,1])
    }
    if (model.name %in% c("fGARCH")) {
      return(object.name@fit$matcoef[,1])
    }
    if (model.name %in% c("lme","nlme")) {
      return(.summary.object$tTable[,1])
    }
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$out[,1]))
    }
    if (model.name %in% c("mlogit")) {
      return(as.vector(.summary.object$CoefTable[,1]))
    }
    if (model.name %in% c("coeftest")) {
      return(as.vector(object.name[,1]))
    }
    if (model.name %in% c("selection", "heckit")) {
      if (!gbl$sel.equation) {
        indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
      return(as.vector(.summary.object$estimate[indices,1]))
    }
    if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(.summary.object$estimate[,1]))
    }
    if (model.name %in% c("hetglm")) {
      return(as.vector(.summary.object$coefficients$mean[,1]))
    }
    if (model.name %in% c("lmer","glmer","nlmer")) {
      coefs <- .summary.object$coefficients[,1]
      return(coefs)
    }
    if (model.name %in% c("ergm")) {
      return(.summary.object$coefs[,1])
    }
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$Coef[,1])
    }
    if (model.name %in% c("rq","felm")) {
      return(.summary.object$coefficients[,1])
    }
    if (model.name %in% c("clm")) {
      if (fmt$ordered.intercepts == FALSE) {
        return(.summary.object$coefficients[(length(object.name$alpha)+1):(length(object.name$coefficients)),1])
      }
      else {
        return(.summary.object$coefficients[,1])
      }
    }
    else if (model.name %in% c("pmg")) {
      return(.summary.object$coefficients)
    }
    else if (model.name %in% c("zeroinfl", "hurdle")) {
      if (gbl$zero.component==FALSE) {
        return(.summary.object$coefficients$count[,"Estimate"])  
      }
      else {
        return(.summary.object$coefficients$zero[,"Estimate"])
      }
    }
    else if (model.name %in% c("normal.gee", "logit.gee", "probit.gee", "poisson.gee", "gamma.gee", "gee()")) {
      return(.summary.object$coefficients[,"Estimate"])
    }
    else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
      return(.summary.object$p.coeff)
    }
    else if (model.name %in% c("coxph", "clogit")) {
      return(.summary.object$coef[,"coef"])
    }
    else if (model.name %in% c("exp","lognorm","weibull","tobit","survreg()")) {
      return(.summary.object$table[,"Value"])
    }
    else if (model.name %in% c("rlm")) {
      return(suppressMessages(.summary.object$coefficients[,"Value"]))
    }
    else if (model.name %in% c("ologit", "oprobit", "polr()")) {
      coef.temp <- suppressMessages(.summary.object$coefficients[,"Value"])
      if (fmt$ordered.intercepts == FALSE) { return(coef.temp[seq(from=1, to=length(coef.temp)-(length(suppressMessages(.summary.object$lev))-1))]) }
      else { return(coef.temp) }
    }
    else if (model.name %in% c("arima", "rem.dyad")) {
      return( object.name$coef )
    }
    else if (model.name %in% c("tobit(AER)")){
      return(.summary.object$coefficients[,"Estimate"])
    }
    else if (model.name %in% c("multinom")){
      if (is.null(nrow(.summary.object$coefficients))) {
        coef.temp <- .summary.object$coefficients
      }
      else {
        coef.temp <- .summary.object$coefficients[model.num,]
      }
      return(coef.temp)
    }
    else if (model.name %in% c("betareg")){
      return(.summary.object$coefficients$mean[,"Estimate"])
    }
    else if (model.name %in% c("gls")) {
      coef.temp <- object.name$coefficients
      return(coef.temp)
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      return( object.name$coefficients )
    }
    else { return(NULL) }
    
  }

.get.coefficients <-
  function(object.name, user.given=NULL, model.num=1, .summary.object) {
    out <- .get.coefficients.1(object.name, user.given, model.num, .summary.object)
    
    coef.vars <- .coefficient.variables(object.name, .summary.object)
    
    if (is.null(names(out))) {  
      
      if (length(out) < length(coef.vars)) {
        out.temp <- rep(NA, times=length(coef.vars)-length(out))
        out <- c(out, out.temp)
      }
      else if (length(out) > length(coef.vars)) {
        out <- out[1:length(coef.vars)]
      }
      
      names(out) <- coef.vars   
    }
    else {
      out.temp <- rep(NA, times = length(coef.vars))
      names(out.temp) <- coef.vars
      for (i in 1:length(out)) {
        name <- names(out)[i]
        if (name %in% coef.vars) {
          out.temp[name] <- out[i]
        }
      }
      out <- out.temp
      
    }
    return(out)
  }


.r.squared <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina","coeftest","nlmer", "glmer", "lmer","Gls","Arima"))) {
      if (model.name %in% c("heckit")) {
        return(.summary.object$rSquared$R2)
      }
      if (model.name %in% c("felm")) {
        return(.summary.object$r2)
      }
      if (model.name %in% c("mlogit")) {
        return(.summary.object$mfR2[1])
      }
      if (model.name %in% c("plm")) {
        return(as.vector(.summary.object$r.squared["rsq"]))
      }
      else if (model.name %in% c("betareg")) {
        return(as.vector(.summary.object$pseudo.r.squared))
      }
      else if (!is.null(.summary.object$r.squared)) {
        return(as.vector(.summary.object$r.squared)) 
      }
      else if (model.name %in% c("coxph", "clogit")) {
        return(as.vector(.summary.object$rsq[1]))
      }
      else if (model.name %in% c("pmg")) {
        return(as.vector(.summary.object$rsqr))
      }
      else if (model.name %in% c("cph","lrm","ols","psm")) {
        return(as.vector(object.name$stats["R2"]))
      }
    }
    return(NA)
  }


.residual.deviance <-
  function(object.name) {
    residual.deviance.output <- as.vector(rep(NA,times=3))
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","Arima","coeftest", "Gls","multinom","lmer","glmer","nlmer"))) {
      if (model.name %in% c("rem.dyad")) {
        residual.deviance.value <- object.name$residual.deviance
        residual.deviance.output <- as.vector(c(residual.deviance.value, NA, NA))
      }
      else if (model.name %in% c("mclogit")) {
        residual.deviance.value <- object.name$deviance
        residual.deviance.output <- as.vector(c(residual.deviance.value, NA, NA))
      }
      else if (model.name %in% c("maBina")) {
        residual.deviance.value <- object.name$w$deviance
        df.value <- object.name$w$df.residual
        residual.deviance.output <- as.vector(c(residual.deviance.value, df.value, NA))
      }
      else if (!is.null(.summary.object$deviance)) {
        residual.deviance.value <- suppressMessages(.summary.object$deviance)
        df.value <- object.name$df.residual
        residual.deviance.output <- as.vector(c(residual.deviance.value, df.value, NA))
      }
      else if (!is.null(object.name$deviance)) {
        residual.deviance.value <- object.name$deviance
        df.value <- object.name$df.residual
        residual.deviance.output <- as.vector(c(residual.deviance.value, df.value, NA))
      }
    }
    
    names(residual.deviance.output) <- c("statistic","df1","p-value")
    return(cbind(residual.deviance.output))
  }


.null.deviance <-
  function(object.name, .summary.object) {
    null.deviance.output <- as.vector(rep(NA,times=3))
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","Arima","coeftest","Gls","lmer","glmer","nlmer", "ergm"))) {
      if (model.name %in% c("rem.dyad", "mclogit")) {
        null.deviance.value <- object.name$null.deviance
        null.deviance.output <- as.vector(c(null.deviance.value, NA, NA))
      }
      else if (model.name %in% c("maBina")) {
        null.deviance.value <- object.name$w$null.deviance
        df.value <- object.name$w$df.null
        null.deviance.output <- as.vector(c(null.deviance.value, df.value, NA))
      }
      else if (!is.null(suppressMessages(.summary.object$null.deviance))) {
        null.deviance.value <- suppressMessages(.summary.object$null.deviance)
        df.value <- object.name$df.null
        
        null.deviance.output <- as.vector(c(null.deviance.value, df.value, NA))
      }
      else if (!is.null(object.name$null.deviance)) {
        null.deviance.value <- object.name$null.deviance
        df.value <- object.name$df.null
        
        null.deviance.output <- as.vector(c(null.deviance.value, df.value, NA))
      }
    }
    
    names(null.deviance.output) <- c("statistic","df1","p-value")
    return(cbind(null.deviance.output))
  }

.number.observations <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("ls", "normal", "logit", "probit", "relogit",
                          "poisson", "negbin", "normal.survey", "poisson.survey",
                          "probit.survey", "logit.survey", "gamma", "gamma.survey",
                          "z.arima", "brglm","glm()", "Glm()", "svyglm()")) {
      return(length(object.name$residuals))
    }
    else if (model.name %in% c("fGARCH")) {
      return(length(object.name@data))
    }
    else if (model.name %in% c("maBina")) {
      return(length(object.name$w$residuals))
    }
    else if (model.name %in% c("mlogit")) {
      return(sum(object.name$freq))
    }
    else if (model.name %in% c("felm")) {
      return(object.name$N)
    }
    else if (model.name %in% c("mclogit")) {
      return(object.name$N)
    }
    else if (model.name %in% c("selection", "heckit")) {
      return(.summary.object$param$nObs)
    }
    else if (model.name %in% c("binaryChoice", "probit.ss")) {
      return(object.name$param$nObs)
    }
    else if (model.name %in% c("lmer","glmer","nlmer")) {
      return(length(resid(object.name)))  
    }
    else if (model.name %in% c("gmm")) {
      return(object.name$n)
    }
    else if (model.name %in% c("plm", "pgmm", "pmg", "rlm", "lmrob", "glmrob", "dynlm", "rq", "lagsarlm", "errorsarlm", "rem.dyad")) {
      return(as.vector(length(object.name$residual)))
    }
    else if (model.name %in% c("mnlogit")) {
      return(as.vector(.summary.object$model.size$N))
    }
    else if (model.name %in% c("hurdle", "zeroinfl")) {
      return(as.vector(object.name$n))
    }
    else if (model.name %in% c("ivreg","clm","hetglm")) {
      return(as.vector(object.name$nobs))
    }
    if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee",
                          "probit.gee", "gamma.gee", "gee()", "betareg")) {
      return(as.vector(.summary.object$nobs))
    }
    else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam",
                               "poisson.gam", "coxph", "clogit", "exp", "lognorm", "weibull", "survreg()",
                               "gam()")) {
      return(as.vector(.summary.object$n))
    }
    else if (model.name %in% c("ologit", "oprobit", "polr()")) {
      return(as.vector(.summary.object$nobs))
    }
    else if (model.name %in% c("gls")) {
      return(as.vector(object.name$dims$N))
    }
    else if (model.name %in% c("tobit(AER)")) {
      return(as.vector(.summary.object$n["Total"]))
    }
    else if (model.name %in% c("Arima","censReg","lme","nlme","weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      return(as.vector(nobs(object.name)))
    }
    return(NA)
  }

.log.likelihood <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("coeftest","maBina","gamma.net","logit.net","probit.net","cloglog.net")) {
      return(NA) 
    }
    if (model.name %in% c("fGARCH")) {
      return(object.name@fit$value)
    }
    if (model.name %in% c("mlogit", "mnlogit")) {
      return(as.vector(object.name$logLik[1]))
    }
    if (model.name %in% c("arima", "betareg", "zeroinfl", "hurdle", "hetglm", "Arima")) {
      return(as.vector(object.name$loglik))
    }
    if (model.name %in% c("selection","binaryChoice", "probit.ss")) {
      return(as.vector(.summary.object$loglik))
    }  	
    if (model.name %in% c("lme","nlme","lmer", "glmer", "nlmer","censReg")) { 
      return(as.vector(logLik(object.name)[1]))
    }
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(as.vector(.summary.object$LL))
    }  	
    if (model.name %in% c("clm", "gls")) {
      return(as.vector(object.name$logLik))
    }
    else if (model.name %in% c("coxph", "clogit", "exp", "weibull", "lognorm","tobit", "tobit(AER)", "survreg()")) {
      return(as.vector(.summary.object$loglik[2]))
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg")) {
      return(as.vector(object.name$loglik[2]))
    }
    else if (!is.null(object.name$aic)) {
      return(as.vector(-(0.5)*(object.name$aic-2*length(.summary.object$coefficients[,"Estimate"]))))
    }
    return(NA)
  }

.logrank.stat <-
  function(object.name) {
    logrank.output <- as.vector(rep(NA,times=3))
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina", "coeftest", "Gls", "lmer", "glmer", "nlmer"))) {
      if (!is.null(.summary.object$logtest)) {
        logrank.value <- suppressMessages(.summary.object$sctest[1])
        df.value <- suppressMessages(.summary.object$sctest[2])
        logrank.p.value <- suppressMessages(.summary.object$sctest[3])
        logrank.output <- as.vector(c(logrank.value, df.value, logrank.p.value))
      }
      
    }
    
    names(logrank.output) <- c("statistic","df1","p-value")
    return(cbind(logrank.output))
  }

.lr.stat <-
  function(object.name) {
    log.output <- as.vector(rep(NA,times=3))
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("mlogit")) {
      log.value <- as.vector(.summary.object$lratio$statistic["chisq"]) 
      if (!is.null(log.value)) {
        df.value <- as.vector(length(object.name$coeff))
        log.p.value <- as.vector(pchisq(log.value,df.value,lower.tail=FALSE))
        log.output <- as.vector(c(log.value, df.value, log.p.value))
      }
    }
    else if (model.name %in% c("lagsarlm", "errorsarlm")) {
      log.value <- as.vector(.summary.object$LR1$statistic)
      df.value <- as.vector(.summary.object$LR1$parameter)
      log.p.value <- as.vector(.summary.object$LR1$p.value)
      log.output <- as.vector(c(log.value, df.value, log.p.value))
    }
    else if (!(model.name %in% c("arima","fGARCH","Arima","maBina","coeftest","Gls","lmer","glmer","nlmer"))) {
      if (!is.null(.summary.object$logtest)) {
        log.value <- suppressMessages(.summary.object$logtest[1])
        df.value <- suppressMessages(.summary.object$logtest[2])
        log.p.value <- suppressMessages(.summary.object$logtest[3])
        log.output <- as.vector(c(log.value, df.value, log.p.value))
      }
      
    }   
    
    names(log.output) <- c("statistic","df1","p-value")
    return(cbind(log.output))
  }

.max.r.squared <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","fGARCH","Arima","maBina", "coeftest", "lmer", "glmer", "nlmer", "Gls", "Arima"))) {
      if (model.name %in% c("coxph", "clogit")) {
        return(as.vector(.summary.object$rsq[2]))
      }
    }
    return(NA)
  }


.AIC <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("coeftest")) {
      return(NA)
    }
    
    if (model.name %in% c("lmer","lme","nlme","glmer","nlmer", "ergm", "gls", "Gls", "lagsarlm", "errorsarlm", "", "Arima")) {
      return(as.vector(AIC(object.name)))
    }
    
    if (model.name %in% c("censReg")) {
      return(as.vector(AIC(object.name)[1]))
    }
    
    if (model.name %in% c("fGARCH")) {
      return(object.name@fit$ics["AIC"])
    }
    
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$w$aic))
    }
    
    if (model.name %in% c("arima")) {
      return(as.vector(object.name$aic))
    }
    else if (!is.null(.summary.object$aic)) {
      return(as.vector(.summary.object$aic)) 
    }
    else if (!is.null(object.name$AIC)) {
      return(as.vector(object.name$AIC)) 
    }
    
    return(NA)
  }

.BIC <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("coeftest","maBina","Arima")) {
      return(NA)
    }
    
    if (model.name %in% c("censReg")) {
      return(as.vector(BIC(object.name)[1]))
    }
    
    if (model.name %in% c("fGARCH")) {
      return(object.name@fit$ics["BIC"])
    }
    
    if (model.name %in% c("lmer","lme","nlme","glmer","nlmer", "ergm", "gls", "Gls")) {
      return(as.vector(BIC(object.name)))
    }
    
    if (model.name %in% c("arima")) {
      return(as.vector(object.name$bic))
    }
    else if (!is.null(.summary.object$bic)) {
      return(as.vector(.summary.object$bic)) 
    }
    else if (!is.null(object.name$BIC)) {
      return(as.vector(object.name$BIC)) 
    }
    
    return(NA)
  }


.chi.stat <-
  function(object.name) {
    chi.output <- as.vector(rep(NA,times=3))
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina","coeftest","lmer", "Gls", "glmer", "nlmer", "normal.gam","logit.gam","probit.gam","poisson.gam","gam()"))) {
      if (!is.null(.summary.object$chi)) {
        chi.value <- suppressMessages(.summary.object$chi)
        df.value <- suppressMessages(.summary.object$df) - suppressMessages(.summary.object$idf)
        chi.p.value <- pchisq(chi.value, df.value, ncp=0, lower.tail = FALSE, log.p = FALSE)
        chi.output <- as.vector(c(chi.value, df.value, chi.p.value))
      }
      else if (model.name %in% c("cph", "lrm", "ols", "psm")) {
        chi.value <- object.name$stat["Model L.R."]
        df.value <- object.name$stat["d.f."]
        chi.p.value <- pchisq(chi.value, df.value, ncp=0, lower.tail = FALSE, log.p = FALSE)
        chi.output <- as.vector(c(chi.value, df.value, chi.p.value))
      }
      else if (model.name %in% c("probit.ss")) {
        chi.value <- object.name$LRT$LRT
        df.value <- object.name$LRT$df
        chi.p.value <- pchisq(chi.value, df.value, ncp=0, lower.tail = FALSE, log.p = FALSE)
        chi.output <- as.vector(c(chi.value, df.value, chi.p.value))
      }
    }
    
    names(chi.output) <- c("statistic","df1","p-value")
    return(cbind(chi.output))
  }


.get.p.values.1 <-
  function(object.name, user.given=NULL, auto=TRUE, f.coef=NULL, f.se=NULL, user.coef=NULL, user.se=NULL,  model.num=1) {
    
    if (!is.null(user.given)) {
      
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { 
          user.given <- as.vector(user.given[model.num,]) 
        }
      }
      
      return(user.given) 
    }
    
    
    if (auto == TRUE) {
      if ((!is.null(user.coef)) | (!is.null(user.se))) {
        
        #if (.model.identify(object.name) == "multinom") {
        #  f.coef <- as.vector(f.coef[model.num,])
        #  f.se <- as.vector(f.se[model.num,])
        #}
        
        
        # set the lengths of the vectors to be equal to each other
        coef.div <- .fill.NA(f.coef, f.se)
        se.div <- .fill.NA(f.se, f.coef)
        
        t.out <- (coef.div / se.div)
        
        auto.return <- 2*pnorm(abs(t.out), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
        names(auto.return) <- names(f.coef)
        return( auto.return  )
      }
    }
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.survey",
                          "cloglog.net", "gamma.net", "logit.net", "probit.net", "brglm", "glm()", "Glm()", "svyglm()", "plm", "pgmm", "ivreg", "lmrob", "glmrob", "dynlm", "rq", "gmm","mclogit","felm")) {
      return(.summary.object$coefficients[,4])
    }
    if (model.name %in% c("censReg")) {
      return(.summary.object$estimate[,4])
    }
    if (model.name %in% c("mnlogit")) {
      return(.summary.object$CoefTable[,4])
    }
    if (model.name %in% c("fGARCH")) {
      return(object.name@fit$matcoef[,4])
    }
    if (model.name %in% c("lme", "nlme")) {
      return(.summary.object$tTable[,5])
    }
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$out[,4]))
    }
    if (model.name %in% c("mlogit")) {
      return(as.vector(.summary.object$CoefTable[,4]))
    }
    if (model.name %in% c("coeftest")) {
      return(as.vector(object.name[,4]))
    }
    if (model.name %in% c("hetglm")) {
      return(as.vector(.summary.object$coefficients$mean[,4]))
    }
    if (model.name %in% c("selection","heckit")) {
      if (!gbl$sel.equation) {
        indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
      return(as.vector(.summary.object$estimate[indices,4]))
    }
    if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(.summary.object$estimate[,4]))
    }
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$Coef[,4])
    }
    if (model.name %in% c("lmer", "glmer", "nlmer")) {
      Vcov <- as.matrix(vcov(object.name, useScale = FALSE))
      coefs <- .summary.object$coefficients[,1]
      se <- sqrt(diag(Vcov))
      tstat <- coefs / se
      pval <- 2 * pnorm(abs(tstat), lower.tail = FALSE)
      names(pval) <- names(coefs)
      return(pval)
    }
    if (model.name %in% c("Arima")) {
      coef.temp <- object.name$coef
      se.temp <- sqrt(diag(object.name$var.coef))
      tstat <- coef.temp / se.temp 
      pval <- 2 * pnorm(abs(tstat), lower.tail = FALSE)
      return(pval)
    }
    if (model.name %in% c("ergm")) {
      return(.summary.object$coefs[,4])
    }
    if (model.name %in% c("clm")) {
      if (fmt$ordered.intercepts == FALSE) {
        return(.summary.object$coefficients[(length(object.name$alpha)+1):(length(object.name$coefficients)),4])
      }
      else {
        return(.summary.object$coefficients[,4])
      }
    }
    else if (model.name %in% c("pmg")) {
      coef.temp <- .summary.object$coefficients
      std.err.temp <- sqrt(diag(.summary.object$vcov))
      t.stat.temp <- coef.temp / std.err.temp
      df.temp <- length(.summary.object$residuals)
      return( 2 * pt(abs(t.stat.temp), df=df.temp, lower.tail = FALSE, log.p = FALSE) )
    }
    else if (model.name %in% c("zeroinfl", "hurdle")) {
      if (gbl$zero.component==FALSE) {
        return(.summary.object$coefficients$count[,4])  
      }
      else {
        return(.summary.object$coefficients$zero[,4])
      }
      
    }
    else if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee", "probit.gee", "gamma.gee", "gee()")) {
      return(2*pnorm(abs(.summary.object$coefficients[,"Robust z"]), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE))
    }
    else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
      return(.summary.object$p.pv)
    }
    else if (model.name %in% c("coxph", "clogit")) {
      return(.summary.object$coef[,"Pr(>|z|)"])
    }
    else if (model.name %in% c("exp","lognorm","weibull","tobit", "survreg()")) {
      return(.summary.object$table[,"p"])
    }
    else if (model.name %in% c("rlm")) {
      coef.temp <- suppressMessages(.summary.object$coefficients[,"t value"])
      coef.temp <- 2*pnorm(abs(coef.temp[seq(from=1, to=length(coef.temp))]), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
      return(coef.temp)
    }
    else if (model.name %in% c("ologit", "oprobit", "polr()")) {
      coef.temp <- suppressMessages(.summary.object$coefficients[,"t value"])
      if (fmt$ordered.intercepts == FALSE) { return(2*pnorm(abs(coef.temp[seq(from=1, to=length(coef.temp)-(length(suppressMessages(.summary.object$lev))-1))]), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)) }
      else { 
        return( 2*pnorm(abs(coef.temp[seq(from=1, to=length(coef.temp))]), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE) ) 
      }
      
    }
    else if (model.name %in% c("arima")) {
      return(2*pnorm( abs(object.name$coef / (sqrt(diag(object.name$var.coef))) ), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE))
    }
    else if (model.name %in% c("tobit(AER)")){
      return(.summary.object$coefficients[,"Pr(>|z|)"])
    }
    else if (model.name %in% c("multinom")) {
      if (is.null(nrow(.summary.object$coefficients))) {
        coef.temp <- .summary.object$coefficients
        se.temp <- .summary.object$standard.errors
      }
      else {
        coef.temp <- .summary.object$coefficients[model.num,]
        se.temp <- .summary.object$standard.errors[model.num,]
      }
      return( 2*pnorm( abs( (coef.temp) / (se.temp) ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE) )
    }
    else if (model.name %in% c("betareg")) {
      return(.summary.object$coefficients$mean[,"Pr(>|z|)"])
    }
    else if (model.name %in% c("gls")) {
      coef.temp <- object.name$coefficients
      se.temp <- sqrt(diag(object.name$varBeta))
      t.temp <- coef.temp / se.temp
      p.temp <- 2*pnorm( abs( t.temp ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
      return(p.temp)
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      coef.temp <- object.name$coefficients
      se.temp <- sqrt(diag(object.name$var))
      t.temp <- coef.temp / se.temp 
      p.temp <- 2*pnorm( abs( t.temp ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
      return(p.temp)
    }
    else if (model.name %in% c("rem.dyad")) {
      coef.temp <- object.name$coef
      se.temp <- sqrt(diag(object.name$cov))
      t.temp <- coef.temp / se.temp
      p.temp <- 2*pnorm( abs( t.temp ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
      return(p.temp)
    }
    return(NULL)
  }

.get.p.values <-
  function(object.name, user.given=NULL, auto=TRUE, f.coef=NULL, f.se=NULL, user.coef=NULL, user.se=NULL, model.num=1) {
    out <- .get.p.values.1(object.name, user.given, auto, f.coef, f.se, user.coef, user.se, model.num)
    
    coef.vars <- .coefficient.variables(object.name)
    if (is.null(names(out))) {  
      
      if (length(out) < length(coef.vars)) {
        out.temp <- rep(NA, times=length(coef.vars)-length(out))
        out <- c(out, out.temp)
      }
      else if (length(out) > length(coef.vars)) {
        out <- out[1:length(coef.vars)]
      }
      
      names(out) <- coef.vars   
    }
    else {
      out.temp <- rep(NA, times = length(coef.vars))
      names(out.temp) <- coef.vars
      for (i in 1:length(out)) {
        name <- names(out)[i]
        if (name %in% coef.vars) {
          out.temp[name] <- out[i]
        }
      }
      out <- out.temp
    }
    return(out)
  }


.get.scale <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina", "coeftest", "Gls", "lmer", "glmer", "nlmer"))) {
      if (!is.null(object.name$scale)) {
        if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee", "probit.gee", "gamma.gee", "gee()", "exp","lognorm","weibull","tobit","survreg()","tobit(AER)")) {
          return(as.vector(object.name$scale))
        }
      }
    }
    return(NA)
  }

.get.sigma2 <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("arima","fGARCH","maBina", "coeftest", "Gls", "lmer", "glmer", "nlmer")) {
      return(NA)
    }
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$s2)
    }  
    if (!is.null(object.name$sigma2)) {
      return(as.vector(object.name$sigma2))
    }
    return(NA)
  }

.get.rho <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    rho.output <- as.vector(rep(NA,times=4))
    
    if (model.name %in% c("selection")) {
      i <- object.name$param$index$rho
      if (is.null(i)) { i <- object.name$param$index$errTerms["rho"] }
      if (!is.null(i)) {
        rho.output <- as.vector(.summary.object$estimate[i,])
      }
    }
    if (model.name %in% c("heckit")) {
      if (object.name$method == "2step") {
        i <- object.name$param$index$rho
        rho.output <- as.vector(.summary.object$estimate[i,])
      }
    }
    
    names(rho.output) <- c("statistic","se","tstat","p-value")
    return(cbind(rho.output))
  }

.get.mills <-
  function(object.name) {
    
    model.name <- .get.model.name(object.name)
    mills.output <- as.vector(rep(NA,times=4))
    
    if (model.name %in% c("heckit", "selection")) {
      i <- object.name$param$index$Mills
      if (!is.null(i)) {
        mills.output <- as.vector(.summary.object$estimate[i,])
      }
    }
    
    names(mills.output) <- c("statistic","se","tstat","p-value")
    return(cbind(mills.output))
  }

.get.standard.errors.1 <-
  function(object.name, user.given=NULL, model.num=1, .summary.object) {
    
    if (!is.null(user.given)) { 
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { user.given <- as.vector(user.given[model.num,]) }
      }
      
      return(user.given) 
    }
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.survey",
                          "cloglog.net", "gamma.net", "logit.net", "probit.net", "brglm", "glm()", "Glm()", "svyglm()", "plm", "pgmm", "ivreg", "lmrob", "glmrob", "dynlm", "gmm","mclogit")) {
      return(.summary.object$coefficients[,"Std. Error"])
    }
    if (model.name %in% c("Arima")) {
      return(sqrt(diag(object.name$var.coef)))
    }
    if (model.name %in% c("censReg")) {
      return(.summary.object$estimate[,2])
    }
    if (model.name %in% c("mnlogit")) {
      return(.summary.object$CoefTable[,2])
    }
    if (model.name %in% c("fGARCH")) {
      return(object.name@fit$matcoef[,2])
    }
    if (model.name %in% c("lme", "nlme")) {
      return(.summary.object$tTable[,2])
    }
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$out[,2]))
    }
    if (model.name %in% c("mlogit")) {
      return(as.vector(.summary.object$CoefTable[,2]))
    }
    if (model.name %in% c("coeftest")) {
      return(as.vector(object.name[,2]))
    }
    if (model.name %in% c("selection","heckit")) {
      if (!gbl$sel.equation) {
        indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
      return(as.vector(.summary.object$estimate[indices,2]))
    }
    if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(.summary.object$estimate[,2]))
    }
    if (model.name %in% c("hetglm")) {
      return(as.vector(.summary.object$coefficients$mean[,2]))
    }
    if (model.name %in% c("lmer", "glmer", "nlmer")) {
      Vcov <- as.matrix(vcov(object.name, useScale = FALSE))
      coefs <-.summary.object$coefficients[,1]
      se <- sqrt(diag(Vcov))
      names(se) <- names(coefs)
      return(se)
    }
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$Coef[,2])
    }    
    if (model.name %in% c("ergm")) {
      return(.summary.object$coefs[,2])
    }
    if (model.name %in% c("rq","felm")) {
      return(.summary.object$coefficients[,2])
    }
    if (model.name %in% c("clm")) {
      if (fmt$ordered.intercepts == FALSE) {
        return(.summary.object$coefficients[(length(object.name$alpha)+1):(length(object.name$coefficients)),2])
      }
      else {
        return(.summary.object$coefficients[,2])
      }
    }
    else if (model.name %in% c("pmg")) {
      return (sqrt(diag(.summary.object$vcov)))
    }
    if (model.name %in% c("zeroinfl", "hurdle")) {
      if (gbl$zero.component == FALSE) {
        return(.summary.object$coefficients$count[,"Std. Error"])  
      }
      else {
        return(.summary.object$coefficients$zero[,"Std. Error"])
      }
    }
    else if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee",  "probit.gee", "gamma.gee", "gee()")) {
      return(.summary.object$coefficients[,"Robust S.E."])
    }
    else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
      temp.se <- .summary.object$se
      names(temp.se) <- names(.summary.object$p.coeff)
      return(temp.se)
    }
    else if (model.name %in% c("coxph")) {
      return(.summary.object$coef[,"se(coef)"])
    }
    else if (model.name %in% c("clogit")) {
      return(.summary.object$coef[,"se(coef)"])
      
    }
    else if (model.name %in% c("exp","lognorm","weibull","tobit","survreg()")) {
      return(.summary.object$table[,"Std. Error"])
    }
    else if (model.name %in% c("rlm")) {
      return(suppressMessages(.summary.object$coefficients[,"Std. Error"]))
    }
    else if (model.name %in% c("ologit", "oprobit", "polr()")) {
      se.temp <- suppressMessages(.summary.object$coefficients[,"Std. Error"])
      if (fmt$ordered.intercepts == FALSE) { return(se.temp[seq(from=1, to=length(se.temp)-(length(suppressMessages(.summary.object$lev))-1))]) }
      else { return(se.temp) }
    }
    else if (model.name %in% c("arima")) {
      return( sqrt(diag(object.name$var.coef)) )
    }
    else if (model.name %in% c("tobit(AER)")){
      return(.summary.object$coefficients[,"Std. Error"])
    }
    else if (model.name %in% c("multinom")) {
      if (is.null(nrow(.summary.object$coefficients))) {
        se.temp <- .summary.object$standard.errors
      }
      else {
        se.temp <- .summary.object$standard.errors[model.num,]
      }
      return(se.temp)
    }
    else if (model.name %in% c("betareg")) {
      return(.summary.object$coefficients$mean[,"Std. Error"])
    }
    else if (model.name %in% c("gls")) {
      se.temp <- sqrt(diag(object.name$varBeta))
      return(se.temp)
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      return( sqrt(diag(object.name$var) ) )
    }
    else if (model.name %in% c("rem.dyad")) {
      return( sqrt(diag(object.name$cov) ) )
    }
    return(NULL)
  }

.get.standard.errors <-
  function(object.name, user.given=NULL, model.num=1, .summary.object) {
    out <- .get.standard.errors.1(object.name, user.given, model.num, .summary.object)
    
    coef.vars <- .coefficient.variables(object.name)
    if (is.null(names(out))) {  
      
      if (length(out) < length(coef.vars)) {
        out.temp <- rep(NA, times=length(coef.vars)-length(out))
        out <- c(out, out.temp)
      }
      else if (length(out) > length(coef.vars)) {
        out <- out[1:length(coef.vars)]
      }
      
      names(out) <- coef.vars   
    }
    else {
      out.temp <- rep(NA, times = length(coef.vars))
      names(out.temp) <- coef.vars
      for (i in 1:length(out)) {
        name <- names(out)[i]
        if (name %in% coef.vars) {
          out.temp[name] <- out[i]
        }
      }
      out <- out.temp
    }
    return(out)
  }

.get.ci.lb.1 <-
  function(object.name, user.given=NULL, model.num=1) {
    if (!is.null(user.given)) { 
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { user.given <- as.vector(user.given[model.num,]) }
      } 
      return(user.given) 
    }
    return(NULL)
  }

.get.ci.lb <-
  function(object.name, user.given=NULL, model.num=1) {
    
    out <- .get.ci.lb.1(object.name, user.given, model.num)
    
    coef.vars <- .coefficient.variables(object.name)
    if (is.null(names(out))) {  
      
      if (length(out) < length(coef.vars)) {
        out.temp <- rep(NA, times=length(coef.vars)-length(out))
        out <- c(out, out.temp)
      }
      else if (length(out) > length(coef.vars)) {
        out <- out[1:length(coef.vars)]
      }
      
      names(out) <- coef.vars   
    }
    else {
      out.temp <- rep(NA, times = length(coef.vars))
      names(out.temp) <- coef.vars
      for (i in 1:length(out)) {
        name <- names(out)[i]
        if (name %in% coef.vars) {
          out.temp[name] <- out[i]
        }
      }
      out <- out.temp
    }
    return(out)
  }

.get.ci.rb.1 <-
  function(object.name, user.given=NULL, model.num=1) {
    if (!is.null(user.given)) { 
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { user.given <- as.vector(user.given[model.num,]) }
      } 
      return(user.given) 
    }
    return(NULL)
  }

.get.ci.rb <-
  function(object.name, user.given=NULL, model.num=1) {
    
    out <- .get.ci.rb.1(object.name, user.given, model.num)
    
    coef.vars <- .coefficient.variables(object.name)
    if (is.null(names(out))) {  
      
      if (length(out) < length(coef.vars)) {
        out.temp <- rep(NA, times=length(coef.vars)-length(out))
        out <- c(out, out.temp)
      }
      else if (length(out) > length(coef.vars)) {
        out <- out[1:length(coef.vars)]
      }
      
      names(out) <- coef.vars   
    }
    else {
      out.temp <- rep(NA, times = length(coef.vars))
      names(out.temp) <- coef.vars
      for (i in 1:length(out)) {
        name <- names(out)[i]
        if (name %in% coef.vars) {
          out.temp[name] <- out[i]
        }
      }
      out <- out.temp
    }
    return(out)
  }

.get.t.stats.1 <-
  function(object.name, user.given=NULL, auto=TRUE, f.coef=NULL, f.se=NULL, user.coef=NULL, user.se=NULL, model.num=1) {
    
    if (!is.null(user.given)) { 
      
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { 
          user.given <- as.vector(user.given[model.num,]) 
        }
      }
      
      return(user.given) 
    }
    
    if (auto == TRUE) {
      if ((!is.null(user.coef)) | (!is.null(user.se))) {
        
        #if (.model.identify(object.name) == "multinom") {
        #  f.coef <- as.vector(f.coef[model.num,])
        #  f.se <- as.vector(f.se[model.num,])
        #}
        
        # set the lengths of the vectors to be equal to each other
        coef.div <- .fill.NA(f.coef, f.se)
        se.div <- .fill.NA(f.se, f.coef) 
        
        auto.return <- coef.div / se.div
        names(auto.return) <- names(f.coef)
        
        return(auto.return)
      }
    }
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.survey",
                          "cloglog.net", "gamma.net", "logit.net", "probit.net", "glm()", "Glm()", "svyglm()","plm", "pgmm", "ivreg", "lmrob", "glmrob", "dynlm", "gmm", "mclogit", "felm")) {
      return(.summary.object$coefficients[,3])
    }
    if (model.name %in% c("censReg")) {
      return(.summary.object$estimate[,3])
    }
    if (model.name %in% c("mnlogit")) {
      return(.summary.object$CoefTable[,3])
    }
    if (model.name %in% c("fGARCH")) {
      return(object.name@fit$matcoef[,3])
    }
    if (model.name %in% c("lme", "nlme")) {
      return(.summary.object$tTable[,4])
    }
    if (model.name %in% c("coeftest")) {
      return(as.vector(object.name[,3]))
    }
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$out[,3]))
    }
    if (model.name %in% c("mlogit")) {
      return(as.vector(.summary.object$CoefTable[,3]))
    }
    if (model.name %in% c("selection","heckit")) {
      if (!gbl$sel.equation) {
        indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
      return(as.vector(.summary.object$estimate[indices,3]))
    }
    if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(.summary.object$estimate[,3]))
    }
    if (model.name %in% c("hetglm")) {
      return(as.vector(.summary.object$coefficients$mean[,3]))
    }
    if (model.name %in% c("lmer", "glmer", "nlmer")) {
      Vcov <- as.matrix(vcov(object.name, useScale = FALSE))
      coefs <- .summary.object$coefficients[,1]
      se <- sqrt(diag(Vcov))
      tstat <- coefs / se
      names(tstat) <- names(coefs)
      
      return(tstat)
    }
    if (model.name %in% c("ergm")) {
      return((.summary.object$coefs[,1])/(.summary.object$coefs[,2]))
    }
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$Coef[,3])
    }    
    if (model.name %in% c("rq")) {
      return(.summary.object$coefficients[,3])
    }
    if (model.name %in% c("clm")) {
      if (fmt$ordered.intercepts == FALSE) {
        return(.summary.object$coefficients[(length(object.name$alpha)+1):(length(object.name$coefficients)),3])
      }
      else {
        return(.summary.object$coefficients[,3])
      }
    }
    else if (model.name %in% c("pmg")) {
      coef.temp <- .summary.object$coef
      std.err.temp <- sqrt(diag(.summary.object$vcov))
      t.stat.temp <- coef.temp / std.err.temp
      return(t.stat.temp)
    }
    else if (model.name %in% c("zeroinfl", "hurdle")) {
      if (gbl$zero.component == FALSE) {
        return(.summary.object$coefficients$count[,3])  
      }
      else {
        return(.summary.object$coefficients$zero[,3])
      }
      
    }
    else if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee",  "probit.gee", "gamma.gee", "gee()")) {
      return(.summary.object$coefficients[,"Robust z"])
    }
    else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
      return(.summary.object$p.t)
    }
    else if (model.name %in% c("coxph", "clogit")) {
      return(.summary.object$coef[,"z"])
    }
    else if (model.name %in% c("exp","lognorm","weibull", "tobit","survreg()")) {
      return(.summary.object$table[,"z"])
    }
    else if (model.name %in% c("rlm")) {
      return(suppressMessages(.summary.object$coefficients[,"t value"]))
    }
    else if (model.name %in% c("ologit", "oprobit", "polr()")) {
      tstat.temp <- suppressMessages(.summary.object$coefficients[,"t value"])
      if (fmt$ordered.intercepts == FALSE) { return(tstat.temp[seq(from=1, to=length(tstat.temp)-(length(suppressMessages(.summary.object$lev))-1))]) }
      else { return(tstat.temp) }
    }
    else if (model.name %in% c("arima")) {
      return( object.name$coef / (sqrt(diag(object.name$var.coef))) )
    }
    else if (model.name %in% c("tobit(AER)")){
      return(.summary.object$coefficients[,"z value"])
    }
    else if (model.name %in% c("multinom")) {
      if (is.null(nrow(.summary.object$coefficients))) {
        coef.temp <- .summary.object$coefficients
        se.temp <- .summary.object$standard.errors
      }
      else {
        coef.temp <- .summary.object$coefficients[model.num,]
        se.temp <- .summary.object$standard.errors[model.num,]
      }
      return( (coef.temp) / (se.temp) )
    }
    else if (model.name %in% c("betareg")) {
      return(.summary.object$coefficients$mean[,"z value"])
    }
    else if (model.name %in% c("gls")) {
      coef.temp <- object.name$coefficients
      se.temp <- sqrt(diag(object.name$varBeta))
      return(coef.temp / se.temp)
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      coef.temp <- object.name$coefficients
      se.temp <- sqrt(diag(object.name$var))
      return(coef.temp / se.temp )
    }
    else if (model.name %in% c("Arima")) {
      coef.temp <- object.name$coef
      se.temp <- sqrt(diag(object.name$var.coef))
      return(coef.temp / se.temp )
    }
    else if (model.name %in% c("rem.dyad")) {
      coef.temp <- object.name$coef
      se.temp <- sqrt(diag(object.name$cov))
      return(coef.temp / se.temp )
    }
    
    return(NULL)
  }

.get.t.stats <-
  function(object.name, user.given=NULL, auto=TRUE, f.coef=NULL, f.se=NULL, user.coef=NULL, user.se=NULL, model.num=1) {
    out <- .get.t.stats.1(object.name, user.given, auto, f.coef, f.se, user.coef, user.se, model.num)
    
    coef.vars <- .coefficient.variables(object.name)
    if (is.null(names(out))) {  
      
      if (length(out) < length(coef.vars)) {
        out.temp <- rep(NA, times=length(coef.vars)-length(out))
        out <- c(out, out.temp)
      }
      else if (length(out) > length(coef.vars)) {
        out <- out[1:length(coef.vars)]
      }
      
      names(out) <- coef.vars   
    }
    else {
      out.temp <- rep(NA, times = length(coef.vars))
      names(out.temp) <- coef.vars
      for (i in 1:length(out)) {
        name <- names(out)[i]
        if (name %in% coef.vars) {
          out.temp[name] <- out[i]
        }
      }
      out <- out.temp
    }
    return(out)
  }


.get.theta <-
  function(object.name) {
    theta.output <- as.vector(rep(NA,times=4))
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina", "coeftest", "Gls", "lmer", "glmer", "nlmer"))) {
      if ((!is.null(object.name$theta)) & (!is.null(object.name$SE.theta))) {
        theta.value <- object.name$theta
        theta.se.value <- object.name$SE.theta
        theta.tstat.value <- theta.value / theta.se.value
        theta.p.value <- 2*pnorm(abs(theta.tstat.value), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
        
        theta.output <- as.vector(c(theta.value, theta.se.value, theta.tstat.value, theta.p.value))
      }
    }
    
    names(theta.output) <- c("statistic","se","tstat","p-value")
    return(cbind(theta.output))
  }



.SER <-
  function(object.name) {
    SER.output <- as.vector(rep(NA,times=3))
    
    model.name <- .get.model.name(object.name)
    
    if (!(model.name %in% c("arima","lme","nlme","fGARCH","Arima","maBina","coeftest","lmer","glmer","nlmer","gls","Gls"))) {
      if (model.name %in% c("felm")) {
        SER.output <- as.vector(c(.summary.object$rse, .summary.object$rdf, NA))
      }
      else if (!is.null(suppressMessages(.summary.object$sigma))) {
        sigma.value <-suppressMessages(.summary.object$sigma)
        if (model.name %in% c("rlm")) {
          df.residual.value <- .summary.object$df[2]
        } 
        else {
          df.residual.value <- object.name$df.residual
        }
        SER.output <- as.vector(c(sigma.value, df.residual.value, NA))
      }
    }
    
    names(SER.output) <- c("statistic","df1","p-value")
    return(cbind(SER.output))
  }



.coefficient.table.part <-
  function(part, which.variable, variable.name=NULL, fmt, gbl) {
    
    # coefficient variable name
    if (part=="variable name") {
      
      # use intercept name for intercept, otherwise variable name
      if (is.na(fmt$covariate.labels[.which.variable.label])) {
        if (fmt$coefficient.variables.capitalize == TRUE) { cat(" ", fmt$coefficient.variables.left, toupper(variable.name), fmt$coefficient.variables.right, sep="") }
        else { cat(" ", fmt$coefficient.variables.left, variable.name, fmt$coefficient.variables.right, sep="") }
      }
      else { cat(" ", fmt$coefficient.variables.left, fmt$covariate.labels[.which.variable.label], fmt$coefficient.variables.right, sep="") }
    }
    
    # coefficients and stars
    else if ((part=="coefficient") | (part=="coefficient*")) {
      for (i in seq(1:length(gbl$models))) {
        if (!is.na(gbl$coefficients[gbl$coefficient.variables[which.variable],i])) {
          
          # report the coefficient
          cat(" & ", .iround(gbl$coefficients[gbl$coefficient.variables[which.variable],i],fmt$round.digits, fmt=fmt),sep="")
          
          # add stars to denote statistical significance
          if (part=="coefficient*") { 
            p.value <- gbl$p.values[gbl$coefficient.variables[which.variable],i]
            .enter.significance.stars(p.value, fmt=fmt) 
          }
          
        }
        else {
          cat(" & ",sep="")
        }
        
        # if single-row, follow up with standard error / confidence interval
        if ((fmt$single.row == TRUE) & (("standard error" %in% fmt$coefficient.table.parts) | ("standard error*" %in% fmt$coefficient.table.parts))) {
          
          if (fmt$dec.mark.align == TRUE) { space.char <- "$ $"}
          else { space.char <- " "}
          
          if (!is.na(gbl$std.errors[gbl$coefficient.variables[which.variable],i])) {
            
            # report standard errors or confidence intervals
            
            fmt$ci.use <- fmt$ci[i]
            if (is.na(fmt$ci.use)) {
              for (j in i:1) {
                if (!is.na(fmt$ci[j])) {
                  fmt$ci.use <- fmt$ci[j]
                  break
                }
              }
            }
            
            if (fmt$ci.use == TRUE) {
              
              # if ci level is NA, find the most recent set level
              fmt$ci.level.use <- fmt$ci.level[i]
              if (is.na(fmt$ci.level.use)) {
                for (j in i:1) {
                  if (!is.na(fmt$ci.level[j])) {
                    fmt$ci.level.use <- fmt$ci.level[j]
                    break
                  }
                }
              }
              
              z.value <- qnorm((1 + fmt$ci.level.use)/2)
              coef <- gbl$coefficients[gbl$coefficient.variables[which.variable],i]
              se <- gbl$std.errors[gbl$coefficient.variables[which.variable],i]
              ci.lower.bound <- coef - z.value * se
              ci.upper.bound <- coef + z.value * se
              
              if (!is.null(ci.custom[[i]])) {
                ci.lower.bound.temp <- gbl$ci.lb[gbl$coefficient.variables[which.variable],i]
                ci.upper.bound.temp <- gbl$ci.rb[gbl$coefficient.variables[which.variable],i]
                if (!is.na(ci.lower.bound.temp)) (ci.lower.bound <- ci.lower.bound.temp)
                if (!is.na(ci.upper.bound.temp)) (ci.upper.bound <- ci.upper.bound.temp)
              }
              
              if (!is.null(apply.ci)) { 
                ci.lower.bound <- do.call(apply.ci, list(ci.lower.bound))
                ci.upper.bound <- do.call(apply.ci, list(ci.upper.bound))
              }
              
              if (fmt$dec.mark.align == TRUE) {
                hyphen <- paste("$",fmt$ci.separator,"$", sep="")
              }
              else {
                hyphen <- fmt$ci.separator
              }
              
              cat(space.char, 
                  fmt$std.errors.left, 
                  .iround(ci.lower.bound,fmt$round.digits, fmt=fmt), 
                  hyphen,
                  .iround(ci.upper.bound,fmt$round.digits, fmt=fmt),
                  fmt$std.errors.right,sep="")              
              
            }
            else { 
              cat(space.char, fmt$std.errors.left, 
                  .iround(gbl$std.errors[gbl$coefficient.variables[which.variable],i],fmt$round.digits, fmt=fmt),fmt$std.errors.right,sep="")
            }
            
            # add stars to denote statistical significance
            if ("standard error*" %in% fmt$coefficient.table.parts) { 
              p.value <- gbl$p.values[gbl$coefficient.variables[which.variable],i]
              .enter.significance.stars(p.value, fmt=fmt) 
            }
            
          }
        }
      }
      cat(" \\\\ \n ")
    }
    
    # standard errors
    else if (((part=="standard error") | (part=="standard error*")) & (fmt$single.row==FALSE)) {
      for (i in seq(1:length(gbl$models))) {
        if (!is.na(gbl$std.errors[gbl$coefficient.variables[which.variable],i])) {
          
          # report standard errors or confidence intervals
          fmt$ci.use <- fmt$ci[i]
          if (is.na(fmt$ci.use)) {
            for (j in i:1) {
              if (!is.na(fmt$ci[j])) {
                fmt$ci.use <- fmt$ci[j]
                break
              }
            }
          }
          
          if (fmt$ci.use == TRUE) {
            # if ci level is NA, find the most recent set level
            fmt$ci.level.use <- fmt$ci.level[i]
            if (is.na(fmt$ci.level.use)) {
              for (j in i:1) {
                if (!is.na(fmt$ci.level[j])) {
                  fmt$ci.level.use <- fmt$ci.level[j]
                  break
                }
              }
            }
            
            z.value <- qnorm((1 + fmt$ci.level.use)/2)
            coef <- gbl$coefficients[gbl$coefficient.variables[which.variable],i]
            se <- gbl$std.errors[gbl$coefficient.variables[which.variable],i]
            ci.lower.bound <- coef - z.value * se
            ci.upper.bound <- coef + z.value * se
            
            if (!is.null(ci.custom[[i]])) {
              ci.lower.bound.temp <- gbl$ci.lb[gbl$coefficient.variables[which.variable],i]
              ci.upper.bound.temp <- gbl$ci.rb[gbl$coefficient.variables[which.variable],i]
              if (!is.na(ci.lower.bound.temp)) (ci.lower.bound <- ci.lower.bound.temp)
              if (!is.na(ci.upper.bound.temp)) (ci.upper.bound <- ci.upper.bound.temp)
            }
            
            if (!is.null(apply.ci)) { 
              ci.lower.bound <- do.call(apply.ci, list(ci.lower.bound))
              ci.upper.bound <- do.call(apply.ci, list(ci.upper.bound))
            }
            
            if (fmt$dec.mark.align == TRUE) {
              hyphen <- paste("$",fmt$ci.separator,"$", sep="")
            }
            else {
              hyphen <- fmt$ci.separator
            }
            
            if (fmt$dec.mark.align == TRUE) {
              cat(" & \\multicolumn{1}{c}{", fmt$std.errors.left, .iround(ci.lower.bound,fmt$round.digits, fmt=fmt),hyphen,.iround(ci.upper.bound,fmt$round.digits, fmt=fmt),fmt$std.errors.right,"}",sep="")
            }
            else {
              cat(" & ", fmt$std.errors.left, .iround(ci.lower.bound,fmt$round.digits, fmt=fmt),hyphen,.iround(ci.upper.bound,fmt$round.digits, fmt=fmt),fmt$std.errors.right,sep="")              
            }
            
            
          }
          else { 
            cat(" & ", fmt$std.errors.left, .iround(gbl$std.errors[gbl$coefficient.variables[which.variable],i],fmt$round.digits, fmt=fmt),fmt$std.errors.right,sep="")
          }
          
          # add stars to denote statistical significance
          if (part=="standard error*") { 
            p.value <- gbl$p.values[gbl$coefficient.variables[which.variable],i]
            .enter.significance.stars(p.value, fmt=fmt) 
          }
          
        }
        else {
          cat(" & ",sep="")
        }
      }
      cat(" \\\\ \n ")
    }
    
    
    # p-values
    else if ((part=="p-value") | (part=="p-value*")) {
      for (i in seq(1:length(gbl$models))) {
        if (!is.na(gbl$p.values[gbl$coefficient.variables[which.variable],i])) {
          
          # report p-values
          cat(" & ", fmt$p.values.left, .iround(gbl$p.values[gbl$coefficient.variables[which.variable],i],fmt$round.digits,round.up.positive=TRUE, fmt=fmt),fmt$p.values.right,sep="")
          
          # add stars to denote statistical significance
          if (part=="p-value*") { 
            p.value <- gbl$p.values[gbl$coefficient.variables[which.variable],i]
            .enter.significance.stars(p.value, fmt=fmt) 
          }
          
        }
        else {
          cat(" & ",sep="")
        }
      }
      cat(" \\\\ \n ")
    }
    
    # t-statistics
    else if ((part=="t-stat") | (part=="t-stat*")) {
      for (i in seq(1:length(gbl$models))) {
        if (!is.na(gbl$t.stats[gbl$coefficient.variables[which.variable],i])) {
          # report t-statistics
          cat(" & ", fmt$t.stats.left, .iround(gbl$t.stats[gbl$coefficient.variables[which.variable],i],fmt$round.digits, fmt=fmt),fmt$t.stats.right,sep="")
          
          # add stars to denote statistical significance
          if (part=="t-stat*") { 
            p.value <- gbl$p.values[gbl$coefficient.variables[which.variable],i]
            .enter.significance.stars(p.value, fmt=fmt) 
          }
          
        }
        else {
          cat(" & ",sep="")
        }
      }
      cat(" \\\\ \n ")
    }
    
    
    # empty line
    else if (part==" ") {
      .table.empty.line(fmt=fmt, gbl=gbl)
    }
    
    # horizontal line
    else if (part=="-") {
      cat("\\hline ")
      .table.insert.space(fmt=fmt)
      cat(" \n")
    }
    
    # double horizontal line
    else if (part=="=") {
      cat("\\hline \n") 
      cat("\\hline ")
      .table.insert.space(fmt=fmt)
      cat(" \n")
    }
    
  }

.coefficient.variables <-
  function(object.name, .summary.object) {
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.gee", "logit.gee", "probit.gee", "poisson.gee", "normal.gam", 
                          "logit.gam", "probit.gam", "poisson.gam", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.gee", "gamma.survey",
                          "exp", "weibull", "coxph", "clogit", "lognorm", "tobit", "tobit(AER)", "brglm", "glm()", "Glm()", "svyglm()", "gee()", "survreg()", "gam()", "plm", "ivreg", "pmg", "lmrob", "glmrob", 
                          "dynlm", "gls", "rq", "lagsarlm", "errorsarlm", "gmm", "mclogit")) {
      return(as.vector(names(object.name$coefficients)))
    }
    else if (model.name %in% c("Arima")) {
      return(names(object.name$coef))
    }
    else if (model.name %in% c("fGARCH")) {
      return(rownames(object.name@fit$matcoef))
    }
    else if (model.name %in% c("censReg")) {
      return(rownames(.summary.object$estimate))
    }
    else if (model.name %in% c("mnlogit")) {
      return(rownames(.summary.object$CoefTable))
    }
    else if (model.name %in% c("lme","nlme")) {
      return(rownames(.summary.object$tTable))
    }
    else if (model.name %in% c("felm")) {
      return(row.names(object.name$coefficients))
    }
    else if (model.name %in% c("maBina")) {
      return(as.vector(rownames(object.name$out)))
    }
    else if (model.name %in% c("mlogit")) {
      return(as.vector(rownames(.summary.object$CoefTable)))
    }
    else if (model.name %in% c("hetglm")) {
      return(as.vector(names(object.name$coefficients$mean)))
    }
    else if (model.name %in% c("selection","heckit")) {
      if (!gbl$sel.equation) {
        indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
      return(as.vector(names(.summary.object$estimate[indices, 1])))
    }
    else if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(names(.summary.object$estimate[,1])))
    }
    else if (model.name %in% c("coeftest")) {
      return(as.vector(rownames(object.name)))
    }
    else if (model.name %in% c("clm")) {
      if (fmt$ordered.intercepts == FALSE) { return(as.vector(names(object.name$beta))) }
      else { return(c(as.vector(names(object.name$beta)), as.vector(names(object.name$alpha)))) }
    }
    else if (model.name %in% c("lmer", "glmer", "nlmer", "pgmm")) {
      return(as.vector(rownames(.summary.object$coefficients)))
    }
    else if (model.name %in% c("ergm", "rem.dyad")) {
      return(as.vector(names(object.name$coef)))
    }
    else if (model.name %in% c("betareg")) {
      return(as.vector(names(object.name$coefficients$mean)))
    }
    else if (model.name %in% c("zeroinfl", "hurdle")) {
      if (gbl$zero.component==FALSE) {
        return(as.vector(names(object.name$coefficients$count)))
      }
      else {
        return(as.vector(names(object.name$coefficients$zero)))
      }
    }
    else if (model.name %in% c("cloglog.net", "gamma.net", "logit.net", "probit.net")) {
      return(as.vector(rownames(.summary.object$coefficients))) 
    }
    else if (model.name %in% c("rlm")) {
      return(as.vector(rownames(suppressMessages(.summary.object$coefficients))))
    }
    else if (model.name %in% c("ologit", "oprobit", "polr()")) {
      coef.temp <- as.vector(rownames(suppressMessages(.summary.object$coefficients)))
      if (fmt$ordered.intercepts == FALSE) { return(coef.temp[seq(from=1, to=length(coef.temp)-(length(suppressMessages(.summary.object$lev))-1))]) }
      else { return(coef.temp) }
    }
    else if (model.name %in% c("arima")) {
      return(as.vector(names(object.name$coef)))
    }
    else if (model.name %in% c("multinom")) {
      return(as.vector(object.name$coefnames))
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      return(as.vector(names(object.name$coefficients)))
    }
    
    
    return(NULL)
  }

.dependent.variable <-
  function(object.name, model.num=1, .summary.object) {
    
    model.name <- .get.model.name(object.name)
    if (model.name %in% c("lmer", "glmer", "nlmer", "gls")) {
      return(as.vector(as.character(formula(object.name))[2]))
    }
    if (model.name %in% c("Arima")) {
      return(as.character(object.name$call$x))
    }
    if (model.name %in% c("fGARCH")) {
      return(as.character(object.name@call$data))
    }
    if (model.name %in% c("multinom")) {
      if (!is.null(rownames(.summary.object$coefficients))) {
        return(as.vector(rownames(.summary.object$coefficients)[model.num]))
      }
    }
    if (model.name %in% c("rem.dyad", "coeftest")) {
      return(as.vector(as.character(" ")))
    }
    if (model.name %in% c("gmm")) {
      formula <- object.name$call[2]
      position <- regexpr("~", formula, fixed=T)
      return( trimws(substr(formula, 1, position-1)) )
    }
    if (model.name %in% c("selection","heckit")) {
      if (!gbl$sel.equation) {
        formula <- object.name$call["outcome"]    ### outcome
      }
      else {
        formula <- object.name$call["selection"]    ### outcome
      }
      position <- regexpr("~", formula, fixed=T)
      return( trimws(substr(formula, 1, position-1)))
    }
    if (model.name %in% c("probit.ss","binaryChoice")) {
      formula <- object.name$call["formula"]
      position <- regexpr("~", formula, fixed=T)
      return( trimws(substr(formula, 1, position-1)))
    }
    if (model.name %in% c("maBina")) {
      object.name <- object.name$w
    }
    
    if (model.name %in% c("lme")) {
      object.name$call$formula <- object.name$call$fixed
    }
    if (model.name %in% c("nlme")) {
      object.name$call$formula <- object.name$call$model
    }
    
    if (!is.null(object.name$call$formula)) {
      if (is.symbol(object.name$call$formula)) {
        formula.temp <- as.formula(object.name)  
      }
      else {
        formula.temp <- object.name$call$formula
      }
      
      if (length(as.vector(as.character(formula.temp)))>1) {
        return(as.vector(as.character(formula.temp)[2]))
      }
    }
    if (!is.null(object.name$formula)) {
      if (is.symbol(object.name$formula)) {
        formula.temp <- as.formula(object.name)  
      }
      else {
        formula.temp <- object.name$formula
      }
      
      if (length(as.vector(as.character(formula.temp)))>1) {   # this is for zelig$result ones
        return(as.vector(as.character(formula.temp)[2])) 
      }
    }
    if (!is.null(object.name$formula2)) {
      if (is.symbol(object.name$formula2)) {
        formula.temp <- as.formula(object.name)  
      }
      else {
        formula.temp <- object.name$formula2
      }
      
      if (length(as.vector(as.character(formula.temp)))>1) {   # z.ls
        return(as.vector(as.character(formula.temp)[2])) 
      }      
    }
    return("")  
  }

.dependent.variable.written <-
  function(object.name, model.num=1) {
    
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("tobit","ologit","oprobit", "relogit", "coxph","exp","lognorm","weibull","survreg()","arima",
                          "aftreg", "weibreg", "coxreg", "phreg", "bj", "cph", "psm")) {
      written.var <- .inside.bracket(.dependent.variable(object.name))[1] 
    }
    else if (model.name %in% c("clogit","mclogit")) {
      written.var <- .inside.bracket(.dependent.variable(object.name))[2] 
    }
    else { written.var <- .dependent.variable(object.name, model.num) }
    
    # some formatting changes
    # remove everything before and including he last dollar sign from variable name
    temp <- strsplit(written.var,"$",fixed=TRUE)
    written.var <- temp[[1]][length(temp[[1]])]
    
    # if underscore or ^, etc. in variable name, then insert an escape \ before it
    written.var <- .remove.special.chars(written.var)
    
    return(written.var)
  }

.enter.significance.stars <-
  function(p.value, force.math=FALSE, fmt) {
    if ((!is.na(p.value)) & (!is.null(p.value))) {
      
      if (fmt$dec.mark.align == TRUE) {
        c <- "" 
      }
      else {
        c <- "$"  
      }
      if (force.math == TRUE) { c <- "$" }
      
      cutoffs <- fmt$cutoffs[length(fmt$cutoffs):1]
      stars <- fmt$stars[length(fmt$stars):1]
      
      for (i in 1:length(cutoffs)) {
        if (!is.na(cutoffs[i])) {
          if (p.value < cutoffs[i]) {
            cat(c,"^{",stars[i],"}",c,sep="") 
            break
          }    
        }
      }
      
      
    }
    
  }
