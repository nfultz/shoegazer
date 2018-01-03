.get.model.name <-
  function(object.name) {
    return.value <- .model.identify(object.name)
    if (substr(return.value,1,5)=="glm()") { return.value <- "glm()" }
    if (substr(return.value,1,8)=="svyglm()") { return.value <- "svyglm()" }
    if (substr(return.value,1,5)=="gee()") { return.value <- "gee()" }
    if (substr(return.value,1,5)=="gam()") { return.value <- "gam()" }
    if (substr(return.value,1,6)=="polr()") { return.value <- "polr()" }
    if (substr(return.value,1,9)=="survreg()") { return.value <- "survreg()" }
    return(return.value)
  }


.model.identify <-
  function(object.name) {
    
    if (class(object.name)[1]=="NULL") {   #### !!!!! continue this
      return("NULL")
    }
    
    if (class(object.name)[1]=="Arima") {
      return("Arima")
    }
    
    if (class(object.name)[1]=="fGARCH") {
      return("fGARCH")
    }
    
    if (class(object.name)[1]=="censReg") {
      return("censReg")
    }
    
    if (class(object.name)[1]=="ergm") {
      return("ergm")
    }
    
    if (class(object.name)[1]=="mnlogit") {
      return("mnlogit")
    }
    
    if (class(object.name)[1]=="lme") {
      return("lme")
    }
    
    if (class(object.name)[1]=="nlme") {
      return("nlme")
    }
    
    if (class(object.name)[1]=="felm") {
      return("felm")
    }
    if (class(object.name)[1] %in% c("mclogit","mclogitRandeff")) {
      return("mclogit")
    }
    if (class(object.name)[1]=="mlogit") {
      return("mlogit")
    }
    if (class(object.name)[1]=="maBina") {
      return("maBina")
    }
    if (class(object.name)[1]=="coeftest") {
      return("coeftest")
    }
    if (class(object.name)[1]=="rem.dyad") {
      return("rem.dyad")
    }
    if (class(object.name)[1]=="lmerMod") {
      return("lmer")
    }
    if (class(object.name)[1]=="glmerMod") {
      return("glmer")
    }
    if (class(object.name)[1]=="nlmerMod") {
      return("nlmer")
    }
    
    if (!is.null(object.name$call)) {
      
      if (object.name$call[1]=="lm()") { return("ls") }
      else if ((object.name$call[1]=="glm()") | (object.name$call[1]=="Glm()")) {
        if (object.name$family$family=="gaussian") {
          if (object.name$family$link=="identity") {
            return("normal")
          }
        }
        else if (object.name$family$family=="binomial") {
          if (object.name$family$link=="probit") {
            return("probit")
          }
          if (object.name$family$link=="logit") {
            return("logit")
          }
          
        }
        else if (object.name$family$family=="poisson") {
          if (object.name$family$link=="log") {
            return("poisson")
          }
        }
        else if (object.name$family$family=="Gamma") {
          if (object.name$family$link=="inverse") {
            return("gamma")
          }
        }
        return(paste("glm()#",object.name$family$family,"#",object.name$family$link, sep=""))
      }
      
      else if (object.name$call[1]=="svyglm()") {
        if (object.name$family$family=="gaussian") {
          if (object.name$family$link=="identity") {
            return("normal.survey")
          }
        }
        else if ((object.name$family$family=="binomial") | (object.name$family$family=="quasibinomial")) {
          if (object.name$family$link=="probit") {
            return("probit.survey")
          }
          if (object.name$family$link=="logit") {
            return("logit.survey")
          }
          
        }
        else if (object.name$family$family=="poisson") {
          if (object.name$family$link=="log") {
            return("poisson.survey")
          }
        }
        else if (object.name$family$family=="Gamma") {
          if (object.name$family$link=="inverse") {
            return("gamma.survey")
          }
        }
        return(paste("svyglm()#",object.name$family$family,"#",object.name$family$link, sep=""))
      }
      
      else if (object.name$call[1]=="gam()") {
        if (object.name$family$family=="gaussian") {
          if (object.name$family$link=="identity") {
            return("normal.gam")
          }
        }
        else if (object.name$family$family=="binomial")  {
          if (object.name$family$link=="probit") {
            return("probit.gam")
          }
          if (object.name$family$link=="logit") {
            return("logit.gam")
          }
          
        }
        else if (object.name$family$family=="poisson") {
          if (object.name$family$link=="log") {
            return("poisson.gam")
          }
        }
        else if (object.name$family$family=="Gamma") {
          if (object.name$family$link=="inverse") {
            return("gamma.gam")
          }
        }
        return(paste("gam()#",object.name$family$family,"#",object.name$family$link, sep=""))
      }
      
      else if (object.name$call[1]=="polr()") {
        if (object.name$method=="logistic") {
          return("ologit")
        }
        else if (object.name$method=="probit") {
          return("oprobit")
        }
        return(paste("polr()#",object.name$method, sep=""))
      }
      
      
      else if (object.name$call[1]=="gee()") {
        if (object.name$family$family=="gaussian") {
          if (object.name$family$link=="identity") {
            return("normal.gee")
          }
        }
        else if (object.name$family$family=="binomial") {
          if (object.name$family$link=="probit") {
            return("probit.gee")
          }
          if (object.name$family$link=="logit") {
            return("logit.gee")
          }
          
        }
        else if (object.name$family$family=="poisson") {
          if (object.name$family$link=="log") {
            return("poisson.gee")
          }
        }
        else if (object.name$family$family=="Gamma") {
          if (object.name$family$link=="inverse") {
            return("gamma.gee")
          }
        }
        return(paste("gee()#",object.name$family$family,"#",object.name$family$link, sep=""))
      }
      
      else if (object.name$call[1]=="survreg()") {
        if (object.name$dist=="exponential") {
          return("exp")
        }
        else if (object.name$dist=="weibull") {
          return("weibull")
        }
        else if (object.name$dist=="lognorm") {
          return("lognormal")
        }
        else if (object.name$dist=="gaussian") {
          return("tobit")
        }
        return(paste("survreg()#",object.name$dist, sep=""))
      }
      
      else if (object.name$call[1]=="glm.nb()") {
        return("negbin")
      }
      else if (object.name$call[1]=="\"glm.nb\"()") {
        return("negbin")
      }
      
      if (!is.null(object.name$userCall)) {
        if (object.name$userCall[1]=="clogit()") {
          return("clogit")
        }
      }
      
      if (object.name$call[1]=="coxph()") {
        return("coxph")
      }
      if (object.name$call[1]=="pmg()") {
        return("pmg")
      }
      if (object.name$call[1]=="selection()") {
        return("selection")
      }
      if (object.name$call[1]=="heckit()") {
        return("heckit")
      }
      if (object.name$call[1]=="probit()") {
        return("probit.ss")
      }
      if (object.name$call[1]=="binaryChoice()") {
        return("binaryChoice")
      }
      if (object.name$call[1]=="brglm()") {
        return("brglm")
      }
      if (object.name$call[1]=="gls()") {
        return("gls")
      }
      if (object.name$call[1]=="clm()") {
        return("clm")
      }
      if (object.name$call[1]=="lmrob()") {
        return("lmrob")
      }
      if (object.name$call[1]=="glmrob()") {
        return("glmrob")
      }
      if (object.name$call[1]=="dynlm()") {
        return("dynlm")
      }
      if (object.name$call[1]=="rq()") {
        return("rq")
      }
      if (object.name$call[1]=="gmm()") {
        return("gmm")
      }
      if (object.name$call[1]=="lagsarlm()") {
        return("lagsarlm")
      }
      if (object.name$call[1]=="errorsarlm()") {
        return("errorsarlm")
      }
      if (object.name$call[1]=="rlm()") {
        return("rlm")
      }
      if (object.name$call[1]=="aftreg()") {
        return("aftreg")
      }
      if (object.name$call[1]=="coxreg()") {
        return("coxreg")
      }
      if (object.name$call[1]=="phreg()") {
        return("phreg")
      }
      if (object.name$call[1]=="weibreg()") {
        return("weibreg")
      }
      if (object.name$call[1]=="bj()") {
        return("bj")
      }
      if (object.name$call[1]=="cph()") {
        return("cph")
      }
      if (object.name$call[1]=="Gls()") {
        return("Gls")
      }
      if (object.name$call[1]=="lrm()") {
        return("lrm")
      }
      if (object.name$call[1]=="ols()") {
        return("ols")
      }
      if (object.name$call[1]=="psm()") {
        return("psm")
      }
      if (object.name$call[1]=="Rq()") {
        return("Rq")
      }
      if (object.name$call[1]=="hetglm()") {
        return("hetglm")
      }
      else if (object.name$call[1]=="relogit()") {
        return("relogit")
      }
      else if (object.name$call[1]=="netbinom()") {
        if (object.name$call$LF=="probit") { return("probit.net") }      
        if (object.name$call$LF=="logit") { return("logit.net") }
        if (object.name$call$LF=="cloglog") { return("cloglog.net") }
      }
      else if (object.name$call[1]=="netgamma()") {
        return("gamma.net")
      }
      
      else if (object.name$call[1]=="zelig()") {
        if (object.name$call$model %in% c("ls","normal","logit","probit","relogit","poisson","poisson.survey",
                                          "negbinom","probit.survey","logit.survey","normal.gee","logit.gee","probit.gee",
                                          "poisson.gee","normal.gam","logit.gam","probit.gam","poisson.gam","exp",
                                          "coxph","weibull","lognorm","normal.survey","gamma","gamma.survey",
                                          "gamma.gee","cloglog.net","logit.net","probit.net","gamma.net","ologit",
                                          "oprobit","arima","tobit")) {
          return(object.name$call$model)
        }
        else { return("unsupported zelig") }
      }
      
      else if (object.name$call[1]=="tobit()") {
        return("tobit(AER)")
      }
      
      else if (object.name$call[1]=="multinom()") {
        return("multinom")
      }
      
      else if (object.name$call[1]=="betareg()") {
        return("betareg")
      }
      else if (object.name$call[1]=="zeroinfl()") {
        return("zeroinfl")
      }
      else if (object.name$call[1]=="hurdle()") {
        return("hurdle")
      }  	
      else if (object.name$call[1]=="plm()") {
        return("plm")
      }
      else if (object.name$call[1]=="pgmm()") {
        return("pgmm")
      }  	
      else if (object.name$call[1]=="ivreg()") {
        return("ivreg")
      } 
    }
    
    return("unknown")
    
  }
