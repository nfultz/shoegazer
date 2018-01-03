.turn.into.list <-
  function(x) {
    if (is.vector(x) | is.matrix(x)) {
      if (!is.list(x)) { return(as.list(x)) }
    }
    return(x)
  }

.is.list.numeric <- 
  function(x) {
    # tolerate NA or NULL
    if (is.null(x)) { return(TRUE) }
    if (!is.list(x)) { return(FALSE) }
    for (i in 1:length(x)) {
      elem <- x[[i]]
      if (!is.null(elem)) {
        if (length(elem) != length(elem[is.numeric(elem) | (is.na(elem))])) { return(FALSE) }
      }
    }
    return(TRUE)
  }

.is.list.numeric.matrix <- 
  function(x) {
    # tolerate NA or NULL
    if (is.null(x)) { return(TRUE) }
    if (!is.list(x)) { return(FALSE) }
    for (i in 1:length(x)) {
      elem <- as.matrix(x[[i]])
      if (!is.null(elem)) {
        if (length(elem) != length(elem[is.numeric(elem) | (is.na(elem))])) { return(FALSE) }
      }
    }
    return(TRUE)
  }

.get.file.extension <-
  function (path) {
    split <- strsplit(path, "\\.")[[1]]
    return( tolower(split[length(split)]) )
  }



# fill in NAs into a if b is the longer vector
.fill.NA <-
  function(a, b) {
    a.temp <- a; b.temp <- b
    if (length(a) >= length(b)) {
      return(a.temp)
    }
    else {
      length(a.temp) <- length(b)
      return(a.temp)
    }
  }


.iround <- 
  function(x, decimal.places=0, round.up.positive=FALSE, simply.output=FALSE) {
    
    x.original <- x
    first.part <- ""
    
    if (is.na(x) | is.null(x)) { return("") }
    
    if (simply.output == TRUE) {
      if (!is.numeric(x)) { return(.remove.special.chars(x)) }
    }
    
    if (x.original < 0) { x <- abs(x) }
    
    if (!is.na(decimal.places)) {
      
      if ((.format.until.nonzero.digit == FALSE) | (decimal.places <= 0)) {
        round.result <- round(x, digits=decimal.places)
      }
      else {
        temp.places <- decimal.places
        if (!.is.all.integers(x)) {
          while ((round(x, digits=temp.places) == 0) & (temp.places < (decimal.places + .format.max.extra.digits))) {
            temp.places <- temp.places + 1
          }
        }
        round.result <- round(x, digits=temp.places)
        decimal.places <- temp.places
      }
      
      if ((round.up.positive==TRUE) & (round.result < x)) {       # useful for p-values that should be rounded up
        if (x > (10^((-1)*(decimal.places+1)))) {
          round.result <- round.result + 10^((-1)*decimal.places)
        }
        else { round.result <- 0 }
      }
    }
    else {      # if the decimal place is NA
      round.result <- x
    }
    
    round.result.char <- as.character(format(round.result, scientific=FALSE))
    split.round.result <- unlist(strsplit(round.result.char, "\\."))
    
    ## first deal with digit separator
    
    for (i in seq(from=1, to=length(.format.digit.separator.where))) {
      if (.format.digit.separator.where[i]<=0) {
        .format.digit.separator.where[i] <<- -1
      }
    }
    
    separator.count <- 1
    length.integer.part <- nchar(split.round.result[1])
    
    digits.in.separated.unit <- 0
    for (i in seq(from=length.integer.part, to=1)) {
      if ((digits.in.separated.unit == .format.digit.separator.where[separator.count]) & (substr(split.round.result[1],i,i)!="-")){
        first.part <- paste(.format.digit.separator,first.part,sep="")
        if (separator.count < length(.format.digit.separator.where)) { separator.count <- separator.count + 1 }
        digits.in.separated.unit <- 0	
      }
      first.part <- paste(substr(split.round.result[1],i,i),first.part,sep="")
      digits.in.separated.unit <- digits.in.separated.unit + 1
      
    }	
    
    # remove initial zero and there are decimal places, if that is requested
    if (.format.initial.zero==FALSE)  {
      if ((round.result > 0) & (round.result < 1)) {
        if ((is.na(decimal.places)) | (decimal.places > 0)) {
          first.part <- ""
        }
      }
    }
    
    if (x.original < 0) {    # use math-mode for a better looking negative sign
      if (.format.dec.mark.align == TRUE) {
        first.part <- paste("-", first.part, sep="")
      }
      else {
        first.part <- paste("$-$", first.part, sep="")  
      }
    }
    
    # now deal with the decimal part
    if (!is.na(decimal.places)) {
      if (decimal.places <= 0) {
        return(first.part) 
      }
    }
    
    
    
    if (length(split.round.result)==2) {
      if (is.na(decimal.places)) { return(paste(first.part,.format.decimal.character,split.round.result[2],sep="")) }
      if (nchar(split.round.result[2]) < decimal.places) {
        decimal.part <- split.round.result[2]
        for (i in seq(from = 1,to = (decimal.places - nchar(split.round.result[2])))) {
          decimal.part <- paste(decimal.part,"0", sep="")
        }
        return(paste(first.part,.format.decimal.character,decimal.part,sep=""))
      }
      else { return(paste(first.part,.format.decimal.character,split.round.result[2],sep="")) }
    }
    else if (length(split.round.result)==1) { 
      if (is.na(decimal.places)) { return(paste(first.part,.format.decimal.character,decimal.part,sep="")) }
      decimal.part <- ""
      for (i in seq(from = 1,to = decimal.places)) {
        decimal.part <- paste(decimal.part,"0", sep="")
      }
      return(paste(first.part,.format.decimal.character,decimal.part,sep=""))
    }
    else { return(NULL) }
  }

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

.is.all.integers <-
  function(x) {
    if (!is.numeric(x)) { return(FALSE) }
    if (length(x[!is.na(x)]) == length(is.wholenumber(x)[(!is.na(x)) & (is.wholenumber(x)==TRUE)])) {
      return(TRUE)
    }
    else { return (FALSE) }
  }

.remove.special.chars <-
  function(s) {
    
    if (!is.character(s)) { s.out <- as.character(s) }
    else { s.out <- s }
    
    # this has to go first
    s.out <- gsub("\\","\\textbackslash ",s.out,fixed=TRUE)
    
    # basic special characters
    s.out <- gsub("_","\\_",s.out,fixed=TRUE)
    s.out <- gsub("#","\\#",s.out,fixed=TRUE)
    s.out <- gsub("~","\\textasciitilde",s.out,fixed=TRUE)
    s.out <- gsub("{","\\{",s.out,fixed=TRUE)
    s.out <- gsub("}","\\}",s.out,fixed=TRUE)    
    s.out <- gsub("%","\\%",s.out,fixed=TRUE)
    s.out <- gsub("$","\\$",s.out,fixed=TRUE)
    
    # pre-defined text-mode commands (add more?)
    s.out <- gsub("*","\\textasteriskcentered ",s.out,fixed=TRUE)
    s.out <- gsub("|","\\textbar ",s.out,fixed=TRUE)
    s.out <- gsub(">","\\textgreater ",s.out,fixed=TRUE)
    s.out <- gsub("<","\\textless ",s.out,fixed=TRUE)
    
    # more substitutions
    s.out <- gsub("^","$\\hat{\\mkern6mu}$",s.out,fixed=TRUE)
    
    return(s.out)
  }


.roman.numeral <-
  function(regular.number) {
    
    # unique representation only for integers between 1 and 3899
    if ((regular.number < 1) | (regular.number > 3899)) {
      return(NULL)
    }
    else {
      roman.output <- ""
      number.remaining <- regular.number
      
      while (number.remaining > 999) {
        roman.output <- paste(roman.output, "M", sep="")
        number.remaining <- number.remaining - 1000
      }
      
      if (number.remaining > 899) {
        roman.output <- paste(roman.output, "CM", sep="")
        number.remaining <- number.remaining - 900
      }
      
      if (number.remaining > 499) {
        roman.output <- paste(roman.output, "D", sep="")
        number.remaining <- number.remaining - 500
      }
      
      if (number.remaining > 399) {
        roman.output <- paste(roman.output, "CD", sep="")
        number.remaining <- number.remaining - 400
      }
      
      if (number.remaining > 399) {
        roman.output <- paste(roman.output, "D", sep="")
        number.remaining <- number.remaining - 400
      }
      
      while (number.remaining > 99) {
        roman.output <- paste(roman.output, "C", sep="")
        number.remaining <- number.remaining - 100
      }
      
      if (number.remaining > 89) {
        roman.output <- paste(roman.output, "XC", sep="")
        number.remaining <- number.remaining - 90
      }
      
      if (number.remaining > 49) {
        roman.output <- paste(roman.output, "L", sep="")
        number.remaining <- number.remaining - 50
      }
      
      if (number.remaining > 39) {
        roman.output <- paste(roman.output, "XL", sep="")
        number.remaining <- number.remaining - 40
      }
      
      while (number.remaining > 9) {
        roman.output <- paste(roman.output, "X", sep="")
        number.remaining <- number.remaining - 10
      }
      
      if (number.remaining > 8) {
        roman.output <- paste(roman.output, "IX", sep="")
        number.remaining <- number.remaining - 9
      }
      
      if (number.remaining > 4) {
        roman.output <- paste(roman.output, "V", sep="")
        number.remaining <- number.remaining - 5
      }
      
      if (number.remaining > 3) {
        roman.output <- paste(roman.output, "IV", sep="")
        number.remaining <- number.remaining - 4
      }
      
      if (number.remaining > 3) {
        roman.output <- paste(roman.output, "IV", sep="")
        number.remaining <- number.remaining - 4
      }
      
      while (number.remaining > 0) {
        roman.output <- paste(roman.output, "I", sep="")
        number.remaining <- number.remaining - 1
      }
      
      return(roman.output)
    }
  }



.apply <-
  function(auto.t, auto.p)
  {
    if ((!is.null(apply.coef)) | ((!is.null(apply.se)))) {
      if (!is.null(apply.coef)) { .global.coefficients <<- apply(.global.coefficients, c(1,2), apply.coef) }
      if (!is.null(apply.se)) { .global.std.errors <<- apply(.global.std.errors, c(1,2), apply.se) }
      
      if (auto.t == TRUE) { .global.t.stats <<- .global.coefficients / .global.std.errors }
      if (auto.p == TRUE) { .global.p.values <<- 2 * pnorm( abs( .global.t.stats ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE) }
      
    }
    
    if (!is.null(apply.t)) { .global.t.stats <<- apply(.global.t.stats, c(1,2), apply.t) }
    if (!is.null(apply.p)) { .global.p.values <<- apply(.global.p.values, c(1,2), apply.p) }
  }

.inside.bracket <-
  function(s) {
    process.string <- ""
    return.vector <- NULL
    
    if (!is.character(s)) { return("") }
    if (is.null(s)) { return("") }
    if (is.na(s)) { return("") }
    if (s=="") { return("") }
    if (length(s) > 1) { return("") }
    
    inside.inner.bracket <- 0
    for (i in seq(from = (regexpr("(",s,fixed=TRUE)[1])+1, to = nchar(s))) {
      letter <- substr(s,i,i)
      if (letter == "(") { inside.inner.bracket <- inside.inner.bracket + 1 }
      if (letter == ")") { inside.inner.bracket <- inside.inner.bracket - 1 }
      
      if ((letter == ",") & (inside.inner.bracket == 0)) {
        return.vector <- c(return.vector, process.string)
        process.string <- ""
      }
      else if (inside.inner.bracket >= 0) { process.string <- paste(process.string, letter, sep="") }
      else { break } 
    }
    if (process.string != "") { return.vector <- c(return.vector, process.string) }
    return (trimws(return.vector))
  }

.rename.intercept <-
  function(x) {
    out <- x
    for (i in seq(1:length(x))) {
      if (x[i] %in% .global.intercept.strings) { 
        out[i] <- .format.intercept.name
      }
    }
    return(out)
  }

.order.reg.table <- 
  function(order) {
    
    # first, find the position of the intercept and rename the variable to be the intercept string
    intercept.position <- NULL
    for (i in seq(1:length(.global.coefficient.variables))) {
      if (.global.coefficient.variables[i] %in% .global.intercept.strings) { 
        intercept.position <- i 
        
        .global.coefficient.variables[i] <<- .format.intercept.name   
        rownames(.global.coefficients)[i] <<- .format.intercept.name
        rownames(.global.std.errors)[i] <<- .format.intercept.name
        rownames(.global.ci.lb)[i] <<- .format.intercept.name
        rownames(.global.ci.rb)[i] <<- .format.intercept.name
        rownames(.global.t.stats)[i] <<- .format.intercept.name
        rownames(.global.p.values)[i] <<- .format.intercept.name
      }
    }
    
    # put intercept on bottom if necessary
    if (!is.null(intercept.position)) {
      # hold contents of last row in placeholder variables
      placehold.coefficient.variables <- .global.coefficient.variables[-intercept.position]
      intercept.coefficient.variables <- .global.coefficient.variables[intercept.position]
      
      if (.format.intercept.bottom) {
        .global.coefficient.variables <<- c(placehold.coefficient.variables, intercept.coefficient.variables)
      }
      
      if (.format.intercept.top) {
        .global.coefficient.variables <<- c(intercept.coefficient.variables, placehold.coefficient.variables)
      }
    } 
    
    
    # order according to user's wishes
    old.order <- 1:length(.global.coefficient.variables)
    new.order <- NULL; add.these <- NULL
    
    if (!is.null(order)) {
      # if order is regular expression...
      if (is.character(order)) {
        not.ordered.yet <- .global.coefficient.variables
        
        for (i in 1:length(order)) {
          add.these <- grep(order[i], not.ordered.yet, perl=.format.perl, fixed=FALSE)
          not.ordered.yet[add.these] <- NA
          if (length(add.these) != 0) {
            new.order <- c(new.order, add.these)
          }
        }
      }
      else if (is.numeric(order)) { # if order contains indices
        order <- unique(order)
        order <- order[order <= max(old.order)]
        new.order <- old.order[order]
      }
    }
    
    if (!is.null(new.order)) {
      remainder <- old.order[-new.order]
      new.order <- c(new.order, remainder)
    }
    else { new.order <- old.order }
    
    # set the right order
    .global.coefficient.variables[old.order] <<- .global.coefficient.variables[new.order]
  }

.insert.col.front <- function(d, new.col) {
  # values
  d.new <- d
  d.new[,seq(2,ncol(d)+1)] <- d[,seq(1,ncol(d))]
  d.new[,1] <- new.col
  
  # column names
  if (!is.null(colnames(d))) { 
    colnames(d.new)[seq(2,ncol(d)+1)] <- colnames(d)[seq(1,ncol(d))] 
    colnames(d.new)[1] <- ""
  }
  
  return(d.new)
}

.order.data.frame <- 
  function(d, order, summary=FALSE) {
    
    if ((.format.rownames == TRUE) & (summary == FALSE)) {  # if we want to report rownames, add them to data frame
      if (!is.null(rownames(d))) { d <- .insert.col.front(d, rownames(d)) }
    }
    
    # order according to user's wishes
    old.order <- 1:length(colnames(d))
    new.order <- NULL; add.these <- NULL
    
    if (!is.null(order)) {
      # if order is regular expression...
      if (is.character(order)) {
        not.ordered.yet <- colnames(d)
        
        for (i in 1:length(order)) {
          add.these <- grep(order[i], d, perl=.format.perl, fixed=FALSE)
          not.ordered.yet[add.these] <- NA
          if (length(add.these) != 0) {
            new.order <- c(new.order, add.these)
          }
        }
      }
      else if (is.numeric(order)) { # if order contains indices
        order <- unique(order)
        order <- order[order <= max(old.order)]
        new.order <- old.order[order]
      }
    }
    
    if (!is.null(new.order)) {
      remainder <- old.order[-new.order]
      new.order <- c(new.order, remainder)
    }
    else { new.order <- old.order }
    
    return( d[new.order] )
  }


.print.additional.lines <-
  function(part.number=NULL) {
    
    # if no additional lines, then quit the function
    if (is.null(.format.add.lines)) { return(NULL) }
    
    max.l <- length(.global.models)+1
    for (line in 1:length(.format.add.lines)) {
      ## add columns if too few, remove if too many
      if (max.l > length(.format.add.lines[[line]])) {
        .format.add.lines[[line]] <- c(.format.add.lines[[line]], rep(NA, times=max.l - length(.format.add.lines[[line]])))		
      }
      else if (max.l < length(.format.add.lines[[line]])) {
        .format.add.lines[[line]] <- .format.add.lines[[line]][1:max.l]
      }
      
      .format.add.lines[[line]] <- .format.add.lines[[line]]
      
      ## print each line
      for (i in 1:max.l) {
        if (!is.na(.format.add.lines[[line]][i])) { 
          if (i==1) {
            cat(.format.add.lines[[line]][i], sep="") 
          }
          else {
            cat(" & ",.format.add.lines[[line]][i], sep="") 
          }
        }
        else { 
          if (i==1) {
            cat("   ", sep="") 
          }
          else {
            cat(" & ", sep="") 
          }
        }
      }
      cat(" \\\\ \n")
    }
    .table.part.published[part.number] <<- TRUE
  }


replace.dec.mark <- function(s) { return (gsub(".", .format.decimal.character, s, fixed=TRUE))}
