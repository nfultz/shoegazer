

.split.line <-    # split line of a LaTeX table into constituent parts separated by &
  function(s) {
    # remove the "\\\\"
    s <- gsub("\\\\", "", s, fixed=TRUE)
    s <- paste("  ",s,"  ", sep="")
    
    return(.trim(strsplit(s, " &", fixed=TRUE)[[1]]))
  }

.remove.extra.spaces <-
  function(s) {
    new.s <- ""
    space <- FALSE
    for (i in 1:nchar(s)) {
      s.i <- substr(s,i,i)
      if (s.i == " ") {
        if (space == FALSE) { 
          space <- TRUE 
          new.s <- paste(new.s, s.i, sep="")
        }
      }
      else {
        space <- FALSE
        new.s <- paste(new.s, s.i, sep="")
      }
    }
    return(new.s)
  }

strpos <-
  function(x, s) {
    return( regexpr(x, s, fixed=TRUE)[1] )
  }

is.alphanumeric <- 
  function(s) {
    alphanum <- FALSE
    
    numbers <- grepl("^[[:digit:]]+$", s) 
    letters <- grepl("^[[:alpha:]]+$", s) 
    both <- grepl("^[[:digit:][:alpha:]]+$", s)
    
    if ((numbers == TRUE) | (letters == TRUE) | (both == TRUE)) {
      alphanum <- TRUE
    }
    
    return(alphanum)
  }

.replace.latex.symbols <-
  function (s) {
    latex.replace <- NULL
    latex.replace <- cbind(latex.replace, c("\\textbackslash","\\"), c("\\_","_"), c("\\#","#"), c("\\textasciitilde","~"), c("\\{","{"), c("\\}","}"), c("\\%","%"))
    latex.replace <- cbind(latex.replace, c("\\textasteriskcentered","*"), c("\\textbar","|"), c("\\textgreater",">"), c("\\textless","<"), c("$\\hat{\\mkern6mu}$","^"))
    
    # Greek letters
    latex.replace <- cbind(latex.replace, c("\\alpha","alpha"), c("\\beta","beta"), c("\\gamma","gamma"), c("\\delta","delta"), c("\\epsilon","epsilon"), c("\\varepsilon","epsilon"), c("\\zeta","zeta"))
    latex.replace <- cbind(latex.replace, c("\\eta","eta"), c("\\theta","theta"), c("\\vartheta","theta"), c("\\iota","iota"), c("\\kappa","kappa"), c("\\lambda","lambda"), c("\\mu","mu"))
    latex.replace <- cbind(latex.replace, c("\\nu","nu"), c("\\xi","xi"), c("\\pi","pi"), c("\\varpi","pi"), c("\\rho","rho"), c("\\varrho","rho"), c("\\sigma","sigma"))
    latex.replace <- cbind(latex.replace, c("\\varsigma","sigma"), c("\\tau","tau"), c("\\upsilon","upsilon"), c("\\phi","phi"), c("\\varphi","phi"), c("\\chi","chi"), c("\\psi","psi"))
    latex.replace <- cbind(latex.replace, c("\\omega","omega"), c("\\Gamma","gamma"), c("\\Delta","delta"), c("\\Theta","theta"), c("\\Lambda","lambda"), c("\\Xi","xi"), c("\\Pi","pi"))
    latex.replace <- cbind(latex.replace, c("\\Sigma","sigma"), c("\\Upsilon","upsilon"), c("\\Phi","phi"), c("\\Psi","psi"), c("\\Omega","omega"))
    
    s.out <- s
    for (item in 1:ncol(latex.replace)) {
      symbol <- latex.replace[1, item]
      replacement <- latex.replace[2, item]
      
      # quick check if any latex characters
      symbol.regexp <- gsub("\\","\\\\",symbol,fixed=TRUE)
      symbol.regexp <- gsub("{","\\{",symbol.regexp,fixed=TRUE)
      symbol.regexp <- gsub("}","\\}",symbol.regexp,fixed=TRUE)
      symbol.regexp <- gsub("$","\\$",symbol.regexp,fixed=TRUE)
      symbol.regexp <- paste(symbol.regexp, "[^[:alnum:]_]+", sep="")
      
      pos <- 1
      while (pos <= nchar(s.out)) {
        
        if (length(grep(symbol.regexp, s.out))==0) { break }
        
        s.pre <- substr(s.out, 1, pos-1)
        s.pos.char <- substr(s.out, pos, pos)
        s.post <- substr(s.out, pos + nchar(symbol), nchar(s.out))
        if (substr(s.out, pos, pos+nchar(symbol)-1) == symbol) {
          if (!is.alphanumeric(substr(s.post, 1, 1))) {
            s.out <- paste(s.pre, replacement, s.post, sep="")
            post <- pos + nchar(replacement) - 1
          }
        }
        pos <- pos + 1
      }
    }
    
    return(s.out)
  }

.remove.control.sequences <-
  function (s, type="text") {
    
    s <- paste("  ",s, "  ", sep="")
    
    # replace latex symbols
    s <- .replace.latex.symbols(s)
    
    # remove dollar signs and underscores  [ what about text-related starts ]
    s <- gsub("\\$", "", s)
    
    # remove extra spaces
    s <- .remove.extra.spaces(s)
    
    # add: replace some sequences with corresponding letters
    
    # walk through the string
    i <- 1
    new.s <- ""
    control.sequence <- ""
    while (i <= nchar(s)) {
      s.i0 <- substr(s, i-1, i)
      s.i <- substr(s, i, i)
      s.i2 <- substr(s, i, i+1)
      
      if ((s.i %in% c("\\", "_", "^")) & (!(s.i2 %in% c("\\_","\\^"))) & (!(s.i0 %in% c("\\_","\\^"))) ) {
        remainder.s <- substr(s, i+1, nchar(s))     # if control character not followed by curly brace
        if ((strpos(" ", remainder.s) < strpos("{", remainder.s)) | (strpos("{", remainder.s)==-1))  {
          i <- i + strpos(" ", remainder.s) + 1
        }
        else {   # control character followed by curly brace
          control.sequence <- substr(s, i, i+strpos("{", remainder.s)-1)
          
          if (type=="html") {
            if (control.sequence == "\\textit") { new.s <- paste(new.s,"<em>",sep="") }
            if (control.sequence == "\\textbf") { new.s <- paste(new.s,"<strong>",sep="") }
            if (control.sequence == "_") { new.s <- paste(new.s,"<sub>",sep="") }
            if (control.sequence == "^") { new.s <- paste(new.s,"<sup>",sep="") }
          }
          if (type=="mmd") {
            if (control.sequence == "\\textit") { new.s <- paste(new.s,"*",sep="") }
            if (control.sequence == "\\textbf") { new.s <- paste(new.s,"**",sep="") }
            if (control.sequence == "~") { new.s <- paste(new.s,"~",sep="") }
            if (control.sequence == "^") { new.s <- paste(new.s,"^",sep="") }
          }
          
          s.sub <- substr(remainder.s, strpos("{", remainder.s), nchar(remainder.s))
          open.brackets <- 0
          bracket.start <- bracket.end <- strpos("{", s.sub)
          
          for (j in 1:nchar(s.sub)) {
            s.sub.j <- substr(s.sub, j, j)
            if (s.sub.j == "{") { 
              open.brackets <- open.brackets + 1
              if (open.brackets == 1) { bracket.start <- j + 1 }
            }
            if (s.sub.j == "}") { 
              open.brackets <- open.brackets - 1 
              if (open.brackets == 0) { bracket.end <- j - 1 }
            }
            if (!(s.sub.j %in% c("{","}"))) {
              if (open.brackets == 0) { break }
            }
          }
          if (bracket.end < bracket.start) { 
            examine.substring <- "" 
          } 
          else {
            examine.substring <- substr(s.sub, bracket.start, bracket.end)
          }
          new.s <- paste(new.s, .remove.control.sequences(examine.substring, type=type), sep="")
          
          if (type=="html") {
            if (control.sequence == "\\textit") { new.s <- paste(new.s,"</em>",sep="") }
            if (control.sequence == "\\textbf") { new.s <- paste(new.s,"</strong>",sep="") }
            if (control.sequence == "_") { new.s <- paste(new.s,"</sub>",sep="") }
            if (control.sequence == "^") { new.s <- paste(new.s,"</sup>",sep="") }
          }
          if (type=="mmd") {
            if (control.sequence == "\\textit") { new.s <- paste(new.s,"*",sep="") }
            if (control.sequence == "\\textbf") { new.s <- paste(new.s,"**",sep="") }
            if (control.sequence == "~") { new.s <- paste(new.s,"~",sep="") }
            if (control.sequence == "^") { new.s <- paste(new.s,"^",sep="") }
          }
          
          i <- i + strpos("{", remainder.s) + bracket.end + 1
          
        }
      }  
      else {  # not inside a control sequence
        new.s <- paste(new.s, s.i, sep="")
        i <- i + 1
      }
      
    }
    
    # replace underscores, etc.
    new.s <- gsub("\\_", "_", new.s, fixed=T)
    new.s <- gsub("\\^", "^", new.s, fixed=T)
    
    return(.trim(new.s))
    
  }

.text.cline <-
  function (cline, max.length, line.char="-") {
    for (i in 1:length(cline)) {
      if ((cline[i]==0) & (sum(cline[i:length(cline)]) != 0)) {
        .repeat.char(" ", rep=max.length[i]+1, new.line=FALSE)
      }
      else if (cline[i]>=1) {
        underline.len <- 0
        for (j in i:(i+cline[i]-1)) {
          underline.len <- underline.len + max.length[j] + 1
        }
        underline.len <- underline.len - 1
        .repeat.char(line.char, rep=underline.len, new.line=FALSE)
        if ((sum(cline[i:length(cline)]) != cline[i])) { cat(" ") }
      }
    }
    cat("\n")
  }

.html.cline <-
  function (cline) {
    cat("<tr>")
    for (i in 1:length(cline)) {
      if ((cline[i]==0) & (sum(cline[i:length(cline)]) != 0)) {
        cat("<td></td>")
      }
      else if (cline[i]>=1) {
        cat("<td colspan=\"",cline[i],"\" style=\"border-bottom: 1px solid black\"></td>",sep="")
      }
    }
    cat("</tr>\n")
  }

.mmd.cline <-
  function (cline) {
    # no support for cline in MMD as far as I am aware
  }

.text.horizontal.line <-
  function (line.char="-", max.length) {
    horizontal.length <- 0
    for (i in 1:length(max.length)) {
      horizontal.length <- horizontal.length + max.length[i] + 1
    }
    horizontal.length = horizontal.length - 1
    
    
    .repeat.char(line.char, rep=horizontal.length, new.line=TRUE)
  }

.html.horizontal.line <-
  function (how.many.columns) {
    cat("<tr><td colspan=\"",how.many.columns,"\" style=\"border-bottom: 1px solid black\"></td></tr>",sep="")
  }

.mmd.horizontal.line <-
  function (how.many.columns) {
    # no support for hline in MMD as far as I am aware
  }

.text.output <-
  function(all.latex.code) {
    
    how.many.tables <- 0
    start.lines <- NULL
    for (i in 1:length(all.latex.code)) {
      if (all.latex.code[i] %in% c("")) { 
        how.many.tables <- how.many.tables + 1
        start.lines <- c(start.lines, i)
      }
    }
    
    
    for (table.number in 1:how.many.tables) {
      
      if (table.number < how.many.tables) {
        latex.code <- all.latex.code[start.lines[table.number]:start.lines[table.number+1]]
      }
      else {
        latex.code <- all.latex.code[start.lines[table.number]:length(all.latex.code)]
      }
      
      how.many.columns <- .get.number.of.columns(latex.code)
      
      r <- 0
      
      matrices <- .matrices(latex.code, how.many.columns)
      t <- matrices[[1]]
      c <- matrices[[2]]
      j <- matrices[[3]]
      
      max.l <- .text.column.width(t, c)
      w <- .width.matrix(c, max.l)
      
      cat("\n")
      
      
      for (row in 1:length(latex.code)) {
        line <- latex.code[row]
        if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") {
          r <- r + 1
          .text.output.line(t, r, w, c, j)
        }
        else if (strpos("\\caption{", line) != -1) {
          inside.caption <- substr(.trim(line), 10, nchar(.trim(line))-1)
          text.title <- .trim(.remove.control.sequences(inside.caption))
          if (text.title != "") { cat(.remove.control.sequences(inside.caption),"\n", sep="") }
        }
        else if (strpos("\\cline{", line) != -1) {
          s <- paste("  ", line, "  ", sep="")
          cline <- rep(0, times=how.many.columns)
          while (strpos("\\cline{", s) != -1) {
            from <- strpos("\\cline{", s) + 7
            to <- strpos("}", s) - 1
            
            underline.columns <- substr(s, from, to)
            split.columns <- strsplit(underline.columns,"-", fixed=TRUE)[[1]]
            
            col.underline.begin <- as.numeric(split.columns[1])
            col.underline.number <- as.numeric(split.columns[2]) - col.underline.begin + 1
            
            cline[col.underline.begin] <- col.underline.number
            
            s <- substr(s, to+1, nchar(s))
            .text.cline(cline, max.l)
          }
        }
        else if (strpos("\\hline",line) != -1) {
          if (!(is.na(latex.code[row+1]))) {
            if (strpos("\\hline", latex.code[row+1]) != -1) {
              .text.horizontal.line("=", max.l)
            } 
            else {
              if (strpos("\\hline", latex.code[row-1]) == -1) {           
                .text.horizontal.line("-", max.l) 
              }
            }
          }
          else {
            if (strpos("\\hline", latex.code[row-1]) == -1) {           
              .text.horizontal.line("-", max.l) 
            }
          }
        }
      }
    }
  }

.html.output <-
  function(all.latex.code) {
    
    how.many.tables <- 0
    start.lines <- NULL
    for (i in 1:length(all.latex.code)) {
      if (all.latex.code[i] %in% c("")) { 
        how.many.tables <- how.many.tables + 1
        start.lines <- c(start.lines, i)
      }
    }
    
    
    for (table.number in 1:how.many.tables) {
      
      if (table.number < how.many.tables) {
        latex.code <- all.latex.code[start.lines[table.number]:start.lines[table.number+1]]
      }
      else {
        latex.code <- all.latex.code[start.lines[table.number]:length(all.latex.code)]
      }
      
      how.many.columns <- .get.number.of.columns(latex.code)
      
      r <- 0
      
      matrices <- .matrices(latex.code, how.many.columns, type="html")
      t <- matrices[[1]]
      c <- matrices[[2]]
      j <- matrices[[3]]
      
      max.l <- .text.column.width(t, c)
      w <- .width.matrix(c, max.l)
      
      cat("\n")
      cat("<table style=\"text-align:center\">")
      
      
      for (row in 1:length(latex.code)) {
        line <- latex.code[row]
        if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") {
          r <- r + 1
          .html.output.line(t, r, w, c, j)
        }
        else if (strpos("\\caption{", line) != -1) {
          inside.caption <- substr(.trim(line), 10, nchar(.trim(line))-1)
          text.title <- .trim(.remove.control.sequences(inside.caption, type="html"))
          if (text.title != "") { cat("<caption><strong>",.remove.control.sequences(inside.caption, type="html"),"</strong></caption>\n", sep="") }
        }
        else if (strpos("\\cline{", line) != -1) {
          s <- paste("  ", line, "  ", sep="")
          cline <- rep(0, times=how.many.columns)
          while (strpos("\\cline{", s) != -1) {
            from <- strpos("\\cline{", s) + 7
            to <- strpos("}", s) - 1
            
            underline.columns <- substr(s, from, to)
            split.columns <- strsplit(underline.columns,"-", fixed=TRUE)[[1]]
            
            col.underline.begin <- as.numeric(split.columns[1])
            col.underline.number <- as.numeric(split.columns[2]) - col.underline.begin + 1
            
            cline[col.underline.begin] <- col.underline.number
            
            s <- substr(s, to+1, nchar(s))
            .html.cline(cline)
          }
        }
        else if (strpos("\\hline",line) != -1) {
          if (!(is.na(latex.code[row+1]))) {
            if (strpos("\\hline", latex.code[row+1]) != -1) {
              .html.horizontal.line(how.many.columns)
            } 
            else {
              if (strpos("\\hline", latex.code[row-1]) == -1) {           
                .html.horizontal.line(how.many.columns) 
              }
            }
          }
          else {
            if (strpos("\\hline", latex.code[row-1]) == -1) {           
              .html.horizontal.line(how.many.columns) 
            }
          }
        }
      }
      cat("</table>\n")
    }
  }

.mmd.output <-
  function(all.latex.code) {
    
    how.many.tables <- 0
    start.lines <- NULL
    for (i in 1:length(all.latex.code)) {
      if (all.latex.code[i] %in% c("")) { 
        how.many.tables <- how.many.tables + 1
        start.lines <- c(start.lines, i)
      }
    }
    
    for (table.number in 1:how.many.tables) {
      
      if (table.number < how.many.tables) {
        latex.code <- all.latex.code[start.lines[table.number]:start.lines[table.number+1]]
      }
      else {
        latex.code <- all.latex.code[start.lines[table.number]:length(all.latex.code)]
      }
      
      how.many.columns <- .get.number.of.columns(latex.code)
      
      r <- 0
      
      matrices <- .matrices(latex.code, how.many.columns, type="mmd")
      t <- matrices[[1]]
      c <- matrices[[2]]
      j <- matrices[[3]]
      
      max.l <- .text.column.width(t, c)
      w <- .width.matrix(c, max.l)
      
      cat("\n")
      cat("<table style=\"text-align:center\">")
      
      
      for (row in 1:length(latex.code)) {
        line <- latex.code[row]
        if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") {
          r <- r + 1
          .mmd.output.line(t, r, w, c, j)
        }
        else if (strpos("\\caption{", line) != -1) {
          inside.caption <- substr(.trim(line), 10, nchar(.trim(line))-1)
          text.title <- .trim(.remove.control.sequences(inside.caption, type="mmd"))
          if (text.title != "") { cat("**",.remove.control.sequences(inside.caption, type="mmd"),"***\n", sep="") }
          ### ADD THE REQUISITE NUMBER OF |s
        }
        else if (strpos("\\cline{", line) != -1) {
          s <- paste("  ", line, "  ", sep="")
          cline <- rep(0, times=how.many.columns)
          while (strpos("\\cline{", s) != -1) {
            from <- strpos("\\cline{", s) + 7
            to <- strpos("}", s) - 1
            
            underline.columns <- substr(s, from, to)
            split.columns <- strsplit(underline.columns,"-", fixed=TRUE)[[1]]
            
            col.underline.begin <- as.numeric(split.columns[1])
            col.underline.number <- as.numeric(split.columns[2]) - col.underline.begin + 1
            
            cline[col.underline.begin] <- col.underline.number
            
            s <- substr(s, to+1, nchar(s))
            .mmd.cline(cline)
          }
        }
        else if (strpos("\\hline",line) != -1) {
          if (!(is.na(latex.code[row+1]))) {
            if (strpos("\\hline", latex.code[row+1]) != -1) {
              .mmd.horizontal.line(how.many.columns)
            } 
            else {
              if (strpos("\\hline", latex.code[row-1]) == -1) {           
                .mmd.horizontal.line(how.many.columns) 
              }
            }
          }
          else {
            if (strpos("\\hline", latex.code[row-1]) == -1) {           
              .mmd.horizontal.line(how.many.columns) 
            }
          }
        }
      }
      cat("</table>\n")
    }
  }

.text.output.line <-
  function(text.matrix, row, width.matrix, column.matrix, justification.matrix) {
    real.c <- 0   # "real" column position
    for (c in 1:ncol(text.matrix)) {
      real.c <- real.c + column.matrix[row,c]
      justify <- justification.matrix[row, c]
      
      if (!(is.na(text.matrix[row,c]))) { 
        .just.cat(text.matrix[row, c], width=width.matrix[row, c], justify=justify)
        if (real.c < ncol(text.matrix)) { cat(" ",sep="")}
      }
    }
    cat("\n")
  }

.html.output.line <-
  function(text.matrix, row, width.matrix, column.matrix, justification.matrix) {
    real.c <- 0   # "real" column position
    cat("<tr>")
    for (c in 1:ncol(text.matrix)) {
      cm <- column.matrix[row,c]
      real.c <- real.c + cm
      justify <- justification.matrix[row, c]
      
      if (!(is.na(text.matrix[row,c]))) {
        cat("<td")
        
        if (cm > 1) { cat(" colspan=\"",cm,"\"", sep="") }
        
        if (justify == "l") { cat(" style=\"text-align:left\"", sep="") }
        if (justify == "r") { cat(" style=\"text-align:right\"", sep="") }
        
        cat(">")
        
        .just.cat(text.matrix[row, c], width=width.matrix[row, c], justify="n")
        
        cat("</td>")
      }
    }
    cat("</tr>\n")
  }

.mmd.output.line <-
  function(text.matrix, row, width.matrix, column.matrix, justification.matrix) {
    real.c <- 0   # "real" column position
    for (c in 1:ncol(text.matrix)) {
      cm <- column.matrix[row,c]
      real.c <- real.c + cm
      justify <- justification.matrix[row, c]
      
      if (!(is.na(text.matrix[row,c]))) {
        
        .just.cat(text.matrix[row, c], width=width.matrix[row, c], justify=justify)
        
        for (i in 1:cm) { cat("|") }
      }
    }
    cat("\n")
  }

.width.matrix <-
  function(column.matrix, max.length) {
    
    w.matrix <- matrix(NA, nrow = nrow(column.matrix), ncol = ncol(column.matrix))
    
    # enter single widths first
    for (r in 1:nrow(column.matrix)) {
      for (c in 1:ncol(column.matrix)) {
        w.matrix[r,c] <- max.length[c]
      }
    }
    
    
    # think about multicolumns
    for (r in 1:nrow(column.matrix)) {
      from.c <- 0   # from which column do I start hoovering up widths?
      for (c in 1:ncol(column.matrix)) {
        
        from.c <- from.c+1
        
        if (column.matrix[r,c] >= 2) {
          total.width <- 0
          for (i in from.c:(from.c+column.matrix[r,c]-1)) {
            
            total.width <- total.width + max.length[i] + 1 
            if (i > from.c) {
              for (j in i:ncol(column.matrix)) {
                if ((j+1) <= ncol(column.matrix)) {
                  w.matrix[r,j] <- w.matrix[r, j+1]
                  w.matrix[r,j+1] <- NA
                }
                else {
                  w.matrix[r,j] <- NA
                }
              }
              
            }
          }
          w.matrix[r,c] <- total.width - 1
          from.c <- from.c + column.matrix[r,c] - 1
        }
        
      }
    }
    
    return(w.matrix)
  }

.text.column.width <-
  function(text.matrix, column.matrix) {
    
    max.length = rep(1, times=ncol(column.matrix))
    temp.text.matrix <- text.matrix
    
    # first, get the maximum width of single columns
    for (r in 1:nrow(text.matrix)) {
      for (c in 1:ncol(text.matrix)) {
        real.c <- 0   # 'real' column number, adjusted for multicolumn
        for (i in 1:c) {
          real.c <- real.c + column.matrix[r, i]
        }
        if (real.c <= ncol(text.matrix)) {
          if (column.matrix[r,c] == 1) { # only look at singles here
            if (nchar(text.matrix[r,c]) > max.length[real.c]) { max.length[real.c] <- nchar(text.matrix[r,c]) }
          }
        }
      }  
    }
    
    # think about multicolumns
    for (r in 1:nrow(text.matrix)) {
      for (c in 1:ncol(text.matrix)) {
        if (!is.na(column.matrix[r,c])) {
          if (column.matrix[r,c] >= 2) {   # only look at multicolumns
            total.width <- 0
            for (i in c:(c+column.matrix[r,c]-1)) {
              total.width <- total.width + max.length[i] 
            }
            while (total.width < nchar(text.matrix[r,c])) {  # if does not fit into single columns, widen the maxima
              relevant.maxima <- NULL
              for (i in c:(c+column.matrix[r,c]-1)) {
                relevant.maxima <- c(relevant.maxima, max.length[i])
                if (max.length[i] == min(relevant.maxima)) { 
                  total.width <- 0
                  for (j in c:(c+column.matrix[r,c]-1)) {
                    total.width <- total.width + max.length[j] 
                  }
                  if (total.width < nchar(text.matrix[r,c])) { max.length[i] <- max.length[i] + 1 }
                }
              }
            }
          }
        }
      }
    }
    
    return(max.length)
  }

.text.table.rows <-
  function(latex.code) {
    
    # figure out how many columns
    rows <- 0
    for (i in 1:length(latex.code)) {
      line <- latex.code[i]
      if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") { 
        rows <- rows + 1
      }
    }      
    
    return(rows)
  }

.get.number.of.columns <- 
  function(latex.code) {
    
    formatting.string <- ""
    
    for (i in 1:length(latex.code)) {
      line <- latex.code[i]
      if ((substr(line, 1, 7) == "\\begin{") & (regexpr("}}",line,fixed=TRUE)[[1]] != -1)) {
        formatting.string <- substr(line, regexpr("}}",line,fixed=TRUE)[[1]]+2, nchar(line)-1)
      }
    }
    
    columns <- 0
    for (i in 1:nchar(formatting.string)) {
      if (substring(formatting.string, i, i) %in% c("l", "c", "r", "D")) { columns <- columns + 1 }
    }
    return(columns)
  }

.matrices <-
  function(latex.code, how.many.columns, type="text") {
    
    rows <- .text.table.rows(latex.code)
    t.matrix <- matrix(NA, nrow = rows, ncol = how.many.columns)
    c.matrix <- matrix(1, nrow = rows, ncol = how.many.columns)
    j.matrix <- matrix(NA, nrow = rows, ncol = how.many.columns)
    
    line.content.j <- rep("c", how.many.columns)
    
    # put strings into matrix
    row <- 0
    for (i in 1:length(latex.code)) {
      line <- latex.code[i]
      if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") { 
        row <- row + 1
        line.content <- .split.line(.remove.control.sequences(line, type=type))
        length(line.content) <- how.many.columns
        t.matrix[row,] <- line.content
        
        line.content.j[1] <- "l"
        line.content.j[2:how.many.columns] <- "c" 
        
        line.split <- .split.line(line)
        
        # add in column widths
        line.column <- rep(1, how.many.columns)
        for (j in 1:length(line.split)) {
          no.of.columns <- 0
          if (regexpr("\\multicolumn{", line.split[j], fixed=TRUE) != -1) {
            # text
            multicolumn.no <- substr(line.split[j], regexpr("{", line.split[j], fixed=TRUE)+1, regexpr("}", line.split[j], fixed=TRUE)-1)
            no.of.columns <- as.numeric(multicolumn.no)
            
            # justification
            from <- regexpr("}{", line.split[j], fixed=TRUE)+2
            rest.of.expression <- substr(line.split[j], from, nchar(line.split[j]))
            to <- regexpr("}", rest.of.expression, fixed=TRUE) - 1
            justification <- substr(rest.of.expression, 1, to)
            line.content.j[j] <- justification
          }
          else {
            no.of.columns <- 1
          }
          line.column[j] <- no.of.columns
        }
        
        # column
        length(line.column) <- how.many.columns
        c.matrix[row,] <- line.column
        
        # justification
        length(line.content.j) <- how.many.columns
        j.matrix[row,] <- line.content.j
        
      }
    }
    return(list(t.matrix,c.matrix,j.matrix))
  }



.repeat.char <-   
  function(ch, rep=1, new.line=FALSE) {
    if (rep >= 1) {
      out.str <- ""
      for (i in 1:rep) {
        out.str <- paste(out.str, ch, sep="")
      }
      if (new.line == TRUE) { out.str <- paste(out.str, "\n", sep="")}
      cat(out.str)
    }
  }

.just.cat <-         # cat that justifies string appropriately over the next couple of paragraphs
  function(s, width, offset.char=" ", justify="c"){
    len <- nchar(s)
    if (width <= len) {
      cat(s)
    }
    else {
      if (justify == "c") {
        offset <- (width - len) %/% 2
        .repeat.char(offset.char, offset)
        cat(s)
        .repeat.char(offset.char, width - len - offset)
      }
      else if (justify == "l") {
        cat(s)
        .repeat.char(offset.char, width - len)
      }
      else if (justify == "r") {
        .repeat.char(offset.char, width - len)
        cat(s)
      }
      else if (justify == "n") { # no justification, just output
        cat(s)
      }
    }
  }
