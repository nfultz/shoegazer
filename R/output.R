############## OUTPUT INTO FILE ##############

### !!!! - add packages
.output.tex <-
  function (file.out, content, header) {
    
    header.tex <- "\\documentclass{article}\n"
    
    required.latex.packages <- NULL
    if (.format.dec.mark.align==TRUE) { required.latex.packages <- c(required.latex.packages, "dcolumn") }
    if (.format.floating.environment=="sidewaystable") { required.latex.packages <- c(required.latex.packages, "rotating") }
    
    if (!is.null(required.latex.packages)) {
      for (i in 1:length(required.latex.packages)) {
        header.tex <- paste(header.tex, "\\usepackage{", required.latex.packages[i], "}\n", sep="")
      }
    }
    
    if (header == TRUE) {
      
      cat( 
        header.tex,
        "\\begin{document}",
        paste(content, collapse="\n"),
        "\\end{document}\n", 
        sep="\n",
        file = file.out
      )
    } else {
      cat( 
        paste(content, collapse="\n"), 
        sep="\n",
        file = file.out
      )
    }
  }

.output.html <-
  function (file.out, content, header) {
    
    if (header == TRUE) {
      cat( 
        "<!DOCTYPE html>",
        "<html>",
        "<body>",
        paste(content, collapse="\n"),
        "</body>",
        "</html>\n",
        sep="\n",
        file = file.out
      )
    } else {
      cat( 
        paste(content, collapse="\n"),
        sep="\n",
        file = file.out
      )
    }
    
  }

.output.txt <-
  function (file.out, content, header) {
    cat( 
      paste(content, collapse="\n"), 
      sep="\n",
      file = file.out
    )    
  }

# !!! -  work on this more in a later version
.output.pdf <-
  function (file.out, content) {
    tex.temp.file <- tempfile("temp", fileext="tex")
    .output.tex(tex.temp.file, content)                     
    capture.output(system(paste( "pdflatex --interaction=nonstopmode", shQuote(tex.temp.file)),  show.output.on.console = FALSE ))
  }

.output.file <-
  function (out, latex.code, text.out, html.out, type, out.header) {
    for (i in 1:length(out)) {
      if (.get.file.extension(out[i])=="tex") { .output.tex(out[i], latex.code, out.header) }
      # else if (.get.file.extension(out[i])=="pdf") { .output.pdf(out[i], latex.code) }
      else if (.get.file.extension(out[i])=="txt") { .output.txt(out[i], text.out, out.header) }
      else if ((.get.file.extension(out[i])=="html") || (.get.file.extension(out[i])=="htm")) { 
        .output.html(out[i], html.out, out.header) 
      }
      else { # if another extension, do latex or text based on 'type'
        if (type == "latex") { .output.tex(out[i], latex.code, out.header) }
        else if (type == "text") { .output.txt(out[i], text.out, out.header) }
        else if (type == "html") { .output.html(out[i], html.out, out.header) }
      }
    }
  }


###########################################
