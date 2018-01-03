.get.objects <- 
  function(list.of.objects) {
    
    objects <- list()
    for (i in 1:length(list.of.objects)) {
      current.object <- list.of.objects[[i]]
      if (class(current.object)[1] == "list") {
        objects <- append(objects, .get.objects(current.object))
      }
      else {
        objects <- append(objects, list(current.object))
      }
    }
    
    return(objects)
  }

# exact object names from ... string
.get.object.names <- function(s) {
  object.names <- NULL
  inside <- .inside.bracket(s)
  
  for (i in 1:length(inside)) {
    if (substr(inside[i],1,nchar("list("))=="list(") {
      object.names <- c(object.names, .get.object.names(inside[i]))
    }
    else {
      object.names <- c(object.names, inside[i])
    }
  }
  
  return(object.names)
}

