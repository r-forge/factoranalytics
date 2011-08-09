# virtual class that unifies all data representations
setClass("fmds")

# fmdsCube representation 
setClass("fmdsCube", representation(
						data="array",
						labels="list",
						info="data.frame",
						auxData="list",
						datefmt="character",
						matchfmt="character",
						contents="matrix",
						index="list",
						labelvar="character",
						buildDate="Date"	),
            contains="fmds")

# fmdsCube.CRSP representation
setClass("fmdsCube.CRSP", representation(
							interp="character"),
              contains="fmdsCube")

setMethod("show","fmdsCube",
  function(object){
  cat(paste("class: ",class(object))) ; cat("\n")
  cat(paste("data: ",paste(dim(object@data),collapse=" x "),"\n")) ;
  cat(paste("dates:",dimnames(object@data)[[1]][1],"to",tail(dimnames(object@data)[[1]],1)))
  cat("\n")
  cat("numeric data:\n")
  cat(paste(dimnames(object@data)[[2]],collapse=", "))
  cat("\n")
  cat("non-numeric data:\n")
  cat(paste(colnames(object@info),collapse=", "))
  cat("\n")
  })
