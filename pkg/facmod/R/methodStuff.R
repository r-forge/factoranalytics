setGeneric("getTickersByPermnos", 
  function(object, values) standardGeneric("getTickersByPermnos")) 
setMethod("getTickersByPermnos",
    signature(object = "fmdsCube.CRSP", values = "vector"),
    function (object, values) 
    {
        object@labels[values]
    }
)

setGeneric("getPermnosByTickers", 
  function(object, values) standardGeneric("getPermnosByTickers")) 
setMethod("getPermnosByTickers",
    signature(object = "fmdsCube.CRSP", values = "vector"),
    function (object, values) 
    {
      labels <- object@labels
      tklist <- unlist(labels)
      m <- match(x=values,tklist)
      if( any(duplicated(m)) ) {
        m <- m[(!tklist==tklist[duplicated(m)])]
      }
      pn <- names(labels)[m]
      names(pn) <- tklist[m]
      pn <- as.list(pn)
      return(pn)
    }
)

setGeneric("getAssetLabels", 
  function(object) standardGeneric("getAssetLabels")) 
setMethod("getAssetLabels",
    signature(object = "fmdsCube"),
    function (object) 
    {
        object@labels
    }
)

setGeneric("getAssetIDs", 
  function(object) standardGeneric("getAssetIDs")) 
setMethod("getAssetIDs",
    signature(object = "fmdsCube"),
    function (object) 
    {
        dimnames(object@data)[[3]]
    }
)

setGeneric("getAvailableTimes", 
  function(object) standardGeneric("getAvailableTimes")) 
setMethod("getAvailableTimes",
    signature(object = "fmdsCube"),
    function (object) 
    {
        dimnames(object@data)[[1]]
    }
)

setMethod("subset",
    signature(x = "fmdsCube"),
    function (x, ...) subset.fmdsCube(x,...)
)

setGeneric("scaleNumericFactors", 
  function(cube, ...) standardGeneric("scaleNumericFactors")) 
setMethod("scaleNumericFactors",
    signature(cube = "fmdsCube"),
    function (cube, ...) scaleNumericFactors.fmdsCube(cube,...)
)

setGeneric("flatten", 
  function(cube, ...) standardGeneric("flatten")) 
setMethod("flatten",
    signature(cube = "fmdsCube"),
    function (cube, ...) flatten.fmdsCube(cube,...)
)
