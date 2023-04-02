
require(doParallel)


.sg.internal.parlag <- function(
      sig.exp,
      lag.mgr,
      parameters,
      nfolds,
      export.names,
      export.packages,
      ...)
{
  while(are.lag.groups.pending(lag.mgr, ...))
  {
    lag.group <- next.lag.group(lag.mgr, ...)
    
    #####
    dfs <- lapply(lag.group, function(l) as.data.frame(l))
    lg <- do.call(rbind, dfs)
    print(lg)
    #####
    
    lags.results <- foreach(
      lag.list = lag.group,
      .inorder = TRUE,
      .errorhandling = "pass",
      .export = export.names,
      .noexport = "lag.mgr",
      .packages = export.packages,
      .verbose = FALSE
      ) %dopar%
        grid.parameters(
          sig.exp = sig.exp,
          lag.list = lag.list,
          parameters = parameters,
          nfolds = nfolds,
          ...
        )
    lag.group.results <- join.lags.results(sig.exp, lags.results, ...)
    
    process.lag.group.results(lag.mgr, lag.group, lag.group.results, ...)
    
    rm(lags.results)
    rm(lag.group.results)
    gc()
  }
  
  get.lag.groups.final.results(lag.mgr, ...)
}


.sg.internal.seqlag <- function(
      sig.exp,
      lag.mgr,
      parameters,
      nfolds,
      ...)
{
  while(are.lag.groups.pending(lag.mgr, ...))
  {
    lag.group <- next.lag.group(lag.mgr, ...)
    
    #####
    dfs <- lapply(lag.group, function(l) as.data.frame(l))
    lg <- do.call(rbind, dfs)
    print(lg)
    #####
    
    .sg.internal.tmp.fun <- function(lag.list) grid.parameters(
      sig.exp = sig.exp,
      lag.list = lag.list,
      parameters = parameters,
      nfolds = nfolds,
      ...
    )
    lags.results <- lapply(lag.group, .sg.internal.tmp.fun)
    lag.group.results <- join.lags.results(sig.exp, lags.results, ...)
    
    process.lag.group.results(lag.mgr, lag.group, lag.group.results, ...)
  }
  
  get.lag.groups.final.results(lag.mgr, ...)
}


grid.lags.and.parameters <- function(
      sig.exp,
      lag.mgr,
      parallel.lagging = c("seq", "par"),
      parameters,
      nfolds,
      ...)
{
  parallel.lagging <- match.arg(parallel.lagging)
  results <- NULL
  
  if(parallel.lagging == "seq")
    results <- .sg.internal.seqlag(
      sig.exp = sig.exp,
      lag.mgr = lag.mgr,
      parameters = parameters,
      nfolds = nfolds,
      ...
    )
  
  if(parallel.lagging == "par")
    results <- .sg.internal.parlag(
          sig.exp = sig.exp,
          lag.mgr = lag.mgr,
          parameters = parameters,
          nfolds = nfolds,
          ...
    )
  
  invisible(results)
}

grid.parameters <- function(
      sig.exp,
      lag.list,
      parameters,
      nfolds,
      parameters.group.size = 25,#250,
      ...)
{
  if(!inherits(parameters, "list"))
    stop("parameters must be a named list [1]")
  parameter.names <- names(parameters)
  if(is.null(parameter.names))
    stop("parameters must be a named list [2]")
  if(any(parameter.names == ""))
    stop("parameters must be a named list [3]")
  
  lagged.ins <- get.lagged.instance(
      sigexp = sig.exp,
      lags = lag.list,
      ...)
  
  folded.ins <- get.folded.instance(
      sigexp = sig.exp,
      lagged.ins = lagged.ins,
      nfolds = nfolds,
      ...)
  
  .sg.internal.tmp.fun1 <- function(v) {l <- as.list(v); names(l) <- names(v); l}
  .sg.internal.tmp.fun2 <- function(param.list) crossValidate(
    sig.exp = sig.exp,
    folded.ins = folded.ins,
    model.params = param.list,
    ...
  )
  
  param.exp <- expand.grid(parameters, KEEP.OUT.ATTRS = FALSE)
  lims <- seq(from = 1, to = nrow(param.exp), by = parameters.group.size)
  niters <- length(lims)
  for(i in 1:niters)
  {
    ini <- lims[i]
    fin <- ifelse(i < niters, lims[i + 1] - 1, nrow(param.exp))
    cat(ini, fin, "\n")
    
    param.lists <- apply(param.exp[ini:fin, ], 1, .sg.internal.tmp.fun1)
    params.results <- lapply(param.lists, .sg.internal.tmp.fun2)
    results <-join.params.results(
      sigexp = sig.exp,
      lags = lag.list,
      params.results = params.results,
      ...
    )
    print(str(results))
    stop()
  }
  stop()
  
  
  results
}



