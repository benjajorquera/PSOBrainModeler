
require(doParallel)


.sg.internal.parmclag <- function(
      exp.mgr,
      lag.mgr,
      parameters,
      nfolds,
      nworkers = 2,
      ...)
{
  registerDoParallel(cores = nworkers)
  
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
      .errorhandling = "pass", .export = "lag.list") %dopar%
        grid.parameters(
          exp.mgr = exp.mgr,
          lag.list = lag.list,
          parameters = parameters,
          nfolds = nfolds,
          ...
        )
    lag.group.results <- join.lags.results(exp.mgr, lags.results, ...)
    
    process.lag.group.results(lag.mgr, lag.group, lag.group.results, ...)
  }
  registerDoSEQ()
  
  get.lag.groups.final.results(lag.mgr, ...)
}


.sg.internal.seqlag <- function(
      exp.mgr,
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
      exp.mgr = exp.mgr,
      lag.list = lag.list,
      parameters = parameters,
      nfolds = nfolds,
      ...
    )
    lags.results <- lapply(lag.group, .sg.internal.tmp.fun)
    lag.group.results <- join.lags.results(exp.mgr, lags.results, ...)
    
    process.lag.group.results(lag.mgr, lag.group, lag.group.results, ...)
  }
  
  get.lag.groups.final.results(lag.mgr, ...)
}


grid.lags.and.parameters <- function(
      exp.mgr,
      lag.mgr,
      parallel.lagging = c("seq", "parmc", "parcl"),
      parameters,
      nfolds,
      ...)
{
  parallel.lagging <- match.arg(parallel.lagging)

  results <- NULL
  
  if(parallel.lagging == "seq")
    results <- .sg.internal.seqlag(
      exp.mgr = exp.mgr,
      lag.mgr = lag.mgr,
      parameters = parameters,
      nfolds = nfolds,
      ...
    )
  
  if(parallel.lagging == "parmc")
    results <- .sg.internal.parmclag(
          exp.mgr = exp.mgr,
          lag.mgr = lag.mgr,
          parameters = parameters,
          nfolds = nfolds,
          ...)
  
  invisible(results)
}

grid.parameters <- function(
      exp.mgr,
      lag.list,
      parameters,
      nfolds,
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
      mgr = exp.mgr,
      lags = lag.list,
      ...)
  
  
  folded.ins <- get.folded.instance(
      mgr = exp.mgr,
      lagged.ins = lagged.ins,
      nfolds = nfolds,
      ...)
  
  
  param.exp <- expand.grid(parameters, KEEP.OUT.ATTRS = FALSE)
  .sg.internal.tmp.fun <- function(v) {l <- as.list(v); names(l) <- names(v); l}
  param.lists <- apply(param.exp, 1, .sg.internal.tmp.fun)
  
  .sg.internal.tmp.fun <- function(param.list) crossValidate(
    exp.mgr = exp.mgr,
    folded.ins = folded.ins,
    model.params = param.list,
    ...
  )
  params.results <- lapply(param.lists, .sg.internal.tmp.fun)
  
  results <-join.params.results(
    mgr = exp.mgr,
    lags = lag.list,
    params.results = params.results,
    ...
  )
  
  results
}



