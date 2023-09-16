
#
# Difference from v15.4.1
# This lag manager does not keep (in RAM) nor saves
# SVM models
#

are.lag.groups.pending <-function(mgr, ...) UseMethod("are.lag.groups.pending")
next.lag.group <-function(mgr, ...) UseMethod("next.lag.group")
process.lag.group.results <-function(mgr, ...) UseMethod("process.lag.group.results")
get.lag.groups.final.results <-function(mgr, ...) UseMethod("get.lag.groups.final.results")


get.lag.manager <- function(
      lag.list,
      tmp.file,
      output.var.name,
      lag.per.group = 1,
      save.every.ngroups = 5,
      keep.nstats = 30,
      ...)
{
  restarting <- file.exists(tmp.file)
  if(restarting)
  {
    mgr <- readRDS(file = tmp.file)
    .lagmgr.internal.is.valid.lagmgr(mgr)
  }
  else
  {
    mgr <- list(
      tmp.file = tmp.file,
      output.var.name = output.var.name,
      lag.per.group = lag.per.group,
      save.every.ngroups = save.every.ngroups,
      keep.nstats = keep.nstats,
      lags.df = expand.grid(lag.list, KEEP.OUT.ATTRS = FALSE),
      stats = NULL,
      models = NULL,
      version = .lagmgr.internal.version
    )
    class(mgr) <- c(class(mgr), .lagmgr.internal.mgr.class)
  }
  
  mgr[["unsaved.stats"]] <- NULL
  mgr[["unsaved.counter"]] <- 0
  
  mgr[["next.indices"]] <- NULL
  if(nrow(mgr[["lags.df"]]) > 0)
  {
    n <- min(mgr[["lag.per.group"]], nrow(mgr[["lags.df"]]))
    mgr[["next.indices"]] <- 1:n
  }
  
  mgr
}


are.lag.groups.pending.LAG.MANAGER <- function(mgr, ...)
{
  .lagmgr.internal.is.valid.lagmgr(mgr)
  !is.null(mgr[["next.indices"]])
}


next.lag.group.LAG.MANAGER <-function(mgr, ...)
{
  .lagmgr.internal.is.valid.lagmgr(mgr)
  
  subdf <- mgr[["lags.df"]][mgr[["next.indices"]], ]
  .lagmgr.internal.tmp.fun <- function(v) {l <- as.list(v); names(l) <- names(v); l}
  lg <- apply(subdf, 1, .lagmgr.internal.tmp.fun)
  
  lg
}

process.lag.group.results.LAG.MANAGER <-function(mgr, lg, lg.results, ...)
{
  .lagmgr.internal.is.valid.lagmgr(mgr)
  
  dfs <- lapply(lg, function(l) as.data.frame(l))
  lg <- do.call(rbind, dfs)
  
  cols <- colnames(lg.results[["stats"]]) %in% colnames(lg)
  lgr <- unique(lg.results[["stats"]][, cols])
  
  if(!all(lg == lgr))
    stop("unexpected lag group results")
  
  m <- .lagmgr.internal.add.lgr(mgr, lg.results)
  
  m[["unsaved.counter"]] <- m[["unsaved.counter"]] + 1
  if(m[["unsaved.counter"]] >= m[["save.every.ngroups"]])
    m <- .lagmgr.internal.save(m)
  else
  {
    next.indices <- m[["next.indices"]] + m[["lag.per.group"]]
    if(tail(next.indices, 1) > nrow(m[["lags.df"]]))
      next.indices <- next.indices[next.indices <= nrow(m[["lags.df"]])]
    if(length(next.indices) > 0)
      m[["next.indices"]] <- next.indices
    else
    {
      if(m[["unsaved.counter"]] > 0)
        m <- .lagmgr.internal.save(m)
      m[["next.indices"]] <- NULL
    }
  }
  
  eval.parent(substitute(mgr <- m))
  TRUE
}


get.lag.groups.final.results.LAG.MANAGER <-function(mgr, ...)
{
  .lagmgr.internal.is.valid.lagmgr(mgr)
  
  list(stats = mgr[["stats"]], models = mgr[["models"]])
}



##### Internals #####

.lagmgr.internal.mgr.class <- "LAG.MANAGER"
.lagmgr.internal.version <- "15.4.2"

.lagmgr.internal.is.valid.lagmgr <- function(mgr)
{
  if(!any(class(mgr) != .lagmgr.internal.mgr.class))
    stop("object is not a lag manager")
  
  if(mgr[["version"]] != .lagmgr.internal.version)
    stop("lag manager of unsupported version")
}

.lagmgr.internal.save <- function(m)
{
  join <- .lagmgr.internal.join(
    stats1 = m[["stats"]],
    stats2 = m[["unsaved.stats"]],
    max.len = m[["keep.nstats"]]
  )
  
  last <- tail(m[["next.indices"]], 1)
  i <- seq_len(last)
    
  m[["lags.df"]] <- m[["lags.df"]][-i, ]
  m[["stats"]] <- join
  m[["unsaved.stats"]] <- NULL
  m[["unsaved.counter"]] <- 0
    
  saveRDS(m, file = m[["tmp.file"]], compress = FALSE)
  cat("partial results saved\n")
  
  if(nrow(m[["lags.df"]]) > 0)
  {
    n <- min(m[["lag.per.group"]], nrow(m[["lags.df"]]))
    m[["next.indices"]] <- 1:n
  }
  else
    m[["next.indices"]] <- NULL
  
  m
}

.lagmgr.internal.join <- function(
      stats1,
      stats2,
      max.len)
{
  stats <- rbind(stats1, stats2)
  
  i <- order(
    -stats[["perf"]],
    -stats[["test.cor"]],
    stats[["model.ord"]],
    stats[["model.mem"]]
  )
  
  stats <- stats[i, ]
  
  if(nrow(stats) > max.len)
  {
    i <- 1:max.len
    stats <- stats[i, ]
  }
  
  rownames(stats) <- NULL
  
  stats
}

.lagmgr.internal.add.lgr <- function(m, lgr)
{
  join <- .lagmgr.internal.join(
    stats1 = m[["unsaved.stats"]],
    stats2 = lgr[["stats"]],
    max.len = m[["keep.nstats"]]
  )
  
  m[["unsaved.stats"]] <- join
    
  m
}
