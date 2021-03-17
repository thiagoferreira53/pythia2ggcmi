library(parallel)

doc <- "Convert pythia per-pixel output files to GGCMI NetCDF files, using SLURM

Usage:

slurm_p2g.R <config_file>
slurm_p2g.R (-h | --help)

Options:
-h --help       This message.
<config_file>   JSON configuration file to run.

Note:
All variables available in pythia2ggcmi.R as automatically run.
"

isValid <- function(config) {
  return(TRUE)
}

generateQueue <- function(config) {
  return(c())
}

dequeue <- function(queue) {
  return(list("value" = queue[1], "queue" = queue[-1]))
}

.testCase <- function(t) {
  Sys.sleep(t)
  return(t*t)
}

.execute <- function(t) {
  return(system2("sleep", args=paste0(t)))
}

runtime <- function(slot.length = 4) {
  slots <- rep(NA, slot.length)
  queue <- floor(runif(n=30, min=10, max=41))
  print(queue)
  print("----------")
  while(length(queue) > 0) {
    slots <- lapply(slots, function(x){
      if(!is.list(x) && is.na(x)) {
        if (length(queue) > 0) {
          qv <- dequeue(queue)
          queue <<- qv$queue
          return(mcparallel(.execute(qv$value)))
        } else {
          return(NA)
        }
      } else {
        return(x)
      }
    })
    procs <- mccollect(wait = FALSE)
    if(!is.null(procs)) {
      print(queue)
      procNames <- names(procs)
      slots <- lapply(slots, function(x){
        if (is.list(x)) {
          if (paste0(x$pid) %in% procNames) {
            return(NA)
          } else {
            return(x)
          }
        } else {
          return(x)
        }
      })
    }
  }
  print("Queue drained. Waiting for final runs.")
  mccollect()
  return(TRUE)
}

if (sys.nframe() == 0) {
  library(docopt)
  argv <- docopt(doc)

  if (!file.exists(argv$config_file)) {
   stop(sprintf("%s not found.", argv$config_file))
  }

  library(jsonlite)
  config <- read_json(argv$config_file)

  if (!isValid(config)) {
    stop(sprintf("%s is not a valid JSON configuration", argv$config_file))
  }
}
