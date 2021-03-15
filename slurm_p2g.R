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

if (sys.nframe() == 0) {
  library(docopt)
  argv <- docopt(doc)

  if (!file.exists(argv$config_file)) {
   stop(sprintf("%s not found.", argv$config_file))
  }

  library(jsonlite)
  config <- read_json(argv$config_file)
}
