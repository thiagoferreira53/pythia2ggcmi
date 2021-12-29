doc <- "pythia2ggcmi.R - Convert pythia outputs to GGCMI outputs.

Version: 0.2.0-beta

Usage:
  pythia2ggcmi.R [-i] <pythia_csv> <output_directory> <ggcmi_variable> <crop> <gcm> <rcp> <soc> <sens> <start_year> <end_year>
  pythia2ggcmi.R (-l | --list-variables)
  pythia2ggcmi.R (-c | --list-crops)
  pythia2ggcmi.R (-h | --help)

Options:
  -h --help             Show this screen.
  -l --list-variables   Show supported GGCMI variables.
  -c --list-crops       Show available GGCMI crop codes.
  -i                    Full irrigation.
  <pythia_csv>          Pythia output file to extract from.
  <output_directory>    Directory to store the resulting NetCDF file.
  <ggcmi_variable>      GGCMI variable to extract.
  <crop>                GGCMI crop code for this file.
  <gcm>                 GCM used for forcing.
  <rcp>                 RCP/SSP used.
  <soc>                 Socio-economic scenario used.
  <sens>                CO2 scenario used.
  <start_year>          First year of simulation.
  <end_year>            Last year of simulation.

"

whereAmI <- function() {
  cmds <- commandArgs()
  res <- gsub("^(?:--file=(.*)|.*)$", "\\1", cmds)
  res <- tail(res[res != ""], 1)
  if (length(res) > 0) {
    return(normalizePath(dirname(res)))
  }
  "."
}

if (sys.nframe() == 0) {
  library(docopt)
  argv <- docopt(doc)

  script.dir <- whereAmI()
  script.import <- file.path(script.dir, "functions.R")
  print(paste0("Attempting to import ", script.import))
  source(script.import)

  if (argv$list_variables) {
    lookup <- BuildEnv("mai", TRUE, TRUE)
    llookup <- lapply(ls(lookup), function(x) paste0(x, ifelse(nchar(x) > 7, "\t", "\t\t"), lookup[[x]]$long_name))
    cat("Supported GGCMI variables\n")
    cat(paste0(llookup, collapse = "\n"))
    quit()
  }

  if (argv$list_crops) {
    codes <- .BuildCodeLookup()
    lcodes <- lapply(ls(codes), function(x) paste0(x, "\t\t", codes[[x]]))
    cat("Supported GGCMI crop codes\n")
    cat(paste0(lcodes, collapse = "\n"))
    quit()
  }

  if (!file.exists(argv$pythia_csv)) {
    stop(sprintf("%s not found.", argv$pythia_csv))
  }

  env <- BuildEnv(argv$crop, argv$i)
  if (is.null(env)) stop("Invalid crop code:", argv$crop)
  if (is.null(env[[argv$ggcmi_variable]])) stop("Invalid GGCMI variable:", argv$ggcmi_variable)

  cat("Importing CSV file...")
  csv <- ImportCSV(argv$pythia_csv)
  cat(" DONE\n")
  lookup <- BuildEnv(argv$crop, argv$i)
  cat("Extracting data from CSV...")
  out_data <- lookup[[argv$ggcmi_variable]]$proc(csv)
  rm(csv)
  cat(" DONE\n")
  out_filename <- GenerateNCFileName(argv$output_directory, argv$gcm, argv$rcp, argv$soc, argv$sens, argv$ggcmi_variable, argv$crop, argv$start_year, argv$end_year, argv$i)
  if (!dir.exists(argv$output_directory)) dir.create(argv$output_directory, recursive = TRUE)
  cat("Writing data to ", out_filename, "...")
  WriteNCDF(out_filename, out_data, lookup, argv$ggcmi_variable)
  cat(" DONE\n")
}
