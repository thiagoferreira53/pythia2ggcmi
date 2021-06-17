library(data.table)
library(udunits2, quietly = TRUE)
library(ncdf4, quietly = TRUE)

# Utility functions
GenerateNCFileName <- function(out_dir, climate_forcing, climate_scenario, soc_scenario, sens_scenario, var, crop, start_year, end_year, irrig = FALSE) {
  irrig <- ifelse(irrig, "firr", "noirr")
  var <- paste(var, crop, irrig, sep = "-")
  model <- "dssat-pythia"
  reanalysis <- paste0(climate_forcing,
                              ifelse(toupper(climate_forcing) == "GSWP3", "-w5e5", "_w5e5"))
  return(file.path(out_dir, paste0(paste(
                              model,
                              reanalysis,
                              climate_scenario,
                              soc_scenario,
                              sens_scenario,
                              var,
                              "global",
                              "annual",
                              start_year,
                              end_year, sep = "_"), ".nc")))
}

ImportCSV <- function(csv) {
  data <- fread(csv, showProgress=FALSE)
  return(data)
}

BuildEnv <- function(crop, irrig = FALSE, cleared = FALSE) {
  global_entry <- list()
  global_entry$inst <- "University of Florida (UF)"
  global_entry$contact <- "Oscar Castillo <ocastilloromero@ufl.edu>"
  codes <- .BuildCodeLookup()
  valid_crop <- codes[[crop]]
  if (is.null(valid_crop)) return(NULL)
  irrig <- ifelse(irrig, "firr", "noirr")
  lookup <- new.env(parent = emptyenv(), hash = TRUE)
  lookup[["yield"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractYield, "yield", "yield", "kg/ha", "t ha-1 gs-1 (dry matter)", cleared)
  lookup[["biom"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractBiom, "biom", "total above ground biomass", "kg/ha", "t ha-1 gs-1 (dry matter)", cleared)
  lookup[["cnyield"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractCnyield, "cnyield", "CN ratio of yield", cleared=cleared)
  lookup[["plantday"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractPlantday, "plantday", "planting day", NULL, "day of year", cleared)
  lookup[["plantyear"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractPlantyear, "plantyear", "planting year", NULL, "calendar year", cleared)
  lookup[["anthday"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractAnthday, "anthday", "days until anthesis", NULL, "days from planting", cleared)
  lookup[["matyday"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractMatyday, "matyday", "days until maturity", NULL, "days from planting", cleared)
  lookup[["harvyear"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractHarvyear, "harvyear", "harvest year", NULL, "calendar year", cleared)
  lookup[["pirnreq"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractPirnreq, "pirnreq", "Potential net irrigation water requirements", "mm", "kg m-2 gs-1", cleared)
  lookup[["aet"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractAet, "aet", "actual evapotranspiration", "mm", "kg m-2 gs-1", cleared)
  lookup[["transp"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractTransp, "transp", "transpiration", "mm", "kg m-2 gs-1", cleared)
  lookup[["soilevap"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractSoilevap, "soilevap", "soil evaporation", "mm", "kg m-2 gs-1", cleared)
  lookup[["runoff"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractRunoff, "runoff", "total runoff", "mm", "kg m-2 gs-1", cleared)
  lookup[["tnrup"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractTnrup, "tnrup", "total uptake of reactive nitrogen", "kg[N]/ha", "kgN ha-1 gs-1", cleared)
  lookup[["n2oemis"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractN2oemis, "n2oemis", "total N2O emissions", "kg[N]/ha", "gN m-2 gs-1", cleared)
  lookup[["nleach"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractNleach, "nleach", "total leaching losses to reactive nitrogen", "kg[N]/ha", "gN m-2 gs-1", cleared)
  lookup[["tcemis"]] <- .BuildEnvEntry(crop, irrig, codes, ExtractTcemis, "tcemis", "total carbon emissions", "kg[C]/ha", "gC m-2 gs-1", cleared)
  lookup[[".extra"]] <- global_entry
  return(lookup)
}

GetFirstSeason <- function() {
  return(1661)
}

GetDSSATAsDate <- function(x) {
  if (x < 1000000 || x > 4000000) return(NA)
  year = (x %/% 1000) - 1
  doy = x %% 1000
  d <- as.Date(doy, origin=paste0(year,"-12-31"))
  return(d)
}

GetDSSATDateDifference <- function(d1, d2) {
  d1 <- GetDSSATAsDate(d1)
  d2 <- GetDSSATAsDate(d2)
  if (is.na(d1) || is.na(d2)) {
    return(NA)
  }
  return (as.integer(d1 - d2))
}

WriteNCDF <- function(f, dt, lookup, var) {
  n_lon <- 720
  n_lat <- 360
  n_gs  <- max(dt[,growing_season]) + 1
  lon   <- as.array(seq(-179.75,179.75,0.50))
  lat   <- as.array(seq(89.75, -89.75, -0.50))
  gs    <- as.array(seq(0, (n_gs - 1)))
  fill_value <- 1e20
  arr <- .BuildFullArray(dt, n_lon, n_lat, n_gs, lon, lat, fill_value)
  .CreateNCDF(f, lookup, var, arr, n_lon, n_lat, n_gs, lon, lat, gs, fill_value)
}

                                        # Always assume the input to these functions is the entire SUMMARY.CSV as a data.table
ExtractYield <- function(dt) {
  index <- GetFirstSeason()
                                        # According to the GGCMI rules, if the plant does not reach maturity, the HWAH should be forced to 0. This makes that happen.
  dt <- dt[, HWAH := ifelse(EDAT < 0 | ADAT < 0 | MDAT < 0, 0, HWAH)]
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = ud.convert(HWAH, "kg/ha", "tonnes/ha"))]
  return(dt)
}

ExtractBiom <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, growing_season = (SDAT %/% 1000) - index, out = ud.convert(CWAM, "kg/ha", "tonnes/ha"), MDAT)]
  return (dt)
}

ExtractCnyield <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, HWAH := ifelse(EDAT < 0 | ADAT < 0 | MDAT < 0, 0, HWAH)]
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = (HWAH / GNAM) * 0.4, MDAT, HWAH, GNAM)]
  dt <- dt[, out := ifelse(is.nan(out), 0, out)]
  return(dt)
}

ExtractPlantday <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = (PDAT %% 1000))]
  return(dt)
}

ExtractPlantyear <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = (PDAT %/% 1000))]
  return (dt)
}

ExtractAnthday <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = GetDSSATDateDifference(ADAT, PDAT))]
  dt <- dt[, .(growing_season = growing_season, lon = lon, lat = lat, out = ifelse(is.na(out),0,out))]
  return(dt)
}

ExtractMatyday <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = GetDSSATDateDifference(MDAT, PDAT))]
  dt <- dt[, .(growing_season = growing_season, lon = lon, lat = lat, out = ifelse(is.na(out),0,out))]
  return(dt)
}

ExtractHarvyear <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = (HDAT %/% 1000))]
  dt <- dt[, .(growing_season = growing_season, lon = lon, lat = lat, out = ifelse(is.na(out),0,out))]
  return (dt)
}


ExtractPirnreq <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = IRCM)]
  return (dt)
}

ExtractAet <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = ifelse(ETCP < 0, 0, ETCP))]
  return (dt)
}

ExtractTransp <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = ifelse(EPCP < 0, 0, EPCP))]
  return (dt)
}

ExtractSoilevap <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = ESCP)]
  return (dt)
}

ExtractRunoff <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = ROCM)]
  return (dt)
}

ExtractTnrup <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = NUCM)]
  return (dt)
}

ExtractN2oemis <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = ud.convert(N2OEC, "kg/ha", "g/m^2"))]
  return (dt)
}

ExtractNleach <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = ud.convert(NLCM, "kg/ha", "g/m^2"))]
  return (dt)
}

ExtractTcemis <- function(dt) {
  index <- GetFirstSeason()
  dt <- dt[, .(growing_season = (SDAT %/% 1000) - index, lon = LONGITUDE, lat = LATITUDE, out = ud.convert(CO2EC, "kg/ha", "g/m^2"))]
  return (dt)
}

.BuildFullArray <- function(dt, n_lon, n_lat, n_time, lon, lat, fill_value) {
  df <- as.data.frame(dt)
  data_array <- array(fill_value, dim=c(n_lon, n_lat, n_time))
  j <- sapply(df$lat, function(x) which.min(abs(lat - x)))
  k <- sapply(df$lon, function(x) which.min(abs(lon - x)))
  nobs <- dim(df)[1]
  l <- rep(1:n_time, length.out=nobs)
  data_array[cbind(k, j, l)] <- df$out
  return(data_array)
}

.CreateNCDF <- function(f, lookup, var, arr, n_lon, n_lat, n_time, lon, lat, t, fill_value) {
  timedim <- ncdf4::ncdim_def("time", paste0("growing seasons since ",GetFirstSeason(),"-01-01, 00:00:00", as.integer(t), longname="Growing Seasons since ", GetFirstSeason(),"-01-01, 00:00:00")
  latdim <- ncdf4::ncdim_def("lat", "degrees_north", as.double(lat), longname = "Latitude")
  londim <- ncdf4::ncdim_def("lon", "degrees_east", as.double(lon), longname = "Longitude")
  out_def <- ncdf4::ncvar_def(lookup[[var]]$standard_name, lookup[[var]]$unit_to, list(londim, latdim, timedim), fill_value, lookup[[var]]$long_name, compression=7)
  ncout <- ncdf4::nc_create(f, list(out_def), force_v4 = TRUE)
  ncdf4::ncvar_put(ncout, out_def, arr)
  ncdf4::ncatt_put(ncout, "lon", "axis", "X")
  ncdf4::ncatt_put(ncout, "lat", "axis", "Y")
  ncdf4::ncatt_put(ncout, "time", "axis", "T")
  ncdf4::ncatt_put(ncout, 0, "institution", lookup[[".extra"]]$inst)
  ncdf4::ncatt_put(ncout, 0, "contact", lookup[[".extra"]]$contact)
  ncdf4::nc_close(ncout)
}


.BuildEnvEntry <- function(crop,
                           irrig,
                           codes,
                           proc,
                           standard_name,
                           long_name,
                           unit_from = NULL,
                           unit_to = NULL,
                           cleared = FALSE) {

  standard_name <- paste(standard_name, crop, irrig, sep = "-")
  lirrig <- paste0(".", irrig)
  if (! cleared) {
    long_name <- paste(long_name, codes[[crop]], codes[[lirrig]], sep = " ")
  }
  return(list(proc = proc,
              standard_name = standard_name,
              long_name = long_name,
              unit_from = ifelse(is.null(unit_from), "", unit_from),
              unit_to = ifelse(is.null(unit_to), "", unit_to)))
}

.BuildCodeLookup <- function() {
  codes <- new.env(parent = emptyenv(), hash = TRUE)
  codes[["mai"]] <- "maize"
  codes[["swh"]] <- "spring wheat"
  codes[["wwh"]] <- "winter wheat"
  codes[["ri1"]] <- "rice1"
  codes[["ri2"]] <- "rice2"
  codes[["bar"]] <- "barley"
  codes[["rye"]] <- "rye"
  codes[["mil"]] <- "millet"
  codes[["sor"]] <- "sorghum"
  codes[["soy"]] <- "soybean"
  codes[["sun"]] <- "sunflower"
  codes[["pot"]] <- "potato"
  codes[["cas"]] <- "cassava"
  codes[["sgc"]] <- "sugar cane"
  codes[["sgb"]] <- "sugar beet"
  codes[["nut"]] <- "groundnut"
  codes[["cot"]] <- "cotton"
  codes[["rap"]] <- "rapeseed"
  codes[["bea"]] <- "beans"
  codes[["pea"]] <- "peas"
  codes[["mgr"]] <- "managed grassland"
  codes[[".firr"]] <- "full irrigation"
  codes[[".noirr"]] <- "no irrigation"
  return(codes)
}
