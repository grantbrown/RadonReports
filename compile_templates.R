# Clearing environment
gc()
rm(list = ls())
## Set Globals: ##
# Create "set_tokens.R" and fill it in with completed versions of the following lines:
#GLOBAL_API_TOKEN <- "token for CDC API goes here"
#GLOBAL_CENSUS_API_KEY <- "token for Census API goes here"
source("set_tokens.R")


source("shared_functions.R")
skip_existing <- FALSE
compile_env <- environment()

### In some states the FIPS between the CDC API and tigris don't match up 
### In South dakota the CDC gives a county with FIPS code of 46113, but this is 
### not a valid FIPS and probably corresponded to Shannon county which was 
### changed to Oglala Lakota county in 2015 with FIPS code 46102

## Bad states
### (Wrong FIPS code) South Dakota, Alaska(02158 in CDC, 02270 in tigris), 
### Virginia(51515 in CDC, was merged with 51019 in county)

## Creating results directory if it doesn't exist
if(!dir.exists("Results")){
  dir.create("Results")
}

KeepModels <- TRUE


if(!file.exists("Scales.rda")){
  GetScales(States, TRUE, FALSE)
}

## Loading in all necessary data
load("AllData.rda")
load("Scales.rda")
load("DateAccessed.rda")
Temporals <- read.csv("Temporals.csv")
PlotRatios <- read.csv("PlotRatios.csv")

warning_log <- new.env()

# Creating reports for all states
for (i in 1:nrow(States)){
  print(States$NAME[i])
  AspectRatio <- PlotRatios$AspectRatio[PlotRatios$geoId == as.numeric(States$STATEFP[i])]
  compile_env2 <- new.env(parent = compile_env)
  compile_env2$State <- States$NAME[i]
  compile_env2$GLOBAL_SEED <- States$STATEFP[i]
  compile_env2$KeepModels <- KeepModels
  # Create a directory where each report can live, to allow the figures to stay so we 
  # can re-compile
  report_dir <- paste0("./Results/", compile_env2$State)
  
  if (skip_existing && dir.exists(report_dir) && file.exists(paste0(report_dir, "/", "report_for_", compile_env2$State, ".pdf"))){
    print(paste0("... skipping, folder exists: ", report_dir))
    #return()
    next
  }
  
  if (!dir.exists(report_dir)){
    dir.create(report_dir)
  }
  # Store current working directory
  cwd <- getwd()
  tryCatch({
    setwd(report_dir)
    if(States$STATEFP[i] %in% c(15, 28)){
      # Hawaii and Missippi have no data
      message("Not implemented: ", compile_env2$State)
    }else if(States$STATEFP[i] == 11){
      # Washington DC
      file.copy("../../Templates/TemplateDC.Rnw", "./template.Rnw", overwrite = TRUE)
      file.copy("../../Media/ala.jpg", "./ala.jpg", overwrite = TRUE)
      knit2pdf(input = "./template.Rnw", 
               output = paste0("./report_for_", compile_env2$State, ".tex"),
               envir = compile_env2)
    }else if(AspectRatio >= .8 && AspectRatio <= 1.25){
      # Theses are considered normal states
      file.copy("../../Templates/Template.Rnw", "./template.Rnw", overwrite = TRUE)
      file.copy("../../Media/ala.jpg", "./ala.jpg", overwrite = TRUE)
      knit2pdf(input = "./template.Rnw", 
               output = paste0("./report_for_", compile_env2$State, ".tex"),
               envir = compile_env2)
    }else if(AspectRatio < .8){
      # These are considered long states
      file.copy("../../Templates/TemplateLong.Rnw", "./template.Rnw", overwrite = TRUE)
      file.copy("../../Media/ala.jpg", "./ala.jpg", overwrite = TRUE)
      knit2pdf(input = "./template.Rnw", 
               output = paste0("./report_for_", compile_env2$State, ".tex"),
               envir = compile_env2)
    }else{
      # These are considered wide states
      file.copy("../../Templates/TemplateWide.Rnw", "./template.Rnw", overwrite = TRUE)
      file.copy("../../Media/ala.jpg", "./ala.jpg", overwrite = TRUE)
      knit2pdf(input = "./template.Rnw", 
               output = paste0("./report_for_", compile_env2$State, ".tex"),
               envir = compile_env2)
    }
    
  }, error = function(e){
    setwd(cwd)
    stop(e$message)
  }#, warning = function(w){
  #  setwd(cwd)
  #  stop(w$message)
  #}
  )
  pdfname <- paste0("report_for_", compile_env2$State, ".pdf")
  if (file.exists(pdfname)){
    file.copy(pdfname, paste0("../", pdfname), overwrite = TRUE)
  }
  setwd(cwd)
}
