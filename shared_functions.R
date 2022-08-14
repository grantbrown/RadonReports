library(knitr)
library(coda)
library(jsonlite)
library(tigris)
library(viridis)
library(tidyverse)
library(tidycensus)
library(spdep)
library(knitr)
library(nimble)
library(tmaptools)
library(kableExtra)

# API access functions
options(timeout = max(300, getOption("timeout")))

executeQuery <- function(query){
  tryCatch({
    result <- fromJSON(readLines(URLencode(query), warn = FALSE))
  }, error = function(e){
    print(paste0("Error: ", e$message))
    stop(paste0("Error executing query: ", query))
  })
  return(result)
}

getContentAreas <- function(token=NULL){
  query <- "http://ephtracking.cdc.gov/apigateway/api/v1/contentareas/json"
  
  if (!is.null(token)){
    query <- paste0(query, "?apiToken=", token)
  }
  
  query_result <- fromJSON(readLines(query, warn = FALSE))
  query_result
}

getIndicators <- function(contentarea, token=NULL){
  query <- paste0("http://ephtracking.cdc.gov/apigateway/api/v1/indicators/",
                  contentarea)
  
  if (!is.null(token)){
    query <- paste0(query, "?apiToken=", token)
  }
  
  query_result <- fromJSON(readLines(query, warn = FALSE))
  query_result
}

getMeasures <- function(indicatorID, token = NULL){
  query <- paste0("http://ephtracking.cdc.gov/apigateway/api/v1/measures/",
                  indicatorID)
  
  if (!is.null(token)){
    query <- paste0(query, "?apiToken=", token)
  }
  
  query_result <- fromJSON(readLines(query, warn = FALSE))
  query_result
}

getGeography <- function(measure, clean=FALSE, token=NULL){
  cachefile <- paste0("cache_measure_", measure, "_geog.rda")
  if (file.exists(cachefile) && !clean){
    load(cachefile)
    return(out)
  } else{
    query1 <- paste0("https://ephtracking.cdc.gov/apigateway/api/v1/geographiclevels/", measure)
    
    if (!is.null(token)){
      query1 <- paste0(query1, "?apiToken=", token)
    }
    
    query_1_result <- fromJSON(readLines(query1, warn = FALSE))
    strata_rslt <- Reduce("rbind", lapply(query_1_result$geographicTypeId, function(gtid){
      fromJSON(readLines(paste0("https://ephtracking.cdc.gov/apigateway/api/v1/measurestratification/", measure, "/", gtid, "/0"), warn=FALSE))
    }))
    
    strata_level_reslt <- Reduce("rbind", lapply(query_1_result$geographicTypeId, function(gtid){
      fromJSON(readLines(paste0("https://ephtracking.cdc.gov/apigateway/api/v1/stratificationlevel/", measure, "/", gtid, "/0"), warn=FALSE))
    }))
    
    stratification_level <- max(strata_level_reslt$id)
    
    
    
    query_3_result <- Reduce("rbind", lapply(query_1_result$geographicTypeId, function(gtid){
      fromJSON(readLines(
        paste0("https://ephtracking.cdc.gov/apigateway/api/v1/geography/", 
               measure, "/", gtid, "/","0"), warn=FALSE))
    }))
    
    
    out <- list(geography_info = query_1_result, geographies = query_3_result)
    
    if (length(strata_rslt) > 0){
      out$strata_info = list(level_id = stratification_level,
                             level_to_append = paste0("?", 
                                                      strata_level_reslt$stratificationType[[2]]$columnName, 
                                                      "=", paste0(strata_rslt$stratificationItem[[1]]$localId, 
                                                                  collapse = ",")))
    } else{
      out$strata_info = list()
    }
    save("out", file = cachefile, compress="bzip2")
    return(out)
  }
}


getTemporals <- function(measure, GTID, FIPS, token=NULL){
  cachefile <- 
  query1 <-  paste0("https://ephtracking.cdc.gov/apigateway/api/v1/temporal/", 
                   measure, "/", GTID, "/", GTID, "/", FIPS)
  
  if (!is.null(token)){
    query1 <- paste0(query1, "?apiToken=", token)
  }
  
  query_1_result <- executeQuery(query1) 
  return(query_1_result)
}

getCoreHolder <- function(measure, GTID, FIPS, temporal, stratainfo, smoothed=0, clean=FALSE, token=NULL){
  if (length(stratainfo) == 0){
    query1 <- paste0("https://ephtracking.cdc.gov/apigateway/api/v1/getCoreHolder/", 
                               measure, "/", GTID, "/", GTID, "/", FIPS, "/",temporal,
                     "/",smoothed,"/0")
    if (!is.null(token)){
      query1 <- paste0(query1, "?apiToken=", token)
    }
  } else{
    query1 <- paste0("https://ephtracking.cdc.gov/apigateway/api/v1/getCoreHolder/", 
                     measure, "/", stratainfo$level_id, "/", GTID, "/", FIPS, "/",temporal,
                     "/",smoothed,"/0", stratainfo$level_to_append)
    if (!is.null(token)){
      query1 <- paste0(query1, "&apiToken=", token)
    }
  }
  
  query_1_result <- executeQuery(query1) 
  return(query_1_result)
}

getDataset <- function(measure, State, temporals){
  ### Getting CDC data
  avail_geog <- getGeography(measure, token = GLOBAL_API_TOKEN)
  GTID <- 2
  
  FIPS <- paste0(avail_geog$geographies$childGeographicId[which(avail_geog$geographies$parentName == State)], collapse = ",")
  dataset <- getCoreHolder(measure, GTID, FIPS, temporals, avail_geog$strata_info, smoothed=0, token=GLOBAL_API_TOKEN)
  dataset$tableResult
}

getDatasetState <- function(measure, State){
  ### Getting CDC data
  avail_geog <- getGeography(measure, token = GLOBAL_API_TOKEN)
  GTID <- 2
  # Get temporals for same measure
  FIPS <- paste0(avail_geog$geographies$childGeographicId[which(avail_geog$geographies$parentName == State)], 
                 collapse = ",")
  avail_temporals <- getTemporals(measure, GTID, FIPS, token=GLOBAL_API_TOKEN)
  temporals <- paste0(avail_temporals$id,collapse=",")
  dataset <- getCoreHolder(measure, GTID, FIPS, temporals, 
                           avail_geog$strata_info, smoothed=0, token=GLOBAL_API_TOKEN)
  dataset$tableResult
}

# Getting datasets

## State and county geometries

States <- states(cb = T, class = "sf", year = 2018) |>
  shift_geometry(position = "below")|> 
  st_transform(3857) |>
  filter(!STATEFP %in% c(78, 69, 66, 60, 72)) |>
  select(NAME, STATEFP)

Counties <- counties(cb = T, class = "sf", year = 2018) |>
  shift_geometry(position = "below")|>  
  st_transform(3857) |>
  filter(!STATEFP %in% c(78, 69, 66, 60, 72))

# Mapping functions
## Makes map with log1p scale
## Digits is used for digits to round to

Log1pMap <- function(Data, FIPS, LowData, Title, Digits, Name,
                      Width, Height,truncate = TRUE){
  
  
  Data1 <- data.frame("Values" = Data, "GEOID" = FIPS, 
                      "LowData" = LowData)
  
  # Detect if we're in a state with insufficient data
  if (truncate && mean(LowData %in% c("No Tests", "< 10 Tests")) >= 0.6 &&  
    mean(LowData %in% c("No Tests")) >= 0.3){
    Data1$Values[Data1$LowData == "No Tests"] <- NA
  }
  
  
  ### It is necessary to round digits before plotting for the legend
  ### If the largest value is rounded up for the legend but not the data, then it will not appear on the legend
  ### The same is true for the smallest value, but if it instead rounded down
  PlotData <- filter(Counties, STATEFP == substring(FIPS[1], 1, nchar(FIPS[1]) - 3)) |>
    left_join(Data1, by = c("GEOID")) |>
    mutate(Values = round(Values, digits = Digits))
  
  
  plot <- ggplot(PlotData, aes(fill = Values, geometry = geometry)) + geom_sf() +
    geom_point(data = filter(PlotData, LowData != ">= 10 Tests"), 
               aes(shape = LowData), fill = "white", color = "black", 
               stat = "sf_coordinates") +
    scale_fill_viridis_c(trans = "log1p", 
                         breaks = function(x){
                           Breaks <- seq(log1p(min(x,na.rm=TRUE)), 
                                         log1p(max(x,na.rm=TRUE)), 
                                         length.out = 5)
                           #Breaks <- magnitudeRound(expm1(Breaks), digits = Digits)
                           Breaks <- round(expm1(Breaks), Digits)
                           Breaks[1] <- min(x, na.rm = TRUE)
                           Breaks[5] <- max(x, na.rm = TRUE)
                           Breaks},
                         labels = function(x){
                           Breaks <- seq(log1p(min(x,na.rm=TRUE)), 
                                         log1p(max(x,na.rm=TRUE)), 
                                         length.out = 5)
                           #Breaks <- magnitudeRound(expm1(Breaks), digits = Digits)
                           Breaks <- round(expm1(Breaks), Digits)
                           Breaks[1] <- min(x, na.rm = TRUE)
                           Breaks[5] <- max(x, na.rm = TRUE)
                           magnitudeRound(Breaks,1)
                         },
                         limits = function(x){c(floor(min(x, na.rm=TRUE)), ceiling(max(x, na.rm=TRUE)))}
                         ) + 
    scale_shape_manual(values = c(16, 23), name = "Tests", 
                       guide = guide_legend(order = 1, 
                                            override.aes = list(size = 4))) +
    scale_color_manual(values=c("#000000", "#FFFFFF")) + 
    labs(fill = Title) +  
    theme_void()
  
  ggsave(Name, plot = plot, device = "pdf", width = Width, height = Height)
  
}

## Makes map with regular scale
RegularMap <- function(Data, FIPS, LowData, Title, Digits, Name, 
                    Width, Height, truncate = TRUE){
  
  Data1 <- data.frame("Values" = Data, "GEOID" = FIPS, 
                      "LowData" = LowData)
  # Detect if we're in a state with insufficient data
  if (truncate && mean(LowData %in% c("No Tests", "< 10 Tests")) >= 0.6 &&  
      mean(LowData %in% c("No Tests")) >= 0.3){
    Data1$Values[Data1$LowData == "No Tests"] <- NA
  }
  ### It is necessary to round digits before plotting for the legend
  ### If the largest value is rounded up for the legend, then it will not appear on the legend
  ### The same is true for the smallest value, but if it instead rounded down
  PlotData <- filter(Counties, STATEFP == substring(FIPS[1], 1, nchar(FIPS[1]) - 3)) |>
    left_join(Data1, by = c("GEOID"))# |>
    #mutate(Values = round(Values, digits = Digits))
  
  
  
  plot <- ggplot(PlotData, aes(fill = Values, geometry = geometry)) + geom_sf() + 
    geom_point(data = filter(PlotData, LowData!= ">= 10 Tests"), 
               aes(shape = LowData), fill = "white", color = "black", 
               stat = "sf_coordinates") +
    scale_fill_viridis_c(breaks = function(x){
      round(seq(min(x,na.rm=TRUE), max(x,na.rm=TRUE), length.out = 5), 
            digits = Digits)},
      labels=function(x){
        magnitudeRound(seq(min(x,na.rm=TRUE), max(x,na.rm=TRUE), length.out = 5), 
                       digits = Digits)
      },
      limits =function(x){c(floor(min(x, na.rm=TRUE)), ceiling(max(x, na.rm=TRUE)))}) + 
    scale_shape_manual(values = c(16, 23), name = "Tests", 
                       guide = guide_legend(order = 1, 
                                            override.aes = list(size = 4))) +
    scale_color_manual(values=c("#000000", "#FFFFFF")) + 
    labs(fill = Title) +   
    theme_void()
  
  ggsave(Name, plot = plot, device = "pdf", width = Width, height = Height)
  
}

# Function to calculate sum of a vector with possible NAs which are set to 0, returns 
# NA if all the values are NA
sumNA <- function(x){
  if(all(is.na(x))){
    return(NA)
  }
  else{return(sum(x, na.rm = TRUE))}
}

orderBy <- function(a,b){
  vapply(1:length(b), function(i){
    which(a == b[i])
  },1)
}

magnitudeRound <- function(x, digits){
  
  ncr <- nchar(as.character(floor(x)))
  out <- as.character(round(x))
  for (i in 1:digits){
    idx <- ncr == i
    out[idx] <- as.character(round(x[idx], digits-(i-1)))
  }
  out
}

# Loops over all states to get scales for need metrics, 
# makes a csv of aspect ratios and one for temporals for each state, 
# also creates a dataframe with all important variables from every state.

GetScales <- function(States, KeepModels, KeepScales){
  
  if(KeepScales && file.exists("Scales.rda")){break}
  
  ### Initializing dataframes
  Scales <- list("Raw" = list("Min" = Inf, "Max" = 0), 
                 "Smoothed" = list("Min" = Inf, "Max" = 0))
  
  AllData <- data.frame("geoId" = character(),"Households" = numeric(),
                        "RawNeed" = numeric(), "SmoothNeed" = numeric(),
                        "RawMean" = numeric(), "SmoothMean" = numeric(),
                        "RawCounts" = numeric(), "SmoothCounts" = numeric(),
                        "RawPositive" = numeric(), "Name" = character())
  
  PlotRatios <- data.frame("geoId" = character(),
                            "AspectRatio" = numeric())
  
  Temporals <- data.frame("geoId" = character(),
                          "MinTemporal" = character(),
                          "MaxTemporal" = character())
  
  ### Creating Results directory if it doesn't exist
  if(!dir.exists("Results")){
    dir.create("Results")
  }
  
  ### Looping over all the states
  for(i in 1:nrow(States)){
    
    State <- States$NAME[i]
    print(State)
    if(States$STATEFP[i] %in% c(20, 34)){
      # New Jersey and Kansas 
      ### Creating current state directory within Results if it doesn't exist
      if(!dir.exists(paste0("Results/", State))){
        dir.create(paste0("Results/", State))
      }
      
      ## Raw data
      ### Getting mean data
      Means <- getDatasetState(1132, State) |>
        group_by(geoId, temporal) |> 
        summarize(Mean = sumNA(as.numeric(dataValue)))
      
      ### Getting count data
      CountData <- getDatasetState(743, State)
      
      TotalCounts <- group_by(CountData, geoId, temporal) |> 
        summarize(Total = sumNA(as.numeric(dataValue)))
      
      PositiveCounts <- filter(CountData, groupById == 3)|>
        select(geoId, dataValue, temporal) |>
        mutate(Positive = as.numeric(dataValue), .keep = "unused")
      
      ### Getting number of years
      
      Years <- max(as.numeric(Means$temporal), na.rm = TRUE) - 
        min(as.numeric(Means$temporal), na.rm = TRUE) + 1
      
      ### Combining mean and count datasets and calculating need metric
      Data <- full_join(TotalCounts, PositiveCounts, by = c("geoId", "temporal")) |>
        full_join(Means, by = c("geoId", "temporal")) |>
        group_by(geoId) |>
        summarize(Mean = sumNA(Mean * Total) / sumNA(Total), 
                  Total = sumNA(Total), Positive = sumNA(Positive)) |>
        mutate(TotalScaled=Total/Years*10) |>
        mutate(LowData = (Total <= 10)) |>
        left_join(CensusData, by = c("geoId" = "GEOID")) |>
        mutate(Households = value,
               NeedToTest = Mean * log10(Households/ TotalScaled))|>
        select(-value)
      
      ### Changing min or max raw need metric scales if we have encountered a new min or max
      Scales$Raw$Min <- min(Scales$Raw$Min, min(Data$NeedToTest, na.rm = TRUE), 
                            na.rm = TRUE)
      
      Scales$Raw$Max <- max(Scales$Raw$Max, max(Data$NeedToTest, na.rm = TRUE), 
                            na.rm = TRUE)
      ## Smoothing
      ### Creating adjacency matrix
      AdjacencyDat <- filter(Counties, GEOID %in% Data$geoId) |>
        arrange(factor(GEOID, levels = Data$geoId)) 
      
      AdjacencyMat <- AdjacencyDat |>
        poly2nb() |>
        nb2mat(zero.policy = TRUE)
      colnames(AdjacencyMat) <- rownames(AdjacencyMat) <- AdjacencyDat$NAME
      
      AdjacencyMat <- (AdjacencyMat != 0) + 0
      
      ### Finding counties with 0 neighbors
      
      ind <- apply(AdjacencyMat, 1, function(x){any(x != 0)}) |>
        as.vector()
      
      ### Checking if the results already exist and if they are we read them in unless 
      ### KeepModels is FALSE
      if (file.exists(paste0("./Results/", State, "/", State, ".rda")) && KeepModels){
        load(paste0("./Results/", State, "/", State, ".rda"))
        Summary <- Results$summary
      }else{
        ### Creating data for model fitting
        AdjacencyMat <- AdjacencyMat[ind,ind]
        
        NimbleData <- as.carAdjacency(AdjacencyMat)
        
        ### Creating dataset with only counties that have neighbors
        DataInd <- Data[ind,]
        DataInd <- DataInd[DataInd$geoId %in% AdjacencyDat$GEOID,]
        DataInd <- DataInd[orderBy(DataInd$geoId, AdjacencyDat$GEOID[ind]),]
        
        ### Checks
        if (length(names(NimbleData$num)) != ncol(AdjacencyMat)){
          stop("Dimension mismatch:", State)
        }
        if(!all(names(NimbleData$num) == colnames(AdjacencyMat))){
          stop("Location mismatch:", State)
        }
        if (nrow(DataInd) != ncol(AdjacencyMat)){
          warning("Mismatched dimension - census vs. geometry:", State)
        }
        if (!all(Data$geoId == AdjacencyDat$GEOID)){
          stop(paste0("Location mismatch:", State))
        }
        
        ### Fitting model 
        N <- length(NimbleData$num)
        constants <- NimbleData
        constants$L <- length(NimbleData$adj)
        constants$N <- N
        constants$Households <- DataInd$Households
        constants$expected_overall <- (sum(DataInd$TotalScaled, na.rm=TRUE) / 
                                         sum(DataInd$Households[!is.na(DataInd$TotalScaled)]))*DataInd$Households
        inits <- list(tau1 = 1, tau2 = 1, s1 = rep(0, N), s2 = rep(0, N))
        data <- list(Total = round(as.numeric(DataInd$TotalScaled)), Mean = as.numeric(DataInd$Mean))

        for (itrs in c(10000,20000,50000)){
          
          Results <- nimbleMCMC(code, constants, data, inits, nchains = 3, niter = itrs, 
                                summary = TRUE, WAIC = FALSE, 
                                monitors = c("MeanEst", "TotEst", "NeedToTest"))
          
          mcl <- as.mcmc.list(
            lapply(Results$samples, function(cn){
              as.mcmc(log(cn[,startsWith(colnames(cn), "MeanEst") | startsWith(colnames(cn), "TotEst")]))
            }
            ))
          # Transform on log scale
          gd <- gelman.diag(mcl, multivariate = FALSE)
          if (max(gd$psrf[,1]) <= 1.2){
            break
          }
        }
        if (max(gd$psrf[,1]) > 1.2){
          stop("State: ", State, " did not satisfactorally converge for spatial model.")
        }
        
        save(list = c("Results"), file = paste0("./Results/", State, "/", State, ".rda"), compress = "bzip2")
      }
      
      ### Getting smoothed values
      ### Smoothed values for counties with 0 neighbors are their raw values
      SmoothNeedToTest <- Data$NeedToTest
      
      combined_chains <- Reduce("rbind", lapply(Results$samples, function(smpl){smpl[(floor(nrow(smpl)/2):nrow(smpl)),]}))
      
      SmoothNeedToTest[ind] <- apply(combined_chains[,startsWith(colnames(combined_chains), "NeedToTest")], 2, median)
      SmoothMean <- Data$Mean
      
      SmoothMean[ind] <- apply(combined_chains[,startsWith(colnames(combined_chains), "MeanEst")], 2, median)
      
      SmoothCounts <- Data$TotalScaled
      
      SmoothCounts[ind] <- apply(combined_chains[,startsWith(colnames(combined_chains), "TotEst")], 2, median)
      
      Scales$Smoothed$Min <- min(Scales$Smoothed$Min, min(SmoothNeedToTest, na.rm = TRUE), 
                                 na.rm = TRUE)
      
      Scales$Smoothed$Max <- max(Scales$Smoothed$Max, max(SmoothNeedToTest, na.rm = TRUE), 
                                 na.rm = TRUE)
      
      ## Updating dataframes
      ### Adding current temporals to the overall temporals dataframe
      Temporals <- bind_rows(Temporals, 
                             data.frame(geoId = States$STATEFP[i], 
                                        MinTemporal = min(Means$temporal, na.rm = TRUE),
                                        MaxTemporal = max(Means$temporal, na.rm = TRUE)))
      
      ### Adding current results to the overall dataframe
      Data <- mutate(Data, RawNeed = NeedToTest, SmoothNeed = SmoothNeedToTest, 
                     RawCounts = Total, RawCountsScaled = TotalScaled, 
                     RawMean = Mean, Households = Households,
                     geoId = geoId,
                     SmoothMean = SmoothMean, SmoothCounts = SmoothCounts, 
                     RawPositive = Positive, Name = NAME,
                     .keep = "none")
      
      AllData <- bind_rows(AllData, Data)
      
      ### Adding current aspect ratio to overall aspect ratio dataframe
      ### Aspect ratios are used to create the maps
      PlotRatios <- bind_rows(PlotRatios,
                              data.frame(geoId = States$STATEFP[i], 
                                         AspectRatio =  filter(Counties, 
                                                               STATEFP == States$STATEFP[i])|>
                                                        get_asp_ratio()))
      
    }else if(States$STATEFP[i] %in% c(15, 28)){
      # Hawaii and Mississippi have no data
    }else if(States$STATEFP[i] == 11){
      # Washington DC
      ### Creating current state directory within Results if it doesn't exist
      if(!dir.exists(paste0("Results/", State))){
        dir.create(paste0("Results/", State))
      }
      ## Raw data
      ### Getting mean data
      Means <- getDataset(971, State, "2017") |>
        mutate(Mean = as.numeric(dataValue), geoId = geoId, 
               temporal = temporal, minimumTemporal = minimumTemporal,
               .keep = "unused") 
      
      ### Getting count data
      CountData <- getDataset(866, State, "2017")
      
      TotalCounts <- group_by(CountData, geoId, temporal, minimumTemporal) |> 
        summarize(Total = sumNA(as.numeric(dataValue)))
      
      PositiveCounts <- filter(CountData, groupById == 3)|>
        select(geoId, dataValue, temporal, minimumTemporal) |>
        mutate(Positive = as.numeric(dataValue), .keep = "unused")
      
      ### Getting number of years
      
      Years <- max(as.numeric(Means$temporal), na.rm = TRUE) - 
        min(as.numeric(Means$minimumTemporal), na.rm = TRUE) + 1
      
      ### Combining mean and count datasets and calculating need metric
      Data <- full_join(TotalCounts, PositiveCounts, 
                        by = c("geoId", "temporal", "minimumTemporal")) |>
        full_join(Means, by = c("geoId", "temporal", "minimumTemporal")) |>
        select(Mean, Positive, Total, geoId, temporal, minimumTemporal) |>
        left_join(CensusData, by = c("geoId" = "GEOID")) |>
        mutate(TotalScaled=Total/Years*10) |>
        mutate(Households = value, 
               NeedToTest = Mean * log10(Households/ TotalScaled)) |>
        select(-value)
      
      ### Changing min or max need metric scales if we have encountered a new min or max
      ### DC is only 1 county, so the smoothed values are the raw values
      Scales$Raw$Min <- min(Scales$Raw$Min, Data$NeedToTest, 
                            na.rm = TRUE)
      
      Scales$Raw$Max <- max(Scales$Raw$Max, Data$NeedToTest, 
                            na.rm = TRUE)
      
      Scales$Smoothed$Min <- min(Scales$Smoothed$Min, Data$NeedToTest, 
                                 na.rm = TRUE)
      
      Scales$Smoothed$Max <- max(Scales$Smoothed$Max,  Data$NeedToTest, 
                                 na.rm = TRUE)
      
      ## Updating dataframes
      ### Adding current temporals to the overall temporals dataframe
      Temporals <- bind_rows(Temporals, 
                             data.frame(geoId = States$STATEFP[i], 
                                        MinTemporal = unique(Data$minimumTemporal),
                                        MaxTemporal = unique(Data$temporal)))
      
      ### Adding current results to the overall dataframe
      Data <- mutate(Data, RawNeed = NeedToTest, SmoothNeed = NeedToTest, 
                     RawCounts = Total, RawCountsScaled=TotalScaled, RawMean = Mean, Households = Households,
                     geoId = geoId,
                     SmoothMean = Mean, SmoothCounts = TotalScaled, 
                     RawPositive = Positive, Name = NAME,
                     .keep = "none")
      
      AllData <- bind_rows(AllData, Data)
      
      ### No maps are made for DC, so there is no aspect ratio
    }else{
      ### Creating current state directory within Results if it doesn't exist
      if(!dir.exists(paste0("Results/", State))){
        dir.create(paste0("Results/", State))
      }
      
      ## Raw data
      ### Getting mean data from 2008-2017
      Means <- getDataset(971, State, "2017") |> 
        mutate(geoId = case_when(
          geoId == "46113" ~ "46102",
          geoId == "51515" ~ "51019",
          geoId == "02270" ~ "02158", 
          TRUE ~ geoId)) |>
        group_by(geoId, temporal, minimumTemporal) |> 
        summarize(Mean = sumNA(as.numeric(dataValue)))
      
      ### Get Years Covered
      
      Years <- max(as.numeric(Means$temporal), na.rm = TRUE) - 
        min(as.numeric(Means$minimumTemporal), na.rm = TRUE) + 1
      
      ### Getting count data
      CountData <- getDataset(866, State, "2017")|> 
        mutate(geoId = case_when(
          geoId == "46113" ~ "46102",
          geoId == "51515" ~ "51019",
          geoId == "02270" ~ "02158", 
          TRUE ~ geoId))
      
      TotalCounts <- group_by(CountData, geoId, temporal, minimumTemporal) |> 
        summarize(Total = sumNA(as.numeric(dataValue))) |>
        mutate(LowData = (is.na(Total) || (Total <= 10)))
      
      PositiveCounts <- filter(CountData, groupById == 3)|>
        select(geoId, dataValue, temporal, minimumTemporal) |>
        group_by(geoId, temporal, minimumTemporal) |>
        summarize(dataValue = sumNA(as.numeric(dataValue)))
      
      ### Combining mean and counts data and calculating need metric
      Data <- full_join(Means, TotalCounts, by = c("geoId","temporal", "minimumTemporal")) |>
        full_join(PositiveCounts, by = c("geoId","temporal", "minimumTemporal")) |>
        left_join(CensusData, by = c("geoId" = "GEOID")) |>
        mutate(TotalScaled=Total/Years*10) |>
        mutate(Positive = dataValue, Households = value, 
               NeedToTest = Mean * log10(Households/ TotalScaled)) |>
        select(-value, -dataValue) |>
        ungroup()
      
      ### Changing min or max raw need metric scales if we have encountered a new min or max
      Scales$Raw$Min <- min(Scales$Raw$Min, min(Data$NeedToTest, na.rm = TRUE), 
                            na.rm = TRUE)
      
      Scales$Raw$Max <- max(Scales$Raw$Max, max(Data$NeedToTest, na.rm = TRUE), 
                            na.rm = TRUE)
      
      
      ## Smoothing
      ### Creating adjacency matrix
      AdjacencyDat <- filter(Counties, GEOID %in% Data$geoId) |>
        arrange(factor(GEOID, levels = Data$geoId)) 
      
      AdjacencyMat <- AdjacencyDat |>
        poly2nb() |>
        nb2mat(zero.policy = TRUE)
      colnames(AdjacencyMat) <- rownames(AdjacencyMat) <- AdjacencyDat$NAME
      
      AdjacencyMat <- (AdjacencyMat != 0) + 0
      
      ### Finding counties with 0 neighbors
      ind <- apply(AdjacencyMat, 1, function(x){any(x != 0)}) |>
        as.vector()
      
      ### Checking if the results already exist and if they are we read them in unless 
      ### KeepModels is FALSE
      if(file.exists(paste0("./Results/", State, "/", State, ".rda")) && KeepModels){
        load(paste0("./Results/", State, "/", State, ".rda"))
        Summary <- Results$summary
      }else{
        ### Creating data for fitting model
        AdjacencyMat <- AdjacencyMat[ind,ind]
        NimbleData <- as.carAdjacency(AdjacencyMat)
        
        ### Creating dataset with only counties that have neighbors
        DataInd <- Data[ind,]
        DataInd <- DataInd[DataInd$geoId %in% AdjacencyDat$GEOID,]
        DataInd <- DataInd[orderBy(DataInd$geoId, AdjacencyDat$GEOID[ind]),]
        
        ### Checks
        if (length(names(NimbleData$num)) != ncol(AdjacencyMat)){
          stop("Dimension mismatch:", State)
        }
        if(!all(names(NimbleData$num) == colnames(AdjacencyMat))){
          stop("Location mismatch:", State)
        }
        if (nrow(DataInd) != ncol(AdjacencyMat)){
          warning("Mismatched dimension - census vs. geometry:", State)
        }
        if (!all(Data$geoId == AdjacencyDat$GEOID)){
          stop(paste0("Location mismatch:", State))
        }
        
        ### Fitting model
        N <- length(NimbleData$num)
        constants <- NimbleData
        constants$L <- length(NimbleData$adj)
        constants$N <- N
        constants$Households <- DataInd$Households
        constants$expected_overall <- (sum(DataInd$TotalScaled, na.rm=TRUE) / 
                                         sum(DataInd$Households[!is.na(DataInd$TotalScaled)]))*DataInd$Households
        inits <- list(tau1 = 1, tau2 = 1, s1 = rep(0, N), s2 = rep(0, N))
        data <- list(Total = round(as.numeric(DataInd$TotalScaled)), Mean = as.numeric(DataInd$Mean))
        
        
        # Try a few number of samples until convergence
        
        for (itrs in c(10000,20000,50000)){
        
          Results <- nimbleMCMC(code, constants, data, inits, nchains = 3, niter = itrs, 
                                summary = TRUE, WAIC = FALSE, 
                                monitors = c("MeanEst", "TotEst", "NeedToTest"))
          
          mcl <- as.mcmc.list(
            lapply(Results$samples, function(cn){
            as.mcmc(log(cn[,startsWith(colnames(cn), "MeanEst") | startsWith(colnames(cn), "TotEst")]))
            }
            ))
          # Transform on log scale
          gd <- gelman.diag(mcl, multivariate = FALSE)
          if (max(gd$psrf[,1]) <= 1.2){
            break
          }
        }
        if (max(gd$psrf[,1]) > 1.2){
          stop("State: ", State, " did not satisfactorally converge for spatial model.")
        }
        save(list = c("Results"), file = paste0("./Results/", State, "/", State, ".rda"), compress = "bzip2")
        
      }
      ### Getting smoothed values
      ### Smoothed vales for counties with 0 neighbors are their raw values
      SmoothNeedToTest <- Data$NeedToTest
      
      combined_chains <- Reduce("rbind", lapply(Results$samples, function(smpl){smpl[(floor(nrow(smpl)/2):nrow(smpl)),]}))

      SmoothNeedToTest[ind] <- apply(combined_chains[,startsWith(colnames(combined_chains), "NeedToTest")], 2, median)
      SmoothMean <- Data$Mean

      SmoothMean[ind] <- apply(combined_chains[,startsWith(colnames(combined_chains), "MeanEst")], 2, median)
      
      SmoothCounts <- Data$TotalScaled
      
      SmoothCounts[ind] <- apply(combined_chains[,startsWith(colnames(combined_chains), "TotEst")], 2, median)
      
      Scales$Smoothed$Min <- min(Scales$Smoothed$Min, min(SmoothNeedToTest, na.rm = TRUE), 
                                 na.rm = TRUE)
      
      Scales$Smoothed$Max <- max(Scales$Smoothed$Max, max(SmoothNeedToTest, na.rm = TRUE), 
                                 na.rm = TRUE)
      
      # Updating dataframes
      ### Adding current temporals to the overall temporals dataframe
      Temporals <- bind_rows(Temporals, 
                             data.frame(geoId = States$STATEFP[i], 
                                        MinTemporal = unique(Data$minimumTemporal),
                                        MaxTemporal = unique(Data$temporal)))
      
      ### Adding current results to the overall dataframe
      Data <- mutate(Data, RawNeed = NeedToTest, SmoothNeed = SmoothNeedToTest, 
                     RawCounts = Total,RawCountsScaled=TotalScaled, RawMean = Mean, Households = Households,
                     geoId = geoId,
                     SmoothMean = SmoothMean, SmoothCounts = SmoothCounts, 
                     RawPositive = Positive, Name = NAME,
                     .keep = "none")
      
      AllData <- bind_rows(AllData, Data)
      
      ### Adding current aspect ratio to the overall aspect ratio dataframe
      ### Aspect ratios are used to create the maps
      PlotRatios <- bind_rows(PlotRatios,
                              data.frame(geoId = States$STATEFP[i], 
                                         AspectRatio = filter(Counties, 
                                                              STATEFP == States$STATEFP[i])|>
                                                       get_asp_ratio()))
    }
  }
  ### Storing current date
  DateAccessed <- Sys.Date()
  
  ### Rescaling the smoothed need metric so that outliers play less of a role
  Scales$Smoothed$Max <- Scales$Smoothed$Max * .25
  
  ## Saving results
  save(list = c("Scales"), 
       file = paste0("./Scales.rda"), 
       compress = "bzip2")
  
  save(list = c("AllData"), 
       file = paste0("./AllData.rda"), 
       compress = "bzip2")
  
  save(list = ("DateAccessed"),
       file = "./DateAccessed.rda")
  
  write.csv(PlotRatios, file = "PlotRatios.csv", row.names = F)
  
  write.csv(Temporals, file = "Temporals.csv", row.names = F)
}


# One-time setup code
if (!exists("CensusData")){
  if (exists("GLOBAL_CENSUS_API_KEY")){
    census_api_key(GLOBAL_CENSUS_API_KEY)  
  } else{
    stop("Need to set GLOBAL_CENSUS_API_KEY")
  }
  
    
  CensusData <- get_estimates(geography = "county", product = "housing") |>
    select(GEOID, value, NAME) |> 
    mutate(NAME = gsub("(.+),.+", "\\1", NAME, ignore.case = TRUE))

  # Code for nimble model
  nlog10 <- nimbleFunction(run= function(X=double(0)){
    return(log(X)/log(10))
   returnType(double(0))})
  
  code <- nimbleCode({
    tau1 ~ dgamma(.001, .001)
    tau2 ~ dgamma(.001, .001)
    
    s1[1:N] ~ dcar_normal(adj[1:L], weights[1:L], num[1:N], tau1)
    s2[1:N] ~ dcar_normal(adj[1:L], weights[1:L], num[1:N], tau2)
    
    for (i in 1:N) {
      ### Total tests
      TotEst[i] <- exp(s2[i] + log(expected_overall[i]))
      Total[i] ~ dpois(TotEst[i])
      
      ### Means
      MeanEst[i] <- exp(s1[i])
      Mean[i] ~ dnorm(MeanEst[i], sd = MeanEst[i] / sqrt(TotEst[i]))
      
      
      NeedToTest[i] <- MeanEst[i] * nlog10(Households[i] / TotEst[i])
      
      
    }
  }
  )
  
  code_nonspatial <- nimbleCode({
    tau1 ~ dgamma(.001, .001)
    tau2 ~ dgamma(.001, .001)
    mu1 ~ dnorm(0,sd=1)
    mu2 ~ dnorm(0,sd=1)
    
    for (i in 1:N) {
      s1[i] ~ dnorm(mu1, sd = tau1)
      s2[i] ~ dnorm(mu2, sd = tau2)
      ### Total tests
      TotEst[i] <- exp(s2[i] + log(expected_overall[i]))
      Total[i] ~ dpois(TotEst[i])
      
      ### Means
      MeanEst[i] <- exp(s1[i])
      Mean[i] ~ dnorm(MeanEst[i], sd = MeanEst[i] / sqrt(TotEst[i]))
      
      NeedToTest[i] <- MeanEst[i] * nlog10(Households[i] / TotEst[i])
    }
  }
  )
}





