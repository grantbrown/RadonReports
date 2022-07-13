library(ggplot2)
library(tigris)
library(kableExtra)

load("AllData.rda")
load("Scales.rda")

ChangeScales <- FALSE

if(ChangeScales){
  Scales$Smoothed$Max <- Scales$Smoothed$Max * .25
  save(list = c("Scales"), 
       file = paste0("./Scales.rda"), 
       compress = "bzip2")
}

AllData <- mutate(AllData, RawNeed = 100 * (RawNeed - Scales$Raw$Min) / 
                                           (Scales$Raw$Max - Scales$Raw$Min),
                  SmoothNeed = 100 * (SmoothNeed - Scales$Smoothed$Min) / 
                    (Scales$Smoothed$Max - Scales$Smoothed$Min))

sumNA <- function(x){
  if(all(is.na(x))){
    return(NA)
  }
  else{return(sum(x, na.rm = TRUE))}
}

# Plots

## Means

ggplot(AllData, aes(x = RawMean)) + geom_histogram(fill = "deepskyblue") + 
  labs(x = "Mean Radon Levels")

ggplot(AllData, aes(x = SmoothMean)) + geom_histogram(fill = "deepskyblue") + 
  labs(x = "Smoothed Mean Radon Levels")

## Need to test metrics

ggplot(AllData, aes(x = RawNeed)) + geom_histogram(fill = "deepskyblue") + 
  labs(x = "Need to Test Metric")

ggplot(filter(AllData, RawNeed > 20), aes(x = RawNeed)) + 
  geom_histogram(fill = "deepskyblue") + 
  labs(x = "Need to Test Metric")

ggplot(AllData, aes(x = SmoothNeed)) + geom_histogram(fill = "deepskyblue") + 
  labs(x = "Smoothed Need to Test Metric")

# Ranking states

## Creating dataset

States <- states(cb = T, class = "sf") |>
  shift_geometry(position = "below")|> 
  st_transform(3857) |>
  filter(!STATEFP %in% c(78, 69, 66, 60, 72, 11)) |>
  select(NAME, STATEFP)

TableData <- mutate(AllData, STATEFP = substr(geoId, 1, 2)) |>
             group_by(STATEFP)|>
             summarize(across(c(RawNeed, SmoothNeed), 
                            list("Median" = median), na.rm = TRUE),
                       across(c(Households, RawCounts, SmoothCounts, Positive), 
                              list("Sum" = sumNA)),
                       RawMean = sumNA(RawMean * RawCounts) / sumNA(RawCounts),
                       SmoothMean = sumNA(SmoothMean * SmoothCounts) / sumNA(SmoothCounts)) |>
             inner_join(States, by = "STATEFP")

## Making tables

### Ranking by raw need

RawNeedTable <- arrange(TableData, RawNeed_Median) |>
                mutate(Rank = row_number()) |>
                select(NAME, Rank, RawNeed_Median)

kbl(RawNeedTable, col.names = c("State", "Rank", "Median Need Metric"), digits = 3) |>
  kable_classic_2(lightable_options = "striped", full_width = FALSE)

### Ranking by smoothed need

SmoothNeedTable <- arrange(TableData, SmoothNeed_Median) |>
  mutate(Rank = row_number()) |>
  select(NAME, Rank, SmoothNeed_Median)

kbl(SmoothNeedTable, col.names = c("State", "Rank", "Median Smoothed <br/>Need Metric"), 
    escape = F, digits = 3) |>
  kable_classic_2(lightable_options = "striped", full_width = FALSE)

### Ranking by raw mean

RawMeanTable <- arrange(TableData, RawMean) |>
  mutate(Rank = row_number()) |>
  select(NAME, Rank, RawMean)

kbl(RawMeanTable, col.names = c("State", "Rank", "Mean Radon Level"), digits = 3) |>
  kable_classic_2(lightable_options = "striped", full_width = FALSE)

### Ranking by smoothed mean

SmoothMeanTable <- arrange(TableData, SmoothMean) |>
  mutate(Rank = row_number()) |>
  select(NAME, Rank, SmoothMean)

kbl(SmoothMeanTable, col.names = c("State", "Rank", "Smoothed  Mean<br/>Radon Level"), 
    escape = F, digits = 3) |>
  kable_classic_2(lightable_options = "striped", full_width = FALSE)

# Making histogram files

Original <- ggplot(AllData, aes(x = SmoothNeed / 4)) + geom_histogram(fill = "deepskyblue") + 
  labs(x = "Smoothed Need to Test Metric")

Modified <- ggplot(AllData, aes(x = ifelse(SmoothNeed > 100, 100, SmoothNeed))) + 
  geom_histogram(fill = "deepskyblue") + 
  labs(x = "Smoothed Need to Test Metric")

pdf("OriginalGlobalScale.pdf", width = 6, height = 6)
print(Original)
dev.off()

pdf("ModifiedGlobalScale.pdf", width = 6, height = 6)
print(Modified)
dev.off()

