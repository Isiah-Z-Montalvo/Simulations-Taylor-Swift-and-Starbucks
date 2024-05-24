---
author: Isiah Montalvo
date: 2/27/2022
output: html_document
title: Final
---

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

## Instructions

-   You can use the packages `maps`, `dplyr`, `ggplot2` and any packages
    in `Tidyverse`. If you wish to use any additional packages beyond
    "Base R" (the default) and these packages email Rebecca
    (`rkurt001@ucr.edu`) for permission.
-   Use R to answer the questions below.
-   Check Piazza regularly for clarification on questions, there may be
    important posts that will not be announced through *Elearn*.\
-   You can use any iteration technique to answer the prompts below.
-   You are allowed to use outside resources to help with understanding,
    but all work you submit MUST BE YOUR OWN, do not copying things from
    outside sources. In addition, for this assignment there is
    absolutely no communicating with other humans. This includes people
    from class, and online forums (StackOverflow, Chegg, etc.).\
-   Email questions to Jericho (jlaws011@ucr.edu ) or Rebecca
    (rkurt001@ucr.edu).

## Cumulative-Proportion/Cumulative-Win Plots

Sometimes it is helpful to track things over time, or visualize how the
estimated probability of an event changes as we consider more data. We
can visualize this using a cumulative proportion plot. For example,
suppose we want to estimate how often we can expect a coin to land on a
"head". We can flip a coin multiple times and record a "1" if it land on
a head, and "0" otherwise. Then we can calculate the proportion of the
times that the coin landed on a head, which is the (arithmetic) mean of
the 0s and 1s. If we only flip the coin a few times, then the proportion
that we calculate may not be accurate due to random fluctuation.
However, the more coin flips we consider the more likely the proportion
we calculate approaches the truth. We can observe this using a
cumulative proportion plot. In a cumulative proportion plot we have the
number of trials on the x-axis, and the estimated proportion of
successes using all data up to trial "x" on the y-axis. For example,
suppose we flip one coin five times and obtain the results 1, 1, 0, 1,
0. Then the proportion of successes using "x" number of trials is the
following.

-   When the number of trials is x = 1, the proportion of times we
    observe a head is y = (1)/1 = 1
-   When the number of trials is x = 2, the proportion of times we
    observe a head is y = (1+1)/2 = 1
-   When the number of trials is x = 3, the proportion of times we
    observe a head is y = (1+1+0)/3 = 2/3
-   When the number of trials is x = 4, the proportion of times we
    observe a head is y = (1+1+0+1)/4 = 3/4
-   When the number of trials is x = 5, the proportion of times we
    observe a head is y = (1+1+0+1+0)/5 = 3/5

An example of a running average for this particular case is posted on
*Elearn*. You can also see an example posted here:
http://www.rossmanchance.com/applets/2021/randombabies/RandomBabies.html.
When using cumulative proportion plots for something that results in a
success or a failure, like the coin flip example, we sometimes call this
a "cumulative win plot".

## Running-Average Plot

A running average plot is the same basic idea, but instead of
considering 1s and 0s for wins and loses, we can consider different
numbers. For example, in statistics when calculating the (arithmetic)
mean, we believe that in general if we use more data our estimated mean
will be more accurate. For instance, suppose we sample the height of UCR
students. The (arithmetic) mean calculated with 50 observations is
likely more accurate the (arithmetic) mean of 2 observations. We can
illustrate this with a running average plot. Just like the cumulative
proportion plot above, the x-axis is integers between 1 and the maximum
number of observations we consider. On the y-axis we have the calculated
(arithmetic) mean for the first `x` number of observations.

## Questions

``` {r}
library(tidyverse)
library(maps)
```

**Question 1: Simulation Recreation (12 points)**

For this question we will be recreating parts of the simulation from
this
[website](http://www.rossmanchance.com/applets/2021/randombabies/RandomBabies.html).
In this simulation there is a hospital in which a few babies are all
born at the same time. The hospital mixes them up, and the families go
home with a randomly selected baby. The focus is studying the
number/proportion of babies that go to the correct home under various
situations.

1a) (2 point) Create a function with one input:

\-`babies`: Numeric vector of length 1. Represents how many babies are
at the hospital.

In this function generate a vector that has elements (1, 2, 3, 4, ...,
`babies`) in a random order. Let (1, 2, 3, ..., `babies`) represent the
true order. Count how many elements in the random vector match the true
order and have your function return this number.

For example, suppose `babies = 4`, and a simulation generates the order:
1, 3, 2, 4. Then the function should return 2 because the first and last
baby were correctly matched.

``` {r}
set.seed(400)
babySimulation <- function(babies){
  randOrder <- sample(1:babies, babies, replace = FALSE)
  trueOrder <- c(1:babies)
  matches <- 0
  for (x in 1:babies){
    if (trueOrder[x] == randOrder[x]){
      matches <- matches + 1
    } else{
      next
    }
  }
  return(matches)
}

babySimulation(4)
```

1b) (3 points) Create a function with two inputs: `babies` and `trial`.
In this function we call our function in Q1a `trial` times, where each
trial has the same number of babies (`babies`). Have this function
return a *named list* with three elements:

-   `Data`: a numeric vector of length `trial` where each element
    contains the number of matches for a particular trial.

-   `CumulativeResults`: a data frame with three columns, "Matches",
    "Count", and "Prop", which resembles that found on this
    [website](http://www.rossmanchance.com/applets/2021/randombabies/RandomBabies.html)

-   `Parameters`: a *named* vector of length 2 that contains `babies`
    and `trial`.

``` {r}
set.seed(400)
dataSimulation <- function(babies, trial){
  data <- replicate(trial, babySimulation(babies))
  matches <- c(1:babies)
  count <- c()
  count[1] <- length(data[data == 0])
  for (x in matches){
    count[x+1] <- length(data[data == x])
  }
  
  cumulativeResults <- data.frame(Matches = 0:babies, Count = count, Prop = count/trial)
  parameters <- c(babies, trial)
  names(parameters) <- c("Babies", "Trials")
  
  lst <- list(data, cumulativeResults, parameters)
  names(lst) <- c("Data", "Cumulative Results", "Parameters")
  
  return(lst)
}

dataSimulation(4, 4)
```

1c) (3 points) Create a function that has the following three arguments.
In this function you will either create a cumulative proportion plot for
a particular number of babies that went to the right home, OR a running
average plot (arithmetic mean number of babies that went to the right
home). The plot you create corresponds to the plot(s) in the lower right
of this
[website](http://www.rossmanchance.com/applets/2021/randombabies/RandomBabies.html).

-   `Data`: A numeric vector. This vector should contain the results
    that corresponds to the first element of the returned list from the
    function in Q1b.

-   `PlotType`: A character vector of length 1. If "average" (default),
    then plot a cumulative average plot for the `Data` (the first
    argument for this function). If "relative frequency", then plot the
    cumulative proportion plot for when `Matches` occurs.

-   `Matches`: A numeric vector of length 1. Contains the scenario to
    calculate the relative frequency plot if
    `PlotType = "relative frequency"` is selected. Can be any whole
    number between 0 and `babies` for the given scenario.

HINT: A loop could be useful for creating the data to be plotted.

``` {r}
set.seed(400)
plotSimulation <- function(data, plotType, matches){
  trls <- c(1:length(data)) 
  if (plotType == "average"){
    cumAvg <- cummean(data)
    cumAvgData <- as_tibble(cumAvg)
    cumAvgData <- add_column(cumAvgData, trials = trls)
    tbl <- ggplot(cumAvgData, aes(x = trials, y = value)) + geom_point() + expand_limits(x = 0, y = 0) + labs(x = "Number of Trials", y = "Average Matches")
  } else if (plotType == "relative frequency"){
    relFreq <- c()
    count <- 0
    for (x in trls){
      if (data[x] == matches){
        count <- count + 1
        relFreq[x] <- count / x
      } else{
        relFreq[x] <- count / x
      }
    }
    relFreqData <- as_tibble(relFreq)
    relFreqData <- add_column(relFreqData, trials = trls)
    tbl <- ggplot(relFreqData, aes(x = trials, y = value)) + geom_point() + expand_limits(x = 0, y = 0) + labs(x = "Number of Trials", y = "Relative Frequency")
  }
  return(tbl)
}

tst <- dataSimulation(4, 4)
dta <- unname(unlist(tst[1]))
plotSimulation(dta, "average", 2)
plotSimulation(dta, "relative frequency", 2)
```

1d) (2 points) Use your functions above to generate 500 trials for when
there are 7 babies. Output/Display:

-   `CumulativeResults` table for your trials

-   The cumulative average plot

-   The relative frequency plot for when there are exactly two matches.

``` {r}
set.seed(400)
simulationData <- dataSimulation(7, 500)
data <- unname(unlist(simulationData[1]))
simulationData[2]
plotSimulation(data, "average", 2)
plotSimulation(data, "relative frequency", 2)
```

1e) (2 points) Create a bar chart using that corresponds to your results
in Q1d. It should somewhat resemble the plot in the lower left corner
from this
[website](http://www.rossmanchance.com/applets/2021/randombabies/RandomBabies.html).
It should be professional, have an appropriate title, labels, colors,
etc. Change the color scheme to be something other than the default. You
can use `ggplot` or base R.

``` {r}
df <- as.data.frame(simulationData[2])
barPlt <- ggplot(df, aes(x = factor(Cumulative.Results.Matches), y = Cumulative.Results.Count, fill = Cumulative.Results.Matches)) + geom_col() + labs(title = "Frequency Of Babies Successfully Matched Over 500 Trials", x = "Matches", y = "Number of Matches")
barPlt
```

**Question 2: Taylor Swift Albums (9 points)**

Load the data set `TaylorSwift.csv` from *ELearn*. This is a *modifed*
data set from
<https://www.kaggle.com/thespacefreak/taylor-swift-song-lyrics-all-albums>.
Each row corresponds to a particular line/lyric for a particular song
and album by Taylor Swift.

``` {r}
tswift <- read.csv("C:\\Users\\Swift\\Downloads\\TaylorSwift.csv")
head(tswift)
```

2a) (2 points) Create a 2D object (data frame or tibble) that has the
following columns for each track for each album (that is, each row
corresponds to a particular track in a particular album). Have this data
set contain the following columns:

-   `Album`: The name of the album the track is on

-   `TrackTitle`: The name of the given track

-   `TrackNum`: Which track number this is for the given album

-   `NumLines`: Total number of lines for the given track

Organize the 2D object by album name, and then by track number. Display
the first few rows.

``` {r}
grpSwift <- tswift %>% group_by(track_title) %>% slice_max(n = 1, line)
tswiftTibble <- grpSwift %>% select(album_name, track_title, track_n, line) %>% arrange(album_name) %>% relocate(track_n, .before = track_title) %>% distinct(album_name, track_n, track_title, line) %>% arrange(album_name, track_n)
head(tswiftTibble)
```

2b) (3 points) Create a subset of all rows of the data set created in
Q2a where the track titles are also the album title. Show how to do this
using THREE different techniques (i.e. indexing, tidyverse, apply
functions, loops, subset(), etc.). NOTE: You are allowed to use multiple
different indexing techniques.

``` {r}
indexing <- tswiftTibble[tswiftTibble$album_name == tswiftTibble$track_title, ]
indexing

subsetting <- subset(tswiftTibble, tswiftTibble$album_name == tswiftTibble$track_title)
subsetting

filtering <- tswiftTibble %>% filter(album_name == track_title)
filtering
```

2c) (2 points) Create a plot for the data set created in Q2a. Have this
plot display the relationship between the total number of lines for each
track, track number, and album. Your plot should be professional, and
have appropriate labels/scales/colors/legends.

``` {r}
tswiftPlot <- ggplot(tswiftTibble, aes(x = line, y = album_name, fill = track_n)) + geom_col() + labs(title = "Relationship between Albums, Track numbers, and amount of Lines", x = "Total number of lines for each track per album", y = "Album Names")
tswiftPlot
```

2d) (2 point) Describe what you plotted in Q2c in detail. What type of
plot did you create? If there are axis, describe them. If there is a
color scheme, or other aspects of the plot that change by a variable,
describe them as well.

The type of plot I made is a horizontal boxplot using ggplot. The y-
axis represents all of the album names present within the dataset while
the x-axis represents the total number of lines present throughout each
album. This relationship is further supported by the color scheme which
depicts the relationship between the track numbers of each album and how
many lines they contribute to the total album line count. This is
represented within the graph with some albums having a higher
contribution rate from the 1st half of the album such as reputation and
others by the second half of the album like fearless.

**Question 3: Getting Familiar with the Starbucks Data Set (10 points)
**

For this question you need the `Starbucks.csv` data set on ELearn. This
data set comes from
[Kaggle.com](https://www.kaggle.com/starbucks/store-locations) and
contains general information about all the Starbucks locations worldwide
as of February 2017.

``` {r}
starbucks <- read.csv("C:\\Users\\Swift\\Downloads\\starbucks.csv")
```

3a) (1 point) Create a 2D object (data frame or tibble) called
`usa_starbucks` that only contains the Starbucks locations that are in
the United States. Output the first few observations of this tibble.
HINT: Check the columns `Brand`, AND `Country`.

``` {r}
usa_starbucks <- starbucks %>% filter(Country == "US")
head(usa_starbucks)
```

3b) (2 points) Use `map()` function for the `maps` package to create a
plot of all the Starbucks locations that are in the United States (you
do not have to consider Alaska or Hawaii). Have the points on the map
differ according to the `Ownership.Type` column. Include a legend in
your plot.

``` {r}
map("state")

points(usa_starbucks$Longitude, usa_starbucks$Latitude, col = unique(factor(usa_starbucks$OwnershipType)))

legend(x = "bottomleft", legend = unique(factor(usa_starbucks$OwnershipType)), fill = unique(factor(usa_starbucks$OwnershipType)))
```

3c) (3 points) Create a 2D object (data frame or tibble) where each row
corresponds to a state in the `usa_starbucks` tibble, and has the
following columns: - City name for the most amount of Starbucks
locations in the state. - (arithmetic) mean longitude values for the
given city - (arithmetic) mean latitude values for the given city -
total number of Starbucks locations for the given city. There should be
a total of 51 rows when we count all states and Washington DC.

``` {r}
tibby1 <- usa_starbucks %>% group_by(City) %>% summarise_at(vars(Longitude, Latitude), mean)
tibby2 <- usa_starbucks %>% group_by(StateProvince) %>% count(City) %>% slice_max(n = 1, n) %>%  distinct(StateProvince, .keep_all = TRUE)
starbucksTibble <- left_join(tibby2, tibby1, by = 'City')
starbucksTibble <- relocate(starbucksTibble, n, .after = Latitude)
starbucksTibble <- rename(starbucksTibble, c(StarbucksCapital = City, AverageLongitude = Longitude, AverageLatitude = Latitude, TotalStarbucksLocations = n))
starbucksTibble
```

3d) (2 points) Plot the longitude and latitude coordinates found in the
previous problem using the `maps` package. The following code is the
longitude and latitude coordinates for each state capital. Plot the
state capital points below as well, but use a different color or shape.
Make sure there is a legend for your plot.

This will create a map of the capitals for each state, and the
"Starbucks Capital" for each state.

``` {r}
# Coordinates for US State Capitals 
us_cities <- us.cities[us.cities$capital==2, ]
map("state")
points(us_cities$long, us_cities$lat, pch = 22, col = "blue")
points(starbucksTibble$AverageLongitude, starbucksTibble$AverageLatitude, pch = 24, col = "purple")

legend(x = "bottomleft", legend = c("State Capital", "Starbucks Capital"), fill = c("blue", "purple"))
```

3e) (2 points) Generate a plot of your choice that compares
`OwnershipType` between `USA` and another country of your choice. (HINT:
A `geom_bar()` using `ggplot`, or a pie chart might work well, but you
are not limited to this.)

``` {r}
jp_starbucks <- starbucks %>% filter(Country == "JP")

pie1 <- ggplot(usa_starbucks, aes(x = "", y = Country, fill = OwnershipType)) + geom_col() + coord_polar(theta = "y") + labs(title = "Proportion of Ownership Type for Starbucks Locations within the US", x = "Values", y = "United States Locations")

pie2 <- ggplot(jp_starbucks, aes(x = "", y = Country, fill = OwnershipType)) + geom_col() + coord_polar(theta = "y") + labs(title = "Proportion of Ownership Type for Starbucks Locations within Japan", x = "Values", y = "Japan Locations")

pie1
pie2
```
