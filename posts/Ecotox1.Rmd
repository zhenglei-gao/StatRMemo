---
title: Repeated Measure and Longitudinal Data
date: 2013-12-12
tags: [Ecotox, Anova, Mixed_Models]
---

<style>
p {
  text-align: justify;
}
</style>

## Data 

The kind of data is like:

```r
data <- rnorm(100,0,1)
```
## Possible Methods

### 1. Univariate Methods

0. Williams' Test or t-test
1. Repeated Measure ANOVA
2. Linear Mixed Models
3. Generalized Linear Mixed Models

### 2. Multivariate Methods

## Conclusion

???

#### Step 0. Download R

We will be using the statistical environment R for creating this visualization. R is open source, simple to use and works across multiple platforms. So go ahead and download R!

#### Step 1. Load libraries

One of the key strengths of R is the availability of several user-written packages that simplify the coding process. It is easy to install any R package by just typing install.packages('package.name') on the R console. For this visualization, we will be using the zipcode package to get long/lat for each location, lubridate to work with dates, ggplot2/maps to create the plots and animation to create an animated plot. In addition, we also source a couple of custom ggplot themes for the maps.


```r
library(zipcode)
```

```
## Error: there is no package called 'zipcode'
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.0.2
```

```r
library(scales)
```

```
## Warning: package 'scales' was built under R version 3.0.2
```

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.0.2
```

```r
library(maps)
```

```
## Error: there is no package called 'maps'
```

```r
library(animation)
```

```
## Error: there is no package called 'animation'
```

```r
library(ggthemes)
```

```
## Error: there is no package called 'ggthemes'
```

```r
library(plyr)
knitr::opts_chunk$set(tidy = FALSE)
```


#### Step 2. Load data

We use the same [data](http://www.econ.umn.edu/~holmes/data/WalMart/index.html) source on Walmart store openings as used by Nathan Yau of the FlowingData blog. This data was originally collected by Prof. Thomas Holmes, and you can find the documentation for this data-set on his webpage. We convert opening dates to the correct format and also add a variable indicating the year of opening.


```r
walmart = read.csv("http://goo.gl/4EWpS", stringsAsFactors = F);
walmart$OPENDATE = as.Date(walmart$OPENDATE,  "%m/%d/%Y");
walmart$openyear = year(walmart$OPENDATE);
```


#### Step 3. Merge with zipcodes


We merge our store openings data with zipcode data to get long/lat information for every location. We sort the merged data by opening date and add an id variable to represent the sequence of store openings.


```r
data(zipcode)
```

```
## Warning: data set 'zipcode' not found
```

```r
walmart      = rename(walmart, replace = c("ZIPCODE" = "zip"))
walmart      = merge(walmart, zipcode)
```

```
## Error: object 'zipcode' not found
```

```r
walmart      = plyr::arrange(walmart, OPENDATE)
walmart$id = as.numeric(rownames(walmart))
```


#### Step 4. Construct map

We use the map_data function in ggplot2 to extract the US map with state boundaries. We construct a data frame with state centers and abbreviated state names to be used to annotate the map. We remove Alaska and Hawaii in order to maximize the visibility of plot details.


```r
usmap     = map_data("state"); 
```

```
## Error: maps package required for this functionality.  Please install and
## try again.
```

```r
state.info = data.frame(state.center, state.abb);
state.info = subset(state.info, !state.abb %in% c("AK", "HI"));
```


#### Step 5. Plot store openings

The next step is to create a function that plots a given number of stores on the US map. This is the basic function that we would be using while creating our animations. We use ggplot2 to create the plot by adding a layer of yellow points denoting stores on a US map with state boundaries. We also use a bigger red point to represent the most recent store opening for that subset.


```r
plotStore <- function(.id){
  df = subset(walmart, id <= .id);
  yr = year(df$OPENDATE[.id])
  p1 = ggplot(df, aes(x = longitude, y = latitude)) +
    geom_polygon(data = usmap, aes(x = long, y = lat, group = group), 
      fill = 'gray10', colour = 'gray40', linetype = 2) +
    geom_text(data = state.info, aes(x = x, y = y, label = state.abb), 
      colour = 'white') +
    geom_point(colour = 'yellow', size = 1) + 
    geom_point(subset = .(id == .id), colour = alpha('red', 0.7),
      size = 9)+
    annotate('text', x = -70, y = 31, label = 'YEAR') + 
    annotate('text', x = -70, y = 29, label =  yr, colour = 'red') +
    annotate('text', x = -70, y = 27, label = 'STORES') +
    annotate('text', x = -70, y = 25, label =  .id, colour = 'blue') +
    theme_tufte(ticks = FALSE) +
    labs(title = 'GROWTH OF WALMART, 1962 TO 2010', x = "", y = "") +
    theme(
      plot.title = element_text(colour = 'blue', face = 'bold', size = 20),
      axis.text = element_blank()
    ) +
    coord_map(project="albers", lat0=37.5, lat1=29.5)      
}
```


#### Step 6. Plot number of store openings

We now create a function to plot the number of stores opened by date. We show the trend in number of stores opened using a blue line and a red point at the end.


```r
plotNumStores <- function(.id){
  
  df = subset(walmart, id <= .id)
  p0 = ggplot(walmart, aes(x = OPENDATE, y = id)) +
    geom_point(colour = 'white') +
    geom_line(subset = .(id <= .id), colour = 'blue') +
    geom_point(subset = .(id == .id), colour = alpha('red', 0.7), 
      size = 2) +
    scale_y_continuous(breaks = c(1000, 2000, 3000)) +
    xlab(NULL) + ylab(NULL) +
    theme_tufte(ticks = FALSE)
}
```


#### Step 7. Combine plots

The next step is to create a function that combines the two plots in Step 5 and Step 6. To do this, we use a trick illustrated in the learnr blog of creating a viewport and using it to display one of the plots as an inset.


```r
# create a viewport on the bottom left corner
vp = grid::viewport(width = 0.4, 
  height = 0.3, x = 0,
  y      = grid::unit(0.7, 'lines'),   
  just   = c('left','bottom')
);

# combine both plots into a single plot
animateStores <- function(.id){
  print(plotStore(.id));
  print(plotNumStores(.id), vp = vp);
}
animateStores(10)
```

```
## Error: object 'usmap' not found
```


#### Step 8. Create animation

We are almost done. The final step is to create the animation using the animation package. We just need to throw the animateStore function created in Step 7 into a loop, and the animation package takes care of the rest! As the process of generating a gif file is very time consuming, I have only shown the output for 100 store openings in this post.


```r
for (i in 1:10) {
  animateStores(i)
}
```

```
## Error: object 'usmap' not found
```


Although, the final output is not as impressive as the visualization on FlowingData, it is not bad considering that it took less than two hours of time and 100 lines of R code. Note that it is easy to customize this code and create such a visualization for any dataset with store opening dates and zip-codes!




