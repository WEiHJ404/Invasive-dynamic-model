## Here we show the Invasion phase calculation and spatial-autocorrelation analysis 

### 1.Break-point-segementation.R
In this code, we run the Time Series Segmentation of Piecewise Trend Approximation to our theoretical curves with diffrent Reff; Then we get the break points of invasion phase in each city.

### 2.Observed-curve-Break-point.R
Among this code, we get Segmentations points from theoretical curve, but we still need to find which segmentations points in our observed COVID-19 curves in four cities. 

### 3.Invasion Phase with Reff in all cities.R
In this code, we plot our supplementary Figure S5-S8 by the results of Invasion phases in theoretical curves with various scnarios.



### 4.Hexmap.R
In this code, we construct a grid with 5 km spacing between the centres of adjacent hexagons within each city

### 5.MoranI.R
Onece we get grids in each city, through `MoranI.R`, the case events distributed in each grid, then we calculate the Moran's I in each periods. The periods times stap is defined in our Invasion Phase study.

