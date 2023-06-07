Práctica 02
================

## 1. Assessing Data Quality

## Load the following packages: dplyr, na.tools, tidyimpute (version from github decisionpatterns/tidyimpute”)

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(na.tools)
library(tidyimpute)
```

## Load the carInsurance data set about the insurance risk rating of cars based on several characteristics of each car

``` r
data <- read.csv('../data/01_datamanip/carInsurance.data', header=FALSE)

data[data == "?"] <- NA

carInsurance <- data.frame(data)

# List of names obtained of webpage
header <- c("symboling","normalizedLosses","make","fuelType","aspiration","nDoors","bodyStyle","driveWheels","engineLocation","wheelBase","length","width","height","curbWeight","engineType","nCylinders","engineSize","fuelSystem","bore","stroke","compression-ratio","horsepower","peakRpm","cityMpg","highwayMpg","price")

# Assign header a names of df
names(carInsurance) <- header

head(carInsurance,10)
```

    ##    symboling normalizedLosses        make fuelType aspiration nDoors
    ## 1          3             <NA> alfa-romero      gas        std    two
    ## 2          3             <NA> alfa-romero      gas        std    two
    ## 3          1             <NA> alfa-romero      gas        std    two
    ## 4          2              164        audi      gas        std   four
    ## 5          2              164        audi      gas        std   four
    ## 6          2             <NA>        audi      gas        std    two
    ## 7          1              158        audi      gas        std   four
    ## 8          1             <NA>        audi      gas        std   four
    ## 9          1              158        audi      gas      turbo   four
    ## 10         0             <NA>        audi      gas      turbo    two
    ##      bodyStyle driveWheels engineLocation wheelBase length width height
    ## 1  convertible         rwd          front      88.6  168.8  64.1   48.8
    ## 2  convertible         rwd          front      88.6  168.8  64.1   48.8
    ## 3    hatchback         rwd          front      94.5  171.2  65.5   52.4
    ## 4        sedan         fwd          front      99.8  176.6  66.2   54.3
    ## 5        sedan         4wd          front      99.4  176.6  66.4   54.3
    ## 6        sedan         fwd          front      99.8  177.3  66.3   53.1
    ## 7        sedan         fwd          front     105.8  192.7  71.4   55.7
    ## 8        wagon         fwd          front     105.8  192.7  71.4   55.7
    ## 9        sedan         fwd          front     105.8  192.7  71.4   55.9
    ## 10   hatchback         4wd          front      99.5  178.2  67.9   52.0
    ##    curbWeight engineType nCylinders engineSize fuelSystem bore stroke
    ## 1        2548       dohc       four        130       mpfi 3.47   2.68
    ## 2        2548       dohc       four        130       mpfi 3.47   2.68
    ## 3        2823       ohcv        six        152       mpfi 2.68   3.47
    ## 4        2337        ohc       four        109       mpfi 3.19   3.40
    ## 5        2824        ohc       five        136       mpfi 3.19   3.40
    ## 6        2507        ohc       five        136       mpfi 3.19   3.40
    ## 7        2844        ohc       five        136       mpfi 3.19   3.40
    ## 8        2954        ohc       five        136       mpfi 3.19   3.40
    ## 9        3086        ohc       five        131       mpfi 3.13   3.40
    ## 10       3053        ohc       five        131       mpfi 3.13   3.40
    ##    compression-ratio horsepower peakRpm cityMpg highwayMpg price
    ## 1                9.0        111    5000      21         27 13495
    ## 2                9.0        111    5000      21         27 16500
    ## 3                9.0        154    5000      19         26 16500
    ## 4               10.0        102    5500      24         30 13950
    ## 5                8.0        115    5500      18         22 17450
    ## 6                8.5        110    5500      19         25 15250
    ## 7                8.5        110    5500      19         25 17710
    ## 8                8.5        110    5500      19         25 18920
    ## 9                8.3        140    5500      17         20 23875
    ## 10               7.0        160    5500      16         22  <NA>

### (a) Check if there are any missing values.

``` r
has_missing <- any_na(carInsurance)

if (has_missing) {
  print("There are missing values in the dataset.")
} else {
  print("There are no missing values in the dataset.")
}
```

    ## [1] "There are missing values in the dataset."

### (b) Count the number of cases that have, at least, one missing value

``` r
# Count the number of cases with at least one missing value
num_missing_cases <- carInsurance %>%
  filter_any_na() %>%
  count()

print(num_missing_cases)
```

    ##     n
    ## 1 159

## (c) Create a new data set by removing all the cases that have missing values.

``` r
carInsurance_sin_faltantes <- drop_rows_any_na(carInsurance)
head(carInsurance_sin_faltantes,10)
```

    ##    symboling normalizedLosses      make fuelType aspiration nDoors bodyStyle
    ## 4          2              164      audi      gas        std   four     sedan
    ## 5          2              164      audi      gas        std   four     sedan
    ## 7          1              158      audi      gas        std   four     sedan
    ## 9          1              158      audi      gas      turbo   four     sedan
    ## 11         2              192       bmw      gas        std    two     sedan
    ## 12         0              192       bmw      gas        std   four     sedan
    ## 13         0              188       bmw      gas        std    two     sedan
    ## 14         0              188       bmw      gas        std   four     sedan
    ## 19         2              121 chevrolet      gas        std    two hatchback
    ## 20         1               98 chevrolet      gas        std    two hatchback
    ##    driveWheels engineLocation wheelBase length width height curbWeight
    ## 4          fwd          front      99.8  176.6  66.2   54.3       2337
    ## 5          4wd          front      99.4  176.6  66.4   54.3       2824
    ## 7          fwd          front     105.8  192.7  71.4   55.7       2844
    ## 9          fwd          front     105.8  192.7  71.4   55.9       3086
    ## 11         rwd          front     101.2  176.8  64.8   54.3       2395
    ## 12         rwd          front     101.2  176.8  64.8   54.3       2395
    ## 13         rwd          front     101.2  176.8  64.8   54.3       2710
    ## 14         rwd          front     101.2  176.8  64.8   54.3       2765
    ## 19         fwd          front      88.4  141.1  60.3   53.2       1488
    ## 20         fwd          front      94.5  155.9  63.6   52.0       1874
    ##    engineType nCylinders engineSize fuelSystem bore stroke compression-ratio
    ## 4         ohc       four        109       mpfi 3.19   3.40              10.0
    ## 5         ohc       five        136       mpfi 3.19   3.40               8.0
    ## 7         ohc       five        136       mpfi 3.19   3.40               8.5
    ## 9         ohc       five        131       mpfi 3.13   3.40               8.3
    ## 11        ohc       four        108       mpfi 3.50   2.80               8.8
    ## 12        ohc       four        108       mpfi 3.50   2.80               8.8
    ## 13        ohc        six        164       mpfi 3.31   3.19               9.0
    ## 14        ohc        six        164       mpfi 3.31   3.19               9.0
    ## 19          l      three         61       2bbl 2.91   3.03               9.5
    ## 20        ohc       four         90       2bbl 3.03   3.11               9.6
    ##    horsepower peakRpm cityMpg highwayMpg price
    ## 4         102    5500      24         30 13950
    ## 5         115    5500      18         22 17450
    ## 7         110    5500      19         25 17710
    ## 9         140    5500      17         20 23875
    ## 11        101    5800      23         29 16430
    ## 12        101    5800      23         29 16925
    ## 13        121    4250      21         28 20970
    ## 14        121    4250      21         28 21105
    ## 19         48    5100      47         53  5151
    ## 20         70    5400      38         43  6295

## (d) Create a new data set by imputing all the missing values with 0.

``` r
# Crear una copia del conjunto de datos original
carInsurance_imputed <- carInsurance

# Reemplazar los valores faltantes con 0
carInsurance_imputed[is.na(carInsurance_imputed)] <- 0
head(carInsurance_imputed,10)
```

    ##    symboling normalizedLosses        make fuelType aspiration nDoors
    ## 1          3                0 alfa-romero      gas        std    two
    ## 2          3                0 alfa-romero      gas        std    two
    ## 3          1                0 alfa-romero      gas        std    two
    ## 4          2              164        audi      gas        std   four
    ## 5          2              164        audi      gas        std   four
    ## 6          2                0        audi      gas        std    two
    ## 7          1              158        audi      gas        std   four
    ## 8          1                0        audi      gas        std   four
    ## 9          1              158        audi      gas      turbo   four
    ## 10         0                0        audi      gas      turbo    two
    ##      bodyStyle driveWheels engineLocation wheelBase length width height
    ## 1  convertible         rwd          front      88.6  168.8  64.1   48.8
    ## 2  convertible         rwd          front      88.6  168.8  64.1   48.8
    ## 3    hatchback         rwd          front      94.5  171.2  65.5   52.4
    ## 4        sedan         fwd          front      99.8  176.6  66.2   54.3
    ## 5        sedan         4wd          front      99.4  176.6  66.4   54.3
    ## 6        sedan         fwd          front      99.8  177.3  66.3   53.1
    ## 7        sedan         fwd          front     105.8  192.7  71.4   55.7
    ## 8        wagon         fwd          front     105.8  192.7  71.4   55.7
    ## 9        sedan         fwd          front     105.8  192.7  71.4   55.9
    ## 10   hatchback         4wd          front      99.5  178.2  67.9   52.0
    ##    curbWeight engineType nCylinders engineSize fuelSystem bore stroke
    ## 1        2548       dohc       four        130       mpfi 3.47   2.68
    ## 2        2548       dohc       four        130       mpfi 3.47   2.68
    ## 3        2823       ohcv        six        152       mpfi 2.68   3.47
    ## 4        2337        ohc       four        109       mpfi 3.19   3.40
    ## 5        2824        ohc       five        136       mpfi 3.19   3.40
    ## 6        2507        ohc       five        136       mpfi 3.19   3.40
    ## 7        2844        ohc       five        136       mpfi 3.19   3.40
    ## 8        2954        ohc       five        136       mpfi 3.19   3.40
    ## 9        3086        ohc       five        131       mpfi 3.13   3.40
    ## 10       3053        ohc       five        131       mpfi 3.13   3.40
    ##    compression-ratio horsepower peakRpm cityMpg highwayMpg price
    ## 1                9.0        111    5000      21         27 13495
    ## 2                9.0        111    5000      21         27 16500
    ## 3                9.0        154    5000      19         26 16500
    ## 4               10.0        102    5500      24         30 13950
    ## 5                8.0        115    5500      18         22 17450
    ## 6                8.5        110    5500      19         25 15250
    ## 7                8.5        110    5500      19         25 17710
    ## 8                8.5        110    5500      19         25 18920
    ## 9                8.3        140    5500      17         20 23875
    ## 10               7.0        160    5500      16         22     0
