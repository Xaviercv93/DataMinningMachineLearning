Práctica 02
================

# 2 Hands On: Data Quality and Pre-Processing

## 1. Assessing Data Quality

### Load the following packages: dplyr, na.tools, tidyimpute (version from github decisionpatterns/tidyimpute”)

``` r
library(na.tools)
library(devtools)
```

    ## Loading required package: usethis

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
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.4
    ## ✔ ggplot2   3.4.2     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.2     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.1     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tidyimpute)
library(sos)
```

    ## Loading required package: brew
    ## 
    ## Attaching package: 'sos'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     ?

``` r
library(ggplot2)
```

### Load the carInsurance data set about the insurance risk rating of cars based on several characteristics of each car

``` r
#lee archivo csv, se indica que no contiene fila de encabezado
data <- read.csv('../data/02_data/carInsurance.data', header=FALSE)

#reemplaza valores ? por NA
data[data == "?"] <- NA

#crea un dataframe llamado carInsurance
#se usa el objeto data como argumento para la funcion data.frame()
carInsurance <- data.frame(data)

# vector de nombres de encabezado para el dataframe
header <- c("symboling","normalizedLosses","make","fuelType","aspiration","nDoors","bodyStyle","driveWheels","engineLocation","wheelBase","length","width","height","curbWeight","engineType","nCylinders","engineSize","fuelSystem","bore","stroke","compression-ratio","horsepower","peakRpm","cityMpg","highwayMpg","price")

# Establecer el encabezado al dataframe
names(carInsurance) <- header

#imprimir el dataframe, solo 10 filas
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
columnas_valores_faltantes <- colSums(is.na(carInsurance))>0
#head(columnas_valores_faltantes,10)
nombres_columnas_valores_faltantes <- names(columnas_valores_faltantes[columnas_valores_faltantes])
nombres_columnas_valores_faltantes
```

    ## [1] "normalizedLosses" "nDoors"           "bore"             "stroke"          
    ## [5] "horsepower"       "peakRpm"          "price"

``` r
valores_faltantes <- any_na(carInsurance)

#valores_faltantes

if (valores_faltantes) {
  print("Faltan valores en el conjunto de datos.")
} else {
  print("No hay valores faltantes en el conjunto de datos.")
}
```

    ## [1] "Faltan valores en el conjunto de datos."

### (b) Count the number of cases that have, at least, one missing value

``` r
num_valores_faltantes <- carInsurance %>%
  filter_any_na() %>%
  count()
typeof(num_valores_faltantes)
```

    ## [1] "list"

``` r
print(num_valores_faltantes)
```

    ##     n
    ## 1 159

### (c) Create a new data set by removing all the cases that have missing values.

``` r
#head(carInsurance,10)
#count(carInsurance)
carInsurance_sin_NA <- drop_rows_any_na(carInsurance)
head(carInsurance_sin_NA,10)
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

``` r
#count(carInsurance_sin_NA)
```

### (d) Create a new data set by imputing all the missing values with 0.

``` r
# Crear una copia del conjunto de datos original y reemplaza los valores NA por ceros, se aplica a todo el conjunto de datos
carInsuranceImpute <- carInsurance %>% impute_zero_all()

# Reemplazar los valores NA por ceros en todas las columnas
#carInsuranceImpute <- impute_zero_all(carInsuranceImpute)

head(carInsuranceImpute, 10)
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

``` r
#count(carInsuranceImpute)
```

### (e) Create a new data set by imputing the mean in all the columns which have double type values.

``` r
# Identificar columnas con valores de tipo double
columnas_double <- sapply(carInsuranceImpute, is.double)

#nombre de las columnas_double
nombres_columnas_double <- names(columnas_double[columnas_double])
#nombres_columnas_double
#typeof(nombres_columnas_double)

# Calcular la media de cada columna
media <- colMeans(carInsuranceImpute[,nombres_columnas_double])
media
```

    ##         wheelBase            length             width            height 
    ##          98.75659         174.04927          65.90780          53.72488 
    ## compression-ratio 
    ##          10.14254

``` r
# Crear un nuevo conjunto de datos imputando la media en las columnas correspondientes
carInsuranceMean <- carInsuranceImpute

for (col in nombres_columnas_double) {
  carInsuranceMean[[col]] <- media[col]
}
head(carInsuranceMean, 10)
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
    ##      bodyStyle driveWheels engineLocation wheelBase   length   width   height
    ## 1  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 2  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 3    hatchback         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 4        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 5        sedan         4wd          front  98.75659 174.0493 65.9078 53.72488
    ## 6        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 7        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 8        wagon         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 9        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 10   hatchback         4wd          front  98.75659 174.0493 65.9078 53.72488
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
    ## 1           10.14254        111    5000      21         27 13495
    ## 2           10.14254        111    5000      21         27 16500
    ## 3           10.14254        154    5000      19         26 16500
    ## 4           10.14254        102    5500      24         30 13950
    ## 5           10.14254        115    5500      18         22 17450
    ## 6           10.14254        110    5500      19         25 15250
    ## 7           10.14254        110    5500      19         25 17710
    ## 8           10.14254        110    5500      19         25 18920
    ## 9           10.14254        140    5500      17         20 23875
    ## 10          10.14254        160    5500      16         22     0

### (f) Create a new data set by imputing the mode in all the columns which have integer type values.

``` r
# Cargar el paquete necesario
library(modeest)
```

    ## Warning: package 'modeest' was built under R version 4.3.1

``` r
#identificar columnas int
columnas_integer <- sapply(carInsuranceMean, is.integer)

#print(columnas_integer)




#nombres de las columnas int
nombres_columnas_integer <- names(columnas_integer[columnas_integer])
#nombres_columnas_integer


# Crear una función personalizada para calcular la moda de un vector
moda_personalizada <- function(x) {
  moda <- unique(x)[which.max(tabulate(match(x, unique(x))))]
  if (length(moda) == 0) {  # Si no hay moda, devolver NA
    return(NA)
  } else {
    return(moda)
  }
}


# Calcular la moda en las columnas de tipo double
moda <- sapply(carInsuranceMean[, nombres_columnas_integer], moda_personalizada)

# Resultado: vector con las modas de cada columna
moda
```

    ##  symboling curbWeight engineSize    cityMpg highwayMpg 
    ##          0       2385        122         31         25

``` r
# Crear un nuevo conjunto de datos imputando la media en las columnas correspondientes
carInsuranceMode <- carInsuranceMean 
head(carInsuranceMode, 10)
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
    ##      bodyStyle driveWheels engineLocation wheelBase   length   width   height
    ## 1  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 2  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 3    hatchback         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 4        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 5        sedan         4wd          front  98.75659 174.0493 65.9078 53.72488
    ## 6        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 7        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 8        wagon         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 9        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 10   hatchback         4wd          front  98.75659 174.0493 65.9078 53.72488
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
    ## 1           10.14254        111    5000      21         27 13495
    ## 2           10.14254        111    5000      21         27 16500
    ## 3           10.14254        154    5000      19         26 16500
    ## 4           10.14254        102    5500      24         30 13950
    ## 5           10.14254        115    5500      18         22 17450
    ## 6           10.14254        110    5500      19         25 15250
    ## 7           10.14254        110    5500      19         25 17710
    ## 8           10.14254        110    5500      19         25 18920
    ## 9           10.14254        140    5500      17         20 23875
    ## 10          10.14254        160    5500      16         22     0

``` r
for (col in nombres_columnas_integer) {
  carInsuranceMode[[col]] <- moda[col]
}
head(carInsuranceMode, 10)
```

    ##    symboling normalizedLosses        make fuelType aspiration nDoors
    ## 1          0                0 alfa-romero      gas        std    two
    ## 2          0                0 alfa-romero      gas        std    two
    ## 3          0                0 alfa-romero      gas        std    two
    ## 4          0              164        audi      gas        std   four
    ## 5          0              164        audi      gas        std   four
    ## 6          0                0        audi      gas        std    two
    ## 7          0              158        audi      gas        std   four
    ## 8          0                0        audi      gas        std   four
    ## 9          0              158        audi      gas      turbo   four
    ## 10         0                0        audi      gas      turbo    two
    ##      bodyStyle driveWheels engineLocation wheelBase   length   width   height
    ## 1  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 2  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 3    hatchback         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 4        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 5        sedan         4wd          front  98.75659 174.0493 65.9078 53.72488
    ## 6        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 7        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 8        wagon         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 9        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 10   hatchback         4wd          front  98.75659 174.0493 65.9078 53.72488
    ##    curbWeight engineType nCylinders engineSize fuelSystem bore stroke
    ## 1        2385       dohc       four        122       mpfi 3.47   2.68
    ## 2        2385       dohc       four        122       mpfi 3.47   2.68
    ## 3        2385       ohcv        six        122       mpfi 2.68   3.47
    ## 4        2385        ohc       four        122       mpfi 3.19   3.40
    ## 5        2385        ohc       five        122       mpfi 3.19   3.40
    ## 6        2385        ohc       five        122       mpfi 3.19   3.40
    ## 7        2385        ohc       five        122       mpfi 3.19   3.40
    ## 8        2385        ohc       five        122       mpfi 3.19   3.40
    ## 9        2385        ohc       five        122       mpfi 3.13   3.40
    ## 10       2385        ohc       five        122       mpfi 3.13   3.40
    ##    compression-ratio horsepower peakRpm cityMpg highwayMpg price
    ## 1           10.14254        111    5000      31         25 13495
    ## 2           10.14254        111    5000      31         25 16500
    ## 3           10.14254        154    5000      31         25 16500
    ## 4           10.14254        102    5500      31         25 13950
    ## 5           10.14254        115    5500      31         25 17450
    ## 6           10.14254        110    5500      31         25 15250
    ## 7           10.14254        110    5500      31         25 17710
    ## 8           10.14254        110    5500      31         25 18920
    ## 9           10.14254        140    5500      31         25 23875
    ## 10          10.14254        160    5500      31         25     0

### (g) Create a new data set by imputing the most frequent value to the column ”nDoors”.

### Tip: use the function impute_replace()

``` r
nDoorsMode <- names(which.max(table(carInsuranceMode$nDoors)))
nDoorsMode
```

    ## [1] "four"

``` r
carInsurancenDoorsMode <- carInsuranceMode %>%
  mutate(nDoors = impute_replace_all(nDoors, nDoorsMode))
head(carInsurancenDoorsMode, 10)
```

    ##    symboling normalizedLosses        make fuelType aspiration nDoors
    ## 1          0                0 alfa-romero      gas        std    two
    ## 2          0                0 alfa-romero      gas        std    two
    ## 3          0                0 alfa-romero      gas        std    two
    ## 4          0              164        audi      gas        std   four
    ## 5          0              164        audi      gas        std   four
    ## 6          0                0        audi      gas        std    two
    ## 7          0              158        audi      gas        std   four
    ## 8          0                0        audi      gas        std   four
    ## 9          0              158        audi      gas      turbo   four
    ## 10         0                0        audi      gas      turbo    two
    ##      bodyStyle driveWheels engineLocation wheelBase   length   width   height
    ## 1  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 2  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 3    hatchback         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 4        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 5        sedan         4wd          front  98.75659 174.0493 65.9078 53.72488
    ## 6        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 7        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 8        wagon         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 9        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 10   hatchback         4wd          front  98.75659 174.0493 65.9078 53.72488
    ##    curbWeight engineType nCylinders engineSize fuelSystem bore stroke
    ## 1        2385       dohc       four        122       mpfi 3.47   2.68
    ## 2        2385       dohc       four        122       mpfi 3.47   2.68
    ## 3        2385       ohcv        six        122       mpfi 2.68   3.47
    ## 4        2385        ohc       four        122       mpfi 3.19   3.40
    ## 5        2385        ohc       five        122       mpfi 3.19   3.40
    ## 6        2385        ohc       five        122       mpfi 3.19   3.40
    ## 7        2385        ohc       five        122       mpfi 3.19   3.40
    ## 8        2385        ohc       five        122       mpfi 3.19   3.40
    ## 9        2385        ohc       five        122       mpfi 3.13   3.40
    ## 10       2385        ohc       five        122       mpfi 3.13   3.40
    ##    compression-ratio horsepower peakRpm cityMpg highwayMpg price
    ## 1           10.14254        111    5000      31         25 13495
    ## 2           10.14254        111    5000      31         25 16500
    ## 3           10.14254        154    5000      31         25 16500
    ## 4           10.14254        102    5500      31         25 13950
    ## 5           10.14254        115    5500      31         25 17450
    ## 6           10.14254        110    5500      31         25 15250
    ## 7           10.14254        110    5500      31         25 17710
    ## 8           10.14254        110    5500      31         25 18920
    ## 9           10.14254        140    5500      31         25 23875
    ## 10          10.14254        160    5500      31         25     0

### (h) Combine the three last imputations to obtain a final dataset. Are there any duplicated cases?

### Tip: use the functions distinct() and count()

``` r
# Verificar el resultado
newCarInsuranceFInal <- carInsurancenDoorsMode

# Verificar si existen casos duplicados
casosDuplicados <- newCarInsuranceFInal %>%
  distinct() %>%
  count()

# Imprimir el resultado
if (casosDuplicados$n == nrow(newCarInsuranceFInal)) {
  print("No hay casos duplicados en el conjunto de datos.")
} else {
  print("Existen casos duplicados en el conjunto de datos.")
}
```

    ## [1] "No hay casos duplicados en el conjunto de datos."

``` r
# Verificar duplicados por columna
duplicadosPorColumna <- newCarInsuranceFInal %>%
  group_by(across(everything())) %>%
  count() %>%
  filter(n > 1) %>%
  select(-n)

# Imprimir el resultado
if (nrow(duplicadosPorColumna) == 0) {
  print("No hay columnas con duplicados en el conjunto de datos.")
} else {
  print("Columnas con duplicados y su conteo respectivo:")
  print(duplicadosPorColumna)
}
```

    ## [1] "No hay columnas con duplicados en el conjunto de datos."

``` r
head(newCarInsuranceFInal,10)
```

    ##    symboling normalizedLosses        make fuelType aspiration nDoors
    ## 1          0                0 alfa-romero      gas        std    two
    ## 2          0                0 alfa-romero      gas        std    two
    ## 3          0                0 alfa-romero      gas        std    two
    ## 4          0              164        audi      gas        std   four
    ## 5          0              164        audi      gas        std   four
    ## 6          0                0        audi      gas        std    two
    ## 7          0              158        audi      gas        std   four
    ## 8          0                0        audi      gas        std   four
    ## 9          0              158        audi      gas      turbo   four
    ## 10         0                0        audi      gas      turbo    two
    ##      bodyStyle driveWheels engineLocation wheelBase   length   width   height
    ## 1  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 2  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 3    hatchback         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 4        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 5        sedan         4wd          front  98.75659 174.0493 65.9078 53.72488
    ## 6        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 7        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 8        wagon         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 9        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 10   hatchback         4wd          front  98.75659 174.0493 65.9078 53.72488
    ##    curbWeight engineType nCylinders engineSize fuelSystem bore stroke
    ## 1        2385       dohc       four        122       mpfi 3.47   2.68
    ## 2        2385       dohc       four        122       mpfi 3.47   2.68
    ## 3        2385       ohcv        six        122       mpfi 2.68   3.47
    ## 4        2385        ohc       four        122       mpfi 3.19   3.40
    ## 5        2385        ohc       five        122       mpfi 3.19   3.40
    ## 6        2385        ohc       five        122       mpfi 3.19   3.40
    ## 7        2385        ohc       five        122       mpfi 3.19   3.40
    ## 8        2385        ohc       five        122       mpfi 3.19   3.40
    ## 9        2385        ohc       five        122       mpfi 3.13   3.40
    ## 10       2385        ohc       five        122       mpfi 3.13   3.40
    ##    compression-ratio horsepower peakRpm cityMpg highwayMpg price
    ## 1           10.14254        111    5000      31         25 13495
    ## 2           10.14254        111    5000      31         25 16500
    ## 3           10.14254        154    5000      31         25 16500
    ## 4           10.14254        102    5500      31         25 13950
    ## 5           10.14254        115    5500      31         25 17450
    ## 6           10.14254        110    5500      31         25 15250
    ## 7           10.14254        110    5500      31         25 17710
    ## 8           10.14254        110    5500      31         25 18920
    ## 9           10.14254        140    5500      31         25 23875
    ## 10          10.14254        160    5500      31         25     0

## 2. Data Pre-Processing

### 2. Load the package dlookr. Use the same car insurance data set above and apply the following transformations to the price attribute. Be critical regarding the obtained results

``` r
library(dlookr)
```

    ## Warning: package 'dlookr' was built under R version 4.3.1

    ## Registered S3 method overwritten by 'httr':
    ##   method         from  
    ##   print.response rmutil

    ## 
    ## Attaching package: 'dlookr'

    ## The following object is masked from 'package:modeest':
    ## 
    ##     skewness

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    ## The following object is masked from 'package:base':
    ## 
    ##     transform

### (a) Apply range-based normalization and z-score normalization.

### Tip: use the function transform().

``` r
newCarInsuranceFInal$price <- as.numeric(newCarInsuranceFInal$price)

newCarInsuranceFInal %>%
   mutate(rangeNorm = transform(newCarInsuranceFInal$price, method = "minmax"),
   zScoreNorm = transform(newCarInsuranceFInal$price, method = "zscore")) %>%
   select(rangeNorm, zScoreNorm) %>%
   head(20)
```

    ##    rangeNorm  zScoreNorm
    ## 1  0.2972467  0.06752913
    ## 2  0.3634361  0.43947911
    ## 3  0.3634361  0.43947911
    ## 4  0.3072687  0.12384768
    ## 5  0.3843612  0.55706729
    ## 6  0.3359031  0.28475782
    ## 7  0.3900881  0.58924932
    ## 8  0.4167401  0.73901953
    ## 9  0.5258811  1.35233472
    ## 10 0.0000000 -1.60284192
    ## 11 0.3618943  0.43081472
    ## 12 0.3727974  0.49208435
    ## 13 0.4618943  0.99276244
    ## 14 0.4648678  1.00947234
    ## 15 0.5410793  1.43774087
    ## 16 0.6775330  2.20453959
    ## 17 0.9100220  3.51100616
    ## 18 0.8123348  2.96205502
    ## 19 0.1134581 -0.96526643
    ## 20 0.1386564 -0.82366551

``` r
head(newCarInsuranceFInal,10)
```

    ##    symboling normalizedLosses        make fuelType aspiration nDoors
    ## 1          0                0 alfa-romero      gas        std    two
    ## 2          0                0 alfa-romero      gas        std    two
    ## 3          0                0 alfa-romero      gas        std    two
    ## 4          0              164        audi      gas        std   four
    ## 5          0              164        audi      gas        std   four
    ## 6          0                0        audi      gas        std    two
    ## 7          0              158        audi      gas        std   four
    ## 8          0                0        audi      gas        std   four
    ## 9          0              158        audi      gas      turbo   four
    ## 10         0                0        audi      gas      turbo    two
    ##      bodyStyle driveWheels engineLocation wheelBase   length   width   height
    ## 1  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 2  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 3    hatchback         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 4        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 5        sedan         4wd          front  98.75659 174.0493 65.9078 53.72488
    ## 6        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 7        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 8        wagon         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 9        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 10   hatchback         4wd          front  98.75659 174.0493 65.9078 53.72488
    ##    curbWeight engineType nCylinders engineSize fuelSystem bore stroke
    ## 1        2385       dohc       four        122       mpfi 3.47   2.68
    ## 2        2385       dohc       four        122       mpfi 3.47   2.68
    ## 3        2385       ohcv        six        122       mpfi 2.68   3.47
    ## 4        2385        ohc       four        122       mpfi 3.19   3.40
    ## 5        2385        ohc       five        122       mpfi 3.19   3.40
    ## 6        2385        ohc       five        122       mpfi 3.19   3.40
    ## 7        2385        ohc       five        122       mpfi 3.19   3.40
    ## 8        2385        ohc       five        122       mpfi 3.19   3.40
    ## 9        2385        ohc       five        122       mpfi 3.13   3.40
    ## 10       2385        ohc       five        122       mpfi 3.13   3.40
    ##    compression-ratio horsepower peakRpm cityMpg highwayMpg price
    ## 1           10.14254        111    5000      31         25 13495
    ## 2           10.14254        111    5000      31         25 16500
    ## 3           10.14254        154    5000      31         25 16500
    ## 4           10.14254        102    5500      31         25 13950
    ## 5           10.14254        115    5500      31         25 17450
    ## 6           10.14254        110    5500      31         25 15250
    ## 7           10.14254        110    5500      31         25 17710
    ## 8           10.14254        110    5500      31         25 18920
    ## 9           10.14254        140    5500      31         25 23875
    ## 10          10.14254        160    5500      31         25     0

### b) Discretize it into 4 equal-frequency ranges an into 4 equal-width ranges.

### Tip: use the function binning().

``` r
library(dplyr)
library(binr)

newCarInsuranceFInal <- newCarInsuranceFInal %>%
  mutate(equalFrequency = binning(price, nbins = 4, type = "quantile"),
         equalWidth = binning(price, nbins = 4, type = "equal")) 
head(newCarInsuranceFInal,10)
```

    ##    symboling normalizedLosses        make fuelType aspiration nDoors
    ## 1          0                0 alfa-romero      gas        std    two
    ## 2          0                0 alfa-romero      gas        std    two
    ## 3          0                0 alfa-romero      gas        std    two
    ## 4          0              164        audi      gas        std   four
    ## 5          0              164        audi      gas        std   four
    ## 6          0                0        audi      gas        std    two
    ## 7          0              158        audi      gas        std   four
    ## 8          0                0        audi      gas        std   four
    ## 9          0              158        audi      gas      turbo   four
    ## 10         0                0        audi      gas      turbo    two
    ##      bodyStyle driveWheels engineLocation wheelBase   length   width   height
    ## 1  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 2  convertible         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 3    hatchback         rwd          front  98.75659 174.0493 65.9078 53.72488
    ## 4        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 5        sedan         4wd          front  98.75659 174.0493 65.9078 53.72488
    ## 6        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 7        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 8        wagon         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 9        sedan         fwd          front  98.75659 174.0493 65.9078 53.72488
    ## 10   hatchback         4wd          front  98.75659 174.0493 65.9078 53.72488
    ##    curbWeight engineType nCylinders engineSize fuelSystem bore stroke
    ## 1        2385       dohc       four        122       mpfi 3.47   2.68
    ## 2        2385       dohc       four        122       mpfi 3.47   2.68
    ## 3        2385       ohcv        six        122       mpfi 2.68   3.47
    ## 4        2385        ohc       four        122       mpfi 3.19   3.40
    ## 5        2385        ohc       five        122       mpfi 3.19   3.40
    ## 6        2385        ohc       five        122       mpfi 3.19   3.40
    ## 7        2385        ohc       five        122       mpfi 3.19   3.40
    ## 8        2385        ohc       five        122       mpfi 3.19   3.40
    ## 9        2385        ohc       five        122       mpfi 3.13   3.40
    ## 10       2385        ohc       five        122       mpfi 3.13   3.40
    ##    compression-ratio horsepower peakRpm cityMpg highwayMpg price equalFrequency
    ## 1           10.14254        111    5000      31         25 13495  (10198,16500]
    ## 2           10.14254        111    5000      31         25 16500  (10198,16500]
    ## 3           10.14254        154    5000      31         25 16500  (10198,16500]
    ## 4           10.14254        102    5500      31         25 13950  (10198,16500]
    ## 5           10.14254        115    5500      31         25 17450  (16500,45400]
    ## 6           10.14254        110    5500      31         25 15250  (10198,16500]
    ## 7           10.14254        110    5500      31         25 17710  (16500,45400]
    ## 8           10.14254        110    5500      31         25 18920  (16500,45400]
    ## 9           10.14254        140    5500      31         25 23875  (16500,45400]
    ## 10          10.14254        160    5500      31         25     0   [0,7662.333]
    ##       equalWidth
    ## 1  (11350,22700]
    ## 2  (11350,22700]
    ## 3  (11350,22700]
    ## 4  (11350,22700]
    ## 5  (11350,22700]
    ## 6  (11350,22700]
    ## 7  (11350,22700]
    ## 8  (11350,22700]
    ## 9  (22700,34050]
    ## 10     [0,11350]

``` r
# Imprimir los niveles de equalFrequency
cat("Niveles de equalFrequency: ", levels(newCarInsuranceFInal$equalFrequency), "\n")
```

    ## Niveles de equalFrequency:  [0,7662.333] (7662.333,10198] (10198,16500] (16500,45400]

``` r
# Imprimir los niveles de equalWidth
cat("Niveles de equalWidth: ", levels(newCarInsuranceFInal$equalWidth), "\n")
```

    ## Niveles de equalWidth:  [0,11350] (11350,22700] (22700,34050] (34050,45400]
