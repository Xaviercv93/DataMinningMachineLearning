R Notebook
================

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

ss

``` r
# Cargar el conjunto de datos

load("../data/01_datamanip/carIns_final.Rdata")
ls()
```

    ## [1] "carIns_final"

``` r
carIns_final
```

    ## # A tibble: 205 × 26
    ##     symb normLoss make        fuelType aspiration nDoors bodyStyle   driveWheels
    ##    <int>    <int> <fct>       <fct>    <fct>      <fct>  <fct>       <fct>      
    ##  1     3      161 alfa-romero gas      std        two    convertible rwd        
    ##  2     3      161 alfa-romero gas      std        two    convertible rwd        
    ##  3     1      161 alfa-romero gas      std        two    hatchback   rwd        
    ##  4     2      164 audi        gas      std        four   sedan       fwd        
    ##  5     2      164 audi        gas      std        four   sedan       4wd        
    ##  6     2      161 audi        gas      std        two    sedan       fwd        
    ##  7     1      158 audi        gas      std        four   sedan       fwd        
    ##  8     1      161 audi        gas      std        four   wagon       fwd        
    ##  9     1      158 audi        gas      turbo      four   sedan       fwd        
    ## 10     0      161 audi        gas      turbo      two    hatchback   4wd        
    ## # ℹ 195 more rows
    ## # ℹ 18 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>

ss

``` r
# Comprobar si hay valores faltantes en el conjunto de datos
missing_values <- sum(is.na(carIns_final))
if (missing_values > 0) {
  print("Hay valores faltantes en el conjunto de datos")
} else {
  print("No hay valores faltantes en el conjunto de datos")
}
```

    ## [1] "No hay valores faltantes en el conjunto de datos"

ss

``` r
library(dplyr)

# Verificar si hay algún valor faltante en cada fila
missing_cases <- carIns_final %>% 
  summarise(has_missing = anyNA(.))

# Contar el número de casos con al menos un valor faltante
num_missing_cases <- sum(missing_cases$has_missing)

# Imprimir el resultado
cat("Número de casos con al menos un valor faltante:", num_missing_cases, "\n")
```

    ## Número de casos con al menos un valor faltante: 0

``` r
# Contar el número de casos con al menos un valor faltante
num_missing_cases2 <- sum(is.na(carIns_final))

# Imprimir el resultado
cat("Número de casos con al menos un valor faltante:", num_missing_cases2, "\n")
```

    ## Número de casos con al menos un valor faltante: 0
