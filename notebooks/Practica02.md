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
# Cargar el archivo de datos "carInsurance.Rdata"
load("../data/02_data/carInsurance.Rdata") 
carIns
```

    ## # A tibble: 205 × 26
    ##     symb normLoss make        fuelType aspiration nDoors bodyStyle   driveWheels
    ##    <int>    <int> <fct>       <fct>    <fct>      <fct>  <fct>       <fct>      
    ##  1     3       NA alfa-romero gas      std        two    convertible rwd        
    ##  2     3       NA alfa-romero gas      std        two    convertible rwd        
    ##  3     1       NA alfa-romero gas      std        two    hatchback   rwd        
    ##  4     2      164 audi        gas      std        four   sedan       fwd        
    ##  5     2      164 audi        gas      std        four   sedan       4wd        
    ##  6     2       NA audi        gas      std        two    sedan       fwd        
    ##  7     1      158 audi        gas      std        four   sedan       fwd        
    ##  8     1       NA audi        gas      std        four   wagon       fwd        
    ##  9     1      158 audi        gas      turbo      four   sedan       fwd        
    ## 10     0       NA audi        gas      turbo      two    hatchback   4wd        
    ## # ℹ 195 more rows
    ## # ℹ 18 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>

``` r
# Asignar los datos cargados a la variable "carInsurance"
carInsurance <- carIns
head(carInsurance, 10)
```

    ## # A tibble: 10 × 26
    ##     symb normLoss make        fuelType aspiration nDoors bodyStyle   driveWheels
    ##    <int>    <int> <fct>       <fct>    <fct>      <fct>  <fct>       <fct>      
    ##  1     3       NA alfa-romero gas      std        two    convertible rwd        
    ##  2     3       NA alfa-romero gas      std        two    convertible rwd        
    ##  3     1       NA alfa-romero gas      std        two    hatchback   rwd        
    ##  4     2      164 audi        gas      std        four   sedan       fwd        
    ##  5     2      164 audi        gas      std        four   sedan       4wd        
    ##  6     2       NA audi        gas      std        two    sedan       fwd        
    ##  7     1      158 audi        gas      std        four   sedan       fwd        
    ##  8     1       NA audi        gas      std        four   wagon       fwd        
    ##  9     1      158 audi        gas      turbo      four   sedan       fwd        
    ## 10     0       NA audi        gas      turbo      two    hatchback   4wd        
    ## # ℹ 18 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>

``` r
# forma anterior
# #lee archivo csv, se indica que no contiene fila de encabezado
# data <- read.csv('../data/02_data/carInsurance.data', header=FALSE)
# 
# #reemplaza valores ? por NA
# data[data == "?"] <- NA
# 
# #crea un dataframe llamado carInsurance
# #se usa el objeto data como argumento para la funcion data.frame()
# carInsurance <- data.frame(data)
# 
# # vector de nombres de encabezado para el dataframe
# header <- c("symboling","normalizedLosses","make","fuelType","aspiration","nDoors","bodyStyle","driveWheels","engineLocation","wheelBase","length","width","height","curbWeight","engineType","nCylinders","engineSize","fuelSystem","bore","stroke","compression-ratio","horsepower","peakRpm","cityMpg","highwayMpg","price")
# 
# # Establecer el encabezado al dataframe
# names(carInsurance) <- header
# 
# 
# 
# # Imprimir el dataframe, solo 10 filas
# head(carInsurance, 10)
```

### (a) Check if there are any missing values.

``` r
# Identificar columnas con valores faltantes
columnas_valores_faltantes <- colSums(is.na(carInsurance))>0
#head(columnas_valores_faltantes,10)
nombres_columnas_valores_faltantes <- names(columnas_valores_faltantes[columnas_valores_faltantes])
nombres_columnas_valores_faltantes
```

    ## [1] "normLoss"   "nDoors"     "bore"       "stroke"     "horsePower"
    ## [6] "peakRpm"    "price"

``` r
# Verificar si existen valores faltantes en el conjunto de datos
valores_faltantes <- any_na(carInsurance)

#valores_faltantes

# Imprimir mensaje dependiendo de la existencia de valores faltantes
if (valores_faltantes) {
  print("Faltan valores en el conjunto de datos.")
} else {
  print("No hay valores faltantes en el conjunto de datos.")
}
```

    ## [1] "Faltan valores en el conjunto de datos."

### (b) Count the number of cases that have, at least, one missing value

``` r
# Contar el número de valores faltantes por columna
num_valores_faltantes <- carInsurance %>%
  filter_any_na() %>%
  count()

# Mostrar el tipo de datos de la variable num_valores_faltantes
typeof(num_valores_faltantes)
```

    ## [1] "list"

``` r
# Imprimir el número de valores faltantes por columna
print(num_valores_faltantes)
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   159

### (c) Create a new data set by removing all the cases that have missing values.

``` r
# Calcular el recuento de filas en el conjunto de datos original
#count(carInsurance)

# Eliminar filas con valores faltantes
carInsurance_sin_NA <- drop_rows_any_na(carInsurance)

head(carInsurance_sin_NA,10)
```

    ## # A tibble: 10 × 26
    ##     symb normLoss make      fuelType aspiration nDoors bodyStyle driveWheels
    ##    <int>    <int> <fct>     <fct>    <fct>      <fct>  <fct>     <fct>      
    ##  1     2      164 audi      gas      std        four   sedan     fwd        
    ##  2     2      164 audi      gas      std        four   sedan     4wd        
    ##  3     1      158 audi      gas      std        four   sedan     fwd        
    ##  4     1      158 audi      gas      turbo      four   sedan     fwd        
    ##  5     2      192 bmw       gas      std        two    sedan     rwd        
    ##  6     0      192 bmw       gas      std        four   sedan     rwd        
    ##  7     0      188 bmw       gas      std        two    sedan     rwd        
    ##  8     0      188 bmw       gas      std        four   sedan     rwd        
    ##  9     2      121 chevrolet gas      std        two    hatchback fwd        
    ## 10     1       98 chevrolet gas      std        two    hatchback fwd        
    ## # ℹ 18 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>

``` r
# Calcular el recuento de filas en el conjunto de datos sin valores faltantes
#count(carInsurance_sin_NA)
```

### (d) Create a new data set by imputing all the missing values with 0.

``` r
# Crear una copia del conjunto de datos original y reemplaza los valores NA por ceros, se aplica a todo el conjunto de datos
carInsuranceImpute <- carInsurance %>% impute_zero_all()

# Reemplazar los valores NA por ceros en todas las columnas
#carInsuranceImpute <- impute_zero_all(carInsuranceImpute)

head(carInsuranceImpute, 20)
```

    ## # A tibble: 20 × 26
    ##     symb normLoss make        fuelType aspiration nDoors bodyStyle   driveWheels
    ##    <int>    <int> <fct>       <fct>    <fct>      <fct>  <fct>       <fct>      
    ##  1     3        0 alfa-romero gas      std        two    convertible rwd        
    ##  2     3        0 alfa-romero gas      std        two    convertible rwd        
    ##  3     1        0 alfa-romero gas      std        two    hatchback   rwd        
    ##  4     2      164 audi        gas      std        four   sedan       fwd        
    ##  5     2      164 audi        gas      std        four   sedan       4wd        
    ##  6     2        0 audi        gas      std        two    sedan       fwd        
    ##  7     1      158 audi        gas      std        four   sedan       fwd        
    ##  8     1        0 audi        gas      std        four   wagon       fwd        
    ##  9     1      158 audi        gas      turbo      four   sedan       fwd        
    ## 10     0        0 audi        gas      turbo      two    hatchback   4wd        
    ## 11     2      192 bmw         gas      std        two    sedan       rwd        
    ## 12     0      192 bmw         gas      std        four   sedan       rwd        
    ## 13     0      188 bmw         gas      std        two    sedan       rwd        
    ## 14     0      188 bmw         gas      std        four   sedan       rwd        
    ## 15     1        0 bmw         gas      std        four   sedan       rwd        
    ## 16     0        0 bmw         gas      std        four   sedan       rwd        
    ## 17     0        0 bmw         gas      std        two    sedan       rwd        
    ## 18     0        0 bmw         gas      std        four   sedan       rwd        
    ## 19     2      121 chevrolet   gas      std        two    hatchback   fwd        
    ## 20     1       98 chevrolet   gas      std        two    hatchback   fwd        
    ## # ℹ 18 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>

``` r
#count(carInsuranceImpute)
```

### (e) Create a new data set by imputing the mean in all the columns which have double type values.

``` r
# Identificar columnas con valores de tipo double
columnas_double <- sapply(carInsurance, is.double)

#nombre de las columnas_double
nombres_columnas_double <- names(columnas_double[columnas_double])
#nombres_columnas_double
#typeof(nombres_columnas_double)

# Calcular la media de cada columna
media <- colMeans(carInsurance[,nombres_columnas_double], na.rm = TRUE)
media
```

    ##        wheelBase           length            width           height 
    ##        98.756585       174.049268        65.907805        53.724878 
    ##             bore           stroke compressionRatio 
    ##         3.329751         3.255423        10.142537

``` r
# Crear un nuevo conjunto de datos imputando la media en las columnas correspondientes
carInsuranceMean <- carInsurance

for (col in nombres_columnas_double) {
  carInsuranceMean[[col]][is.na(carInsuranceMean[[col]])] <- media[col] 
}

head(carInsurance[, sapply(carInsurance, is.double)])
```

    ## # A tibble: 6 × 7
    ##   wheelBase length width height  bore stroke compressionRatio
    ##       <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>            <dbl>
    ## 1      88.6   169.  64.1   48.8  3.47   2.68              9  
    ## 2      88.6   169.  64.1   48.8  3.47   2.68              9  
    ## 3      94.5   171.  65.5   52.4  2.68   3.47              9  
    ## 4      99.8   177.  66.2   54.3  3.19   3.4              10  
    ## 5      99.4   177.  66.4   54.3  3.19   3.4               8  
    ## 6      99.8   177.  66.3   53.1  3.19   3.4               8.5

``` r
head(carInsuranceMean[, sapply(carInsuranceMean, is.double)])
```

    ## # A tibble: 6 × 7
    ##   wheelBase length width height  bore stroke compressionRatio
    ##       <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>            <dbl>
    ## 1      88.6   169.  64.1   48.8  3.47   2.68              9  
    ## 2      88.6   169.  64.1   48.8  3.47   2.68              9  
    ## 3      94.5   171.  65.5   52.4  2.68   3.47              9  
    ## 4      99.8   177.  66.2   54.3  3.19   3.4              10  
    ## 5      99.4   177.  66.4   54.3  3.19   3.4               8  
    ## 6      99.8   177.  66.3   53.1  3.19   3.4               8.5

### (f) Create a new data set by imputing the mode in all the columns which have integer type values.

``` r
library(modeest)
```

    ## Warning: package 'modeest' was built under R version 4.3.1

``` r
# Identificar las columnas con valores faltantes
columnas_valores_faltantes <- colSums(is.na(carInsuranceMean))>0
#head(columnas_valores_faltantes,10)
nombres_columnas_valores_faltantes <- names(columnas_valores_faltantes[columnas_valores_faltantes])
print(nombres_columnas_valores_faltantes)
```

    ## [1] "normLoss"   "nDoors"     "horsePower" "peakRpm"    "price"

``` r
# Verificar si existen valores faltantes en el conjunto de datos
valores_faltantes <- any_na(carInsuranceMean)
if (valores_faltantes) {
  print("Faltan valores en el conjunto de datos.")

} else {
  print("No hay valores faltantes en el conjunto de datos.")

}
```

    ## [1] "Faltan valores en el conjunto de datos."

``` r
# Identificar las columnas de tipo integer
columnas_integer <- sapply(carInsuranceMean, is.integer)
#nombres de las columnas integer
nombres_columnas_integer <- names(columnas_integer[columnas_integer])

# Función personalizada para calcular la moda de un vector
moda_personalizada <- function(x) {
  moda <- unique(x)[which.max(tabulate(match(x, unique(x))))]
  if (length(moda) == 0) {  # Si no hay moda, devolver NA
    return(NA)
  } else {
    return(moda)
  }
}


# Calcular la moda en las columnas de tipo integer
moda <- sapply(carInsuranceMean[, nombres_columnas_integer], moda_personalizada)
# Resultado: vector con las modas de cada columna
moda
```

    ##       symb   normLoss curbWeight engineSize horsePower    peakRpm    cityMpg 
    ##          0         NA       2385        122         68       5500         31 
    ## highwayMpg      price 
    ##         25         NA

``` r
# Crear un nuevo conjunto de datos imputando la moda en las columnas correspondientes
carInsuranceMode <- carInsuranceMean
for (col in nombres_columnas_integer) {
  valores_faltantes <- is.na(carInsuranceMode[[col]])
  carInsuranceMode[[col]][valores_faltantes] <- moda[col]
}

head(carInsuranceMode[, sapply(carInsuranceMode, is.integer)])
```

    ## # A tibble: 6 × 9
    ##    symb normLoss curbWeight engineSize horsePower peakRpm cityMpg highwayMpg
    ##   <int>    <int>      <int>      <int>      <int>   <int>   <int>      <int>
    ## 1     3       NA       2548        130        111    5000      21         27
    ## 2     3       NA       2548        130        111    5000      21         27
    ## 3     1       NA       2823        152        154    5000      19         26
    ## 4     2      164       2337        109        102    5500      24         30
    ## 5     2      164       2824        136        115    5500      18         22
    ## 6     2       NA       2507        136        110    5500      19         25
    ## # ℹ 1 more variable: price <int>

### (g) Create a new data set by imputing the most frequent value to the column ”nDoors”.

### Tip: use the function impute_replace()

``` r
# Encontrar la moda de la variable nDoors en el conjunto de datos carInsuranceMode
nDoorsMode <- names(which.max(table(carInsuranceMode$nDoors)))
nDoorsMode
```

    ## [1] "four"

``` r
# Reemplazar los valores faltantes en la variable nDoors con la moda encontrada
carInsurancenDoorsMode <- carInsuranceMode %>%
  mutate(nDoors = impute_replace_all(nDoors, nDoorsMode))

head(carInsurancenDoorsMode)
```

    ## # A tibble: 6 × 26
    ##    symb normLoss make        fuelType aspiration nDoors bodyStyle   driveWheels
    ##   <int>    <int> <fct>       <fct>    <fct>      <fct>  <fct>       <fct>      
    ## 1     3       NA alfa-romero gas      std        two    convertible rwd        
    ## 2     3       NA alfa-romero gas      std        two    convertible rwd        
    ## 3     1       NA alfa-romero gas      std        two    hatchback   rwd        
    ## 4     2      164 audi        gas      std        four   sedan       fwd        
    ## 5     2      164 audi        gas      std        four   sedan       4wd        
    ## 6     2       NA audi        gas      std        two    sedan       fwd        
    ## # ℹ 18 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>

### (h) Combine the three last imputations to obtain a final dataset. Are there any duplicated cases?

### Tip: use the functions distinct() and count()

``` r
#imputar valores NA
carInsurancenDoorsModeImpute <- carInsurancenDoorsMode %>% impute_zero_all()



newCarInsuranceFInal <- carInsurancenDoorsModeImpute

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

    ## # A tibble: 10 × 26
    ##     symb normLoss make        fuelType aspiration nDoors bodyStyle   driveWheels
    ##    <int>    <int> <fct>       <fct>    <fct>      <fct>  <fct>       <fct>      
    ##  1     3        0 alfa-romero gas      std        two    convertible rwd        
    ##  2     3        0 alfa-romero gas      std        two    convertible rwd        
    ##  3     1        0 alfa-romero gas      std        two    hatchback   rwd        
    ##  4     2      164 audi        gas      std        four   sedan       fwd        
    ##  5     2      164 audi        gas      std        four   sedan       4wd        
    ##  6     2        0 audi        gas      std        two    sedan       fwd        
    ##  7     1      158 audi        gas      std        four   sedan       fwd        
    ##  8     1        0 audi        gas      std        four   wagon       fwd        
    ##  9     1      158 audi        gas      turbo      four   sedan       fwd        
    ## 10     0        0 audi        gas      turbo      two    hatchback   4wd        
    ## # ℹ 18 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>

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
# Realizar la normalización de la variable price utilizando los métodos de minmax y zscore
newCarInsuranceFInal %>%
   mutate(
     rangeNorm = transform(newCarInsuranceFInal$price, method = "minmax"),
   zScoreNorm = transform(newCarInsuranceFInal$price, method = "zscore")
   ) %>%
   select(rangeNorm, zScoreNorm) %>%
   head(20)
```

    ## # A tibble: 20 × 2
    ##    rangeNorm  zScoreNorm 
    ##    <transfrm> <transfrm> 
    ##  1 0.2972467   0.06752913
    ##  2 0.3634361   0.43947911
    ##  3 0.3634361   0.43947911
    ##  4 0.3072687   0.12384768
    ##  5 0.3843612   0.55706729
    ##  6 0.3359031   0.28475782
    ##  7 0.3900881   0.58924932
    ##  8 0.4167401   0.73901953
    ##  9 0.5258811   1.35233472
    ## 10 0.0000000  -1.60284192
    ## 11 0.3618943   0.43081472
    ## 12 0.3727974   0.49208435
    ## 13 0.4618943   0.99276244
    ## 14 0.4648678   1.00947234
    ## 15 0.5410793   1.43774087
    ## 16 0.6775330   2.20453959
    ## 17 0.9100220   3.51100616
    ## 18 0.8123348   2.96205502
    ## 19 0.1134581  -0.96526643
    ## 20 0.1386564  -0.82366551

``` r
head(newCarInsuranceFInal,10)
```

    ## # A tibble: 10 × 26
    ##     symb normLoss make        fuelType aspiration nDoors bodyStyle   driveWheels
    ##    <int>    <int> <fct>       <fct>    <fct>      <fct>  <fct>       <fct>      
    ##  1     3        0 alfa-romero gas      std        two    convertible rwd        
    ##  2     3        0 alfa-romero gas      std        two    convertible rwd        
    ##  3     1        0 alfa-romero gas      std        two    hatchback   rwd        
    ##  4     2      164 audi        gas      std        four   sedan       fwd        
    ##  5     2      164 audi        gas      std        four   sedan       4wd        
    ##  6     2        0 audi        gas      std        two    sedan       fwd        
    ##  7     1      158 audi        gas      std        four   sedan       fwd        
    ##  8     1        0 audi        gas      std        four   wagon       fwd        
    ##  9     1      158 audi        gas      turbo      four   sedan       fwd        
    ## 10     0        0 audi        gas      turbo      two    hatchback   4wd        
    ## # ℹ 18 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>

### b) Discretize it into 4 equal-frequency ranges an into 4 equal-width ranges.

### Tip: use the function binning().

``` r
library(dplyr)
library(binr)

# Realizar la transformación mediante el método de quantile y equal
newCarInsuranceFInal <- newCarInsuranceFInal %>%
  mutate(equalFrequency = binning(price, nbins = 4, type = "quantile"),
         equalWidth = binning(price, nbins = 4, type = "equal")) 
head(newCarInsuranceFInal,10)
```

    ## # A tibble: 10 × 28
    ##     symb normLoss make        fuelType aspiration nDoors bodyStyle   driveWheels
    ##    <int>    <int> <fct>       <fct>    <fct>      <fct>  <fct>       <fct>      
    ##  1     3        0 alfa-romero gas      std        two    convertible rwd        
    ##  2     3        0 alfa-romero gas      std        two    convertible rwd        
    ##  3     1        0 alfa-romero gas      std        two    hatchback   rwd        
    ##  4     2      164 audi        gas      std        four   sedan       fwd        
    ##  5     2      164 audi        gas      std        four   sedan       4wd        
    ##  6     2        0 audi        gas      std        two    sedan       fwd        
    ##  7     1      158 audi        gas      std        four   sedan       fwd        
    ##  8     1        0 audi        gas      std        four   wagon       fwd        
    ##  9     1      158 audi        gas      turbo      four   sedan       fwd        
    ## 10     0        0 audi        gas      turbo      two    hatchback   4wd        
    ## # ℹ 20 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>, equalFrequency <bins>,
    ## #   equalWidth <bins>

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

## 3. With the seed 111019 obtain the following samples on the car insurance data set.

### Tip: use the function sample_frac().

### (a) A random sample of 60% of the cases, with replacement

``` r
# Establecer la semilla para la generación de números aleatorios
set.seed(111019)

# Obtener una muestra del 60% de los datos
sample_60_percent <- newCarInsuranceFInal %>%
  sample_frac(0.6, replace = TRUE)

# Imprimir las primeras filas de la muestra
head(sample_60_percent)
```

    ## # A tibble: 6 × 28
    ##    symb normLoss make       fuelType aspiration nDoors bodyStyle   driveWheels
    ##   <int>    <int> <fct>      <fct>    <fct>      <fct>  <fct>       <fct>      
    ## 1     1      128 nissan     diesel   std        two    sedan       fwd        
    ## 2     1      101 honda      gas      std        two    hatchback   fwd        
    ## 3     0      161 peugot     gas      std        four   sedan       rwd        
    ## 4     2        0 audi       gas      std        two    sedan       fwd        
    ## 5     0      102 subaru     gas      std        four   sedan       4wd        
    ## 6     3        0 volkswagen gas      std        two    convertible fwd        
    ## # ℹ 20 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>, equalFrequency <bins>,
    ## #   equalWidth <bins>

### (b) A stratified sample of 60% of the cases of cars, according to the fuelType attribute.

``` r
set.seed(111019)  # Establece la semilla para reproducibilidad


# Calcula el tamaño de la muestra para cada categoría de fuelType
tamaños_muestra <- newCarInsuranceFInal %>%
  group_by(fuelType) %>%
  summarise(tamaño_muestra = round(n() * 0.6))

# Obtiene la muestra estratificada
muestra_estratificada <- newCarInsuranceFInal %>%
  inner_join(tamaños_muestra, by = "fuelType") %>%
  group_by(fuelType) %>%
  sample_frac(size = 0.6, replace = FALSE)

# Muestra los primeros casos de la muestra estratificada
head(muestra_estratificada)
```

    ## # A tibble: 6 × 29
    ## # Groups:   fuelType [1]
    ##    symb normLoss make          fuelType aspiration nDoors bodyStyle driveWheels
    ##   <int>    <int> <fct>         <fct>    <fct>      <fct>  <fct>     <fct>      
    ## 1     0        0 mazda         diesel   std        four   sedan     rwd        
    ## 2    -1       93 mercedes-benz diesel   turbo      four   sedan     rwd        
    ## 3     2       94 volkswagen    diesel   std        four   sedan     fwd        
    ## 4     0       93 mercedes-benz diesel   turbo      two    hardtop   rwd        
    ## 5    -1       95 volvo         diesel   turbo      four   sedan     rwd        
    ## 6     0       91 toyota        diesel   std        four   hatchback fwd        
    ## # ℹ 21 more variables: engineLocation <fct>, wheelBase <dbl>, length <dbl>,
    ## #   width <dbl>, height <dbl>, curbWeight <int>, engineType <fct>,
    ## #   nrCylinds <fct>, engineSize <int>, fuelSystem <fct>, bore <dbl>,
    ## #   stroke <dbl>, compressionRatio <dbl>, horsePower <int>, peakRpm <int>,
    ## #   cityMpg <int>, highwayMpg <int>, price <int>, equalFrequency <bins>,
    ## #   equalWidth <bins>, tamaño_muestra <dbl>

### (c) Use the table() function to inspect the distribution of values in each of the two samples above

``` r
# Obtener la tabla de frecuencias de la variable fuelType en la muestra del 60%
table(sample_60_percent$fuelType)
```

    ## 
    ## diesel    gas 
    ##      8    115

``` r
# Obtener la tabla de frecuencias de la variable fuelType en la muestra estratificada
table(muestra_estratificada$fuelType)
```

    ## 
    ## diesel    gas 
    ##     12    111

## 4. Load the package corrplot and select the numeric attributes of the car insurance data set.

### (a) Using the function cor(), obtain the pearson correlation coefficient between each pair of variables

``` r
# Cargar el paquete corrplot
library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 4.3.1

    ## corrplot 0.92 loaded

``` r
# Seleccionar las variables numéricas del conjunto de datos
numericVars <- newCarInsuranceFInal %>%
  select_if(is.numeric)

# Calcular la matriz de correlación de Pearson
corMatrix <- cor(numericVars, method = "pearson")

# Visualizar la matriz de correlación
corrplot(corMatrix, method = "color")
```

![](Practica02_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# otra forma de hacerlo
# Seleccionar las variables enteras del conjunto de datos
intnewCarInsuranceFInal <- newCarInsuranceFInal %>% 
    select_if(is.integer)

# Obtener los nombres de las columnas con variables enteras
colint <- newCarInsuranceFInal %>% 
    select_if(is.integer) %>%
    names()

# Calcular la matriz de correlación para las variables enteras
resMat <- cor(intnewCarInsuranceFInal)

# Visualizar la matriz de correlación usando el método 'AOE'
corrplot.mixed(resMat, order = 'AOE')
```

![](Practica02_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

### (b) Apply the function cor.mtest() to the previous result to calculate the p-values and confidence intervals of the correlation coefficient for each pair of variables.

``` r
# Calcular los p-valores y los intervalos de confianza para cada coeficiente de correlación
testRes = cor.mtest(numericVars, conf.level = 0.95)

# Visualizar la matriz de correlación con p-valores significativos
corrplot(corMatrix, p.mat = testRes$p, sig.level = 0.10,
         addCoef.col ='black', order = 'hclust', addrect = 2,tl.cex = 0.9, number.cex = 0.5)
```

![](Practica02_files/figure-gfm/unnamed-chunk-18-1.png)<!-- --> \### (c)
Plot the all correlation information using the function corrplot.
Explore some of its parameters.

``` r
# Cargar el paquete corrplot
library(corrplot)



# Visualizar la matriz de correlación utilizando diferentes métodos
# Método: Números
corrplot(corMatrix, method = 'number', order = 'AOE', diag = FALSE,tl.cex = 0.9, number.cex = 0.5)
```

![](Practica02_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
# Método: Círculos
corrplot(corMatrix, method = 'circle', order = 'AOE', diag = FALSE,tl.cex = 0.9, number.cex = 0.5)
```

![](Practica02_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
# Método: Sombreado
corrplot(corMatrix, method = 'shade', order = 'AOE', diag = FALSE,tl.cex = 0.9, number.cex = 0.5)
```

![](Practica02_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

``` r
# Método: Gráficos de pastel
corrplot(corMatrix, method = 'pie', order = 'AOE', diag = FALSE,tl.cex = 0.9, number.cex = 0.5)
```

![](Practica02_files/figure-gfm/unnamed-chunk-19-4.png)<!-- -->

``` r
# Método: Sombreado (parte inferior) y gráficos de pastel (parte superior)
corrplot(corMatrix, method = 'shade', order = 'AOE', type = 'lower', diag = FALSE,tl.cex = 0.9, number.cex = 0.5)
```

![](Practica02_files/figure-gfm/unnamed-chunk-19-5.png)<!-- -->

``` r
# Método mixto: Sombreado (parte inferior) y gráficos de pastel (parte superior)
corrplot.mixed(corMatrix, lower = 'shade', upper = 'pie', order = 'hclust',tl.cex = 0.9, number.cex = 0.5)
```

![](Practica02_files/figure-gfm/unnamed-chunk-19-6.png)<!-- -->

## 5. Load the data set USJudgeRatings, from the datasets package, containing lawyers’ ratings of state judges in the US Superior Court regarding a set of attributes.

### (a) Apply the function prcomp() to obtain the principal components. Inspect how each variable is obtained by the linear combination of each component.

``` r
# Cargar el conjunto de datos USJudgeRatings
data(USJudgeRatings)

head(USJudgeRatings,10)
```

    ##                CONT INTG DMNR DILG CFMG DECI PREP FAMI ORAL WRIT PHYS RTEN
    ## AARONSON,L.H.   5.7  7.9  7.7  7.3  7.1  7.4  7.1  7.1  7.1  7.0  8.3  7.8
    ## ALEXANDER,J.M.  6.8  8.9  8.8  8.5  7.8  8.1  8.0  8.0  7.8  7.9  8.5  8.7
    ## ARMENTANO,A.J.  7.2  8.1  7.8  7.8  7.5  7.6  7.5  7.5  7.3  7.4  7.9  7.8
    ## BERDON,R.I.     6.8  8.8  8.5  8.8  8.3  8.5  8.7  8.7  8.4  8.5  8.8  8.7
    ## BRACKEN,J.J.    7.3  6.4  4.3  6.5  6.0  6.2  5.7  5.7  5.1  5.3  5.5  4.8
    ## BURNS,E.B.      6.2  8.8  8.7  8.5  7.9  8.0  8.1  8.0  8.0  8.0  8.6  8.6
    ## CALLAHAN,R.J.  10.6  9.0  8.9  8.7  8.5  8.5  8.5  8.5  8.6  8.4  9.1  9.0
    ## COHEN,S.S.      7.0  5.9  4.9  5.1  5.4  5.9  4.8  5.1  4.7  4.9  6.8  5.0
    ## DALY,J.J.       7.3  8.9  8.9  8.7  8.6  8.5  8.4  8.4  8.4  8.5  8.8  8.8
    ## DANNEHY,J.F.    8.2  7.9  6.7  8.1  7.9  8.0  7.9  8.1  7.7  7.8  8.5  7.9

``` r
# Aplicar la función prcomp() para obtener los componentes principales
pca <- prcomp(USJudgeRatings)

# Inspeccionar cómo cada variable se obtiene mediante la combinación lineal de cada componente
var_weights <- pca$rotation

# Visualizar los pesos de cada variable en cada componente
print(var_weights)
```

    ##              PC1          PC2         PC3          PC4         PC5          PC6
    ## CONT  0.00599117  0.933248839 -0.31985402  0.112932711  0.09462326 -0.002863094
    ## INTG -0.23476045 -0.138724061 -0.36981582  0.252211159  0.04590748 -0.463156453
    ## DMNR -0.34774394 -0.232070496 -0.66343490  0.034666432 -0.19413671  0.360824575
    ## DILG -0.28678321  0.047953743  0.22433991  0.272806418 -0.37555702 -0.563858944
    ## CFMG -0.27201855  0.163199298  0.18935451 -0.024818035 -0.47964024  0.169861226
    ## DECI -0.25330240  0.117624159  0.24920335 -0.024620949 -0.42029465  0.368776968
    ## PREP -0.30910791  0.047023458  0.21702209  0.191109898  0.14553660 -0.063861767
    ## FAMI -0.30510111  0.014033493  0.26692334  0.168833090  0.47073965  0.107554568
    ## ORAL -0.33195277  0.009669279  0.03718715  0.007519155  0.25286574  0.141530777
    ## WRIT -0.31396635 -0.018092578  0.11487882  0.141585317  0.29475017  0.227041086
    ## PHYS -0.27750639  0.096014861  0.03603598 -0.859239241  0.09522754 -0.241473592
    ## RTEN -0.35932337 -0.033595756 -0.19563408 -0.152859043  0.02899495 -0.164203864
    ##               PC7          PC8         PC9        PC10        PC11         PC12
    ## CONT  0.017715828  0.049191638 -0.03452922 -0.02698275  0.01671326  0.007431325
    ## INTG -0.365508583 -0.417669580 -0.37748813  0.18025018 -0.15992927 -0.006194162
    ## DMNR  0.394379189  0.167204881 -0.12329552 -0.03720489  0.11310307 -0.055818131
    ## DILG  0.255305398  0.282668734 -0.02924283 -0.41571794  0.09706351 -0.059529611
    ## CFMG  0.108985626 -0.680468974  0.26897827  0.13173613  0.19431559 -0.040188043
    ## DECI -0.482835530  0.317726213 -0.40752158  0.09720424 -0.18697238 -0.045701668
    ## PREP  0.383738578  0.169372297  0.07226971  0.64066551 -0.34000143  0.293499296
    ## FAMI  0.024308258  0.005991298 -0.22947394  0.12291413  0.53543928 -0.467666232
    ## ORAL -0.007364589 -0.116735981  0.27199080 -0.35542800 -0.63676735 -0.429930400
    ## WRIT -0.102306863 -0.141542794 -0.06726958 -0.43503830  0.10601295  0.702784185
    ## PHYS  0.158736876 -0.046966945 -0.27452313 -0.02638736 -0.01173895  0.056591229
    ## RTEN -0.465619269  0.294439861  0.62456547  0.15469084  0.24508578  0.052358190

### (b) Load the package ggbiplot and plot the two first components with the function ggbiplot(). You can label each point with the lawyer’s name by setting the labels parameter.

``` r
# Cargar el paquete "ggbiplot"
library(plyr)
```

    ## Warning: package 'plyr' was built under R version 4.3.1

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
library(ggbiplot)
```

    ## Loading required package: scales

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

    ## Loading required package: grid

``` r
# Plotear los dos primeros componentes utilizando ggbiplot
ggbiplot(pca, labels = rownames(USJudgeRatings), obs.scale = 1, var.scale = 1)
```

![](Practica02_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->
