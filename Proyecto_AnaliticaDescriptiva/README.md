# Proyecto - Analítica Descriptiva

## Curso: Data Mining y Machine Learning 2023-A

## Autor: Javier Criollo

## Video explicativo del proyecto



| Plataforma | URL |
| ------ | ------ |
| Youtube | https://youtu.be/z5HAPr-j-h0 |

[![Proyecto Analítica Descriptiva - Javier Criollo](https://res.cloudinary.com/marcomontalbano/image/upload/v1688004014/video_to_markdown/images/youtube--z5HAPr-j-h0-c05b58ac6eb4c4700831b2b3070cd403.jpg)](https://youtu.be/z5HAPr-j-h0 "Proyecto Analítica Descriptiva - Javier Criollo")
## Tema: 

### Se busca ver cuál es la provincia donde hubo más desaparecidos en Ecuador, encontrando el perfil de la persona que es más frecuente a desaparecer

## Objetivos:

- Determinar el perfil de persona más susceptible a desaparecer por provincia.

- Categorizar los datos por rangos de edad.

- Determinar el mes donde hubo más desaparecidos, siendo los meses desde Enero a Abril del año 2023.

## Datos:

### Fuentes de datos:

Se usa el dataset de personas desaparecidas en ecuador, de enero a abril del 2023

El [dataset de Personas Desaparecidas](https://www.datosabiertos.gob.ec/dataset/personas-desaparecidas) del Ministerio de Gobierno se obtiene en [datosabiertos.gob.ec/group/seguridad-y-defensa](https://www.datosabiertos.gob.ec/group/seguridad-y-defensa) 

### Tipos de datos:

Los datos son de tipo numéricos, string y date, siendo las columnas para usar son:

Provincia, edad, sexo, Motivo Desaparición, Fecha Desaparición.

### Procesamiento:

Se normaliza la cantidad de perfiles por cada provincia, para ello previamente en cada provincia se agrupa por perfiles y se cuenta cuantas veces se repite cada perfil, se guarda ese dato en la columna Cantidad y se divide cada dato de la columna Cantidad por el numero de habitantes de cada provincia. 

Una provincia puede tener muchos perfiles.

1 perfil puede repetirse muchas veces por esa razon su cantidad puede ser 1 o más.

Se graficá la 

Se categorizara por rangos de edad, siendo los rangos:

| Edad  | Categoria |
| ------ | ------ |
| [0-11] | Niños |
| [12-17] | Jóvenes |
| Los adulto desde los 18 años en adelante |  Se crea rangos cada 5 años<br>Ejemplos:<br>18-22<br>23-27 |

Se crea una nueva columna llamada Mes, los datos de Mes se obtiene desde la columna Fecha Desaparición.

## Introduccion 

El proyecto de Analitica descriptiva busca responder cual es el perfil de persona mas suceptible a desaparecer en el Ecuador y por cada provincia.

Un perfil se entiende como la relacion que existe entre los datos de Provincia, Edad Aprox., Sexo, Motivo Desaparción.

## Estructura del Repositorio

```
    |-- data
    |   |-- mdg_personasdesaparecidas_pm_2023_enero_abril.csv
    |
    |-- Propuesta-ProyectoAnaliticaDescriptiva_JavierCriollo.pdf
    |-- Proyecto_AnalíticaDescriptiva_PersonasDesaparecidasEcuador.ipynb
    |-- README.md
```

