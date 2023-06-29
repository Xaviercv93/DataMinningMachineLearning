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

## Prerequisites

To work with the exercises in this repository, it is recommended to attend classes in order to have a good understanding of data analysis, as well as strong competence in programming concepts.

## Repository Structure

The repository is organized as follows:

```
    |-- data
    |   |-- 01_datamanip
    |   |   |-- Index
    |   |   |-- echocardiogram.data
    |   |   |-- echocardiogram.names 
    |   |-- 02_dataq
    |   |-- ...
    |
    |-- notebooks
    |   |-- 01_DataManip.ipynb
    |   |-- 02_DataQuality.ipynb
    |   |-- ...
    |
    |-- LICENSE
    |-- README.md
    |-- main.py
    |-- requirements.txt
```

The data directory contains datasets used for our exercises.

The notebooks directory contains the set of Jupyter Notebook files (.ipynb) that solve our Python-based and R-based exercises.

The README.md file provides an overview of the repository and instructions for usage.

The requirements.txt file provides the set of libraries required for both Python and R exercises.

## Usage

1. Clone the repository to your local machine using the following command:

```
git clone https://github.com/ivan-carrera/handson_2023A.git
```

2. Navigate to the exercise directory (notebooks).

3. Open the exercise file (.ipynb) using Jupyter Notebook or an integrated development environment (IDE) of your choice.

4. Read the exercise description, follow the instructions, and implement the required code or complete the given tasks.

5. Experiment with different approaches and modify the code as needed to deepen your understanding of the concepts.

6. Use the provided sample solutions as a reference to validate your answers and compare your implementation.

7. Repeat the process for other exercises in the repository to further enhance your skills in Data Mining and Machine Learning.

## Contributing

Contributions to this repository, including the addition of new exercises, bug fixes, and improvements, are welcome. If you would like to contribute, please follow the guidelines outlined in the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## License

This repository is licensed under the [MIT License](LICENSE). By contributing to this repository, you agree that your contributions will be licensed under the same license.

The [MIT License](LICENSE) grants users the freedom to use, modify, and distribute the code and exercises in this repository for both personal and commercial purposes. However, it provides no warranty or liability and requires users to include the original license and copyright notice in any copies or redistributions.

We encourage you to review the full text of the [MIT License](LICENSE) for more details on your rights and responsibilities.

Please note that any contributions you make to this repository will be subject to the terms of the MIT License. Make sure you are comfortable with these terms before submitting your contributions.

If you have any questions or concerns regarding the licensing or usage of this repository, please feel free to reach out.

Happy learning!