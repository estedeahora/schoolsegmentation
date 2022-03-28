
# ¿Uno o muchos sistemas? Diferencia entre escuelas en la ciudad de Buenos Aires (Argentina)

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/estedeahora/schoolsegmentation/master?urlpath=rstudio)

This repository contains the data and code for my paper:

*Este repositorio contiene los datos y el código de mi paper:*

> Serrati, P. S., (2022). *¿Uno o muchos sistemas? Diferencia entre
> escuelas en la ciudad de Buenos Aires (Argentina)*. En revisión
> <!-- \> <https://doi.org/xxx/xxx> -->

<!-- Our pre-print is online here: -->

> <!-- Authors, (YYYY). r Title. Name of journal/book, Accessed r format(Sys.Date(), "%d %b %Y"). Online at https://doi.org/xxx/xxx -->

### How to cite the compendium / Cómo citar el compendium

Please cite this compendium as:

*Por favor cite este compendium como:*

> Serrati, P. S. (2022). *Compendium of R code and data for ¿Uno o
> muchos sistemas? Diferencia entre escuelas en la ciudad de Buenos
> Aires (Argentina)*. Accessed 28 mar. 2022.
> <!-- > Online at <https://doi.org/xxx/xxx> -->

## Contents / Contenidos

The *`analysis`* directory contains:

*La carpeta `analysis` contiene:*

<!-- -   [:file_folder: analysis](/analysis/): R Markdown source document -->
<!--     for manuscript (only after peer-review). -->

-   [:file_folder: scripts](/scripts): Includes R code to reproduce the
    analysis ([02_analysis.R](/analysis/scripts/02_analysis.R)), figures
    and tables
    ([03_tables-and-figures.R](/analysis/scripts/03_tables-and-figures.R)).
    // *El código r para reproducir el análisis, las figuras y las
    tablas del artículo.*
-   [:file_folder: data](/analysis/data): Anonimiced data used in the
    analysis. The data was processed to guarantee the anonymization of
    the original CUE. The data location that was assigned is random up
    to 300m from the original location. // *La base de dato anonimizada
    utilizada en el análisis. Los datos fueron procesados para
    garantizar el anonimato del CUE original. La localización fue
    asignada aleatoriamente hasta 300m de la ubicación original.*
-   [:file_folder: figures](/analysis/figures): Plots and other
    illustrations. // *Figuras resultantes del análisis.*
-   [:file_folder:
    supplementary-materials](/analysis/supplementary-materials):
    Supplementary materials including notes and other documents prepared
    and collected during the analysis. // *Materiales suplementarios y
    anexos*.

## How to run in your browser or download and run locally / Cómo ejecutar localmente el anáisis

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

*Este compendio de investigación fue desarrollado usando el lenguaje de
programación estadística R. Para trabajar con el compendium es necesario
tener instalado en su computadora* [R
software](https://cloud.r-project.org/) y, opcionalmente, [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium as a zip from this URL:
[main.zip](/archive/main.zip). After unzipping:

*Puede descargar el compendium como un archivo zip desde:
[main.zip](/archive/main.zip). Luego de descomprimirlo:*

-   open the `.Rproj` file in RStudio. // *Abra el archivo que finaliza
    como `.Rproj` en Rstudio.*
-   run `devtools::install()` to ensure you have the packages this
    analysis depends on (also listed in the [DESCRIPTION](/DESCRIPTION)
    file). // *Ejecute `devtools::install()` en su consola para
    asegurarse de que tiene instalados todos los paquetes necesarios
    (también listados en el archivo de [DESCRIPTION](/DESCRIPTION)).*
-   finally, open `analysis/scripts` folder and run the code
    sequentially. // *Finalmente, abra la carpeta `analysis/scripts` y
    ejecute el código secuencialmente.*

### Licenses

<!-- **Text and figures :** -->
<!-- [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/) -->

**Code :** See the [DESCRIPTION](DESCRIPTION) file

<!-- **Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/) -->
<!-- attribution requested in reuse -->

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
