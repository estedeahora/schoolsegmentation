
# Diferencia entre escuelas en la ciudad de Buenos Aires (Argentina)

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/estedeahora/schoolsegmentation/main?urlpath=rstudio)

### How to cite / Cómo citar

This repository contains the data and code for my paper:

*Este repositorio contiene los datos, el código y materiales
suplementarios de mi artículo:*

> Serrati, P. S., (2023). *Diferencia entre escuelas en la ciudad de
> Buenos Aires (Argentina)*. En revisión
> <!-- \> <https://doi.org/xxx/xxx> -->

<!-- Our pre-print is online here: -->

> <!-- Authors, (YYYY). r Title. Name of journal/book, Accessed r format(Sys.Date(), "%d %b %Y"). Online at https://doi.org/xxx/xxx -->

Please cite this compendium as:

*Por favor cite este compendium como:*

> Serrati, P. S. (2023). *Compendium of R code and data for Diferencia
> entre escuelas en la ciudad de Buenos Aires (Argentina)*. Accessed 30
> may. 2023. <!-- > Online at <https://doi.org/xxx/xxx> -->

## Contents / Contenidos

The *`analysis`* directory contains:

*La carpeta `analysis` contiene:*

<!-- -   [:file_folder: analysis](/analysis/): R Markdown source document -->
<!--     for manuscript (only after peer-review). -->

- [:file_folder: scripts](/scripts): Includes R code to reproduce the
  analysis ([02_analysis.R](/analysis/scripts/02_analysis.R)), figures
  and tables
  ([03_tables-and-figures.R](/analysis/scripts/03_tables-and-figures.R)
  and
  ([04_tables-and-figures-appendix.R](/analysis/scripts/04_tables-and-figures-appendix.R)).
  // *Incluye el código R necesario para reproducirel análisis
  ([02_analysis.R](/analysis/scripts/02_analysis.R)); y las figuras y
  tablas del cuerpo
  ([03_tables-and-figures.R](/analysis/scripts/03_tables-and-figures.R));
  del anexo
  ([04_tables-and-figures-appendix.R](/analysis/scripts/04_tables-and-figures-appendix.R)).*
- [:file_folder: data](/analysis/data): Anonimiced data used in the
  analysis. The data was processed to guarantee the anonymization of the
  original CUE. // *La base de dato anonimizada utilizada en el
  análisis. Los datos fueron procesados para garantizar el anonimato del
  CUE original.*
- [:file_folder: figures](/analysis/figures): Plots and other
  illustrations. // *Figuras resultantes del análisis.*
- [:file_folder:
  supplementary-materials](/analysis/supplementary-materials):
  Supplementary materials including notes and other documents prepared
  and collected during the analysis. // *Materiales suplementarios
  (Anexos).*

The Appendix is available at:
<https://pabloserrati.netlify.app/en/supplementary/2022_school-segmentation/>

*El Anexo está disponible online en:
<https://pabloserrati.netlify.app/supplementary/2022_school-segmentation/>*

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

- open the `.Rproj` file in RStudio. // *Abra el archivo que finaliza
  como `.Rproj` en Rstudio.*
- run `devtools::install()` to ensure you have the packages this
  analysis depends on (also listed in the [DESCRIPTION](/DESCRIPTION)
  file). // *Ejecute `devtools::install()` en su consola para asegurarse
  de que tiene instalados todos los paquetes necesarios (también
  listados en el archivo de [DESCRIPTION](/DESCRIPTION)).*
- finally, open `analysis/scripts` folder and run the code sequentially.
  // *Finalmente, abra la carpeta `analysis/scripts` y ejecute el código
  secuencialmente.*

### Licenses

**Text and figures:**
[CC-BY-NC-4.0](http://creativecommons.org/licenses/by-nc/4.0/)

**Code :**
[CC-BY-NC-4.0](http://creativecommons.org/licenses/by-nc/4.0/)

**Data :**
[CC-BY-NC-4.0](http://creativecommons.org/licenses/by-nc/4.0/)

<!-- attribution requested in reuse -->

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
