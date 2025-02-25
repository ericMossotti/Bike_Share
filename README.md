# Bike Share Analysis Project

This is a reproducible report using a cutting-edge language agnostic data analysis framework. It is a Quarto website project rendered from .QMD files in the style of a website, and utilizes custom CSS/SCSS styling. This project was written in R, so it enlists a *'renv'* or reproducible R-environment, so the computational environment can be restored from files contained in this repository on your local system. I have found the Posit/Rstudio/R ecosystem, currently offers the most in the realm of data analysis and creating approachable, user-friendly, and nice outputs. I hope you enjoy this project and find inspiration for creating your own future projects. That said, I like using Rstudio for these kinds of projects, but it could also be reproduced in IDEs such as VScode.

## Summary

-   Bridges a new language agnostic data analysis framework with more traditional HTML/CSS/JS type web development frameworks
-   Reproducible, eye-catching visualizations

## To Reproduce 

First, have R and Python installed globally on your system.

In Rstudio:

Go to: `File`

Click `New Project`

Select `Version Control`

Paste this repo's code link

in R console:

``` r
install.packages("renv")
```

``` r
renv::restore()
```

``` r
renv::snapshot()
```

Follow prompts to install R and Python packages during these steps where necessary. If doing this without an R console, you might be able to run that R code in R-code chunk(s) within the `index.qmd` file of the project.
