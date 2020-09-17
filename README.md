# DGINN-visualization: a Shiny app associated to DGINN

This Shiny app was developed to provide an easy visualization of results obtained from screening multiple genes using [DGINN](https://github.com/leapicard/DGINN).

An online version is available on [ShinyApps](https://leapicard.shinyapps.io/DGINN-visualization/). However, to avoid an overburden on the online version, the visualization is limited to 100 genes. Alternatively, it is possible to use the app locally, by using it directly in R with the following command:

```{R}
shiny::runGitHub("leapicard/DGINN-visualization",  launch.browser =T)
```