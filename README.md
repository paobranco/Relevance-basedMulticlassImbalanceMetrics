## Relevance-based Evaluation Metrics for Multi-class Imbalanced Domains - PAKDD 2017 

This repository has all the code used in the experiments carried out in the paper *"Relevance-based Evaluation Metrics for Multi-class Imbalanced Domains"* [1].


This repository is organized as follows:

* **R_Code** folder - contains all the code for reproducing the experiments presented in the paper and the additional experiments carried out with real world multiclass data sets;
* **Figures** folder - contains all the figures obtained from the experimental evaluation on 16 real world data sets;
* **Data** folder - contains the 16 multiclass data sets used in the additional experiments carried out with real world data sets;


### Requirements

The experimental design was implemented in R language. Both code and data are in a format suitable for R environment.

In order to replicate these experiments you will need a working installation
  of R. Check [https://www.r-project.org/] if you need to download and install it.

In your R installation you also need to install the following additional R packages:

  - DMwR
  - e1071
  - rpart
  - igraph

  All the above packages, can be installed from CRAN Repository directly as any "normal" R package. Essentially you need to issue the following command within R:

```r
install.packages(c("DMwR", "e1071", "rpart", "igraph"))
```
To replicate the figure in this repository you will also need to install the package:

  - ggplot2

As with any R package, we only need to issue the following command:

```r
install.packages("ggplot2")
```

Check the other README files in each folder to see more detailed instructions on how to run the experiments.

*****

### References
[1] Branco, P. and Torgo, L. and Ribeiro R.P. (2017) *"Relevance-based Evaluation Metrics for Multi-class Imbalanced Domains"* Advances in Knowledge Discovery and Data Mining - 21th Pacific-Asia Conference, PAKDD 2017, Jeju, South Korea. (to appear).