## Reproducing Paper Experiments

## Contents

  This folder contains  all code and data that is necessary to replicate the
  experiments reported in the paper *"Relevance-based Evaluation Metrics for
    Multi-class Imbalanced Domains"*[1]. 

  The folder contains the following files:

  - **MetricsImpl.R**            the implemented metrics 

  - **threeCasesResults.R**      the script for obtaining the metrics results
                                 for the three cases discussed in the paper

  - **discriminationTests.R**    the script for obtaining the assessment of the
                                 discrimination capability of all metrics 

  - **README.md**               this file

## Necessary Software

To replicate these experiments you will need a working installation
  of R. Check [https://www.r-project.org/] if you need to download and install it.

In your R installation you also need to install the following additional R package:

  - igraph

The above package i necessary for obtaining the partial and total order of classes as defined in the paper. igraph package can be installed from CRAN Repository directly as any "normal" R package. Essentially you only need to issue the following command in R:

```r
install.packages("igraph")
```


## Running the experiences:
  
  To run the experiments for the three cases described in the paper 
  you execute R in the folder with the code and then issue the command:
```r
source("threeCasesResults.R")
```

  To run the experiments for assessing the discrimination capability of metrics
  you execute R in the folder with the code and then issue the command:
```r
 source("discriminationTests.R")
```
  Alternatively, you may run the experiments directly from a Linux terminal
  (useful if you want to logout because some experiments take a long
  time to run):

  - for the three cases:
```bash
nohup R --vanilla --quiet < threeCasesResults.R &
```
  - for the discrimination tests:
```bash  
 nohup R --vanilla --quiet < discrimination Tests.R &
```

### References
[1] Branco, P. and Torgo, L. and Ribeiro R.P. (2017) *"Relevance-based Evaluation Metrics for Multi-class Imbalanced Domains"* Advances in Knowledge Discovery and Data Mining - 21th Pacific-Asia Conference, PAKDD 2017, Jeju, South Korea. (to appear).

