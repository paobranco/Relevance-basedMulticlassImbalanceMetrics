## Additional Experiments with Real World Multiclass Data Sets

## Contents

  This folder contains all code and data that is necessary to replicate the
  experiments regarding the additional results on real world data reported in the paper
  *"Relevance-based Evaluation Metrics for Multi-class Imbalanced Domains"*[1].

  The folder contains the following files:
  
  * **MulticlassDataSets.RData**    the real world data sets used

  * **MetricsImpl.R**            the implemented metrics 

  * **Auxs.R**                     the script with auxiliary functions for 
                               obtaining evaluation statistics

  * **ExpsMulticlassData.R**       the script for obtaining the performance results 
                               on the given data sets for all considered learning systems
                               and all metrics analised in the paper 

  * **README.md**                  this file


## Necessary Software

To replicate these experiments you will need a working installation
  of R. Check [https://www.r-project.org/] if you need to download and install it.

In your R installation you also need to install the following additional R packages:

  - DMwR  (for the evaluation infrastructure)
  - igraph  (for obtaining the partial and total order of classes as defined in the paper)
  - rpart  (for the decision tree learning system)
  - e1071  (for the svm and naiveBayes learning systems)

  All the above packages, can be installed from CRAN Repository directly as any "normal" R package. Essentially you need to issue the following command within R:

```r
install.packages(c("DMwR", "e1071", "rpart", "igraph"))
```


## Running the experiences:
  
To run the experiments for the three cases described in the paper 
  you execute R in the folder with the code and then issue the command:
  
```r
 source("ExpsMulticlassData.R")
```
  
  Alternatively, you may run the experiments directly from a Linux terminal as follows:
  
```bash
 nohup R --vanilla --quiet < ExpsMulticlassData.R &
```


### References
[1] Branco, P. and Torgo, L. and Ribeiro R.P. (2017) *"Relevance-based Evaluation Metrics for Multi-class Imbalanced Domains"* Advances in Knowledge Discovery and Data Mining - 21th Pacific-Asia Conference, PAKDD 2017, Jeju, South Korea. (to appear).
