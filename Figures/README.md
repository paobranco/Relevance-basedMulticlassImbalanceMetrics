## Relevance-based Evaluation Metrics for Multi-class Imbalanced Domains - PAKDD 2017 (Extra Material)

This folder contains figures obtained from running the code under **CodeExtraMaterial** folder. This code regards experiences with 16 real world multiclass data sets.
This folder contains the following folders with figures:

- **Relevance** folder - contains both a user specified relevance and the relevance automatically estimated form the classes prevalence
- **PartialTotalOrder** folder - contains graphs that represent the total order and partial order used for the metrics calculations 
- **GlobalMetricsRes** folder - includes the global metrics results obtained
- **ResByMetricType** folder - includes the metrics results clustered by metric type


### Relevance Provided and Automatically Determined for each Data Set

Folder **Relevance** contains Figures named as follows: Relevance\<ID\>, where \<ID\> is the data set ID number. Each figure shows both the user provided values and the automatically obtained estimation of each class relevance values for data set \<ID\>. For instance, Relevance1 provides the relevance values provided and estimated for data set 1 (balanceScale).


### Partial Order and Total Order considered for each Data Set

Folder **PartialTotalOrder** contains figures that display the graphs representing the total and partial order considered for a given data set. Figures with names POTO\<ID\> contain two directed graphs. One of the graphs represents the Partial Order considered for the data set \<ID\> and the other represents the Total Order considered for the same data set. For instance, POTO3 displays the directed graph for the partial order considered on data set with ID 5, the dermatology data set, and the directed graph for the totak order considered on the same data set.


### Global Metrics Results

Folder **GlobalMetricsRes** contains figures named Global\<ID\>. Each one of these figures presents the global results for data set \<ID\>, on the three learning systems considered in the experiments and all the 33 metrics analised in the paper. 


### Evaluation Results by Metric Type

Folder **ResByMetricType** includes the metrics results clustered by metric type. This folder contains one figure for each of the metrics types: Average F1-based, F1-based, CBA-based, precision-based and recall-based. Each figure shows the results on each data set, considering the aggregated values of the metrics on the three learning systems.
