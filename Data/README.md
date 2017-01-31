# Multiclass Data Sets Used #

## Load the Data Sets in R ##
For loading all the 16 data sets in R you need to issue the following command:

```r
load("MulticlassDataSets.RData")
```

This will load into R an object named `DSs` which contains a list with 16 objects of class `dataset` from package DMwR.


## Description ##
The main characteristics of the 16 multiclass data sets included in this folder are as follows:

|ID |              name  |     TgtName  | NrFeat  | NrNominal  | NrNumeric |  NrExamp  | NrClasses  | Source  |
|-- | ------------------ | ------------ | ------- | ---------- | ---------- | -------- | ---------- | --------- |
|1  |      balanceScale  |       class  |      4  |         0  |         4  |     625  |         3  | UCI |
|2  |            blocks  |       class  |     10  |         0  |        10  |    5473  |         5  | UCI |
|3  |               car  |       class  |      6  |         6  |         0  |    1728  |         4  | UCI |
|4  |     contraceptive  |       class  |      9  |         0  |         9  |    1473  |         3  | UCI |
|5  |       dermatology  |       Class  |     34  |         0  |        34  |     358  |         6  | UCI |
|6  |             ecoli  |       class  |      7  |         0  |         7  |     336  |         8  | UCI |
|7  |             glass  | TypeOfGlass  |     10  |         0  |        10  |     214  |         6  | UCI |
|8  |        newThyroid  |       Class  |      5  |         0  |         5  |     215  |         3  | UCI |
|9  | solarFlare2Cclass  |     C.class  |      9  |         3  |         6  |    1066  |         8  | UCI |
|10 | solarFlare2Mclass  |     M.class  |      9  |         3  |         6  |    1066  |         6  | UCI |
|11 | solarFlare2Xclass  |     X.class  |      9  |        3   |         6  |    1066  |         3  | UCI |
|12 |            splice  |       class  |     60  |        60  |         0  |    3175  |         3  | UCI |
|13 |    wineQualityRed  |     quality  |     11  |         0  |        11  |    1599  |         6  | UCI |
|14 |  wineQualityWhite  |     quality  |     11  |         0  |        11  |    4898  |         7  | UCI |
|15 |             yeast  |       class  |      8  |         0  |         8  |    1484  |        10  | UCI |
|16 |               zoo  |        type  |     16  |         0  |        16  |     101  |         7  | UCI |



More details in the UCI Machine Learning Repository[1].


## References 
[1] Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
