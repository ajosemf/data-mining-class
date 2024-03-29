# Data Mining Class
Tasks developed in the PPCIC data mining course at Federal Center for Technological Education of Rio de Janeiro (CEFET/RJ)

## Course outline
Study of data mining techniques, i.e., knowledge discovery from data (KDD). The KDD process includes exploratory data analysis, data preprocessing identification of outliers, pattern mining, prediction, clustering, and data warehouses.

## Datasets
The tasks _Exploratory Data Analysis_, _Data Preprocessing_, _Frequent Pattern Mining_ and _Prediction_ were applied on the [Brazilian Flight Dataset (BFD)](https://ieee-dataport.org/documents/brazilian-flights-dataset). This dataset was built by CEFET/RJ researchers and includes the data sources from regular flights (VRA) and weather data. 

For the _Clustering_ task, the [Transporte Rodoviário: GPS dos Ônibus](https://www.data.rio/documents/PCRJ::transporte-rodovi%C3%A1rio-gps-dos-%C3%B4nibus/about) dataset were used, which contains the GPS coordinates of the buses in the city of Rio de Janeiro collected minute by minute.

## Requirements
[R](http://cran.rstudio.com/bin/linux/ubuntu/) >= 4.2.1

## Preliminaries
[Download](https://ieee-dataport.org/documents/brazilian-flights-dataset) the BFD dataset and deploy it in the [data directory](data).

[Download](https://docs.google.com/uc?export=download&id=1D2BlwCf-ZCTHrnXInrOFPmIt0gbeWtxa) the _Transporte Rodoviário: GPS dos ônibus_ sample dataset and deploy it in the [data directory](data).

## Exploratory Data Analysis
All plots produced in the exploratory analysis are available in the [plots directory](exploratory_analysis/plots) and can be reproduced by running the [exploratory_analysis](exploratory_analysis/exploratory_analysis.R) file.

## Data Preprocessing
Removing columns with unique values. Removal of outliers and inconsistent records. Missing data imputation and data transformation. As a result, the dataset has been shrunk from 820.403 records and 46 features to 612.287 records and 41 features. Data preprocessing can be reproduced by running the [data_preprocessing](data_preprocessing/data_preprocessing.R) file.

## Frequent Pattern Mining
Discretization and selection of variables. Application of frequent pattern mining techniques. Code available in [pattern_mining](pattern_mining/patern.R) file.

## Prediction
Decision Tree, KNN and Naive Bayes models were applied to predict the delay of flights. The code is available in the [prediction](prediction/prediction.R) file.

## Clustering
A generalized implementation of K-means were applied to simultaneously cluster and detect outliers. In this task we used a sample of the dataset of the public transport system by bus in the city of RJ. The code is available in the [clustering](clustering/clustering.R) file.
