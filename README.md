# Data Mining Class
Tasks developed in the PPCIC data mining course at Federal Center for Technological Education of Rio de Janeiro (CEFET/RJ)

## Course outline
Study of data mining techniques, i.e., knowledge discovery from data (KDD). The KDD process includes exploratory data analysis, data preprocessing identification of outliers, pattern mining, prediction, clustering, and data warehouses.

## Datasets
The tasks were applied on the Brazilian Flight Dataset (BFD). This dataset was built by CEFET/RJ researchers and includes the data sources from regular flights (VRA) and weather data. For more information, see [this resource](https://ieee-dataport.org/documents/brazilian-flights-dataset).

## Requirements
[R](http://cran.rstudio.com/bin/linux/ubuntu/) >= 4.2.1

## Preliminaries
[Download](https://ieee-dataport.org/documents/brazilian-flights-dataset) the BFD dataset and deploy it in the [data directory](data).

## Exploratory Data Analysis
All plots produced in the exploratory analysis are available in the [plots directory](exploratory_analysis/plots) and can be reproduced by running the [exploratory_analysis](exploratory_analysis/exploratory_analysis.R) file.

## Data Preprocessing
Removing columns with unique values. Removal of outliers and inconsistent records. Missing data imputation and data transformation. As a result, the dataset has been shrunk from 820.403 records and 46 features to 612.287 records and 41 features. Data preprocessing can be reproduced by running the [data_preprocessing](data_preprocessing/data_preprocessing.R) file.

## Frequent Pattern Mining
Discretization and selection of variables. Application of frequent pattern mining techniques. Code available in [pattern_mining](pattern_mining/patern.R) file.
