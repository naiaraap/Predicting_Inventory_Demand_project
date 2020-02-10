Forecasting Sales Demand for Inventor

A project for predict weekly inventory demand for a particular store in Mexico developed during data scientist training at the Data Science Academy.

DESCRIPTION:
Planning a celebration is a balancing act of preparing just enough food to go around without being stuck eating the same leftovers for the next week. The key is anticipating how many guests will come. Grupo Bimbo must weigh similar considerations as it strives to meet daily consumer demand for fresh bakery products on the shelves of over 1 million stores along its 45,000 routes across Mexico. In this project, I am suppose to develop a model to accurately forecast inventory demand based on historical sales data.

The scripts were developed in R language at Rstudio.

The original datasets are available on kaggle. Link for download: https://www.kaggle.com/c/grupo-bimbo-inventory-demand/data.
The dataset used in this project is in the "data" folder. The train dataset sample was obtained by a python script: GetSampleTrain.py

The project was done in six sequencial scripts and an auxiliar script which stores useful functions and variables:
1.InventoryDemandFeature Engineering.R: obtains, modifies and handles training and test datasets.

2.InventoryDemandExploratoryAnalysis.R: runs exploratory and visual data analysis.

3.InventoryDemandFeatureSelection.R: runs feature selection by importance assessment.

4.InventoryDemandCreateModel.R: runs models creation.

5.InventoryDemandScoreModel.R: runs models score.

6.InventoryDemandEvaluateModel.R: runs model optimization and evaluation.

InventoryDemandTools.R: contains useful functions and variables.

The generated graphics are in the "graphs" folder and the text output, in the "statistic" folder, "Statistic.txt" file.
The project documentation with the scripts, outputs and step-by-step description is in the file "InventoryDemand.pdf".

Computer specifications:<br/>
Operational System: Linux, Ubuntu v.18.04.1<br/>
CPU Capacity: 3100MHz.
