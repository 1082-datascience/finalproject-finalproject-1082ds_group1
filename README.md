# Breast Cancer Wisconsin (Diagnostic) Data Set | Kaggle

### Groups
* 許育庭, 108753127
* 吳映函, 108753102
* 陳瑀芋, 108753144

### Goal

Our goal is to create a model that will allow us to predict whether a breast cancer cell is benign or malignant.

### Demo 

```R
Rscript code/your_script.R --data data/training --output results/performance.tsv
```
* any on-line visualization 

*(use shiny)*

## Folder organization and its related information

### docs
* Your presentation, 1082_datascience_FP_group1.ppt/pptx/pdf, by **Jun. 16**
*(remeber final ppt)*

### data

* Source
	* Breast Cancer Wisconsin (Diagnostic) Data Set | Kaggle
	* https://www.kaggle.com/uciml/breast-cancer-wisconsin-data
* Input format
	* One .csv file.
	* Attribute Information:
		* 1) ID number
		* 2) Diagnosis (M = malignant, B = benign)
		* 3-32) Ten real-valued features are computed for each cell nucleus:
			* a) radius (mean of distances from center to points on the perimeter)
			* b) texture (standard deviation of gray-scale values)
			* c) perimeter
			* d) area
			* e) smoothness (local variation in radius lengths)
			* f) compactness (perimeter^2 / area - 1.0)
			* g) concavity (severity of concave portions of the contour)
			* h) concave points (number of concave portions of the contour)
			* i) symmetry
			* j) fractal dimension ("coastline approximation" - 1)
		* The *(mean)*, *(standard error)* and *("worst" or largest)* (mean of the threelargest values) of these features were computed for each image, resulting in 30 features. For instance, field 3 is Mean Radius, field 13 is Radius SE, field 23 is Worst Radius.
		* Which is 10 features x 3 measurements = 30 features

* Any preprocessing?
  * In the beginning, we use covariance matrix to do the PCA, but the Scree-plots suggest that using a covariance matrix is not the correct approach for calculating the principal components.
  * We chose correlation matrix to try again.

### code

* Which method do you use?
* What is a null model for comparison?
* How do your perform evaluation? ie. Cross-validation, or extra separated data

### results

* Which metric do you use 
  * precision, recall, R-square
* Is your improvement significant?
* What is the challenge part of your project?

## Reference
* https://www.kaggle.com/shravank/predicting-breast-cancer-using-pca-lda-in-r
* https://www.kaggle.com/mirichoi0218/classification-breast-cancer-or-not-with-15-ml
* https://www.kaggle.com/paultimothymooney/decision-trees-for-binary-classification-0-99
* https://www.kaggle.com/kanncaa1/statistical-learning-tutorial-for-beginners/notebook
* https://www.kaggle.com/kanncaa1/statistical-learning-tutorial-for-beginners/notebook
* https://www.kaggle.com/bbloggsbott/feature-selection-correlation-and-p-value/data
* Packages
	* argparse
	* corrplot
	* caret
	* rpart
	* ROCR
	* e1071
	* randomForest
	* Formula
	* class
	* highcharter
	* gbm



