---
title: "R Notebook"
output:
  word_document: default
  pdf_document: default
html_notebook: default
---
# Factor Analysis of boston housing data:
### 1. Load
and check the data

```{.python .input}
%%R
x <- read.table("bostonh.dat")
x[1:5,]
```

### 2. Transforming the data
As per given in the book, we would transform the
data to obtain the best interpretation by bringing the data on comparable scale.

```{.python .input}
%%R
xt = x
xt[,1]  = log(boston[,1])
xt[,2] = boston[,2]/10
xt[,3]  = log(boston[,3])
xt[,5]  = log(boston[,5])
xt[,6]  = log(boston[,6])
xt[,7] = boston[,7]^2.7/10000
xt[,8]  = log(boston[,8])
xt[,9]  = log(boston[,9])
xt[,10]  = log(boston[,10])
xt[,11] = exp(0.4*boston[,11])/1000
xt[,12] = boston[,12]/100
xt[,13] = sqrt(boston[,13])
xt[,14] = log(boston[,14])
```

#### Removing the variable x4

```{.python .input}
%%R
xt <- xt[,-c(4)]
colnames(x) = c("X1", "X2", "X3", "X5", "X6", "X7", "X8", "X9", 
                   "X10", "X11", "X12", "X13", "X14")
xt[1:5,]
```

### 3. Standardizing the data
We are standardizing the dataset to bring the
whole data on a same scale.
Standardized data will have the mean equals to
"zero" and standard deviation equals to "one".

```{.python .input}
%%R
scaled_data <- scale(xt)
scaled_data[1:5,]
```

### 4. Factor Analysis
For factor analysis of the data, first we will obtain the
covariance matrix.

```{.python .input}
%%R
cov <- cov(scaled_data)
cov[1:5,]
```

We are not sure about the how many factors we should consider for analysis.
Hence, first we are calculating P-Values of factors.

```{.python .input}
%%R
i = 1:8
for(a in i){
  print(factanal(scaled_data, factors = a, rotation = "none")$PVAL)
}
```

As P-Values are beyonf 0.05 after factor 6, hence we can consider maximum 6
factors for analysis.

As given in the question, we are considering first 3
factors only. Let's apply factor analysis on standardized data.

```{.python .input}
%%R
load_mat <- factanal(scaled_data, factors = 3, rotation = "none")
```

#### Loadings
Displaying the loading corresponding to first 3 factos. Loadings
explains the correlation between the factors and variables.

```{.python .input}
%%R
load_mat$loadings
```

#### Uniqueness of variables
Lesser the uniqueness of a variable, more the
variance explained by common factors (F).

```{.python .input}
%%R
load_mat$uniquenesses
```

### 5. Plot
Plotting the two factors at a time.

```{.python .input}
%%R
par(mfcol = c(2,2))
plot(load_mat$loadings[,1], load_mat$loadings[,2], type='n', xlab = "Factor1", ylab = "Factor2")
text(load_mat$loadings[,1], load_mat$loadings[,2], colnames(x))
abline(h=0,v=0)

#Factor1 & Factor3
plot(load_mat$loadings[,1], load_mat$loadings[,3], type="n", xlab = "F1", ylab = "F3")
text(load_mat$loadings[,1], load_mat$loadings[,3], colnames(x))
abline(h=0,v=0)
#Factor2 can be interpreted as a "residential factor" - highly correlated with X6, and X13.

#Factor2 & Factor3
plot(load_mat$loadings[,2], load_mat$loadings[,3], type="n", xlab = "Factor2", ylab = "Factor3")
text(load_mat$loadings[,2], load_mat$loadings[,3], colnames(x))
abline(h=0,v=0)
```

### 6. Applying the varimax rotation
By applying the rotation to the above
calculated factors, we obtain the new factors which explains the maximum
variance amongst all the factors.

```{.python .input}
%%R
load_mat_vm = factanal(scaled_data, factors = 3)
load_mat_vm$loadings
```

#### Uniqueness
Lesser the uniqueness of a variable, more the variance explained
by common factors (F).

```{.python .input}
%%R
load_mat_vm$uniquenesses
```

#### Applying varimax rotation to the factors

```{.python .input}
%%R
varimax(load_mat_vm$loadings)
```

## Interpretation

the interpretation remains same irrespective of the rotation
of the factors.Varimax does not significantly change the interpretation of any
factors obtained by our analysis.

Factor1 is positively correlated with
X1,X3,X5,X10 and negatively correlated with variable X8. it can be interpreted
as quality of life since the increment in x1,x3,x5 and x10 relates with the
Quality of life

Factor2 is highly correlated with X6 and X13. it can be
interpreted as residential factors. the room and lower status highly suggest the
impact of residential factors

Factor3 is highly correlated with X8, X9 and
X5.it can be interpreted as employment factor. the variable employment centres
and accesibilty is closely related with the employment factors.










#
Principal component Analysis

### A. Principal Components

### 1. Load and check
the data

```{.python .input}
%%R
x <- read.table("uscomp2.dat")
x[1:5,]
```

There are 8 variables: V1 to V8. There are 6 numerical and 2 categorical
variables, viz. V1 and V8. First variable V1 implies the name of a company and
variable V8 implies the different sectors. We will omit both categorical
variables to obtain the new dataset having only numerical variables.


#### Omit
the categorical variables and convert the dataset to matrix form

```{.python .input}
%%R
my_data <- subset(x, select = -c(V1,V8))
my_data <- data.matrix(my_data)
my_data[1:5,]
```

### 2. Standardize the data
We are standardizing the dataset to bring the whole
data on a same scale.
Standardized data will have the mean equals to "zero" and
standard deviation equals to "one".

```{.python .input}
%%R
scaled_data <- as.data.frame(scale(my_data))
scaled_data[1:5,]
```

### 3. PCA
Principle Componenet Analysis is a method of extracting important
variables in form of components from a large set of variables available in a
data set. It extracts low dimensional set of features from a high dimensional
data set with a motive to capture as much information as possible. The steps to
obtain principle components are described below.

#### 3.1 Obtain the eigen-
values and eigen-vectors of covariance matrix of original dataset

```{.python .input}
%%R
e = eigen(cov(scaled_data))
e$vectors <- -1*e$vectors
e$vectors
```

#### Variance explained by PCs

```{.python .input}
%%R
100*e$values/sum(e$values)
```

#### 3.2 Principle Components
Priciple components (PC) are obtained by directly
multiplying the standardize data with eigenvectors.

```{.python .input}
%%R
y <- as.matrix(scaled_data)%*%e$vectors
y[1:5,]

```

The above six columns represents the Pricipal Components PC1 to PC6. As we know,
PC1 retains the maximum variance and so on.

## B. Plot
First change the labels
of different sectors to make the convenient representation of the datapoints on
scatter-plot.

```{.python .input}
%%R
v8 <- x$V8
v8 <- as.character(v8)
v8[grepl('Manufacturing', v8)] <- 'M'
v8[grepl('Communication', v8)] <- 'H'
v8[grepl('HiTech', v8)] <- 'H'
v8[grepl('Energy', v8)] <- 'E'
v8[grepl('Finance', v8)] <- 'F'
v8[grepl('Retail', v8)] <- 'R'
v8[grepl('Medical', v8)] <- '*'
v8[grepl('Other', v8)] <- '*'
v8[grepl('Transportation', v8)] <- '*'
v8
```

#### Scatter-plot of all observations with respect to PC1 and PC2

```{.python .input}
%%R
plot(y[,1], y[,2], pch = as.character(v8),
     xlab = "PC1", ylab = "PC2")
```

## C Removing the outliers and performing PCA on new dataset

```{.python .input}
%%R
which.max(y[,1])
my_data <- my_data[-c(40),]
y <- y[-c(40),]
```

```{.python .input}
%%R
which.max(y[,1])
my_new_data <- my_data[-c(38),]
my_new_data[36:41,]
```

Here, we have removed two rightmost outliers on above scatter-plot. Now, we will
again follow the above process to get principal components for the new data
(data after removing outliers).
 
### 1. Standardize the data

```{.python .input}
%%R
scaled_new_data <- scale(my_new_data)
scaled_new_data[1:5,]
```

### 2. Obtain the eigen-values and eigen-vectors of covariance matrix of
original dataset

```{.python .input}
%%R
e_new = eigen(cov(scaled_new_data))
e_new$vectors[,2] <- -1*e_new$vectors[,2]
e_new$vectors[1:6,]
e_new$values
```

```{.python .input}
%%R
100*e_new$values/sum(e_new$values)
```

### 3. Principle Components

```{.python .input}
%%R
y_new <- as.matrix(scaled_new_data)%*%e_new$vectors
y_new[1:5,]
```

## D. Plot
First change the labels of different sectors to make the convenient
representation of the datapoints on scatter-plot.

```{.python .input}
%%R
x_new<- x[-c(38,40),]
v8_new <- x_new$V8
v8_new <- as.character(v8)
v8_new[grepl('Manufacturing', v8)] <- 'M'
v8_new[grepl('Communication', v8)] <- 'H'
v8_new[grepl('HiTech', v8)] <- 'H'
v8_new[grepl('Energy', v8)] <- 'E'
v8_new[grepl('Finance', v8)] <- 'F'
v8_new[grepl('Retail', v8)] <- 'R'
v8_new[grepl('Medical', v8)] <- '*'
v8_new[grepl('Other', v8)] <- '*'
v8_new[grepl('Transportation', v8)] <- '*'
```

### Scatter-plot of all observations with respect to PC1 and PC2

```{.python .input}
%%R
plot(y_new[,1], y_new[,2], pch = as.character(v8),
     xlab = "PC1", ylab = "PC2")
```

## How the PCA results in the latter case differ from the results in earlier
case?

```{.python .input}
%%R
par(mfrow=c(1, 2))
plot(y[,1], y[,2], pch = as.character(v8),
     xlab = "PC1", ylab = "PC2")
plot(y_new[,1], y_new[,2], pch = as.character(v8_new),
     xlab = "PC1", ylab = "PC2")
```

```{.python .input}
%%R
e$vectors
e_new$vectors
```

# Interpretation
1) The PC1 is dominant factor in our original PC Analysis,
explaining almost 83% of the variation among all PCs. After removing outliers,
The Variance explained by PC1 drops down to 53% and the total variance explained
by combined PC1 and PC2 accounts to 79%.

2) The old PC1 consists of the
following factors sales, market value, profits, cash flow and employees. All 5
were positively correalted with eachother. Unlike old PC1, The new PC1 does not
depends upon the factor "profit", implying that the presence of outlier impacts
the old PC1. 

Similarly, "Assets" was the dominated factor in the old PC1 while
the new PC2 encorporates the effects of profit, cash flow, assets and sales.
Both old and new PC1 denotes the effects of the "size".Overall, The old PCs were
highly comprised of the factor "size" only, while the new PCs is comprised of
both the factors "size" and "shape"
