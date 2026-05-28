# 🎬 Marvel vs DC Movies – Model-Based Clustering & Classification

## **Overview**

This project analyzes a dataset of **39 Marvel and DC movies** released between **2000 and 2019**, with the goal of exploring similarities between films and evaluating the effectiveness of **model-based clustering and classification techniques**.

The dataset contains financial, temporal, and rating-related variables collected from Kaggle.

---

## **Dataset**

The dataset includes:

* **39 observations**
* **10 variables**
* Movies produced by **Marvel** and **DC**

Main variables:

* **Rate** → IMDb score
* **Metascore** → Metacritic score
* **Budget**
* **Opening Weekend USA**
* **Gross USA**
* **Gross Worldwide**
* **Minutes**
* **Release year**

---

## **Exploratory Data Analysis**

The dataset was transformed into a tidy format and analyzed for:

* missing values
* outliers
* variable distributions
* relationships between variables

### **Key Findings**

* No missing values were detected.
* Several relevant outliers emerged:

  * **Avengers: Endgame** showed extremely high revenue and budget values.
  * **Catwoman** and **Jonah Hex** showed unusually low ratings and duration.

General trends observed:

* **Marvel movies** tend to have:

  * higher ratings
  * larger budgets
  * larger revenues

* **DC movies** show greater variability across variables.

A **3D scatterplot** was used to analyze the relationship between:

* IMDb rating
* Budget
* Worldwide gross revenue

---

## **Correlation Analysis & PCA**

Correlation analysis revealed strong positive relationships between:

* **Opening Weekend USA** and **Gross USA**
* **Gross USA** and **Gross Worldwide**
* **IMDb Rate** and **Metascore**

Principal Component Analysis (**PCA**) showed that:

* the first **3 principal components** explain more than **80%** of total variance.

This motivated an additional clustering analysis using reduced dimensions.

---

## **Model-Based Clustering**

A Gaussian **model-based clustering** approach was applied using **BIC** and **ICL** for model selection.

### **Selected Model**

* **VEV model**
* **2 clusters**
* **BIC = 7484.402**
* **ICL = 7484.416**

Cluster sizes:

* Cluster 1 → 21 movies
* Cluster 2 → 18 movies

### **Performance**

Using the true labels (**Marvel/DC**):

* **Accuracy ≈ 94.9%**
* **CER = 0.051**
* **ARI = 0.800**

Misclassified movies:

* *Avengers: Endgame*
* *Avengers: Infinity War*

The clustering structure proved highly coherent with the actual movie franchises.

---

## **PCA-Based Clustering Comparison**

The clustering procedure was repeated using only PCA-selected variables.

Although the same VEV structure was selected:

* performance significantly worsened
* **CER increased to 0.385**
* **ARI dropped to 0.028**

This suggests that dimensionality reduction caused relevant information loss.

---

## **Model-Based Classification (EDDA)**

An **EDDA classifier** was trained using the known labels.

### **Best Model**

* **Gaussian_pk_Lk_C (VEE)**
* **2 clusters**
* **BIC = 7126.291**
* **Cross-validation error ≈ 0.103**

The classifier achieved strong predictive performance on the test observations.

Alternative models considered:

* VVE
* EEE (LDA)

---

## **Mixture Discriminant Analysis (MDA)**

A **Mixture Discriminant Analysis** was performed for comparison.

Results confirmed:

* the presence of **2 groups**
* consistency with the clustering solution

However, MDA did not significantly improve performance compared to EDDA.

---

## **Finite Mixtures of Regression Models**

A finite mixture regression model was also tested.

Although the model still identified **2 groups**:

* classification performance deteriorated considerably
* classification error ≈ **0.436**

This indicates that regression mixtures are less suitable for this dataset.

---

## **Conclusion**

All model-based approaches consistently identified two latent groups corresponding to **Marvel** and **DC** movies.

Among the tested methods:

* **model-based clustering** and **EDDA classification** provided the best results
* PCA-based reduction reduced classification quality
* finite mixture regression models showed poor adaptation to the data

This project demonstrates the effectiveness of probabilistic clustering and classification methods in identifying hidden structures within entertainment industry data.

---

## **Methods & Techniques**

* Exploratory Data Analysis (EDA)
* Correlation Analysis
* Principal Component Analysis (PCA)
* Gaussian Mixture Models
* Model-Based Clustering
* EDDA Classification
* Mixture Discriminant Analysis (MDA)
* Finite Mixture Regression Models

---

## **Tech Stack**

* **R**
* **mclust**
* **ggplot2**
* **plotly**
* **tidyverse**

