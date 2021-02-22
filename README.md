# NOIS_index
Naïve Overfitting Index Selection 

Empirical models using hyperspectral data to predict plant traits are very likely to lead to significant overfitting, even when they are selected by robust cross-validation. Complex and less restrictive models as Machine Learning (ML) capture more than underlying relationships between trait and spectra, as such high dimensional data often contain redundant and noisy predictors. 

A method named Naïve Overfitting Index Selection (NOIS) was developed to quantify overfitting while selecting model complexity (tuning). The NOIS method was tested against a cross-validation approach by using five hyperspectral datasets and seven (machine learning) regression techniques [1]. 

The main advantages of the new method are that it: 

a) produces comparable estimates of overfitting between different regression techniques and databases, as model complexity can be hardly standardized; 

b) allows to control model complexity, showing clearly the trade-off between prediction accuracy and overfitting; and 

c) quantifies overfitting based on a single error estimation, using all available observations to select the best model.

The code NOIS_method show an example for the model approaches PLSR and SVM, but it works in any machine learning that do not apply internal feature selection. The data Spectra_DF.csv can be used as a guide to test the method. 

# Referece
'[1] Rocha, A.D., Groen, T.A., Skidmore, A.K., Darvishzadeh, R., Willemen, L., 2017. The Naïve Overfitting Index Selection (NOIS): A new method to optimize model complexity for hyperspectral data. ISPRS J. Photogramm. Remote Sens. 133, 61–74. https://doi.org/10.1016/j.isprsjprs.2017.09.012'
