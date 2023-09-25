# salary-prediction-job-description
Salary prediction using job description

Abstract:

The existing literature has shown that skill requirement can be part of the explanation of wage heterogeneity within occupation. It means that skill requirement can bring additional information compared to occupation characteristics and then better predict the salary. I use the recent French Jocas database constructed by the French Ministry of Labor from millions of online job postings. I developed a deep machine learning model with the pre-trained RoBERTa language model as the first layer. The input of the model is the job description of job vacancies. The language model outperforms two baseline fixed effect models. It increases the coefficient of variation by 7% and decrease the root mean square error by 93%. Overall, the language model is able to explain 59% of the variation in the salary. These results are promising considering that the model has only been trained on 80,000 observations for technical reasons. Increasing the training sample should improve the performance of the model.

R scripts:
- Descriptive statistics Jocas: script to read the Jocas database and have some descriptive statistics on the data.
- Descriptive statistics Pole Emploi: script to load the data from Pole Emploi and have some descriptive statistics on the data.

Jupyter notebooks:
- Salary prediction using job description: deep machine learning model with the job description as the input.
- Salary prediction - fixed effect analysis: occupation and location fixed effects to predict salary with the Jocas database.
- Salary prediction - fixed effect analysis (2): occupation and location fixed effect to predict salary with the Pole Emploi database.

