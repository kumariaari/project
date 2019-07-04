Multicollinearity using HeatMap


import seaborn as sns
import matplotlib.pyplot as plt%matplotlib inline
corr = df.corr()
sns.heatmap(corr, 
        xticklabels=corr.columns,
        yticklabels=corr.columns)
