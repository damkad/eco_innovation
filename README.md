Data
In this analysis, 3,059,034 patent data were obtained from Bureau Van Dijk's Amadeus from 1952 to 2019. Bureau Van Dijk's Amadeus is a platform that stores accounting, financial, and patent information of about 9 million firms across 34 EU. Using the Cooperate Patent Classification, patents were obtained for applications related to mitigation or adaptation against climate change (Y02 class). 
Data cleaning
From the patent data obtained from Amadeus database, data with incomplete information were removed, reducing the dataset to 1,713,773. In this study, the patent count was used as a proxy for innovation. Also, the country of ownership was used in the analysis in comparison to the inventors' country because while the inventor country reflects where the inventor originates from, the ownerships' country reflects where the innovation firm came from. This gives an idea of the innovation performance of the country. 
Furthermore, Environmental Stringency Policy Index (ESPI) was used as a proxy for environmental regulation across different countries. This data was obtained from OECD (Organisation for Economic Cooperation and Development) statistics database on environmental related matters between 1990 and 2015. The ESPI is a country-specific and globally equivalent indicator based on the degree of stringency of 14 environmental-policy instruments, related to climate and air pollution (OECD, 2016). 
Methodology
To understand the magnitude and direction of the relationship between environmental regulation and innovation on a country level, Pearson's correlation was used. The correlation coefficient, r, tells us about the statistical relationship between environmental regulation and eco-innovation by measuring the covariance. 
Pearson correlation formula; 
r=(∑▒〖(x-n_x)(y-n_y)〗)/√(∑▒〖(x-n_x)〗^2  ∑▒〖(y-n_y)〗^2 )

Where x (environmental regulation) and y (eco-innovation) are two vectors of length n, nx and ny corresponds to the means of x and y, respectively. 
The hypothesis test lets us decide if the value of the sample is close to zero, or significantly different from zero, using a significant level of 5%. If the test concludes that the correlation coefficient is significantly different from zero, we say the correlation is "significant"; otherwise, it is not significant.
Using the R programming language, the correlation was estimated for top ten innovative countries on an annual basis.
