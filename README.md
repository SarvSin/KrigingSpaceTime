## Introduction

_Note: This is a reproduction using historical data of a 1st-place hackathon project in collaboration with the MET Office, UK (2022)_

An ability to predict volatile, and potentially disastrous natural events using past and current data is crucial. This report aims to compare and contrast spatial, and temporal influences on prediction of maximum temperature during the 2020 August heatwave in Southern England. This heatwave was one of the most significant heatwaves to affect southern England in the last sixty years. Powerful thunderstorms, heathland fires occurred, and registered deaths rose during the week that this heatwave lasted from August 7, to August 12.
<p align="center">
<img width="253" alt="image" src="https://github.com/SarvSin/KrigingSpaceTime/assets/117599272/e5f05e34-62ee-4eaa-8e12-f8162c9442ac">
</p>
<h6 align="center">Figure 1: MET site locations grouped by Elevation</h6>
                      
A hot and humid air mass built as air moved north from the near continent to enter a region of slack air flow over south-east England. Parts of central England faced also faced temperatures exceeding 30°C, with 35°C reached during the week of heatwave, and 36°C on the 7th and 11th of August. Maximum temperature, elevation, and location data is given for the UK Met Office’s monitoring sites in affected places of London, Marham, and High Wycombe. The maximum daily temperature recorded at these sites, and across the affected South-East and Central England will be predicted using past, and ‘current’ spatial and temporal temperature data. 

Doing so will allow for a critical analysis of any discrepancy between actual maximum temperatures and predicted maximum temperatures obtained using historical temporal data. In addition, assuming statistically significant spatial correlation between temperatures across regions, techniques of spatial statistics will be used to predict maximum temperatures observed in the focus region, the South-East of England, but also interpolating temperatures for non-measurement sites across England during the heatwave. 

## Data Pre-processing

Firstly, the meta-data file containing location and elevation information for each of the 20 Met Office monitoring sites is explored. The range of elevation across all sites is 842m, with the highest location being Dun Fell at 847m, and the lowest at London.  The extreme height of the Dun Fell site is an anomaly considering elevation for the remaining sites is smaller than or approximately equal to 200m. 

All measuring sites are mapped according to location data given in MET Office file in Figure 1. The sites are further grouped into two – stations at elevation greater than or lesser than 160m. From the plot, it is evident that sites on higher altitude appear to be located on a line running approximately centrally through England. Next, the temperature file is read in; temperature observations are available at each location for 366 days, in °C. Correlations between temperature data at each location are found to be positive and highly significant in all cases, confirming spatial correlations in temperature and their importance in further analysis. The pattern of temperatures rising through the year, peaking in July/early August is also observed in almost all sites. 

However, interest in temperature measurements is not limited to the monitoring sites. Forecasting temperature in all villages, towns, and cities requires interpolating these measurements using spatial statistical methods. This will be achieved by leveraging Gaussian Processes and Maximum Likelihood Estimation methods. 

To demonstrate, a spatial model is fit to predict the (maximum) daily temperature at the Leconfield, Yeovilton, and High Wycombe monitoring sites on August 15, 2020. Essentially, the temperature predictions are made conditional on actual measurements in locations exclusive of the sites in focus. The methodology will be described in detail in the following section.

Given records at 17 sites, temperature can be interpolated not just for the remaining 3 monitoring locations mentioned above, but also across the region at a desired level of detail. In Figure 3, temperature is interpolated for the whole of UK on August 15, 2020 over a coordinate grid of 0.1°. In particular, the predicted temperatures for High Wycombe, Leconfield, and Yeovilton are 19.8°C, 17.5°C, and 18.9°C respectively. 
<p align="center">
<img align="center" width="395" alt="image" src="https://github.com/SarvSin/KrigingSpaceTime/assets/117599272/7af52e1b-2188-4810-9dd3-a75a4f67eba1">
</p>
<h6 align="center">Figure 2: Distribution of temperature on Aug. 15, 2020 (means)</h6>

 Additionally, interest in temperature measurements is not limited to forecasts on one day. Predicting the temperature in both the short- and long-term future requires taking into account the temporal correlations of temperature. This can be achieved through time-series modelling techniques, the use of which will be detailed in the following section. 

However, to demonstrate an application, (maximum) temperature will be predicted for one of the monitoring locations, Lerwick during the first week of October, 2020. Dependencies on similar macro-environmental conditions naturally lead to correlations between temperature across time in the micro-scale. Auto-regressive Integrated Moving-average (ARIMA) models can be used to quantify and consider these dependencies. 

<p align="center">
<img align="center" width="370" alt="image" src="https://github.com/SarvSin/KrigingSpaceTime/assets/117599272/860a1f04-f21f-4b4b-a84f-7081f4d388e2">
</p>
<h6 align="center">Figure 3: Distribution of temperature on Aug.15, 2020 (variances)</h6>
            
In Figure 4, the temperature at Lerwick up and not including October 1 is shown. The trend of increasing temperature and non-stationarity is apparent. The management of this non-stationarity is explained in the following section, but the predictions of temperature for the first week of October are given in Table 1.

<p align="center">
<img align="center" width="320" alt="image" src="https://github.com/SarvSin/KrigingSpaceTime/assets/117599272/878895ac-1f68-4ec6-b965-4f788eca52c5">
</p>
<h6 align="center"> Figure 4: Daily temperature at Lerwick up to Oct.1, 2020</h6>
                                                                              
## Methods

The prediction of max. daily temperature at Yeovilton, Leconfield, and High Wycombe, was performed using Gaussian processes and Maximum Likelihood Estimation methods. Firstly, data was wrangled to remove observations for stations where temperature was to be predicted, this wrangled data was then merged with meta data including location and elevation information, and transformed into a GeoData format. 

On plotting this GeoData object, significant spatial correlation in temperature was observed. Higher temperatures clustered around the south-west, and lower temperatures were plotted mostly in the north and north-west parts of the UK. Additionally, temperature data was found to be distributed approximately normally. Hence, fitting a Gaussian process to this data was regarded as not posing statistical issues. Checking for anisotropy using a conditional variogram revealed a probable differential variance in the north, or 0° direction. Therefore, in model building, parameters for anisotropy ratio and angle were allowed to be estimated. 

<p align="center">
<img align="center" width="156" alt="image" src="https://github.com/SarvSin/KrigingSpaceTime/assets/117599272/a9a839ff-4f24-4259-831c-7f6b9b25a74c">
</p>
<h6 align="center"> Table 1: Prediction of temperature at Lerwick (Oct.1-Oct.7, 2020)</h6>
                                                           
Further, an initial analysis of temperature data and its relationship with x-coordinates, y-coordinates, and elevation was conducted. Doing so demonstrated that the relationship between temperature and the y-coordinate or longitude was more linear and dispersed, than the relationship between temperature and the x-coordinate or latitude. Finally, a strong negative correlation was found between temperature and elevation. The models were compared on the basis of AIC, likelihood values, and residual and predicted data distributions against actual data. The best fit in this case was found to be squared exponential with a 1st order trend including Elevation. The residuals were distributed randomly not just against predicted data, but also against predictor values of x and y coordinates. The QQ plot showed that observations were most concentrated along the diagonal with this model. 

To predict temperatures for 3 specific monitoring sites, a data frame consisting of their coordinates was created. This was input into the kriging function that would interpolate temperatures at unseen locations additionally given data at 17 locations, and the model built previously to fit this data. The predicted temperatures, as given previously, were 19.8°C, 17.5°C, and 18.9°C for High Wycombe, Leconfield, and Yeovilton respectively.

Next, a rectangular grid was created to fit the coordinate dimensions of the UK on a 0.1° granularity. At all points on this grid, temperatures were interpolated using the given data at 17 locations, and the model built previously to fit this data. 
Once the predictions were calculated, their means and variances were plotted on a grid. See Figure 5. Additionally, to aid contextual geographical understanding, the grid was fitted to a bonne projection with the map of the UK superimposed on the distribution of temperatures. 

Although a similar procedure was followed to choose the best spatial model for exploring the heatwave on August 7-12 in south-east England, some differences in interpolating temperatures are worth noting. Examining the dispersion of heatwave in central parts of England, and analysis of spatial correlation between temperatures in the region was made possible by removing monitoring locations affected by the heatwave.  Additionally, the spatial correlation model fit on day one of the heatwave was used to interpolate temperatures in the following six days. This choice was made to retain consistency in systematic relationship between the spatial correlation of temperatures in the region. Hence for each day of the heatwave, new data on actual temperature data was given more weightage relative to a stable model of correlation between points on the grid and sites. The plots for means are given in Figure 6(a)-(f). 

<p align="center">
<img align="center" width="614" alt="Screenshot 2023-09-10 at 17 30 27" src="https://github.com/SarvSin/KrigingSpaceTime/assets/117599272/40fb32c0-52f3-4a89-ae02-9c6e266d7f12">
</p>
<h6 align="center">Figure 5: Top row, (a) Aug 7, (b) Aug 8; Middle row (c) Aug 9, (d) Aug 10, Bottom row (e) Aug 11, (f) Aug 12</h6>
                               
To predict temperature using the ARMA/ARIMA models as mentioned in the previous section, it is crucial to confirm that assumption of stationarity is met. This requires an absence of mean trend, and constant in the distribution of data over time. Differencing the temperature data once gave a time-series plot that already looked stationary, but the data was copied and differenced again to confirm this through diagnosis methods. 

Next, the ACF and PACF were plotted to help determine the suitable number of lags for the moving average, and auto-regressive parts of the model. A slow tendency towards zero of the PACF, and a sharp drop to zero of the ACF suggested influence of the moving average component in the model despite the noisiness in both plots. Multiple models were fit manually on both the 1st and 2nd order differenced data. 

Comparison of models was made using AIC values, p-values for the Ljung-Box statistic, and distribution of standardized residuals against time. In addition, selected manually-fit models for 1st and 2nd order data were then compared against output from an auto-arima function. Finally, an ARIMA(2,1,1) function was chosen for prediction purposes. 

The predicted values are given in Table 1. It is clear that the predictions are distributed closely with actual observations, so the fit appears to be good. To assess this model’s fit at other monitoring locations, a mean squared error metric was calculated at each location. The procedure is similar - Given the ARIMA(2,1,1) model, temperature predictions were made at other 19 locations for the first week of October, 2020. These predictions were then compared with actual data in computing a Mean Squared Error (MSE) at each location. Monitoring locations are mapped in Figure 8, colour-coded according to magnitude of their MSE with respect to model fit at Lerwick. 

<p align="center">
<img align="center" width="259" alt="image" src="https://github.com/SarvSin/KrigingSpaceTime/assets/117599272/2843d377-175d-4141-a02e-e5ae479ba2b2">
</p>
<h6 align="center">Figure 6: MSE across monitoring sites (Lerwick model)</h6>
                                                                                
Finally, in order to predict temperatures for London, Marham, and High Wycombe during the heatwave from 7th August, 2020 to 12th August, 2020, the previously described steps for building an appropriate model were repeated. An ARIMA(1,1,2) model was finalised for London on the basis of temperature up to but not including 7th August. The same model was then used to predict temperatures at Marham and High Wycombe for the common, targeted period. The predictions are plotted against actual observed temperatures at London, Marham, and High Wycombe in Figure 9(a)-(c). 

## Results

From Figure 9(a)-(c), it is clear that not only were temperatures forecast to be lower than they ended up being, but the trend of temperatures falling on the 8th of August across all sites – London, Marham, and High Wycombe – and rising to peak around the 12th was not captured either. Given temperature data for these monitoring sites right up to the day before official beginning of heatwave, this shows that it would still not have been possible to forecast accurately either the magnitude or trend of temperatures for the week ahead. 
This is sufficient evidence in support of forecasts being informed with data from multiple sources. Environmental changes, spatial correlations, anthropological shifts – all this and more needs to be taken in consideration when producing forecasts for temperature. Collecting and analysing this data in real-time is a mammoth, but crucial task. 

<p align="center">
<img align="center" width="607" alt="Screenshot 2023-09-10 at 17 33 10" src="https://github.com/SarvSin/KrigingSpaceTime/assets/117599272/8951d2b2-5854-4fc2-86ff-b44fd5fbc85c">
</p>
<h6 align="center">Figure 7: Heatwave temperature predictions using spatial Gaussian processes (clockwise: London, High Wycombe, Marham)</h6>              
                                      
Although past temperature data is an important factor in predicting trends and temperature levels on a macro-scale, aberrant climate and geophysical events such as the heatwave, and storms, earthquakes, and make accurate forecasts impossible to obtain using just past and even updated temporal data. Considering the importance of other factors including space in predicting temperature, the results of spatial interpolation of temperatures in the affected regions of the South-east and central England, in addition to the monitoring sites at London, Marham, and High Wycombe, will now be conducted. 

Accounting for the distribution of temperature levels in nearby and faraway regions, predictions of temperatures in the regions and sites mentioned above were still lower than reality. It is evident, however, that due to inclusion of updated/daily temperatures at sites exclusive of the three monitoring sites mentioned above, the pattern of falling and rising temperatures has been captured well (Figure 10 (a)-(c)). 

Considering the density of economic and human activity in London, it is expected that such anthropogenic factors have contributed to and might have exacerbated the heatwave. Unsurprisingly, the difference between actual and predicted temperatures during the heatwave was greatest in London. Indeed, as seen in Figure 10, actual temperatures fell within the 95% confidence interval for predictions in Marham and High Wycombe on few days during the week of heatwave.

From the spatial interpolation of temperatures in England during the heatwave, it is evident that high temperature was concentrated in the eastern part of England on the 7th, and travelled to the south- and central-eastern parts of England before returning closer to the eastern region on the 11th-12th. This corresponds to the pattern of falling temperatures in the Eastern monitoring sites of London, High Wycombe, and Marham between August 8 and 10. Highest temperature on the 7th can be associated with the hot column being concentrated on the Eastern front as is clear in Figure 6. As the hot column of air appears to move back towards the East, temperatures in all sites rise again though not as high as in the beginning of the heatwave on 7th August. This is because the hot air has settled around the South-East, thus reducing the temperature slightly as the bloc of hot air is more dispersed. 

<p align="center">
<img align="center" width="852" alt="Screenshot 2023-09-10 at 17 34 24" src="https://github.com/SarvSin/KrigingSpaceTime/assets/117599272/29171a46-251a-4073-aac1-f158de9123b1">
</p>

## Summary

Due to its chaotic and non-linear character, temperature is a difficult physical phenomenon to forecast. Yet considering the significant damage due to extreme changes in temperature to flora and fauna, and significant impact on multiple economic sectors such as agriculture, energy, and manufacturing, the accuracy of air temperature forecasts is even more important. In particular, predicting the severity, time-span, and extent of spatial dispersion of such phenomena as heatwaves is highly dependent on accurate temperature prediction.

The analysis has shown how considering influencing factors on temperature – regular, annual patterns captured by historical data through time-series models, or climatic variables captured through spatial correlation – in isolation, results in sub-par predictions. Climate models that combine atmospheric, land surface, and anthropological processes in forecasting temperature can be expected to perform much better than models that are based only on leveraging temporal correlations, or only spatial correlations. Different input variables have different strengths in explaining the variation in temperature.
