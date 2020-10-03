# KickStarterSuccessPredictor

In this project, we have studied the probability of success for any Kickstarter campaign based on historical data provided since its inception in 2008 until January 2018.

We have demonstrated how different features like the money/goal we want to raise or the duration of the crowdfunding campaign can increase or decrease the probabilities of success for our project.

We have been able to provide a model that may help us to predict if our Kickstarter campaign will be successful or not with an accuracy of 67.4% (2 of each 3 will be predicted correctly).

It's important to remark that in this analysis, our purpose was always to predict the probability of success **before** launching our campaing. 
That way, only features that we can choose as authors and we have historical success rate information availabe for them have been included in our model: goal to raise, title and duration of the campaign and country of origin.

The project consists on following files/folders:

* KickStarterSuccessScript.R - Script in R with all the commands executed to do the analysis and test different machine learning algorithms

* KickStarterCampaignsSuccessPredictor.Rmd - Rmd file to generate report

* KickStarterCampaignsSuccessPredictor.pdf - Pdf report

* data - Folder that contains the CSV with historical data for KS projects

* figs - Folder containing different figures generated in our analysis
