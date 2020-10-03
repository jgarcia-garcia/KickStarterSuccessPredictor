# Load required libraries
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

#----- LOADING DATA FROM EXTERNAL SOURCE -----#
#Direct download from Kaggle is quite complicated as it needs login and add aditional operations in the script that are not the purpose of this course and project
#So we have downloaded the CSV file containing latest info available and move it into the data folder in the project
#Loading the CSV in the way
ks_projects <- read.csv(file.path("data/ks-projects-201801.csv"), stringsAsFactors=FALSE)


#----- CLEANING THE DATASET AND CREATE PARTITIONS -----#
#We are interested in predicting if a project is going to be succesful or will fail (1 or 0).
#So we need to clean the dataset are remove projects canceled, suspended, that are still live or state is not available/undefined
state_summary <- ks_projects %>% group_by(state) %>% summarize(count=NROW(state))
#state       count
#<chr>       <int>
#  1 canceled    38779
#2 failed     197719
#3 live         2799
#4 successful 133956
#5 suspended    1846
#6 undefined    3562
ks_projects <- ks_projects %>% filter(state %in% c("successful","failed"))

#Now we are going to remove the columns that we are not going to need for sure in our analysis
ks_projects <- ks_projects %>% select(-goal,-pledged,-backers,-usd.pledged)
#Goal, pledged and usd.pledged information is already included and ccy conversions fixed in the columns
#usd_pledged_real and usd_goal_real.
#Additionally, backers is an output, not a column that can be used as input for the algorithm

#Split the dataset into validation, train and test dataset
#We are going to save a 10% for the validation dataset
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
val_index <- createDataPartition(y = ks_projects$state, times = 1, p = 0.1, list = FALSE)
ks_validation <- ks_projects[val_index,]
ks_edx <- ks_projects[-val_index,]

#We are going to split the remaining into train and test dataset (90-10%)
set.seed(13, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(13)` instead
test_index <- createDataPartition(y = ks_edx$state, times = 1, p = 0.1, list = FALSE)
ks_test <- ks_edx[test_index,]
ks_train <- ks_edx[-test_index,]


#----- ANALYZING TRAIN DATASET: FIND IMPORTANTE FEATURES FOR OUR MODEL -----#
#Check the mean of succesful projects and the goal average as a base for comparison
mean_success <- mean(ks_train$state=="successful")
mean_usd_goal <- mean(ks_train$usd_goal_real)
#0.4038771

#Calculate success rate and goal average per main category
maincat_summary <- ks_train %>% group_by(main_category) %>% summarize(count=NROW(state),success_rate=mean(state=="successful"),goal_avg=mean(usd_goal_real))
#15 main categories
#Plot success rate and average goal by main category
p1 <- maincat_summary %>% ggplot(aes(x=reorder(main_category, -success_rate),y=success_rate,fill='red')) + geom_col(position = "dodge",show.legend="FALSE") + geom_hline(yintercept = mean_success, linetype = "longdash") +  coord_flip() + xlab("Main categories") + ylab("Success Rate") + ggtitle(label = "KS projects success rate") + theme(plot.title = element_text(hjust = 0.5)) 
p2 <- maincat_summary %>% ggplot(aes(x=reorder(main_category, goal_avg),y=goal_avg)) + geom_col(position = "dodge",show.legend="FALSE",fill="#56B4E9") + geom_hline(yintercept = mean_usd_goal, linetype = "longdash") +  coord_flip() + xlab("Main categories") + ylab("Goal average") + ggtitle(label = "KS projects goal average") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p2,ncol=2)

#We are going to check if there are differences in success rate inside the same main category
#We are picking a couple of main cats for our analysis. For example: the one with worst success rate (Technology) and one in the average (Film & Video)
filmvideo_cat <- ks_train %>% filter(main_category=="Film & Video") %>% group_by(category) %>% summarize(count=NROW(state),success_rate=mean(state=="successful"),goal_avg=mean(usd_goal_real))
tech_cat <- ks_train %>% filter(main_category=="Technology") %>% group_by(category) %>% summarize(count=NROW(state),success_rate=mean(state=="successful"),goal_avg=mean(usd_goal_real))

p3 <- filmvideo_cat %>% ggplot(aes(x=reorder(category, -success_rate),y=success_rate,fill='red')) + geom_col(position = "dodge",show.legend="FALSE") +  coord_flip() + xlab("Categories") + ylab("Success Rate") + ggtitle(label = "F&V success breakdown") + theme(plot.title = element_text(hjust = 0.5))
p4 <- filmvideo_cat %>% ggplot(aes(x=reorder(category, -success_rate),y=goal_avg)) + geom_col(position = "dodge",show.legend="FALSE",fill="#56B4E9") +  coord_flip() + xlab("Categories") + ylab("Goal average") + scale_y_continuous(breaks = c(0,1000000,2000000),labels = comma) + ggtitle(label = "F&V goal avg breakdown") + theme(plot.title = element_text(hjust = 0.5))
p5 <- tech_cat %>% ggplot(aes(x=reorder(category, -success_rate),y=success_rate,fill='red')) + geom_col(position = "dodge",show.legend="FALSE") +  coord_flip() + xlab("Categories") + ylab("Success Rate") + ggtitle(label = "Technology breakdown") + theme(plot.title = element_text(hjust = 0.5))
p6 <- tech_cat %>% ggplot(aes(x=reorder(category, -success_rate),y=goal_avg)) + geom_col(position = "dodge",show.legend="FALSE",fill="#56B4E9") +  coord_flip() + xlab("Categories") + ylab("Goal average") + scale_y_continuous(breaks = c(0,200000,400000),labels = comma) + ggtitle(label = "Technology breakdown") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p3,p4,p5,p6,ncol=2)

#As there are differences inside categories of same main_category, we are going to take those subcategories into account
#Explore categories
cat_summary <- ks_train %>% group_by(category) %>% summarize(count=NROW(state),success_rate=mean(state=="successful"))
#159 categories if we group only by category
cat_summary <- ks_train %>% group_by(category,main_category) %>% summarize(count=NROW(state),success_rate=mean(state=="successful"))
#170 categories if we group by category and main_category
#That means that there some categories included in different main categories
#So we are going to create a new column with a unique category key: main_category|category
ks_train <- ks_train %>% mutate(unique_cat=str_c(main_category,category,sep="|"))

#Checking that we did it correctly
unique(ks_train$unique_cat)
#170 unique categories, it's fine

unique_cat_summary <- ks_train %>% group_by(unique_cat) %>% summarize(ucat_success_rate=mean(state=="successful")) 

#We are going to focus now on exploring the goal target feature throgh an histogram
#Calculate the max and min and median value, mean already calculated
#mean -> 40519.6814085
median_goal_target <- median(ks_train$usd_goal_real)
#5000
max_goal_target <- max(ks_train$usd_goal_real)
#151395869.92
min_goal_target <- min(ks_train$usd_goal_real)
#0.49

#There is a huge difference between the min and max, while the mean is around 40k and the median 5k
#So probably most of the goal targets amounts are between 0 and 80-100k
aux <- mean(between(ks_train$usd_goal_real,0,100000))
#0.9704157

#We are going to use then 100000 as the goal amount higher limit for our histogram
#If we use 150k million as the higher limit, the visualization is going to be very poor
#Doing an histogram showing the projects that were successful/uncessful depending on goal target
p7 <- ks_train %>% filter(between(usd_goal_real,0,100000)) %>% ggplot(aes(x=usd_goal_real, fill=state, color=state)) + geom_histogram(position="identity", alpha=0.5, bins = 50) + xlab("Goal (USD)") + ylab("Count") + ggtitle(label = "Successful vs Failed Projects per Goal amounts")
p7
#For lower goals (0-6000) the success rate is quite high and start to decrease as long as the goal increases


#Analyze if the time the fundraising time has some influence over the success
#Need to calculate first how much time fundraising was opened
#Add a new column with the days
class(ks_train$deadline)
class(ks_train$launched)
#Both columns are of type character, need to transform them into dates and calculate day difference
ks_train <- ks_train %>% mutate(deadline=as.Date(deadline),launched=as.Date(launched),days_period=difftime(deadline, launched, units = "day"))
days_success_rate <- ks_train %>% group_by(days_period) %>% summarize(success_rate=mean(state=="successful"))
p8 <- days_success_rate %>% ggplot(aes(as.integer(days_period),success_rate))+ geom_point(color='blue') + geom_smooth(method='gam') + xlab("Duration of fund raising (days)") + ylab("Success rate") + ggtitle("Success rate per duration of fund raising (days)") + theme(plot.title = element_text(hjust = 0.5))
#Not concluding: Success rate increases from 0 to 15, then decreases from 15 to 55, increases from 55 to 72 and decreases from there 

#Study the effect of the goal_amount/days ratio. We are rounding to the nearest 100 to facilitate the visualization
#In order to better visualization, we are also filtering by total goal = 100000 (which includes 97% of the population)
goal_days_ratio_success <- ks_train %>% filter(between(usd_goal_real,0,100000)) %>% mutate(goal_days_ratio=round(usd_goal_real/as.integer(days_period),digits=-2)) %>% select(goal_days_ratio,state)
goal_days_ratio_success <- goal_days_ratio_success %>% group_by(goal_days_ratio) %>% summarize(success_rate=mean(state=="successful"),count=NROW(state))

#To improve visualization, we need to filter as well by goal days ratio
#Check % of samples we are taking into account
aux1 <- sum(goal_days_ratio_success$count)
aux2 <- goal_days_ratio_success %>% filter(between(goal_days_ratio,0,4000))
aux3 <- sum(aux2$count)
aux3 / aux1
#Covering 99.88% of rest of the population

p9 <- goal_days_ratio_success %>% filter(between(goal_days_ratio,0,4000)) %>% ggplot(aes(goal_days_ratio,success_rate))+ geom_point(color='blue') + geom_smooth(method='gam') + xlab("Goal per day ratio (Rounded to 100s, USD)") + ylab("Success rate") + ggtitle("Success rate per goal/day ratio") + theme(plot.title = element_text(hjust = 0.5))
#In this case, is quite clear that the goal/day ratio is quite important
#Investigate as well the number of projects for each goal/day ratio
p10 <- goal_days_ratio_success %>% filter(between(goal_days_ratio,0,4000)) %>% ggplot(aes(x=goal_days_ratio,y=count,fill='red')) + geom_col(position = "dodge",show.legend="FALSE") + xlab("Goal per day ratio (Rounded to 100s, USD)") + ylab("Number of projects") + ggtitle(label = "KS projects per goal/day ratio") + theme(plot.title = element_text(hjust = 0.5))
#More than half of the projects (around 160k) have a goal ratio between 0 and 250

#So is going to be an important feature in our model, let's add it
ks_train <- ks_train %>% mutate(goal_day_ratio=usd_goal_real/as.integer(days_period))

#Let's quantify the number of different training examples we have for each goal_day ratio (rounded to 100s)
p10 <- goal_days_ratio_success %>% filter(between(goal_days_ratio,0,4000)) %>% ggplot(aes(x=goal_days_ratio,y=count,fill='red')) + geom_col(position = "dodge",show.legend="FALSE") + xlab("Goal per day ratio (Rounded to 100s, USD)") + ylab("Number of projects") + ggtitle(label = "KS projects per goal/day ratio") + theme(plot.title = element_text(hjust = 0.5))

#Analyze now the country and currency columns: check the average success rate and number of projects per country
#For country
ks_country <- ks_train %>% group_by(country) %>% summarize(success_rate=mean(state=="successful"),count=NROW(state))
#There is a code country "N,0" which seems to be some kind of error in the original data
#There are 165 entries in our train set for that one, we are changing it to "UN" (Unknown)
ks_train$country[ks_train$country == "N,0\""] <- "UN"
ks_country <- ks_train %>% group_by(country) %>% summarize(success_rate=mean(state=="successful"),count=NROW(state))

p11 <- ks_country %>% ggplot(aes(x=reorder(country, -success_rate),y=success_rate,fill=country)) + geom_col(position = "dodge",show.legend="FALSE") + geom_hline(yintercept = mean_success, linetype = "longdash") +  coord_flip() + xlab("Countries") + ylab("Success Rate") + ggtitle(label = "Success rate per country") + theme(plot.title = element_text(hjust = 0.5))

#For currency
ks_currency <- ks_train %>% group_by(currency) %>% summarize(success_rate=mean(state=="successful"),count=NROW(state))
p12 <- ks_currency %>% ggplot(aes(x=reorder(currency, -success_rate),y=success_rate,fill=currency)) + geom_col(position = "dodge",show.legend="FALSE") + geom_hline(yintercept = mean_success, linetype = "longdash") +  coord_flip() + xlab("Currencies") + ylab("Success Rate") + ggtitle(label = "Success rate per currency") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p11,p12,ncol=2)

#Success rate is almost the same for a country and it's currency
#The only remarkable differences are for countries in the EURO area, which share a common currency
#So we can discard currency as a feature if we use the country

#Now we are going to analyze the Title column
#1st we will analyze if the length of the title has some influence over the success rate
#Hypothesis: A project title with some explanation included may have higher success rates
ks_titlewords <- ks_train %>% mutate(title_words=str_count(name," ")+1) %>% group_by(title_words) %>% summarize(success_rate=mean(state=="successful"),count=NROW(state))
p13 <- ks_titlewords %>% ggplot(aes(title_words,success_rate))+ geom_point(color='blue') + xlab("Number of words in title") + geom_smooth(method='loess') + ylab("Success rate") + ggtitle("Success rate vs number of words in title") + scale_x_continuous(limit=c(0,17)) +theme(plot.title = element_text(hjust = 0.5))
#For visualization purposes, only shown until 17 words. That's 99.9999% of population in the train set
#We can observe that success rate increases from 1 word and 0.30 up to 0.45-0.47 from 6 to 14 words, and then starts decreasing
#Add word count into the ks_train dataset
ks_train <- ks_train %>% mutate(title_words=str_count(name," ")+1)

#We were thinking on using as well year/month of launch for the project
#But you will need to calculate the succes rate for that particular month/year
#And you don't have that data available when you launch your project
#As the month/year is still the current one and is not possible to calculate that success rate


#----- 1st ALG. TO STUDY: LOGISTIC LINEAR REGRESSION -----#
#Need to change the expected output to 1 (sucessful) or 0 (failed)
ks_train <- ks_train %>% mutate(success=as.numeric(state=='successful'))
ks_test <- ks_test %>% mutate(success=as.numeric(state=='successful'))

#Add needed columns to test dataset as well
ks_test$country[ks_test$country == "N,0\""] <- "UN"
ks_test <- ks_test %>% mutate(unique_cat=str_c(main_category,category,sep="|"))
ks_test <- ks_test %>% mutate(deadline=as.Date(deadline),launched=as.Date(launched),days_period=difftime(deadline, launched, units = "day"))
ks_test <- ks_test %>% mutate(goal_day_ratio=usd_goal_real/as.integer(days_period))
ks_test <- ks_test %>% mutate(title_words=str_count(name," ")+1)

#Convert days_period into numeric
ks_train <- ks_train %>% mutate(days_period=as.integer(days_period))
ks_test <- ks_test %>% mutate(days_period=as.integer(days_period))

#1st Iteration - Use non-factor features: days_period,goal_day_ratio,title_words
glm_fit1 <- ks_train %>% glm(success~days_period+goal_day_ratio+title_words,data=.,family="binomial")
p_hat1 <- predict(glm_fit1,newdata = ks_test,type='response')
y_hat1 <- ifelse(p_hat1 > 0.5,1,0)
ac1 <- mean(ks_test$success==y_hat1)
#0.6164618 -> Quite poor

#2nd Iteration - Add factors as well: country, unique_cat
glm_fit2 <- ks_train %>% glm(success~days_period+goal_day_ratio+title_words+unique_cat+country,data=.,family="binomial")
p_hat2 <- predict(glm_fit2,newdata = ks_test,type='response')
y_hat2 <- ifelse(p_hat2 > 0.5,1,0)
ac2 <- mean(ks_test$success==y_hat2)
#0.6739138 -> Improved but is still poor

head(glm_fit2$coefficients)
#"New" features created by algorithm, each category (170) is a feature with value 1 or 0
#Same for the country. That is common when using factors.

#3rd Iteration - Represent category and country through their avg success rate
ks_train <- ks_train %>% left_join(unique_cat_summary,by="unique_cat")
ks_test <- ks_test %>% left_join(unique_cat_summary,by="unique_cat")
ks_country <- ks_country %>% mutate(cty_success_rate=success_rate) %>% select(-success_rate,-count)
ks_train <- ks_train %>% left_join(ks_country,by="country")
ks_test <- ks_test %>% left_join(ks_country,by="country")

glm_fit3 <- ks_train %>% glm(success~days_period+goal_day_ratio+title_words+ucat_success_rate+cty_success_rate,data=.,family="binomial")
p_hat3 <- predict(glm_fit3,newdata = ks_test,type='response')
y_hat3 <- ifelse(p_hat3 > 0.5,1,0)
ac3 <- mean(ks_test$success==y_hat3)
#0.6701618 -> Practically same result as previous one but easier to understand
#the coefficients

#There are limitations. We are using a linear models and relationships between
#features and the output are not linear
#If we try to add second order terms into the equation. Example:
glm_fit4 <- ks_train %>% glm(success~days_period+I(days_period^2)+goal_day_ratio+I(goal_day_ratio^2)+title_words+(title_words^2),data=.,family="binomial",maxit = 100)
#glm_fit4 <- ks_train %>% glm(success~days_period+I(days_period^2)+goal_day_ratio+I(goal_day_ratio^2)+title_words+(title_words^2),data=.,family="binomial",maxit = 1000)
#Even increasing the number of maximum iterations, it didn't converge and accuracy is even worst

#Feature scaling - There are convergency problems because there are features with very high values
#Let's scale the features so the values are between 0 and 1 approx
#First, period while fund raising is opened
max_period <- max(ks_train$days_period)
#92
ks_train <- ks_train %>% mutate(period_scaled=as.integer(days_period)/max_period)
ks_test <- ks_test %>% mutate(period_scaled=as.integer(days_period)/max_period)

#Second, words in the title
max_words <- max(ks_train$title_words)
#33
ks_train <- ks_train %>% mutate(words_scaled=title_words/max_words)
ks_test <- ks_test %>% mutate(words_scaled=title_words/max_words)

#Third, work with goal ratio
max_ratio <- max(ks_train$goal_day_ratio)
#11645836, too high. If we use it to scale, a lot of the population is going to be very close to 0
mean(ks_train$goal_day_ratio<=5000)
#0.9825. With 5000 as maximum we are covering 98.3% of the population.
#Let's use it as max in order to avoid that almost all the population is 0
#For the rest of ratios bigger than 5000, we are going to scale it to the new maximum = 1
ks_train <- ks_train %>% mutate(ratio_scaled=goal_day_ratio/5000)
ks_test <- ks_test %>% mutate(ratio_scaled=goal_day_ratio/5000)
ks_train$ratio_scaled[ks_train$ratio_scaled>1] <- 1
ks_test$ratio_scaled[ks_test$ratio_scaled>1] <- 1

# Using 2nd order polynom, including inference between features
glm_fit5 <- ks_train %>% glm(success~poly(period_scaled,ratio_scaled,words_scaled,ucat_success_rate,cty_success_rate,degree = 2),data=.,family="binomial",maxit = 1000)
p_hat5 <- predict(glm_fit5,newdata = ks_test,type='response')
y_hat5 <- ifelse(p_hat5 > 0.5,1,0)
ac5 <- mean(ks_test$success==y_hat5)
#0.6748

#Full 3rd order polynom, including inference between features
glm_fit6 <- ks_train %>% glm(success~poly(period_scaled,ratio_scaled,words_scaled,ucat_success_rate,cty_success_rate,degree = 3),data=.,family="binomial",maxit = 1000)
p_hat6 <- predict(glm_fit6,newdata = ks_test,type='response')
y_hat6 <- ifelse(p_hat6 > 0.5,1,0)
ac6 <- mean(ks_test$success==y_hat6)
#0.6757 - It seems to be this is the limit of using logistic regression
#and higher order polynoms
#Let's try another more suitable algorithms for this problem

#----- 2nd ALG. TO STUDY: NEAREST NEIGHBOURS -----#
#As knn is measuring distances between points, we should use the scaled features
#If not, distance betweens points that have same period, category, country, etc 
#but very diff goal are going to be far far away compare to those ones with same
#goal but very diff period + category + country

#For knn, y (success in our case), must be a factor
ks_train <- ks_train %>% mutate(success_factor=as.factor(success))
ks_test <- ks_test %>% mutate(success_factor=as.factor(success))

#Show that with non-scaled features, the algorithm is worst
knn_fit0 <- knn3(success_factor~days_period+goal_day_ratio+title_words+ucat_success_rate+cty_success_rate,data=ks_train,k=100)
y_hat_knn0 <- predict(knn_fit0,newdata = ks_test,type='class')
acknn0 <-mean(ks_test$success_factor==y_hat_knn0)
#0.6464

knn_fit1 <- knn3(success_factor~period_scaled+ratio_scaled+words_scaled+ucat_success_rate+cty_success_rate,data=ks_train,k=100)
y_hat_knn1 <- predict(knn_fit1,newdata = ks_test,type='class')
acknn1 <- mean(ks_test$success_factor==y_hat_knn1)
#0.6768 - Scaling is important, specially for those predictions with a very high value in the goal ratio

#Find the optimum k: use caret package for tunning parameters
#We are using k-fold crossvalidation, for each k 5 executions with different 80% of the dataset
#being use as train data and 20% as test data -> This will provide the best k
modelLookup("knn")
control <- trainControl(method = "cv", number = 5, p = .8)
knn_cv <- train(success_factor~period_scaled+ratio_scaled+words_scaled+ucat_success_rate+cty_success_rate, method = "knn", 
                      data = ks_train,
                      tuneGrid = data.frame(k = seq(25, 150, 25)),
                      trControl = control)
ggplot(knn_cv, highlight = TRUE)
#Optimized k found: 125
k = knn_cv$bestTune$k

#Calculate now the accuracy with the model using our test set
y_hat_knn2 <- predict(knn_cv,newdata = ks_test,type='raw')
mean(ks_test$success_factor==y_hat_knn2)
#0.6769 - Just a little bit better with k=125 than k=100

#So, with knn we get a best result of 0.6769 and seems to be the maximum we can get
#Try another algorithms
knn_plot1 <- ks_train %>% ggplot() + aes(x=ucat_success_rate,y=ratio_scaled,col=success_factor) + geom_point(size=1) + xlab("Category Success Rate") + ylab("Goal ratio scaled") + scale_color_discrete(name="Success")
knn_plot2 <- ks_train %>% ggplot() + aes(x=cty_success_rate,y=period_scaled,col=success_factor) + geom_point(size=1) + xlab("Country Success Rate") + ylab("Duration scaled") + scale_color_discrete(name="Success")
knn_plot3 <- ks_train %>% ggplot() + aes(x=ratio_scaled,y=words_scaled,col=success_factor) + geom_point(size=1) + xlab("Goal ratio scaled") + ylab("Words in title scaled") + scale_color_discrete(name="Success")
knn_plot4 <- ks_train %>% ggplot() + aes(x=ucat_success_rate,y=cty_success_rate,col=success_factor) + geom_point(size=1) + xlab("Category Success Rate") + ylab("Country Success Rate") + scale_color_discrete(name="Success")
grid.arrange(knn_plot1,knn_plot2,knn_plot3,knn_plot4,ncol=2)

#----- 3rd ALG. TO STUDY: CLASSIFICATION TREES -----#
#We are going to define a classification tree with default parameters
fit_tree1 <- rpart(success_factor~period_scaled+ratio_scaled+words_scaled+ucat_success_rate+cty_success_rate, data = ks_train)
rpart.plot(fit_tree1)
y_hat_tree1 <- predict(fit_tree1,newdata = ks_test, type = "class")
tree_acc1 <- mean(ks_test$success_factor==y_hat_tree1)
#0.6595759 - That simple classification tree is easier to understand

#Use cross validation to find a better model
modelLookup("rpart")
#Parameter to be optimized: cp = Complexity Parameter
train_rpart <- train(success_factor~period_scaled+ratio_scaled+words_scaled+ucat_success_rate+cty_success_rate,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.001, 0.02, len = 20)),
                     data = ks_train)
plot(train_rpart)
#Best with cp=0.001
y_hat_rpart <- predict(train_rpart,newdata = ks_test, type = "raw")
tree_acc <- mean(ks_test$success_factor==y_hat_rpart)
#Better result: 0.677866

#But error can be improved using random Forests

#Now, as a final step, we are going to use the cross validation options
#for ramdon forest algorithm, with the aid of the caret package
modelLookup("Rborist")
#model parameter                         label forReg forClass probModel
#1 Rborist predFixed #Randomly Selected Predictors   TRUE     TRUE      TRUE
#2 Rborist   minNode             Minimal Node Size   TRUE     TRUE      TRUE

#IT TAKES A LOT OF TIME TO EXECUTE, SO CODE HAS BEEN COMMENTED AFTER EXECUTION
#Using the whole population of the train set
#fit_rf<- train(success_factor~period_scaled+ratio_scaled+words_scaled+ucat_success_rate+cty_success_rate,
                  #method = "Rborist",
                  #tuneGrid = data.frame(predFixed = c(4,5), minNode = c(30,80)),
                  #data = ks_train)

#y_hat_rf = predict(fit_rf,newdata=ks_test)
#mean(ks_test$success_factor==y_hat_rf)
#0.6640649

#WE ARE NOW TO COMPARE RESULTS BETWEEN DIFFERENT METHODS
results_summary <- tibble(Algorithm = "Logictic regression", Accuracy = ac1)
results_summary <- bind_rows(results_summary,tibble(Algorithm="K-Nearest Neighbours",Accuracy = ac6))
results_summary <- bind_rows(results_summary,tibble(Algorithm="Classification Tree",Accuracy = tree_acc))
results_summary %>% knitr::kable(caption="Prediction accuracy for different methods")
#Best one seems to be classification tree

#CALCULATE FINAL PERFORMANCE OF THE MODEL WITH VALIDATION DATASET
#Do necessary validation dataset transformations
ks_validation <- ks_validation %>% mutate(unique_cat=str_c(main_category,category,sep="|"))
ks_validation <- ks_validation %>% mutate(deadline=as.Date(deadline),launched=as.Date(launched),days_period=difftime(deadline, launched, units = "day"))
ks_validation <- ks_validation %>% mutate(goal_day_ratio=usd_goal_real/as.integer(days_period))
ks_validation$country[ks_validation$country == "N,0\""] <- "UN"
ks_validation <- ks_validation %>% mutate(title_words=str_count(name," ")+1)
ks_validation <- ks_validation %>% mutate(days_period=as.integer(days_period))
ks_validation <- ks_validation %>% left_join(unique_cat_summary,by="unique_cat")
ks_validation <- ks_validation %>% left_join(ks_country,by="country")
ks_validation <- ks_validation %>% mutate(period_scaled=as.integer(days_period)/max_period)
ks_validation <- ks_validation %>% mutate(words_scaled=title_words/max_words)
ks_validation <- ks_validation %>% mutate(ratio_scaled=goal_day_ratio/5000)
ks_validation$ratio_scaled[ks_validation $ratio_scaled>1] <- 1
ks_validation  <- ks_validation %>% mutate(success=as.numeric(state=='successful'))
ks_validation <- ks_validation %>% mutate(success_factor=as.factor(success))

validation_rpart <- predict(train_rpart,newdata = ks_validation, type = "raw")
final_acc <- mean(ks_validation$success_factor==validation_rpart)
#Final Accuracy = 0.6740

#Compare results with just guessing
set.seed(5, sample.kind="Rounding")
y_random <- sample(c(0,1),length(ks_validation$success_factor),replace=TRUE,prob=c(1-mean_success,mean_success))

#Confusion Matrix, Sensitivity, Specificity and F1 score for our model
f1_clasTree <- F_meas(data=validation_rpart,reference=ks_validation$success_factor)
#0.747158
spec_clasTree <- specificity(data=validation_rpart,reference=ks_validation$success_factor)
#0.476709
sens_clasTree <- sensitivity(data=validation_rpart,reference=ks_validation$success_factor)
#0.807809
confMat_clasTree <- confusionMatrix(data=validation_rpart,reference=ks_validation$success_factor)

#Confusion Matrix, Sensitivity, Specificity and F1 score for random guessing
accur_guess <- mean(ks_validation$success_factor==as.factor(y_random))
f1_guess <- F_meas(data=as.factor(y_random),reference=ks_validation$success_factor)
#0.60001
spec_guess <- specificity(data=as.factor(y_random),reference=ks_validation$success_factor)
#0.405195
sens_guess <- sensitivity(data=as.factor(y_random),reference=ks_validation$success_factor)
#0.6013
confMat_guess <- confusionMatrix(data=as.factor(y_random),reference=ks_validation$success_factor)

#Compare results in a table
metrics_summary <- tibble(Metric = "Accuracy", 'Guessing with 40-60 prob' = accur_guess,  'Trained Classification Tree' = final_acc)
metrics_summary <- bind_rows(metrics_summary,tibble(Metric = "Sensitivity", 'Guessing with 40-60 prob' = sens_guess,  'Trained Classification Tree' = sens_clasTree))
metrics_summary <- bind_rows(metrics_summary,tibble(Metric = "Specificity", 'Guessing with 40-60 prob' = spec_guess,  'Trained Classification Tree' = spec_clasTree))
metrics_summary <- bind_rows(metrics_summary,tibble(Metric = "F1 Score", 'Guessing with 40-60 prob' = f1_guess,  'Trained Classification Tree' = f1_clasTree))
metrics_summary %>% knitr::kable(caption="Comparative metrics between just guessing and our trained algorithm ")

