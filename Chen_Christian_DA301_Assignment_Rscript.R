################################################################################

## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

################################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

################################################################################

# 1. Load and explore the data

#-------------------------------------------------------------------------------

# Install and import Tidyverse and Conflicted (optional).
# install.packages('tidyverse')  # Only if necessary.
# install.packages('conflicted')  # Only if necessary.
library('tidyverse')
library(conflicted)
# Handle conflicts.
conflict_prefer('filter', 'dplyr')
conflict_prefer('lag', 'dplyr')

# Install and import Plotly for plots.
# install.packages('plotly')  # Only if necessary.
library(plotly)
conflict_prefer('layout', 'plotly')

# Set source file location as the current working directory 
# (ONLY works for RStudio).
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Import the data set.
sales <- read.csv("turtle_sales.csv", header=TRUE)
# sales <- read.csv(file.choose(), header=T)  # Optional alternative.

# Number of missing values.
sum(is.na(sales))

# View rows with missing values.
sales[!complete.cases(sales),]
# Only the Year column has missing values, so we can keep these rows,
# as we will be removing this column prior to analysis.

# Check for duplicates of Product-Platform pair
sum(duplicated(subset(sales, select=c(Product, Platform))))

# Print the data frame.
as_tibble(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_clean <- subset(sales, select=-c(Ranking, Year, Genre, Publisher))

# View the data frame.
as_tibble(sales_clean)

# View the descriptive statistics.
summary(sales_clean)

# Create new column for sales made outside of North America and Europe.
# Name this new column "Other_Sales" and calculate by subtracting the
# aforementioned sales from Global Sales. 
sales_clean <- sales_clean %>% mutate(Other_Sales=Global_Sales
                                        -(NA_Sales+EU_Sales),
                                      .after= 'EU_Sales')

# View the data frame.
as_tibble(sales_clean)

# View the descriptive statistics.
summary(sales_clean)

################################################################################

# 2. Review plots to determine insights into the data set.

#-------------------------------------------------------------------------------

## 2a) Scatterplots
# Create scatterplots.

# Scatterplot comparing regional sales.
ggplot(sales_clean, aes(x=Product))+
  geom_point(aes(y=NA_Sales, color="North America", group=1))+
  geom_point(aes(y=EU_Sales, color="Europe", group=1))+
  geom_point(aes(y=Other_Sales, color="Other", group=1))+
  labs(x="Product ID", y="Sales (M£)", color='Region')

# Jitter-scatterplot comparing platforms and global sales.
qplot(Platform, Global_Sales, data=sales_clean, geom='jitter',
      color=I('purple'))+
  labs(x="Platform", y="Global Sales (M£)")

# Jitter-scatterplot comparing platforms and regional sales.
ggplot(sales_clean, aes(x=Platform))+
  geom_jitter(aes(y=NA_Sales, color="North America", group=1))+
  geom_jitter(aes(y=EU_Sales, color="Europe", group=1))+
  geom_jitter(aes(y=Other_Sales, color="Other", group=1))+
  labs(x="Platform", y="Sales (M£)", color='Region')

#-------------------------------------------------------------------------------

## 2b) Histograms
# Create histograms, density plots, and bar plots.

# Histogram of global sales.
qplot(Global_Sales, data=sales_clean, geom='histogram',
      binwidth=1, fill=I('purple'))+
  labs(x="Global Sales (M£)", y="Count")

# Histogram of North-American sales.
qplot(NA_Sales, data=sales_clean, geom='histogram',
      binwidth=1, fill=I('green'))+
  labs(x="North-American Sales (M£)", y="Count")

# Histogram of European sales.
qplot(EU_Sales, data=sales_clean, geom='histogram',
      binwidth=1, fill=I('red'))+
  labs(x="European Sales (M£)", y="Count")

# Histogram of Other sales.
qplot(Other_Sales, data=sales_clean, geom='histogram',
      binwidth=1, fill=I('blue'))+
  labs(x="Other Sales (M£)", y="Count")

# Density plots comparing regional sales.
ggplot(sales_clean)+
  geom_density(aes(x=NA_Sales, fill="North America", group=1), 
               color='green', alpha=.3)+
  geom_density(aes(x=EU_Sales, fill="Europe", group=1),
               color='red', alpha=.3)+
  geom_density(aes(x=Other_Sales, fill="Other", group=1),
               color='blue', alpha=.3)+
  labs(x="Sales (M£)", y="Count", fill='Region')

# Violin density plots comparing regional sales.
ggplot(sales_clean)+
  geom_violin(aes(x="North America", y=NA_Sales, fill="North America", group=1), 
              color='green', alpha=.3)+
  geom_violin(aes(x="Europe", y=EU_Sales, fill="Europe", group=1),
              color='red', alpha=.3)+
  geom_violin(aes(x="Other", y=Other_Sales, fill="Other", group=1),
              color='blue', alpha=.3)+
  labs(x="Sales (M£)", y="Count", fill='Region')+
  theme(legend.position="none")

# Barplot of global sales against platform.
qplot(Platform, Global_Sales, data=sales_clean, geom='col',
      fill=I('purple'))+
  labs(x="Platform", y="Global Sales (M£)")

# Stacked barplot of regional sales against platform.
plot_ly(data=sales_clean, x=~Platform, y=~Other_Sales, type='bar', 
        name='Other') %>%
  add_trace(y=~NA_Sales, name='North America') %>%
  add_trace(y=~EU_Sales, name='Europe') %>%
  layout(barmode = 'stack',
         yaxis=list(title='Sales (M£)'), 
         legend=list(title=list(text='Region')))

#-------------------------------------------------------------------------------

## 2c) Boxplots
# Create boxplots.

# Boxplot of global sales against platform.
qplot(Platform, Global_Sales, data=sales_clean, geom='boxplot',
      color=I('purple'))+
  labs(x="Platform", y="Global Sales (M£)")

# Boxplot of North-American sales against platform.
qplot(Platform, NA_Sales, data=sales_clean, geom='boxplot',
      color=I('green'))+
  labs(x="Platform", y="North-American Sales (M£)")

# Boxplot of European sales against platform.
qplot(Platform, EU_Sales, data=sales_clean, geom='boxplot',
      color=I('red'))+
  labs(x="Platform", y="European Sales (M£)")

# Boxplot of Other sales against platform.
qplot(Platform, Other_Sales, data=sales_clean, geom='boxplot',
      color=I('blue'))+
  labs(x="Platform", y="Other Sales (M£)")

# Boxplot comparing regional sales.
ggplot(sales_clean)+
  geom_boxplot(aes(x="North America", y=NA_Sales, color="North America", group=1))+
  geom_boxplot(aes(x="Europe", y=EU_Sales, color="Europe", group=1))+
  geom_boxplot(aes(x="Other", y=Other_Sales, color="Other", group=1))+
  labs(x="Region", y="Sales (M£)")+
  theme(legend.position="none")

################################################################################

# 3. Observations and insights

# - The most profitable item based on North America, Europe, Other, 
#   and Global sales belongs to the Wii platform.
# - All regional histograms (and global) of sales are skewed to the left, 
#   with majority of regional sales sales <5M£ (and global <10M£)
#   i.e. the most-common sales are lower, with high achievers being outliers.
# - In North America, the top products belong to Wii, NES, GB, and X360.
#   The top total sales come from X360, Wii, and PS3.
# - In Europe, the top products belong to Wii, PS4, PS3, and DS.
#   The top total sales come from Wii, PS3, and X360.
# - In Other, the top products belong to Wii, GB, PS2, and PS2 (again).
#   The top total sales come from DS, Wii, and PS3.

################################################################################

# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

#-------------------------------------------------------------------------------

# View data frame created in Week 4.
as_tibble(sales_clean)

# Check output: Determine the min, max, and mean values.
sales_clean_num <- subset(sales_clean, select = c(Global_Sales, NA_Sales, 
                                                  EU_Sales, Other_Sales))

# The minimum of each numerical column.
apply(sales_clean_num, 2, min)

# The maximum of each numerical column.
apply(sales_clean_num, 2, max)

# The mean of each numerical column.
apply(sales_clean_num, 2, mean)

# View the descriptive statistics.
summary(sales_clean_num)

################################################################################

# 2. Determine the impact on sales per product_id.

#-------------------------------------------------------------------------------

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
product_sales_total <- sales_clean %>% group_by(Product) %>%
  summarise(Global_Sales_Total=sum(Global_Sales),
            NA_Sales_Total=sum(NA_Sales),
            EU_Sales_Total=sum(EU_Sales),
            Other_Sales_Total=sum(Other_Sales))
# Order by descending global sales total.
product_sales_total <- product_sales_total[
  order(product_sales_total$Global_Sales_Total, 
        decreasing=TRUE),]

# View the data frame.
as_tibble(product_sales_total)

# Check dimensions.
dim(product_sales_total)
# 175 rows!!!

# Explore the data frame.
# Group by Platform.
platform_sales_total <- sales_clean %>% group_by(Platform) %>%
  summarise(Global_Sales_Total=sum(Global_Sales),
            NA_Sales_Total=sum(NA_Sales),
            EU_Sales_Total=sum(EU_Sales),
            Other_Sales_Total=sum(Other_Sales))
# Order by descending global sales total.
platform_sales_total <- platform_sales_total[
  order(platform_sales_total$Global_Sales_Total, 
        decreasing=TRUE),]

# View the data frame.
as_tibble(platform_sales_total)

# Check dimensions.
dim(platform_sales_total)
# 22 rows - much more manageable!

# Check dimensions of original data from week 4 assignment.
dim(sales_clean)
# 352 rows!!! Totally unmanageable visually in most cases.

#-------------------------------------------------------------------------------

## 2b) Determine which plot is the best to compare game sales.

# - The number of products is very high (175), so detailed product information
#   requires scatterplots; however, 175 points is a lot to keep track of 
#   visually, so the scatterplots are only good for showing clustering visually,
#   and are not so good for individual product information.
# - The number of platforms is high (22). but not so high that a barplot, or
#   any kind of side-by-side categorical plot (e.g. side-by-side boxplots), 
#   would be inappropriate. There are more plotting options for this dataset.
# - The original plots, from the week 4 assignment above, were plotted prior
#   to grouping, and thus contain duplicate values of product and platform.
#   This means they have many more rows (352), thus rendering many visualisation
#   options totally unusable. Distributions can still be plotted though, as
#   these are based on statistical visualisations that do not increase in 
#   complexity as more data points are added (e.g. histograms, boxplots).
# - Due to the aforementioned duplicates, the original plots must be replotted,
#   with the exception of the statistical plots, which benefit from more
#   granular data, as they are the only way of analysing individual data points.
#   The jitter-scatterplots don't have to be replotted either, but these are not 
#   so useful and can be discarded if needed.

################################################################################

# 3. Determine the normality of the data set.

#-------------------------------------------------------------------------------

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

# Q-Q plot for Global sales total with a reference line.
ggplot(product_sales_total)+
  geom_qq(aes(sample=Global_Sales_Total))+
  geom_qq_line(aes(sample=Global_Sales_Total),
               color='purple')+
  labs(x="Theoretical Quantiles", y="Sample Quantiles")

# Q-Q plot for NA sales total with a reference line.
ggplot(product_sales_total)+
  geom_qq(aes(sample=NA_Sales_Total))+
  geom_qq_line(aes(sample=NA_Sales_Total),
               color='green')+
  labs(x="Theoretical Quantiles", y="Sample Quantiles")

# Q-Q plot for EU sales total with a reference line.
ggplot(product_sales_total)+
  geom_qq(aes(sample=EU_Sales_Total))+
  geom_qq_line(aes(sample=EU_Sales_Total),
               color='red')+
  labs(x="Theoretical Quantiles", y="Sample Quantiles")

# Q-Q plot for Other sales total with a reference line.
ggplot(product_sales_total)+
  geom_qq(aes(sample=Other_Sales_Total))+
  geom_qq_line(aes(sample=Other_Sales_Total),
               color='blue')+
  labs(x="Theoretical Quantiles", y="Sample Quantiles")

# Q-Q plot comparing regional sales totals.
ggplot(product_sales_total)+
  geom_qq(aes(sample=NA_Sales_Total, color="North America", group=1))+
  geom_qq(aes(sample=EU_Sales_Total, color="Europe", group=1))+
  geom_qq(aes(sample=Other_Sales_Total, color="Other", group=1))+
  geom_qq_line(aes(sample=NA_Sales_Total, color="North America", group=1))+
  geom_qq_line(aes(sample=EU_Sales_Total, color="Europe", group=1))+
  geom_qq_line(aes(sample=Other_Sales_Total, color="Other", group=1))+
  labs(x="Theoretical Quantiles", y="Sample Quantiles", color='Region')

# The Q-Q plots show that the data follow roughly straight lines in the centre,
# but deviate from this towards the tail ends. This deviation is above the 
# reference lines, which means that the actual data is more spread out than
# the theoretical quantiles predicted. The plots are also all right skewed,
# indicated by their bowl shapes (as opposed to upside-down bowl shapes).
# In other words, a few stunningly successful products skew the distribution up.

#-------------------------------------------------------------------------------

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
# install.packages('moments')  # Only if necessary.
library('moments')

# Perform Shapiro-Wilk test.

# Shapiro-Wilk test for global sales.
shapiro.test(product_sales_total$Global_Sales_Total)
# The p-value is <0.05, so the data is not normally distributed.

# Shapiro-Wilk test for North-American sales.
shapiro.test(product_sales_total$NA_Sales_Total)
# The p-value is <0.05, so the data is not normally distributed.

# Shapiro-Wilk test for European sales.
shapiro.test(product_sales_total$EU_Sales_Total)
# The p-value is <0.05, so the data is not normally distributed.

# Shapiro-Wilk test for Other sales.
shapiro.test(product_sales_total$Other_Sales_Total)
# The p-value is <0.05, so the data is not normally distributed.

# For all the top four plots of the sales data of the four regions, the 
# Shapiro-Wilk test showed the p values to be less than 0.05, which leads us to 
# reject the null hypothesis of a normal distribution. This confirms our 
# earlier observations showing that the distributions are right skewed due
# to intermittently unusually high-performing products.

#-------------------------------------------------------------------------------

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# Skewness of global sales.
skewness(product_sales_total$Global_Sales_Total)
# Positive skewness of roughly 3.1.

# Kurtosis of global sales.
kurtosis(product_sales_total$Global_Sales_Total)
# Kurtosis of roughly 17.8, much greater than 3, suggesting a heavy tail
# and asymmetry.

# Skewness of North-American sales.
skewness(product_sales_total$NA_Sales_Total)
# Positive skewness of roughly 3.0.

# Kurtosis of North-American sales.
kurtosis(product_sales_total$NA_Sales_Total)
# Kurtosis of roughly 15.6, much greater than 3, suggesting a heavy tail
# and asymmetry.

# Skewness of European sales.
skewness(product_sales_total$EU_Sales_Total)
# Positive skewness of roughly 2.9.

# Kurtosis of European sales.
kurtosis(product_sales_total$EU_Sales_Total)
# Kurtosis of roughly 16.2, much greater than 3, suggesting a heavy tail
# and asymmetry.

# Skewness of Other sales.
skewness(product_sales_total$Other_Sales_Total)
# Positive skewness of roughly 1.6.

# Kurtosis of Other sales.
kurtosis(product_sales_total$Other_Sales_Total)
# Kurtosis of roughly 6.1, much greater than 3, suggesting a heavy tail
# and asymmetry. However, this is lower than NA and EU by a significant margin,
# thus suggesting better symmetry than the aforementioned regions.

# The skewness and kurtosis tests suggest heavily-tailed asymmetric 
# distributions due to positive skewness (right skew). This is in perfect
# accordance with our previous observations. The symmetry is somewhat better
# for Other sales, but only in comparison to the other two regions.

#-------------------------------------------------------------------------------

## 3d) Determine correlation
# Determine correlation.

# Install and import Psych.
# install.packages('psych')  # Only if necessary.
library('psych')

# Create DataFrame with only numerical variables (sales).
product_total_sales_num <- subset(product_sales_total, select=-c(Product))

# Calculate correlation matrix for our numerical variables (sales).
cor(product_total_sales_num)

# Visualise correlation matrix more clearly.
corPlot(product_total_sales_num, cex=1)

# Plot correlation matrix.
pairs(product_total_sales_num)

# There is a strong correlation between Global Sales and NA Sales (0.92) and 
# EU Sales (0.85), and a somewhat strong correlation with Other Sales (0.73).
# The cross correlations between different regions is also positive and 
# significant (>0.5); however somewhat weaker than the aforementioned ones.

################################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

#-------------------------------------------------------------------------------

## 4a) Scatterplots
# Create scatterplots.

# Scatterplot comparing regional sales.
# CHANGED.
ggplot(product_sales_total, aes(x=Product))+
  geom_point(aes(y=NA_Sales_Total, color="North America", group=1))+
  geom_point(aes(y=EU_Sales_Total, color="Europe", group=1))+
  geom_point(aes(y=Other_Sales_Total, color="Other", group=1))+
  labs(x="Product ID", y="Sales (M£)", color='Region')

# Jitter-scatterplot comparing platforms and global sales.
# LEFT UNCHANGED.
qplot(Platform, Global_Sales, data=sales_clean, geom='jitter',
      color=I('purple'))+
  labs(x="Platform", y="Global Sales (M£)")

# Jitter-scatterplot comparing platforms and regional sales.
# LEFT UNCHANGED.
ggplot(sales_clean, aes(x=Platform))+
  geom_jitter(aes(y=NA_Sales, color="North America", group=1))+
  geom_jitter(aes(y=EU_Sales, color="Europe", group=1))+
  geom_jitter(aes(y=Other_Sales, color="Other", group=1))+
  labs(x="Platform", y="Sales (M£)", color='Region')

#-------------------------------------------------------------------------------

## 4b) Histograms
# Create histograms, density plots, and bar plots.

# Histogram of global sales.
# LEFT UNCHANGED.
qplot(Global_Sales, data=sales_clean, geom='histogram',
      binwidth=1, fill=I('purple'))+
  labs(x="Global Sales (M£)", y="Count")

# Histogram of North-American sales.
# LEFT UNCHANGED.
qplot(NA_Sales, data=sales_clean, geom='histogram',
      binwidth=1, fill=I('green'))+
  labs(x="North-American Sales (M£)", y="Count")

# Histogram of European sales.
# LEFT UNCHANGED.
qplot(EU_Sales, data=sales_clean, geom='histogram',
      binwidth=1, fill=I('red'))+
  labs(x="European Sales (M£)", y="Count")

# Histogram of Other sales.
# LEFT UNCHANGED.
qplot(Other_Sales, data=sales_clean, geom='histogram',
      binwidth=1, fill=I('blue'))+
  labs(x="Other Sales (M£)", y="Count")

# Density plots comparing regional sales.
# LEFT UNCHANGED.
ggplot(sales_clean)+
  geom_density(aes(x=NA_Sales, fill="North America", group=1), 
               color='green', alpha=.3)+
  geom_density(aes(x=EU_Sales, fill="Europe", group=1),
               color='red', alpha=.3)+
  geom_density(aes(x=Other_Sales, fill="Other", group=1),
               color='blue', alpha=.3)+
  labs(x="Sales (M£)", y="Count", fill='Region')

# Violin density plots comparing regional sales.
# LEFT UNCHANGED.
ggplot(sales_clean)+
  geom_violin(aes(x="North America", y=NA_Sales, fill="North America", group=1), 
              color='green', alpha=.3)+
  geom_violin(aes(x="Europe", y=EU_Sales, fill="Europe", group=1),
              color='red', alpha=.3)+
  geom_violin(aes(x="Other", y=Other_Sales, fill="Other", group=1),
              color='blue', alpha=.3)+
  labs(x="Sales (M£)", y="Count", fill='Region')+
  theme(legend.position="none")

# Barplot of global sales against platform.
# LEFT UNCHANGED.
qplot(Platform, Global_Sales, data=sales_clean, geom='col',
      fill=I('purple'))+
  labs(x="Platform", y="Global Sales (M£)")

# Stacked barplot of regional sales against platform.
# LEFT UNCHANGED.
plot_ly(data=sales_clean, x=~Platform, y=~Other_Sales, type='bar', 
        name='Other') %>%
  add_trace(y=~NA_Sales, name='North America') %>%
  add_trace(y=~EU_Sales, name='Europe') %>%
  layout(barmode = 'stack',
         yaxis=list(title='Sales (M£)'), 
         legend=list(title=list(text='Region')))

#-------------------------------------------------------------------------------

## 4c) Boxplots
# Create boxplots.

# Boxplot of global sales against platform.
# LEFT UNCHANGED.
qplot(Platform, Global_Sales, data=sales_clean, geom='boxplot',
      color=I('purple'))+
  labs(x="Platform", y="Global Sales (M£)")

# Boxplot of North-American sales against platform.
# LEFT UNCHANGED.
qplot(Platform, NA_Sales, data=sales_clean, geom='boxplot',
      color=I('green'))+
  labs(x="Platform", y="North-American Sales (M£)")

# Boxplot of European sales against platform.
# LEFT UNCHANGED.
qplot(Platform, EU_Sales, data=sales_clean, geom='boxplot',
      color=I('red'))+
  labs(x="Platform", y="European Sales (M£)")

# Boxplot of Other sales against platform.
# LEFT UNCHANGED.
qplot(Platform, Other_Sales, data=sales_clean, geom='boxplot',
      color=I('blue'))+
  labs(x="Platform", y="Other Sales (M£)")

# Boxplot comparing regional sales.
# LEFT UNCHANGED.
ggplot(sales_clean)+
  geom_boxplot(aes(x="North America", y=NA_Sales, color="North America", group=1))+
  geom_boxplot(aes(x="Europe", y=EU_Sales, color="Europe", group=1))+
  geom_boxplot(aes(x="Other", y=Other_Sales, color="Other", group=1))+
  labs(x="Region", y="Sales (M£)")+
  theme(legend.position="none")

################################################################################

# 5. Observations and insights

# - The histograms of global and regional sales have a poissonian-like 
#   disribution, in that they have a maximum non-zero "y-intercept", and
#   exponentially decrease in frequency as sales value goes up.
# - Global sales are strongly correlated to NA sales, followed by EU sales,
#   and lastly, by other sales.
# - Majority of sales made in NA and EU, with all other sales being a minority.
# - Product 107 is the most financially successful across all categories!!!

################################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

################################################################################

# 1. Load and explore the data

#-------------------------------------------------------------------------------

# View data frame created in Week 5.
as_tibble(sales_clean)

# Determine a summary of the data frame.
summary(sales_clean)

# View numerical data frame created in Week 5.
as_tibble(sales_clean_num)

# Determine a summary of the numerical data frame.
summary(sales_clean_num)

################################################################################

# 2. Create a simple linear regression model

#-------------------------------------------------------------------------------

## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

# Calculate correlation matrix for our numerical variables (sales).
cor(sales_clean_num)

# Visualise correlation matrix more clearly.
corPlot(sales_clean_num, cex=1)

# Plot correlation matrix.
pairs(sales_clean_num)

# There is a strong correlation between Global Sales and NA Sales (0.93), 
# EU Sales (0.88), and Other Sales (0.82).
# The cross correlations between different regions is also positive and 
# significant (>0.6); however somewhat weaker than the aforementioned ones.

#-------------------------------------------------------------------------------

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# Create a model with only one x variable.
# _ij indicates the position in the correlation matrix (top-left index is 11).

# Model for global sales and NA sales.
model_12 <- lm(Global_Sales~NA_Sales,
               data=sales_clean_num)

# View the model.
model_12

# Model summary.
summary(model_12)

# Plot residuals (a random scatter indicating a good fit to the model).
plot(model_12$residuals)

# Define coefficients.
coeff_12 = coefficients(model_12)

# Plot correlation between variables with regression line added.
qplot(NA_Sales, Global_Sales, data=sales_clean_num)+
  geom_abline(aes(intercept=coeff_12[1], slope=coeff_12[2]), color='green')+
  labs(x="North-American Sales (M£)", y="Global Sales (M£)")

#-------------------------------------------------------------------------------

# Model for global sales and EU sales.
model_13 <- lm(Global_Sales~EU_Sales,
               data=sales_clean_num)

# View the model.
model_13

# Model summary.
summary(model_13)

# Plot residuals (a random scatter indicating a good fit to the model).
plot(model_13$residuals)

# Define coefficients.
coeff_13 = coefficients(model_13)

# Plot correlation between variables with regression line added.
qplot(EU_Sales, Global_Sales, data=sales_clean_num)+
  geom_abline(aes(intercept=coeff_13[1], slope=coeff_13[2]), color='red')+
  labs(x="European Sales (M£)", y="Global Sales (M£)")

#-------------------------------------------------------------------------------

# Model for global sales and Other sales.
model_14 <- lm(Global_Sales~Other_Sales,
               data=sales_clean_num)

# View the model.
model_14

# Model summary.
summary(model_14)

# Plot residuals (a random scatter indicating a good fit to the model).
plot(model_14$residuals)

# Define coefficients.
coeff_14 = coefficients(model_14)

# Plot correlation between variables with regression line added.
qplot(EU_Sales, Global_Sales, data=sales_clean_num)+
  geom_abline(aes(intercept=coeff_14[1], slope=coeff_14[2]), color='blue')+
  labs(x="Other Sales (M£)", y="Global Sales (M£)")

#-------------------------------------------------------------------------------

# Combine all previous plots together for easy comparison.
ggplot(sales_clean_num, aes(y=Global_Sales))+
  geom_point(aes(x=NA_Sales, color="North America", group=1))+
  geom_point(aes(x=EU_Sales, color="Europe", group=1))+
  geom_point(aes(x=Other_Sales, color="Other", group=1))+
  geom_abline(aes(intercept=coeff_12[1], slope=coeff_12[2],
                  color="North America"))+
  geom_abline(aes(intercept=coeff_13[1], slope=coeff_13[2],
                  color="Europe"))+
  geom_abline(aes(intercept=coeff_14[1], slope=coeff_14[2],
                  color="Other"))+
  labs(x="Sales (M£)", y="Global Sales (M£)", color="Region")

#-------------------------------------------------------------------------------

# SUMMARY:

# Model_12 (Global vs NA): 
# - Adjusted R-squared:  0.8738
# - p-value: < 2.2e-16

# Model_13 (Global vs EU): 
# - Adjusted R-squared:  0.7695
# - p-value: < 2.2e-16

# Model_14 (Global vs Other): 
# - Adjusted R-squared:  0.6725
# - p-value: < 2.2e-16

################################################################################

# 3. Create a multiple linear regression model

#-------------------------------------------------------------------------------

# Select only numeric columns from the original data frame.

# Multiple linear regression model.

# Model for global sales and NA sales + EU sales + Other sales.
model_1234 <- lm(Global_Sales~NA_Sales+EU_Sales+Other_Sales,
               data=sales_clean_num)

# View the model.
model_1234

# Model summary.
summary(model_1234)

# Plot residuals (a random scatter indicating a good fit to the model).
plot(model_1234$residuals)
# Residuals lie much too close to a straight line to be a reliable fit.

# Above summary is unreliable due to the inclusion of Other_Sales, which is 
# calculated via Other_Sales = Global_Sales - (NA_Sales + EU_Sales), thus 
# introducing multicollinearity and rendering the fit perfect, but useless.

#-------------------------------------------------------------------------------

# Dropping Other sales.
model_123 <- lm(Global_Sales~NA_Sales+EU_Sales,
                 data=sales_clean_num)

# View the model.
model_123

# Model summary.
summary(model_123)

# Plot residuals (a random scatter indicating a good fit to the model).
plot(model_123$residuals)
# Residuals are sufficiently randomly scattered now after removing Other sales.

# Q-Q plot for residuals with a reference line.
ggplot()+
  geom_qq(aes(sample=model_123$residuals))+
  geom_qq_line(aes(sample=model_123$residuals),
               color='purple')+
  labs(x="Theoretical Quantiles", y="Sample Quantiles")

# The Q-Q plot shows that the data follow roughly straight lines in the centre,
# but deviate from this towards the tail ends. This deviation is above the 
# reference lines towards the right (which means that the actual data is more 
# spread out than the theoretical quantiles predicted), and below towards the 
# left (which means the reverse). The plot is not quite indicative of a 
# normal distribution, despite its characteristic shape, due to its asymmetry.

#-------------------------------------------------------------------------------

# Confirm residual distribution with histogram.
qplot(model_123$residuals, geom='histogram',
      fill=I('purple'))+
  labs(x="Residuals", y="Count")

#-------------------------------------------------------------------------------

# SUMMARY:

# NEW BEST RESULT!!!
# Model_123 (Global vs NA + EU): 
# - Adjusted R-squared:  0.9685
# - p-value: < 2.2e-16

# Model_12 (Global vs NA): 
# - Adjusted R-squared:  0.8738
# - p-value: < 2.2e-16

# Model_13 (Global vs EU): 
# - Adjusted R-squared:  0.7695
# - p-value: < 2.2e-16

# Model_14 (Global vs Other): 
# - Adjusted R-squared:  0.6725
# - p-value: < 2.2e-16

################################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# The values we want to compare have been given as:
# - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
# - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
# - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
# - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
# - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

#-------------------------------------------------------------------------------

# Find the rows with the given values.
sales_sample <- sales_clean_num[sales_clean_num$NA_Sales %in%
                                  c(34.02, 3.93, 2.73, 2.26, 22.08),]

# View sample and double check it is correct.
sales_sample

# Predict the global sales using model_123.
predict_123 <- predict(model_123, newdata=sales_sample,
                      interval='confidence')

# View the prediction.
predict_123

# Compare prediction to actual values of Global Sales.
sales_sample_123 <- cbind(sales_sample, predict_123)
sales_sample_123 <- subset(sales_sample_123, select=-c(NA_Sales, EU_Sales, Other_Sales))

# View comparison.
as_tibble(sales_sample_123)

# Check accuracy of predictions.

# Install and import Metrics.
# install.packages('Metrics')  # Only if necessary.
library('Metrics')

# Calculate root-mean-square error (RMSE).
rmse(sales_sample$Global_Sales, predict_123)
# RMSE: 2.30.

# Calculate mean-absolute error (MAE)
mae(sales_sample$Global_Sales, predict_123)
# MAE: 1.68.

# Calculate mean-absolute-percentage error (MAPE).
mape(sales_sample$Global_Sales, predict_123)
# MAPE: 10.5%. Less than 10% is excellent, so this prediction is very good.

################################################################################

# 5. Observations and insights

# Multiple linear regression provides a very good model for predicting global
# sales values based on NA sales and EU sales. This is perhaps mostly due to 
# the fact that the majority of sales comes from these two regions, and
# predicted success upon territory expansion should not be taken from this 
# model. However, for now, predictions of global sales based on just these
# regions is fairly accurate, and factoring in more variables could potentially
# increase the viability of the model to account for territory expansion.

# The model used has an MAPE of 10.5%, and while the "excellence" of anything
# below 10% is an arbitrary cutoff, it is still a good rule of thumb that 
# anything approaching, or within, this accuracy threshold constitutes a 
# reliable model.

################################################################################