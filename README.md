### Education
Master of Management in Analytics | McGill University (August 2022)
- McKesson Canada Capstone project
- Innovation Youth Capstone project

Bachelor of Commerce Major in Supply Chain Operations Management Minor in Data Intelligence | Concordia University (December 2019)
- Co-op internship program
- SAS Certfication in Business Analytics and Data Science
- Team member on the John Molson Supply Chain and Business Technology Management Association
- Supply Chain Capstone Project - improving the receiving area of the Glen Hospital using business process re-engineering

### Work Experience
Forecast Analyst @ Abbvie (June 2022 - Present)
- Help drive brand commercial success for the Immunology portfolio through communicating insights derived from market data and interpret market trends in a way that helps AbbVie Canada make more strategic business decisions. 
- Work in partnership with the Finance team to deliver monthly, annual and long-range financial forecasts and with Business Unit and Brand Managers, to address the needs and identify opportunities for improved brand performance.
- Analyze complex business problems and issues using data from internal and external sources and provide perspective and insights on issues/opportunities.

Master Data Technician @ McGill University Health Centre (MUHC) 
_Full-time (January 2019 - July 2021)_
_Co-op internship (May 2018 - January 2019)_
- Implemented electronic data interchange with healthcare suppliers to automate purchasing and accounting processes to improve database information quality by eliminating manual tasks
- Collaborated on a project to improve visibility of on-site inventory system of personal protective equipment (PPE) for COVID-19 that increased management’s overall decision-making power
- Generated product consumption reports and carried out exploratory analyses for management to gain insights and budget, plan, and negotiate accordingly


Procurement Co-op Intern @ Bombardier Business Aircraft (September 2017 - December 2017)
- Carried out market analysis of air management system (AMS) fans to identify potential cost-savings benefits
- Managed Excel file to track payment progress of overdue invoices for weekly AMS supplier meetings to improve supplier relationship and overall business operations


### Data Science Assignments and Projects

#### McKesson Canada Capstone Project
(September 2021 - April 2022)
8 month consulting capstone project for the Master of Management Analytics Program in partnership with McKesson Canada

**Role**: Data Scientist team lead with focus on UX/UI, managed a team of 6 individuals (2 data engineers, 2 modelers, 1 strategist, 1 UX/UI designer) to solve 2 analytics use cases through proof of concepts

_Use Case 1: Scheduled Reporting Automation_
Objective: Design a solution to migrate the McKesson Business Data Strategy and Analytics team's in-house scheduled reporting solution to standardized cloud-based technologies available at McKesson

The solution needed to:
- be cloud based
- be faster to generate reports without delays
- reduce reporting errors

Tools used for the solution:
- Databricks
- Snowflake
- WinSCP
- Apache Spark
- Microsoft Azure
- Outlook
- Power BI

Estimated value/benefit gained from proposed solution:
- Saving +500 hours/year
- Cost savings from manual effors and reduction in errors


_Use Case 2: Exploring Neural Network Models for Sales Forecasting_
Objective: Explore more advanced forecasting models that could potentially outperform existing models and provide more accurate forecasts

Tools used for the exploration:
- Tensorflow
- Keras
- Python

Dataset:
- +2000 products
- 24 months of sales
- +100k rows

Neural Network Models Tested:
- MLP - multi-layer perceptron
- CNN - convolutional neural network
- Simple RNN - recurrent neural network
- LSTM - long short-term memory
- GRU - gated recurrent units

Why Neural Networks?
- Can learn complex/non-linear patterns
- May not require scaled or stationary time series inputs (i.e. they can learn trend and seasonality components directly)
- Support Multivariate forecasting (i.e. multiple input variables)

Modelling Process:
1. Data Preprocessing --> check missing values, convert negative sales to $0, min-max normalization
2. Data Preparation --> Converting time series structure to supervised learning using sliding window method, split the data to train (34 months) and test (12 months)
3. Model training and selection --> hyper-parameter tuning, select the best model based on test-set RMSE
4. 5-month back-testing the chosen model --> assume the sales of the last 5 months don't exist
5. Forecast products using their respective champion models to project the next 24 months
6. Reporting results --> besides reporting the best model according to lowest RMSE, we also reported the MAPE (easy to comprehend measurement), run-time (want to see how heavy the process is to run the NNs), back tested results, and forecasted results




_*Note: As this project contains data/information owned by the Company, it cannot be shared publicly_



#### Innovation Youth Capstone Project
2 month consulting capstone project working with a not-for-profit organization on a data analytics solution.

_Objective_: Develop a forecast modelling framework that would allow Innovation Youth to be able to predict how much produce to buy for the bi-weekly solidarity farmer's markets that could be used to make better ordering decisions to reduce left-overs, waste, and better manage costs.

_Data used_:
- Internal data: invoices, attendee tracking sheet, sales data
- Additional/external data: synthetic data for proof of concept demonstration, seasonal produce guide, weather data

_Tools used_ - google suite:
- Google docs for documentation: [documentation how-to manual can be found here](assignments-projects/Innovation-Youth-Capstone/Documentation.docx)
- Google sheets for data collection and dashboarding: [data collection can be found here](assignments-projects/Innovation-Youth-Capstone/MAIN_FARMERS_MARKET.xlsx) [dashboard can be found here](assignments-projects/Innovation-Youth-Capstone/DASHBOARD.xlsx)
- Google collab for forecasting and optimization models using python: [code can be found here](assignments-projects/Innovation-Youth-Capstone/Code.ipynb)
- Google drive for data and file storage

_Models Used_:
- Moving average
- Auto arima
- Seasonal auto arima
- Simple exponential smoothing
- Holt winter's exponential smoothing
- Naive forecasting
- Random forest
- Multiple linear regression

[Submission report can be found here](assignments-projects/Innovation-Youth-Capstone/InnovationYouth_Final_Solution_Report.pdf)

[All files here](assignments-projects/Innovation-Youth-Capstone)


#### Neural Networks - GANS for internet pictures



#### Black Lives Matter vs. Blue Lives Matter Social Media Movements - A Social Media Network Analysis (INSY 670 Social Media Analytics)


#### Post Interactions Analysis - What makes a post engaging? (MRKT 671)


#### Hypothesis Testing Uber Wait Times (MRKT 671)


#### Hypothesis Testing effectiveness of coupons, demographics, target marketing (MRKT 671)



#### Predicting Emergency Room wait times (INSY 672)



#### Car Brand Analysis - webscraping and topic modelling (INSY 669)


#### Optimization model using integer programming for NFL scheduling (MGSC 662)



#### Predict what makes a good chocolate (MGSC 661)



#### Building a database and queries for an amusement park (INSY 661)


#### building a database and queries for a dating site (INSY 661)
