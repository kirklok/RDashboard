# RDashboard
Mixpanel functionality (plus many more cool features) built using Shiny app in R. 
Sourcing data from PostgreSQL database which should have at least two tables:
* cross-sectional data
* events

Features
* cohort analysis
 * trends (MAU, DAU and any other event)
 * custome formulas (how did my conversion change over time)
* funnel analysis
* retention analysis
  * Dx, where X - number of days
  * retention matrix
* identifying power users
* filtering events and users in real time

Shiny App can be
* run locally
* run on an R server

To run on AWS server please follow these instructions:
http://www.r-bloggers.com/instructions-for-installing-using-r-on-amazon-ec2/

