library(RGoogleAnalytics)
#creat token
oauth_token=Auth(client.id="391820084470-sgfiavnv8cqd7m8rjl2v9e44r8ovimua.apps.googleusercontent.com",
                 client.secret="hFqi7Kv2CIMs3guEuvE4Lq1X")
#password 4212360
save(oauth_token,file="oauth_token")


# This example assumes that a token object is already created
# Create a list of Query Parameters
query.list <- Init(start.date = "2014-11-28",
                   end.date = "2014-12-04",
                   dimensions = "ga:date",
                   metrics = "ga:sessions,ga:pageviews",
                   max.results = 1000,
                   table.id = "ga:47901071")
# Create the query object
ga.query <- QueryBuilder(query.list)
# Fire the query to the Google Analytics API
ga.df <- GetReportData(ga.query, oauth_token)
ga.df <- GetReportData(ga.query, oauth_token, split_daywise=TRUE)
ga.df <- GetReportData(ga.query, oauth_token, paginate_query=TRUE)
GetReportData()