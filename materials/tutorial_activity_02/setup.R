setup<-function() {connection <- dbConnect(RPostgres::Postgres(),
                       dbname = "kickstarter",
                       host = "r7k3-mds1.stat.ubc.ca",
                       port = 5432,
                       user = "dsci100",
                       password = "dsci100")
                    return (connection)}