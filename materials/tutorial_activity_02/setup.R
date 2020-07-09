setup<-function() {connection <- dbConnect(RPostgres::Postgres(),
                       dbname = "kickstarter",
                       host = "dsci100.c8dqirbxxovf.ca-central-1.rds.amazonaws.com",
                       port = 5432,
                       user = "dsci100",
                       password = "dsci100")
                    return (connection)}