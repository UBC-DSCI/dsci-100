setup<-function() {connection <- dbConnect(RPostgres::Postgres(),
                       dbname = "kickstarter",
                       host = "dsci-100-student.stat.ubc.ca",
                       port = 5432,
                       user = "dsci100",
                       password = "dsci100")
                    return (connection)}
