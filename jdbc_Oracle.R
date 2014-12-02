library(DBI)
library(RJDBC)
library(rJava)
drv<-JDBC("oracle.jdbc.driver.OracleDriver", "/usr/share/java/ojdbc14.jar")
conn<-dbConnect(drv, "jdbc:oracle:thin:@//<db_ip>:<port>/<sid>","<id>","<pw>")
query = "SELECT * FROM TABLES"
dbGetQuery(conn, query)