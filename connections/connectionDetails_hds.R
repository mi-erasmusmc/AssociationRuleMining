library("DatabaseConnector")

#Set up connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms="sql server",
  server="healthdatascience.database.windows.net", #This should be your server location. If it is on your local machine, change 'cdm_v6_testing' with the name of the database you chose
  user="sioannou", #Your user name. If you are the owner this will most likely be 'postgres'
  password= "Synpuf2020!", #Your password
  port=1433,  #The port number. You can find this by right-clicking on the server and choosing properties
  extraSettings = "database=hds1;encrypt=true;trustServerCertificate=false;hostNameInCertificate=*.database.windows.net;loginTimeout=30;"
)


#connection <- connect(connectionDetails)

