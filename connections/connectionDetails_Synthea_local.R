library("DatabaseConnector")

#Set up connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms="postgresql",
  server="localhost/cms_synthea_5pct", #This should be your server location. If it is on your local machine, change 'cdm_v6_testing' with the name of the database you chose
  user="postgres", #Your user name. If you are the owner this will most likely be 'postgres'
  password= "solon5768", #Your password
  port=5432 #The port number. You can find this by right-clicking on the server and choosing properties
)


#connection <- connect(connectionDetails)