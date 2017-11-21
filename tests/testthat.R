#Copyright © 2016 RTE Réseau de transport d’électricité

require(testthat)
require(antaresRead)
require(plyr)



is.solaris<-function()grepl('SunOS',Sys.info()['sysname'])

if(!is.solaris()){
  test_check("antaresRead")
}
