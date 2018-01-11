#Copyright © 2016 RTE Réseau de transport d’électricité

require(testthat)
require(antaresRead)
require(plyr)


#Maybe include this in some test-file.R 
#is.solaris<-function(){
#  grepl('SunOS',Sys.info()['sysname'])
#}

#if(!is.solaris()){

test_check("antaresRead")

# }
