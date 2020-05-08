#!/bin/bash

if [[ $# -lt 1 ]];
   then
   echo -e "\033[1mUsage: \033[0msudo $0 NameForTomCatHome"
fi

# Set variables (pass the argument to these variables)
LOCATION="/home/mrwils/Documents/TomCatHomes/$1"
PORT="8081"
CONTROL_PORT="8006"

# Program
su -c "tomcat8-instance-create -p $PORT -c $CONTROL_PORT $LOCATION/tomcat"
su -c "ln -s /usr/share/tomcat8/bin/bootstrap.jar $LOCATION/tomcat/bin/"
su -c "ln -s /usr/share/tomcat8/bin/tomcat-juli.jar $LOCATION/tomcat/bin/"
su -c "ln -s /usr/share/tomcat8/lib/  $LOCATION/tomcat/"

chown -R mrwils $LOCATION

exit
