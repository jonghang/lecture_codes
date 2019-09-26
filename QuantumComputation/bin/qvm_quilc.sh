#!/bin/sh

if [ $# -eq 0 ]
then
    echo "Specify start or stop!"
fi

if [ $1 = "start" ]
then
  echo "******************************************************"
  echo "Starting QVM and QUILC"
  echo "******************************************************"
  
  # docker container start -d -p 5000:5000 --name qvm rigetti/qvm -S
  # docker container start -d -p 5555:5555 --name quilc rigetti/quilc -R
  docker container start qvm
  docker container start quilc
  
  echo "------------------------------------------------------"
  echo "Containers:"
  echo "------------------------------------------------------"
  docker container ls
fi

if [ $1 = "stop" ]
then
  docker container stop qvm
  docker container stop quilc
fi
