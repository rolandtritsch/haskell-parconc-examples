#! /bin/bash

etlas run chapter01-exe
etlas run chapter07fork-exe
echo -n -e "1\n5\nexit\n" | etlas run chapter07reminders-exe
etlas run chapter07mvars-exe
etlas run chapter07logger-exe
#etlas run chapter07phonebook-exe
etlas run chapter08geturlstimed-exe
etlas run chapter08geturlsfirst-exe
etlas run chapter09catchmask-exe ./README.md ./package.yaml ./stack.yaml
etlas run chapter10windowman-exe
#etlas run chapter12server-exe & sleep 2 && echo -e -n "22\n123\nquit\n" | nc localhost 44444
