#! /bin/bash

stack exec chapter01-exe
stack exec chapter07fork-exe
echo -n -e "1\n5\nexit\n" | stack exec chapter07reminders-exe
stack exec chapter07mvars-exe
stack exec chapter07logger-exe
stack exec chapter07phonebook-exe
stack exec chapter08geturlstimed-exe
stack exec chapter08geturlsfirst-exe
stack exec chapter09catchmask-exe ./README.md ./package.yaml ./stack.yaml
stack exec chapter10windowman-exe
stack exec chapter12server-exe & sleep 2 && echo -e -n "22\n123\nquit\n" | nc localhost 44444
stack exec chapter12serverstm-exe & sleep 2 && echo -e -n "22\n*4\n22\nquit\n" | nc localhost 44444
