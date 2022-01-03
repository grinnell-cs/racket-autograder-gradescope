#!/bin/bash

apt-get update -y
apt-get install software-properties-common -y
add-apt-repository ppa:plt/racket -y
apt-get install -y racket
apt-get clean
raco pkg install -D https://github.com/grinnell-cs/csc151.git#main
