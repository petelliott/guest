#!/usr/bin/env bash

# install guest for travis

# install dependecies
sudo apt update
sudo apt install -y guile-2.2 lcov
gem install coveralls-lcov

SITEDIR=$(guile -c "(display (%site-dir))(newline)")

# install guest
sudo git clone --depth 1 https://github.com/Petelliott/guest.git $SITEDIR/guest
sudo mv $SITEDIR/guest/guest /usr/bin/
