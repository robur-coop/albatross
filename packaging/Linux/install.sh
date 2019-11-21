#!/bin/sh
sudo mkdir -m 0700 -p /var/lib/albatross/block

sudo cp ../../_build/default/client/*.exe /usr/local/sbin/
sudo cp ../../_build/default/daemon/*.exe /usr/local/sbin/
sudo cp ../../_build/default/provision/*.exe /usr/local/sbin/
sudo cp ../../_build/default/stats/*.exe /usr/local/sbin/
sudo cp ../../_build/default/tls/*.exe /usr/local/sbin/
sudo cp ./albatross_*.service  /lib/systemd/system/
sudo systemctl daemon-reload
sudo systemctl stop   albatross_console
sudo systemctl start  albatross_console
sudo systemctl stop  albatross_log
sudo systemctl start albatross_log
sudo systemctl stop  albatross_stat
sudo systemctl start albatross_stat
sudo systemctl stop  albatross_daemon
sudo systemctl start albatross_daemon
