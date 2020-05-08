#!/bin/bash

# This script doesn't checks if the folder already exist.
# I am not responsible for broken systems or lost files,
# RUN THIS AT YOUR OWN RISK.

# Requires gnupg and rsync liblz4-tool

# CONFIG
# ______



# SCRIPT
# ______

if [ "$EUID" -ne 0 ]
then
    echo "You need to be root to create the /etc/ files."
    exit 1
fi

# Display warning
echo -n "WARNING some packages like pigz are required. Your system will break without it. Quit? (y/n)"
old_stty_cfg=$(stty -g)
stty raw -echo
answer=$( while ! head -c 1 | grep -i '[ny]' ;do true ;done )
stty $old_stty_cfg
if echo "$answer" | grep -iq "^y" ;then
    exit 1
fi

# Ask if the user wants this to create entries in /boot
should_make_sysd_entries=false
echo -n "Do you want to create /boot/entries for systemd-boot? (y/n) "
old_stty_cfg=$(stty -g)
stty raw -echo
answer=$( while ! head -c 1 | grep -i '[ny]' ;do true ;done )
stty $old_stty_cfg
if echo "$answer" | grep -iq "^y" ;then
    should_make_sysd_entries=true
fi

# Ask if the user if lvm2 service should be disabled
disable_lvm2=false
echo -n "Do you want to disable the lvm2 systemd service? (y/n) "
old_stty_cfg=$(stty -g)
stty raw -echo
answer=$( while ! head -c 1 | grep -i '[ny]' ;do true ;done )
stty $old_stty_cfg
if echo "$answer" | grep -iq "^y" ;then
    disable_lvm2=true
fi

cp -R ./etc/* /etc

if [ "$should_make_sysd_entries" = true ] ; then
    cp -R ./boot/* /boot
fi

if [ "$disable_lvm2" = true ] ; then
    systemctl stop lvm2-lvmetad.service lvm2-lvmetad.socket lvm2-monitor.service
    systemctl disable lvm2-lvmetad.service lvm2-lvmetad.socket lvm2-monitor.service
    systemctl mask lvm2-lvmetad.service lvm2-lvmetad.socket lvm2-monitor.service
fi

exit
