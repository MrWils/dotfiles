#!/bin/bash

# Automount this partition
PARTITION_LABEL="frzr_root"

# This file should be sourced by all POSIX sh-compatible shells upon login
source /etc/profile

# Enable zshrc settings
[[ -f /etc/zsh/zshrc ]] && . /etc/zsh/zshrc

# Start mpd if it is not started yet
pgrep mpd || exec mpd /home/rmw/.config/mpd/mpd.conf &

# Check if our partition exists
partitionExists=`lsblk -o label | grep $PARTITION_LABEL`
if [ -z "$partitionExists" ]
then
    echo "Couldn't find a partition with the label $PARTITION_LABEL"
    exit 1
fi

# Unmount our partition, if it is mounted
mount_location=$(mount | grep $PARTITION_LABEL | cut -d ' ' -f1)
umount mount_location &> /dev/null
unset mount_location &> /dev/null

# mount our partition
mkdir /run/media/rmw/frzr_root
sudo mount -L $PARTITION_LABEL /run/media/rmw/frzr_root &> /dev/null

# If running from tty1 start sway
if [ "$(tty)" = "/dev/tty1" ]; then
    # Make sure that all ssh-agents are killed
    killall ssh-agent

    # Start Sway with SSH-key support
    eval `ssh-agent`
    exec sway &> swaylog
    # export QT_QPA_PLATFORM=''
    # exec startxfce4
fi
