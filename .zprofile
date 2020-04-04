#!/bin/bash

# This file should be sourced by all POSIX sh-compatible shells upon login
source /etc/profile

# Enable zshrc settings
[[ -f /etc/zsh/zshrc ]] && . /etc/zsh/zshrc

# Start mpd if it is not started yet
pgrep mpd || exec mpd /home/rmw/.config/mpd/mpd.conf &

# Make sure that all ssh-agents are killed
killall ssh-agent

# Start Wayland, Sway session with SSH-key support
eval `ssh-agent`
exec sway &> swaylog
