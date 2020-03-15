#!/bin/bash

# This file should be sourced by all POSIX sh-compatible shells upon login
source /etc/profile

# Enable zshrc settings
[[ -f /etc/zsh/zshrc ]] && . /etc/zsh/zshrc

# Start mpd if it is not started yet
pgrep mpd || exec mpd /home/rmw/.config/mpd/mpd.conf &

# Make sure that all ssh-agents are killed
killall ssh-agent

# Show possible sessions with a timeout
time=5
clear
started_session=false

echo "Choose a session:"
echo "1) Wayland, Sway session"
echo "2) Xorg session"
echo "3) None"
echo "\nThe first option will be started automatically if you do not select anything."
while [ $time -gt 0 ]; do
    read -u 0 -k 1 -t 1
    sleep 1
    : $((time--))

    case $REPLY in
        1)
            started_session=true
            # Start Wayland, Sway session with SSH-key support
            eval `ssh-agent`
            exec sway &> swaylog
            break
            ;;
        2)
            started_session=true
            # Start Xorg
            exec startx
            break
            ;;
        3)
            started_session=true
            break
            ;;
        *)
            if [ ! $REPLY = "" ]; then
                echo "\nInvalid option\n"
            fi
    esac
done

if [ ! $started_session ]; then
    # Start Sway with SSH-key support
    eval `ssh-agent`
    exec sway &> swaylog
fi
