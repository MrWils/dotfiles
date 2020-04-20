# Start mpd if it is not started yet
pgrep mpd || exec mpd /home/rmw/.config/mpd/mpd.conf &

# Make sure that all ssh-agents are killed
killall ssh-agent

# Start Wayland, Sway session with SSH-key support
eval `ssh-agent`
# exec sway &> swaylog
[ "$(tty)" = /dev/tty1 ] && startx # exec sway
