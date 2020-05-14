# Use dvorak layout and swap ctrl and caps
setxkbmap -layout dvorak -option ctrl:nocaps

# Use us layout for ergodox keyboard, the firmware seems to require this
# This grep can be shorter, but it is the first thing that I came up with
ergodox_device_id=`xinput | grep "keyboard" | grep '\bErgoDox EZ ErgoDox EZ\b  ' | grep -o '=[[:digit:]]*' | cut -c 2-`
if [ -n ${ergodox_device_id} ]; then setxkbmap -device $ergodox_device_id us; fi

# Make the left Shift key do shift lock
# xmodmap -e "keycode 50 = Shift_Lock"
# Make the right Shift key do shift lock
# xmodmap -e "keycode 62 = Shift_Lock"

# I prefer sticky keys over this, it usually spares me a keypress, unless I need capslock

# Stickykeys. Don't turn on "latch lock", ie pressing a modifier key twice "locks" it on.
# xkbset accessx sticky -twokey -latchlock
# same thing with locking the modifier
xkset accesx sticky -twokey

# don't expire these settings. (run xkbset q exp to see your settings)
# I don't use this since I want to have my settings in a config like this file
# xkbset exp 1 =accessx =sticky =twokey =latchlock

# MOUSE SETTINGS
# Swap middle button with right mouse button for "Logitech Lenovo USB Optical Mouse"
if xinput | grep -q "Logitech Lenovo USB Optical Mouse"; then
    xinput --set-button-map "Logitech Lenovo USB Optical Mouse" 1 3 2
fi
