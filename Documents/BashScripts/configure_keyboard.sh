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

# Stickykeys
xkbset sticky -twokey latchlock ## set sticky keys, don't disable via twokey
xkbset exp 1 =sticky ## don't expire sticky keys

# MOUSE SETTINGS
# Swap middle button with right mouse button for "Logitech Lenovo USB Optical Mouse"
if xinput | grep -q "Logitech Lenovo USB Optical Mouse"; then
    xinput --set-button-map "Logitech Lenovo USB Optical Mouse" 1 3 2
fi
