if xinput | grep -q "HID 256c:006d Pad pad"; then
    # Pen Tablet, at home place
    if xrandr | grep -q "DP2-2"; then
        # Set display
        xsetwacom set "HID 256c:006d Pen stylus" MapToOutput DP2-2

        # Button 1 is named 1
        # Button 2 is named 2
        # Button 3 is named 3
        # Button 4 is named 8
        # Button 5 is named 9
        xsetwacom set "HID 256c:006d Pad pad" button 9 key B
        # Button 6 is named 10
        xsetwacom set "HID 256c:006d Pad pad" button 10 key Ctrl T
        # Button 7 is named 11
        xsetwacom set "HID 256c:006d Pad pad" button 11 key Ctrl
        # Button 8 is named 12
        xsetwacom set "HID 256c:006d Pad pad" button 12 key Ctrl Z
    fi
fi
