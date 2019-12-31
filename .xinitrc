#!/bin/sh

# Copyright (C) 2018-2019 Robin Wils

# Author: Robin Wils
# Maintainer: Robin Wils

# This config is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.

# This config is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this config. If not, see <http://www.gnu.org/licenses/>.

# Disable access control
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Fallback cursor
xsetroot -cursor_name left_ptr

# Keyboard repeat rate
# Increase the first number if you want a higher key press delay
# Increase the second number if you want that holding a key repeats it at a faster rate
xset r rate 175 50

# Screen color temperature
# The smaller the value, the better it is for your eyes.
# (less blue light)
sct 2000

# Start Emacs
# exec dbus-launch --exit-with-session emacs
