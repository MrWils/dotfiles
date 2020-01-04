#!/bin/bash

# Enviroment variables
# Add  npm to PATH
PATH=$PATH:~/.npm-global/bin
export PATH

# Use dark gtk theme
export GTK_THEME=Adwaita:dark
# Enable QT apps to have gtk theme
export QT_QPA_PLATFORMTHEME=qt5ct
# Force wayland on qt apps
export QT_QPA_PLATFORM=wayland
export QT_WAYLAND_FORCE_DPI=96
export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
# Make java apps display correctly in sway
export _JAVA_AWT_WM_NONREPARENTING=1
