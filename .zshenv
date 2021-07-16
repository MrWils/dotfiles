# Enviroment variables
# Add  npm to PATH
# PATH=$PATH:~/.npm-global/bin
# PATH=$PATH:~/go/bin/
# export PATH
# Go lang
# export GOPATH=$HOME/.go
# Use dark gtk theme
#  export GTK_THEME=Adwaita:dark
# Enable QT apps to have gtk theme
# export QT_QPA_PLATFORMTHEME=qt5ct
# Force wayland on qt apps
# export QT_QPA_PLATFORM=wayland
# export QT_WAYLAND_FORCE_DPI=96
# export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
# Make sure that bemenu uses wayland
# export BEMENU_BACKEND=wayland
# Make java apps display correctly in sway
# export _JAVA_AWT_WM_NONREPARENTING=1
# Make sure that gdk (firefox) uses wayland
# export GDK_BACKEND=wayland <-- breaks applications and is not needed,
# because GTK auto detects the best backend
# export MOZ_ENABLE_WAYLAND=1
# Kitty terminal wayland support
# export KITTY_ENABLE_WAYLAND=1
# Node stuff
# export NODE_OPTIONS="--max_old_space_size=6144"
# Use optirun with gamemode if you use bumblebee, or use prime-run, all this nvidia stuff is way too confusing
export GAMEMODERUNEXEC="prime-run"
# TLP and bumblebee can cause issues, set this environment variable if needed
# export RUNTIME_PM_DRIVER_BLACKLIST="nvidia"
# export RUNTIME_PM_DRIVER_BLACKLIST="nouveau nvidia"
