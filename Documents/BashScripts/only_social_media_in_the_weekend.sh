# Blocks social media on the requested days

# Hblock is required for this script
# I use this script in a systemd timer
# /etc/systemd/system/socialMediaBlocker.timer
# sudo systemctl enable socialMediaBlocker.timer && sudo systemctl start socialMediaBlocker.timer

# This script assume that you use sources.list you can add the default
# built in sources to that if if you want those.

# VARIABLES

# Host file
SOCIAL_MEDIA_BLOCKING_HOST_FILE="https://codeberg.org/RobinWils/social_websites_host_file/raw/branch/master/hosts"

# When to block social media
# Days of week (1..7)
# 1 is Monday
APPLY_ON_DAYS="5 6 7"


# SCRIPT

# Only allow to run this with root permissions
if [ "$EUID" -ne 0 ]
then
    echo "You need to be root to create a backup."
    exit 1
fi

# Function which checks if a string contains a substring
string_contain() {
    [ -z "$1" ] || { [ -z "${2##*$1*}" ] && [ -n "$2" ];};
}

# Check which day it is
if string_contain $(date +%u) $APPLY_ON_DAYS
then
    # Don't block
    if grep -q "$SOCIAL_MEDIA_BLOCKING_HOST_FILE" /etc/hblock.d/sources.list
    then
        grep -v "$SOCIAL_MEDIA_BLOCKING_HOST_FILE" /etc/hblock.d/sources.list > /etc/hblock.d/newhosts
        mv /etc/hblock.d/newhosts /etc/hblock.d/sources.list
        sh /usr/local/bin/hblock
    fi
else
    # Block
    if ! grep -q "$SOCIAL_MEDIA_BLOCKING_HOST_FILE" /etc/hblock.d/sources.list
    then
        echo "$SOCIAL_MEDIA_BLOCKING_HOST_FILE" >> /etc/hblock.d/sources.list
        sh /usr/local/bin/hblock
    fi
fi
