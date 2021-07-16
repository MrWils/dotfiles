find /var/jellyfin-media/ -name '*.srt' -exec /home/rmw/Documents/BashScripts/cleanSubs/sub-clean.sh "{}" \;
find /var/lib/jellyfin/metadata/library/ -name '*.srt' -exec /home/rmw/Documents/BashScripts/cleanSubs/sub-clean.sh "{}" \;
