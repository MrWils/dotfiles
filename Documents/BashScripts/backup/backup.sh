#!/bin/bash

# This script doesn't checks if the folder already exist.
# I am not responsible for broken systems or lost files,
# RUN THIS AT YOUR OWN RISK.

# Requires gnupg and rsync liblz4-tool

# CONFIG
# ______

# computer name
readonly COMPUTER_NAME="LenovoW540"
# location for the backup
readonly BACKUP_DIRECTORY="/mnt/Backups/$COMPUTER_NAME/$(hostname)-archlinux/$(date +"%F---%R")"
# location for the music backup
readonly BACKUP_MUSIC_DIRECTORY="/mnt/Backups/Music"
# location for the videos backup
readonly BACKUP_VIDEOS_DIRECTORY="/mnt/Backups/Videos"
# location for the virtual machines backup
readonly BACKUP_VM_DIRECTORY="/mnt/Backups/VirtualMachines/$(date +"%F---%R")"
# label of the partition which you want to backup to
readonly BACKUP_PARTITION_LABEL="MyPassport"


# SCRIPT
# ______

if [ "$EUID" -ne 0 ]
then
    echo "You need to be root to create a backup."
    exit 1
fi

# check if our partition exists
partitionExists=`lsblk -o label | grep $BACKUP_PARTITION_LABEL`
if [ -z "$partitionExists" ]
then
    echo "Couldn't find a partition with the label $BACKUP_PARTITION_LABEL"
    exit 1
fi

# Ask if the user wants to backup the VMs
should_backup_vms=false
echo -n "Do you want to backup the VMs? (y/n) "
old_stty_cfg=$(stty -g)
stty raw -echo
answer=$( while ! head -c 1 | grep -i '[ny]' ;do true ;done )
stty $old_stty_cfg
if echo "$answer" | grep -iq "^y" ;then
    should_backup_vms=true
fi

# unmount our partition, if it is mounted
readonly mount_location=$(mount | grep $BACKUP_PARTITION_LABEL | cut -d ' ' -f1)
umount mount_location &> /dev/null
unset mount_location &> /dev/null

# mount our partition
mount -L $BACKUP_PARTITION_LABEL /mnt &> /dev/null

# make backup directory
mkdir -p $BACKUP_DIRECTORY

# make a new system backup
rsync -avPzz /home/ --exclude-from=exclude.txt "$BACKUP_DIRECTORY/"

# create a file with the packages in the backup directory
touch $BACKUP_DIRECTORY/installed_packages.txt
pacman -Qqe > $BACKUP_DIRECTORY/installed_packages.txt

# update/sync music backup
rsync -avzz --ignore-existing /home/rmw/Music/ "$BACKUP_MUSIC_DIRECTORY/"

# update/sync videos backup
rsync -avzz --ignore-existing /home/rmw/Videos/ "$BACKUP_VIDEOS_DIRECTORY/"

# update/sync VMs backup
if [ "$should_backup_vms" = true ] ; then
    # make backup directory
    mkdir -p $BACKUP_VM_DIRECTORY
    # update/sync VMs backup
    rsync -avPzz /home/rmw/VirtualMachines/ "$BACKUP_VM_DIRECTORY/"
fi
# The used arguments are:
# - a archive mode (this includes a bunch of options, read the man page)
# - v verbose (show output/progress/show verbose)
# - P preserve partial files (it means make the destination like the source)
# - zz use compression

# unmount disk
echo -e "\nUnmounting the backup disk"
umount /mnt

# end message
echo -e "\n\n\nBackup finished\!"
exit
