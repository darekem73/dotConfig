#!/bin/bash

if [[ $# < 3 ]]; then
	echo "Usage $0 vm-name iso-name os-type [start]"
	exit
fi

VM=$1
ISO=$2
#OSTYPE="Arch_64"
#OSTYPE="Debian_64"
OSTYPE=$3
DISK="/home/darek/VirtualBox VMs/$VM.vdi"

if [[ $4 == "start" ]]; then
	AUTOSTART=1
else
	AUTOSTART=0
fi

VBoxManage createhd --filename "$DISK" --size 25000
VBoxManage createvm --name $VM --ostype $OSTYPE --register
VBoxManage storagectl $VM --name "IDE" --add ide --hostiocache on --portcount 2
VBoxManage storageattach $VM --storagectl "IDE" --port 1 --device 0 --type dvddrive --medium $ISO
VBoxManage storagectl $VM --name "SATA" --add sata --controller IntelAHCI --portcount 1
VBoxManage storageattach $VM --storagectl "SATA" --port 0 --device 0 --type hdd --medium "$DISK"
VBoxManage modifyvm $VM --cpus 2
VBoxManage modifyvm $VM --ioapic on
VBoxManage modifyvm $VM --firmware efi
VBoxManage modifyvm $VM --graphicscontroller vmsvga
#VBoxManage modifyvm $VM --graphicscontroller vboxvga
VBoxManage modifyvm $VM --boot1 dvd --boot2 disk --boot3 none --boot4 none
VBoxManage modifyvm $VM --memory 2048 --vram 32
VBoxManage modifyvm $VM --nic1 NatNetwork

if [[ $AUTOSTART -eq 1 ]]; then
	VBoxManage startvm $VM
fi
