#!/bin/bash

wget -c "https://github.com/AppImage/AppImages/raw/master/pkg2appimage"
bash -ex pkg2appimage Google_Chrome

# if not set in grub
#sudo sysctl kernel.unprivileged_userns_clone=1
