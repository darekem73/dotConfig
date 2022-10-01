#!/bin/bash -x

if [[ "$1" == "" ]]; then
	echo "Requires *.AppImage as argument"
	exit 1
fi

APPIMAGE=$(basename $1)

EXECDIR=$HOME/Downloads
TMP=/tmp
DIR=squashfs-root
TARGETDIR=$HOME/.local/share/applications

cp "$APPIMAGE" $TMP
cd $TMP
chmod 755 "$APPIMAGE"
$APPIMAGE --appimage-extract >/dev/null 2>&1
DESKTOP=`find $DIR -iname "*.desktop" | head -1`
DESKTOPFILE=$(basename "$DESKTOP")
cp "$DESKTOP" $TARGETDIR
#sed -i "s|Exec=[^ ]*\(.*\)$|Exec=$EXECDIR/$APPIMAGE\1|" "$TARGETDIR/$DESKTOPFILE"
sed -i "s|Exec=[^ ]*\(.*\)$|Exec=$EXECDIR/$APPIMAGE %F|" "$TARGETDIR/$DESKTOPFILE"
rm -rf $DIR
[ -f $EXECDIR/$APPIMAGE ] || mv $TMP/$APPIMAGE $EXECDIR && rm $TMP/$APPIMAGE && chmod 755 "$EXECDIR/$APPIMAGE"
