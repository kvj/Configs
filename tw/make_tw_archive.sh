#!/bin/sh

PWD_DIR=`pwd`
DIR=`mktemp -d`

_tw_get_uuid () {
	CMD="echo $1|gawk -F/ '{print \$3}'"
	echo `eval $CMD`
	
}

_tw_config () {
	CMD="task show rc.verbose=nothing $1|gawk '/$1\s(.+)$/ {print \$2}'"
	VALUE=`eval $CMD`
	echo $VALUE
}

_SERVER=`_tw_config taskd.server`
_CRED=`_tw_config taskd.credentials`
_UUID=`_tw_get_uuid $_CRED`
TASK_DIR=$DIR/.task/$_UUID
ZIP="$_UUID.`date +%y%m%d%H%M%S`.tw.tar.gz"

_CA_PEM=`_tw_config taskd.ca`
_KEY_PEM=`_tw_config taskd.key`
_CERT_PEM=`_tw_config taskd.certificate`

_CA_NAME=`basename $_CA_PEM`
_KEY_NAME=`basename $_KEY_PEM`
_CERT_NAME=`basename $_CERT_PEM`

mkdir -p $TASK_DIR/cert

eval "cp $_CA_PEM $TASK_DIR/cert/$_CA_NAME"
eval "cp $_KEY_PEM $TASK_DIR/cert/$_KEY_NAME"
eval "cp $_CERT_PEM $TASK_DIR/cert/$_CERT_NAME"

echo "# Created by make_tw_archive.sh\n\ndata.location=~/.task/$_UUID\ntaskd.server=$_SERVER\ntaskd.credentials=$_CRED\ntaskd.ca=~/.task/$_UUID/cert/$_CA_NAME\ntaskd.key=~/.task/$_UUID/cert/$_KEY_NAME\ntaskd.certificate=~/.task/$_UUID/cert/$_CERT_NAME\n\n# include ...\n" > $DIR/.taskrc.$_UUID

tar -zcvf $ZIP -C $DIR .

rm -r $DIR

echo "File $PWD_DIR/$ZIP created"

