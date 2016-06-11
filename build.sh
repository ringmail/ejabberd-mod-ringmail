#!/bin/bash

# change with the erlc path of your current ejabberd installation
EJBR_PATH='/usr/lib64/ejabberd-16.04/include'
EJBR_VERSION="15.07"
EJBR_EBIN="/usr/lib64/ejabberd-16.04/ebin/"
LAGER_EBIN="/usr/lib64/lager-3.0.2/ebin"

FXML_PATH="/usr/lib64/fast_xml-1.1.3/include"
FXML_EBIN="/usr/lib64/fast_xml-1.1.3/ebin"

erlc -pa $FXML_EBIN -pa $LAGER_EBIN -pa $EJBR_EBIN -DNO_EXT_LIB -DLAGER -I $EJBR_PATH -I $FXML_PATH -o ebin/ src/*
