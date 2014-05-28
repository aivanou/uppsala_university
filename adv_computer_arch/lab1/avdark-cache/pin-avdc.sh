#!/bin/bash
#
# Wrapper script to execute PIN with the AvDark cache simulator
# module.
#
# Course: Advanced Computer Architecture, Uppsala University
# Course Part: Lab assignment 1
#
# Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
#
# $Id: pin-avdc.sh 79 2012-09-13 06:50:07Z andse541 $
#

COURSE_DIR=${COURSE_DIR:-"${HOME}/avdark"}
PIN_HOME=${PIN_HOME:-${COURSE_DIR}/pin}

PIN=${PIN_HOME}/pin

TOOL="${COURSE_DIR}/lab1/avdark-cache/obj-intel64/avdc.so"

if [ `basename $PWD` == "avdark-cache" ] && \
    [ -e "$PWD/obj-intel64/avdc.so" ]; then
    TOOL="$PWD/obj-intel64/avdc.so"
fi

if [ ! -e "${TOOL}" ]; then
    echo "Can't find the PIN module for the AvDark cache simulator." 1>&2
    echo "Make sure that you built the PIN module prior to using this script." 1>&2
    exit 1
fi

if [ ! -x "${PIN}" ]; then
    echo "Can't find PIN or the pin binary isn't executable" 1>&2
    exit 1
fi

$PIN -t "${TOOL}" $*
