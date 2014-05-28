#!/bin/bash

TEACHER_PIN="/home/andse541/avdark/pin"
COURSE_DIR=${COURSE_DIR:="${HOME}/avdark"}

AVDARK_PIN_VERSION=2.12-53271-gcc.4.4.7-ia32_intel64-linux

PIN_VERSION_DIR=${COURSE_DIR}/pin-${AVDARK_PIN_VERSION}
PIN_CURRENT=${COURSE_DIR}/pin

PIN_URL="http://software.intel.com/sites/landingpage/pintool/downloads/pin-${AVDARK_PIN_VERSION}.tar.gz"

SRC_URL='http://user.it.uu.se/~andse541/teaching/avdark/2012/lab1.tar.gz'

LAB_NAME="lab1"
LAB_DIR="${COURSE_DIR}/${LAB_NAME}"

if [ -e "$LAB_DIR" ]; then
    echo "The lab directory ($LAB_DIR) already exist."
else

    # Setup course directory
    echo "Creating ${LAB_DIR} directory ${LAB_DIR}"
    mkdir -p "${LAB_DIR}"

    cd "${LAB_DIR}"

    echo -n "Downloading lab source files..."
    wget -O "${COURSE_DIR}"/lab1.tar.gz $SRC_URL
    echo " done."

    echo "Extracting lab source files"
    tar --strip-components 1 -zxf ../lab1.tar.gz

fi

cd "${COURSE_DIR}"
if [ -e ${PIN_CURRENT} ]; then
    echo "There is already a PIN version installed in the course directory"
else

    if [ -e ${TEACHER_PIN} ]; then
	echo "Setting up a symlink to the current PIN version..."
	ln -s ${TEACHER_PIN} $PIN_CURRENT
    else
	echo -n "Downloading pin tool..."
	wget -O "${COURSE_DIR}"/"pin-${AVDARK_PIN_VERSION}.tar.gz" "${PIN_URL}"
	echo " done."

	echo "Decompressing PIN..."
	tar zxf ${COURSE_DIR}/pin-${AVDARK_PIN_VERSION}.tar.gz

	echo "Setting up a symlink to the current PIN version..."
	ln -s $PIN_VERSION_DIR $PIN_CURRENT
    fi

    echo "Done. Pin has been installed in: $PIN_CURRENT"
fi

