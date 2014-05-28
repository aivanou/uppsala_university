TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += main.c \
    ../lab1/MPIbasic/pingpong.c \
    ../lab1/MPIbasic/pi.c \
    ../lab1/MPIbasic/onetoall.c \
    ../lab1/MPIbasic/hello.c \
    ../lab1/MPIbasic/exchange.c \
    ../lab1/MPIbasic/datatypes.c \
    ../lab1/MPIbasic/communicators.c

OTHER_FILES += \
    ../lab1/MPIbasic/pingpong.m \
    ../lab1/MPIbasic/Makefile \
    ../lab1/MPIbasic/a.out

