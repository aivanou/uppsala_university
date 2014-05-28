/* 
 * File:   file_work.h
 * Author: tierex
 *
 * Created on March 19, 2014, 12:52 PM
 */

#ifndef FILE_WORK_H
#define	FILE_WORK_H

#ifdef	__cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

    int readLine(FILE *file, char** line);
    void read(char* filename);

    void transform_data(char*, char*);

#define DATA_LENGTH 8000


#ifdef	__cplusplus
}
#endif

#endif	/* FILE_WORK_H */

