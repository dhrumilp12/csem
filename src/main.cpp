#include <iostream>
#include <cstring>

extern "C" {
    #include "cc.h"
    #include "scan.h"
    #include "semutil.h"
    #include "sem.h"
    #include "sym.h"
    #include "parse.h"
}


/* 
 * main - read a program, and parse it
 */
int
main (int argc, char **argv)
{
    init_IR();
    init_scanner(); 
    parse(); 
    emit_IR();

    exit(0);
}
