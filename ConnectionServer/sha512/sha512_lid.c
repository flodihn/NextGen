/* port_driver.c */

#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

unsigned char* sha512(char *text);

typedef struct {
    ErlDrvPort port;
} sha512_data;

static ErlDrvData sha512_drv_start(ErlDrvPort port, char *buff)
{
    sha512_data* d = (sha512_data*)driver_alloc(sizeof(sha512_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void sha512_drv_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

static void sha512_drv_output(ErlDrvData handle, char *arg, int arglen)
{
    sha512_data* d = (sha512_data*)handle;
    unsigned char* result = sha512(arg);
    if(strlen(result) == 0) {
        char error[] = "error";
        driver_output(d->port, error, strlen(error));
        return;
    }
    /* This depends on a sha512 digest to be 64 bytes long */
    driver_output(d->port, result, 64);
}

ErlDrvEntry sha512_driver_entry = {
    NULL,                       /* F_PTR init, N/A */
    sha512_drv_start,          /* L_PTR start, called when port is opened */
    sha512_drv_stop,           /* F_PTR stop, called when port is closed */
    sha512_drv_output,         /* F_PTR output, called when erlang has sent */
    NULL,                       /* F_PTR ready_input, called when input descriptor ready */
    NULL,                       /* F_PTR ready_output, called when output descriptor ready */
    "sha512_drv",              /* char *driver_name, the argument to open_port */
    NULL,                       /* F_PTR finish, called when unloaded */
    NULL,                       /* F_PTR control, port_command callback */
    NULL,                       /* F_PTR timeout, reserved */
    NULL                        /* F_PTR outputv, reserved */
};

DRIVER_INIT(sha512_drv) /* must match name in driver_entry */
{
    return &sha512_driver_entry;
}


