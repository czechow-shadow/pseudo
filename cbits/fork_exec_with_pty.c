#include <pty.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h> 

#include <HsFFI.h>

#include "fork_exec_with_pty.h"

//char* args[] = {"/bin/bash", "-i", NULL };
char* args[] = {"ls", "-al", NULL };
/* //char* args[] = {"whoami", NULL }; */

/* char buff[1024]; */
/* int packet_mode = 1; */

int fork_exec_with_pty(HsInt* amaster) {
  int pty;
  int r;

  int pid = forkpty(&pty, NULL, NULL, NULL);
  switch (pid) {
  case -1:
    perror("forkpty failed");
    return -1;
  case 0: // child
    r = execvp(args[0], args);
    if (r < 0) perror("execvp failed");
    return r;
  default:
    *amaster = pty;
    /* if (ioctl(pty, TIOCPKT, &packet_mode) == -1) { */
    /*   perror("ioctl failed"); */
    /*   return 3; */
		
    return pid;
  }
}


