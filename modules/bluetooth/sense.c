#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

pid_t childpid = -1;

void usage(char *progname) {
   fprintf(stderr, "Usage: %s [-h] [-n time] [-s signal] <command>\n", progname);
}

static void stirb(int bla) {
   exit(0);
}

static void kindermord(int bla) {
   int status;

   if (childpid == -1) {
      /* no way this could happen */
      fprintf(stderr, "Child not launched!\n");
      exit(1);
   }

   if (wait(&status) < 0) {
      perror("wait");
      exit(1);
   }

   exit(0);
}

#define NSIGS 20
static char *signames[NSIGS] = {
  "hup", "int", "quit", "ill", "trap", "abrt", "emt", 
  "fpe", "kill", "bus", "segv", "sys", "pipe", "alrm",
  "term", "urg", "stop", "tstp", "cont", "chld", 
};

int signame_to_signum(char *sig)
{
        int n;

        if (!strncasecmp(sig, "sig", (size_t)3))
                sig += 3;
        for (n = 1; n < NSIGS; n++) {
                if (!strcasecmp(signames[n], sig))
                        return (n);
        }
        return (-1);
}

int sig = SIGTERM;

int main(int argc, char *argv[], char *envp[]) {
  char *cmd = NULL, *ptr;
   int           retval = EXIT_SUCCESS, c, interval = 2,
                 len, i, j, *lens = NULL;

   while ((c = getopt(argc, argv, "+hn:s:")) != -1) {
     switch (c) {
     case 'h':
       retval = EXIT_SUCCESS;
       goto main_exit;
       break;
       
     case 's':
       sig = atoi(optarg);
       if (!sig) {
	 sig = signame_to_signum(optarg);
       }
       break;
       
     case 'n':
       interval = atoi(optarg);
       if (!interval) {
	 retval = EXIT_SUCCESS;
	 goto main_exit;
       }
       break;
       
     default:
       retval = EXIT_FAILURE;
       goto main_exit;
     }
   }
   
   if (argc <= optind) {
     usage(argv[0]);
     retval = EXIT_FAILURE;
     goto main_exit;
   }
   
   signal(SIGINT,  stirb);
   signal(SIGTERM, stirb);
   signal(SIGHUP,  stirb);
   signal(SIGCHLD, kindermord);
   
   if ((childpid = fork()) == 0) {
     if (execve(argv[optind], argv+optind, envp) < 0) { 
      perror(argv[optind]);
       exit(1);
     }
   } else {
     sleep(interval);
     kill(childpid, sig);
     waitpid(childpid, NULL, 0);
   }
   
 main_exit:
   if (cmd)
     free(cmd);
   if (lens)
     free(lens);
   
   return retval;
}
