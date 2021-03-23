/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/RCS/output2.c,v 1.1 93/03/12 04:26:35 mg Exp $*/

/* This module supports i/o for duel. Normally output goes to stdout,
 * but it could be piped out instead.
 * Still alpha version.
 * HOW: if file duel.pipe exists, it is opened with stdin/out piped
 *      here.  results of expressions evaluations are sent to the pipe
 *             with $$$START $$$END $$$ABORT and $$$SYM/$$$VAL. The pipe's
 *             output is passed back here and is printed to stdout until
 *             a $$$DONE is received. The pipe is normally a perl script,
 *             that can processes the output at a symbolic level.
 */

#include <varargs.h>  /* for duel_printf */
#include <sys/types.h> /* for stat() */
#include <sys/stat.h>  /* " */
#include <fcntl.h>     /* for popen impl. */
#include <signal.h>    /* " */

#include "duel.h"

/* output functions:
 * duel_printf    all duel output goes thru this - like printf()
 * duel_flush     used by duel to flush the output.
 * duel_redirectable_output_start(s) - indicate start of output for exp s that
 *                                     can be redirected to a pipe.
 * duel_redirectable_output_end()   - indicate end of that output.
 * duel_redirectable_output_abort() - indicate output was aborted (error,intr)
 *
 * duel_redirectable_output_init()   - called to init at first duel use.
 */

static FILE *duel_outf = stdout ;      /* current output stream */
static FILE *poutf,*pinf ;             /* pipe i/o streams      */

#define PIPE_CMD "./duel.pipe"          /* command to pipe into */

bool bidir_popen(void)
{
        int pout[2],pin[2];  /* pin[0] is my input, pout[1] my output */
        int pid;
        struct stat s ;

        if(stat(PIPE_CMD,&s)!=0 || !(s.st_mode & S_IEXEC)) return FALSE;
        if (pipe(pout) < 0 || pipe(pin) < 0) return FALSE ;

        if ((pid = fork()) == 0) {
            signal(SIGINT, SIG_IGN);
            signal(SIGQUIT, SIG_IGN);
            dup2(pout[0],0); dup2(pin[1],1);
            close(pin[0]);  close(pin[1]); close(pout[0]); close(pout[1]);
            execl(PIPE_CMD, PIPE_CMD, NULL);
            _exit(127);
        }
        close(pout[0]); close(pin[1]);
        if (pid == -1) return FALSE;
        pinf =fdopen(pin[0], "r");
        poutf=fdopen(pout[1],"w");
        if(fcntl(pin[0],F_SETFL,O_NDELAY) < 0) return FALSE ;
        return pinf!=NULL && poutf!=NULL ;
}


PROC duel_printf(va_alist)      /* like printf, but for duel output */
va_dcl
{
  va_list args;
  char *fmt ;
  va_start(args);
  fmt=va_arg(args,char *);
  vfprintf(duel_outf,fmt,args);
}

PROC duel_flush(void)   /* flush out output from duel */
{
   char s[256];
   fflush(duel_outf);
   while(pinf && fgets(s,sizeof(s),pinf))   /* note pinf is non blocking */
        printf("%s",s);
}

LPROC duel_waitpipe(bool abort) /* wait for pipe to finish working */
{
   char s[256];
   if(abort) duel_printf("$$$ABORT\n");         /* write termination mesg */
   else      duel_printf("$$$DONE\n");
   fflush(duel_outf);
   fcntl(fileno(pinf),F_SETFL,0);       /* restore blocking for pipe */
   while(fgets(s,sizeof(s),pinf)!=NULL && strcmp(s,"$$$DONE\n")!=0)
        if(!abort) printf("%s",s);
   fcntl(fileno(pinf),F_SETFL,O_NDELAY);
   if(abort) printf("\n");
}

/* tells us the output is "directable" now. s is the expression
 * being evaluated for this */

PROC duel_redirectable_output_start(char *s)
{
    duel_flush();
    if(poutf) {
        duel_flush();
        duel_outf=poutf ;
        duel_output_pipe_style=1 ;
        duel_printf("$$$START: %s\n",s);
        duel_flush();
    }
}

PROC duel_redirectable_output_end(void)
{
    duel_flush();
    if(poutf && duel_outf == poutf) {
        duel_waitpipe(FALSE);
        duel_outf=stdout ;
        duel_output_pipe_style=0 ;
    }
}

PROC duel_redirectable_output_abort(void)
{
    if(poutf && duel_outf == poutf) {
        duel_waitpipe(TRUE);
        duel_outf=stdout ;
        duel_output_pipe_style=0 ;
    }
    duel_flush();
}

PROC duel_redirectable_output_init(void)
{
        /* open the duel pipe, if available. Verify that duel.pipe
         * as an existing executable, because popen() does not, and can
         * later fail with pipe signal!
         */
         if(bidir_popen()) {
               duel_printf("Duel results directed to pipe \"duel.pipe\"\n");
        }
        else poutf=pinf=NULL ;
}
