/*   DUEL - A Very High Level Debugging Langauge.  */
/*   Public domain code                            */
/*   Written by Michael Golan mg@cs.princeton.edu  */
/*$Header: /tmp_mnt/n/fs/grad2/mg/duel/tsuite/RCS/tsuite.c,v 1.6 93/03/19 17:32:54 mg Exp $*/

/* this is a test program to be used with tsuite.gdb */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int gint ;
typedef unsigned int uint ;

struct { int a,b ; char *name ; } emp[100] ;
enum { MIKI, DAVE, ELA } us ;
struct snode { int code ; struct snode *left,*right ; } ;
typedef struct snode tnode ;

/* amazing, but DEC compiled strcmp such that gdb thinks it returns void */
int mystrcmp(s,t) char *s; char *t; { return strcmp(s,t); }

void main()
{
     char *s="main string" ;
     FILE *out=stdout ; /* stdout is not a variable on some machines */
                        /* will use fflush(out) from debugger */
     int i ;
     tnode *root,*p,**q ;

      for(i=0 ; i<100 ; i++) {
        emp[i].a=i ; emp[i].b=i*i ;
        emp[i].name=(char*) malloc(10);
        sprintf(emp[i].name,"emp%3.3d",i);
      }
      emp[53].a=76 ;    /* bug */
      emp[36].b-- ;     /* bug */
      emp[74].name[3]='5' ;     /* another bug */

     root=(tnode*) malloc(sizeof(tnode));
     root->left=root->right=0 ;
     root->code=5000 ;
     for(i=1 ; i<10000 ; i++) { /* insert elements into tree */
         int code=((i*997*1013)>>10)%11000 ; /* "fixed" "random" generaor */
         if(code<0) code= -code ;
         p=root ;
         while(p) {
             if(p->code==code) break ;
             if(p->code<code) q= &p->right, p=p->right ;
             else q= &p->left, p=p->left ;
         }
         if(p==0) {
             p=(tnode*) malloc(sizeof(tnode));
             p->code=code ;
             if(i>8680) p->code+=(i<9210)? 1:-1 ;       /* bug */
             p->left=p->right=0 ;
             *q=p ;
         }
     }

    printf("trivial tsuite program\n");
    printf("trivial tsuite program\n");
}
