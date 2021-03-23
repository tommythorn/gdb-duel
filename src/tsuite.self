##   DUEL - A Very High Level Debugging Langauge.
##   Public domain code
##   Written by Michael Golan mg@cs.princeton.edu
## $Header: /tmp_mnt/n/fs/grad2/mg/duel/tsuite/RCS/tsuite.self,v 1.4 93/03/12 09:06:27 mg Exp $

##
## test suite for duel, can be used with duelself or gdb
##

## check constants
(1..5)*(2,(double) 2/3)

## declare array x, set it, search it

int x[100] ;
x[0..99]= -1 ;
x[i:=20..40]=2+i*i ;
x[20..23,38..40]
x[..100] >=? 33*33 <=? 35*35
x[..100]=> ((_>=33*33) & (_<= 35*35)) ==? 1
x
x+5
*(x+7..9)++
*(x+(7..9))++
(*(x+(7..9)))++
y:= &x[7] ;
y[0..2]
x+7 == y
(x[..99]>?0)@(_>500)

printf("x is: "); printf("%d, ",x[0..99]>? 0); printf("\n");
int j ; for(j=0 ; j<100 ; j++) if(x[j]>37*37) printf("x[%d]=%d\n",j,x[j]);

printf("%d, ",1..5); printf("\n");

## check void type
void *p
p = &p
p,*p

## errors

1e++4
& &x
i=4 ;
x=6 ;
x++ ;
--i ;

## cleanup

alias
clear

## access some variables
s
s[3..7]
s[5..]@0
l:=#/s[0..]@0
s[l-2..l]

gint
gint++
gint++
--gint
gint
main.s
main.s++
main.s++

main
printf
main ==? main
main == main

frames_no
frame(0).s

## gint is a global, not local fro main in frame 0
frame(0).gint
## but when we eval (expr) under frame 0 scope, we find it ok.
frame(0).(gint)

T uint myuint ; myuint = -1

sizeof(int)
sizeof(long)
sizeof(int *)
sizeof(signed char*)
sizeof()
sizeof(gint)
sizeof gint
sizeof gint+1
## some errors

main == main
main == printf

main+1
malloc[4]
printf+3

T int x
int x
uint y
T uint y = 5
T uint y

## the end
