set prompt
##   DUEL - A Very High Level Debugging Langauge.
##   Public domain code
##   Written by Michael Golan mg@cs.princeton.edu
##$Header: /tmp_mnt/n/fs/grad2/mg/duel/tsuite/RCS/tsuite.gdb,v 1.6 93/03/19 15:55:25 mg Exp $
##
## test suite for duel, can be used with duelself or gdb
## 
file tsuite
b 60
b 61
r
## check constants 
dl (1..5)*(2,(double) 2/3)
## declare array x, set it, search it
dl int x[100] ; 
dl x[0..99]= -1 ;
dl x[i:=20..40]=2+i*i ;
dl x[20..23,38..40]
dl x[..100] >=? 33*33 <=? 35*35 
dl x[..100]=> ((_>=33*33) & (_<= 35*35)) ==? 1
dl (*(x+(7..9)))++
dl y:= &x[7] ;
dl y[0..2]
dl x+7 == y
dl (x[..99]>?0)@(_>100)
dl printf("%d\n",1..10);
dl printf("x is: "); printf("%d, ",x[0..99]>? 0); printf("\n");fflush(out);
dl int j ; for(j=0 ; j<100 ; j++) if(x[j]>37*37) printf("x[%d]=%d\n",j,x[j]);fflush(out);
dl printf("%d\n",1..5);
## check void type
dl void *p
dl p = &p 
dl p,*p
## errors
dl 1e+++3
dl i=4 ;
dl x=6 ;
dl x++ ;
dl --i ;
## cleanup
dl alias
dl clear
## access some variables
dl s
dl s[4..8]
dl s[6..]@0
dl l:=#/s[0..]@0
dl s[l-2..l]
dl gint 
dl gint++
dl gint++
dl --gint 
dl gint 
dl main.s 
dl ++main.s
dl main ==? main
dl main == main
dl frames_no
dl frame(0).s
dl frame(0).(gint+0)
dl sizeof(gint)
dl sizeof(T uint)
dl sizeof(T uint*)
dl sizeof(T emp)
dl sizeof(emp)
dl sizeof(emp[0])
dl (uint)-1
dl (uint*)(gint=gint-9)
dl *(uint*)&gint
dl T uint myuint ; myuint = -1 
## some errors 
dl frame(0).gint
dl frame(0).(gint)
dl frame(0).ha
dl sizeof(uint)
dl sizeof gint
dl main == printf
dl main+1
dl main > printf
dl printf+3
dl T int x
dl uint y
dl T uint z = 5
## finally, do some serious checks using the bugs in the programs
dl emp[4]
dl emp[k:=..100].a !=? k
dl (emp[k:=..100].a !=? k)[[0]] ; emp[{k}]
dl emp[k:=..100].if(a!=k) _
dl emp[k:=..100].b !=? k*k 
dl emp[k:=..100].if(b!=k*k) _,{k}*{k}
dl emp[k:=..100]=>if(_.b!=k*k) _,{k}*{k}
dl char s[80];
dl ..100 => (sprintf(s,"emp%3.3d",_) ; emp[_].name[0..]@0#j !=? s[j])
dl ..100 => (sprintf(s,"emp%3.3d",_) ; mystrcmp(emp[_].name,s)!=?0)
dl ..100 => (sprintf(s,"emp%3.3d",_) ; emp[_].name=>if(mystrcmp(_,s))_)
## now, lets try some pointers and --> stuff!
dl root-->left[[..8]]->code
dl root-->(left,right)[[..30]]->code
dl #/root-->(left,right)
dl root->(left,right) => #/_-->(left,right)
dl root->(left->left,left->right,right->left,right->right)=> #/_-->(left,right)
## this cause very long symbolic vals, core dump/bad results before duel 1.10.3
dl #/(root+0000000000000000000000000000000000000000000000000000)-->(left,right)
## compute min, max, check for dups, no-show
dl root-->left[[#/root-->left-1]]->code
dl root-->right[[#/root-->right-1]]->code
dl int codes[11000] ; codes[..11000]=0 ; codes[root-->(left,right)->code]++ ; 
dl codes[..11000] >? 1 
dl (codes[..11000] ==? 0 )[[..10]]
dl (..11000 => if(!codes[_]) _)[[..10]]
## check consistency
dl root-->(left,right)->((left!=?0)->code >=? code, (right!=?0)->code <=? code)
dl root-->(left,right)->((left!=?0)->code>=code, (right!=?0)->code<=code)==?1
dl (1000..=>if(&&/( 2,3.._-1 =>__%_)) _)[[..10]]
## enums
dl ELA
dl (T us) 1
dl us=1 ; us
dl MIKI..ELA
## more errors
dl emp[0]=us
dl emp=us
dl us=emp
## the end

