/* The 3n+1 problem */
#include<stdio.h>


/* Given a number, return the next number in the sequence */

int formula (int x)
 { return ( ((x % 2) == 0) ? (x / 2) : ((3*x)+1) );
 }

/* Find the length of a sequence starting at "x" */

int onecycle(int x,int count)
 { int next;
   next = formula(x);
   while (next != 1)
          { next = formula(next);
            count = count + 1;
	      }
   return count+1;
}


/* do many cycles, 1 for each number between "i" and "j"
   Assume "big" is the longest cycle found so far */

int manycycles (int i, int j, int big)
 { int candidate;
   while ( i <= j )
     { candidate = onecycle(i,1);
       if (candidate > big) big = candidate;
       i = i+1;
     }
   return big;
 }


/*  find the longest cycle between "n" and "m" and print the result */
void oneline (int n, int m)
 { int i; int j; int big;
   if (n <= m) { i = n; j = m; }    /* make sure i <= j */
   else        { i = m; j = n; };
   big = manycycles(i,j,1);
   printf("%ld %ld %ld\n",n,m,big);
}

int main ()
{ int n; int m;
  while ( scanf("%ld %ld",&n, &m) != EOF )
        oneline(n,m);
  return 0;
}


