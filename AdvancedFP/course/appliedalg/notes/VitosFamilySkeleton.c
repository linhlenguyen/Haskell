#include<stdio.h>

int main()
{ int numCases = 0;
  int numberOfRelatives = 0;
  int relArray[501];
  int i = 0;
  int hi = 0;
  int lo = 30000;
  int ans;

  scanf("%d", &numCases);
  while(numCases--)
    { hi = 0;
	  lo = 30000;
	  numberOfRelatives = 0;
	  scanf("%d",&numberOfRelatives);
	  for(i = 0; i< numberOfRelatives; i++)
		 scanf("%d", &relArray[i]);
      for(i = 0; i< numberOfRelatives; i++){
		 if(hi < relArray[i]) hi = relArray[i];
		 if(lo > relArray[i]) lo = relArray[i]; }

    }
  return 0;
}

