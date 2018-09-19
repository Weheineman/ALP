#include <stdio.h>
#include <stdlib.h>

int comparar(const void *arg1, const void *arg2)
{
 if(*(char *)arg1 < *(char *)arg2) return -1;
   else if(*(char *)arg1 > *(char *)arg2) return 1;
   else return 0;
}

int main()
{
    
    int n,b,i;
    char arr[100]={0};
    char t[30]={0};	

    scanf("%d",&n); 

    for(i=0;i<n;i++){            
        scanf("%s",t);
        arr[i]=t[0];
    }
        
    qsort(arr, n, sizeof(arr[0]), comparar);
    b=0;
    for(i=0;i<n;i++){                
        if(arr[i]==65+b) b++;        
    }
        
    printf("%d\n",b);

return 0;
}
