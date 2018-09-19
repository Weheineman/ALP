#include <stdio.h>
#include <stdlib.h>

int comparar(const void *arg1, const void *arg2)
{
 if(*(int *)arg1 > *(int *)arg2) return -1;
   else if(*(int *)arg1 < *(int *)arg2) return 1;
   else return 0;
}

int main()
{
    
    long int n,g,ganados,j,empatados;
    int i,s,r;
    int arr[100001];
    while(scanf("%d",&n) ==1){
        scanf("%d",&g);        
        ganados=0;
        empatados=0;
        for(i=0; i<n; i++){
            scanf("%d",&s); scanf("%d",&r);       
            arr[i] = s-r;
            if (s-r>0) ganados++;        
            if (s-r==0) empatados++;        
            }*/
         qsort(arr, n, sizeof(arr[0]), comparar);
        

        

        j=ganados;
            while(j<n && g>0){
                       
                if(1-arr[j] <= g){
                    if(arr[j]==0){ empatados--;}
                    ganados++;
                    
                    g=g-(1-arr[j]);                
                }else if((-1)*arr[j] == g){
                    
                    empatados++;
              

                    g=0;                
                    break;               
                }
                    
        j++;            
        }
        
        printf("%ld\n",3*ganados+empatados);
    
     }
        
    return 0;

}
