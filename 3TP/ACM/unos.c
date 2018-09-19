#include <math.h> 
#include <stdio.h> 

unsigned long long f(unsigned long long n){

    unsigned long long ln,total=0,p,cont=0;

    while (n>0){
        ln=log2(n);
        p=(((unsigned long long)1)<<(ln-1))*ln +1;       
        total+= p+(((unsigned long long)1)<<ln)*cont;
        cont++;
        n-=(((unsigned long long)1)<<ln);
    }

    return total;

}



int main()
{
    unsigned long long a,b;
    while(scanf("%llu",&a) ==1){
        scanf("%llu",&b);
        printf("%llu\n", f(b)- f(a-1));
    }
}
