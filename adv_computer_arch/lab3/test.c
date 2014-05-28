#include <stdio.h>
#include <stdlib.h>

int* changeArr(int init_arr[],int i_len,int* res_len){

    *res_len=0;
    for(int i=0;i<i_len-1;i+=2){
        *res_len+=init_arr[i];
    }

    int* res_arr=(int*)malloc(*res_len*sizeof(int));
    int r_ind=0;
    for(int i=0;i<i_len-1;i+=2){
        for(int j=0;j<init_arr[i];++j){
            res_arr[r_ind]=init_arr[i+1];
            r_ind+=1;
        }
    }
    return res_arr;

}

int max_speed_limit(int scell,int ecell,int* arr,int a_len){
    int sl=1000000;
    int st=scell,en=ecell;
    if(en>a_len) en=a_len;
    for(int i=st;i<en;++i){
        if(sl>arr[i]) sl=arr[i];
    }
    return sl;
}

int* sp_arr(int sp,int* arr_len){
    *arr_len=sp/10;
    int* arr=(int*)malloc(*arr_len*sizeof(int));
    for(int i=0;i<*arr_len;++i){
        arr[i]=(sp-i*10);
    }
    return arr;
}

int* sp_two(int max_s,int min_s,int*arr_len){
    *arr_len=(max_s+min_s)/10;
    int* arr=(int*)malloc(*arr_len*sizeof(int));
    for(int i=0;i<max_s/10;++i){
        arr[i]=(max_s-i*10);
    }
    for(int i=max_s/10;i<*arr_len;++i){
        arr[i]=-1*(min_s-(i-max_s/10)*10);
    }
    return arr;

}

int mem[10000];

int rally(int* sparr,int splen,int* road,int rdlen){


    for(int i=1;i<rdlen;++i){
        int cur_cell=0;
        int cur_speed=0;
        int min_moves=10000;
        printf("%d --\n",i);
        while(cur_cell<=i){
            int temp_acs=0;
            for(int j=0;j<splen;++j){
                if(cur_speed+sparr[j]<=0) continue;
                int scell=cur_cell+1;
                int ecell=cur_cell+(cur_speed+sparr[j])/10;

                int msp=max_speed_limit(scell,ecell,road,rdlen);
                if(msp<(cur_speed+sparr[j])) continue;

                int curr_moves=1+mem[cur_cell];
                mem[ecell]=curr_moves;
//                printf("%d  %d ee %d %d \n",i,j,cur_speed,cur_cell);
                if(min_moves>=curr_moves) {
                    min_moves=curr_moves;
                    cur_cell=ecell;
                    mem[ecell]=curr_moves;
                    temp_acs=sparr[j];
                }
            }
            cur_speed+=temp_acs;
            cur_cell=cur_cell+1+(cur_speed)/10;
            printf("%d\n",cur_cell);
            mem[cur_cell]=min_moves;

        }
    }


//    mem[cc]=min_moves;

    return mem[65];
}


int main(){
    for(int i=0;i<10000;++i){
        mem[i]=0;
    }
    int arr[]={10,100,5,20,3,40,6,100,8,50,7,20,5,60,1,100,6,120,3,50,3,10,5,30,10,100,5,70,3,40,6,100,8,50,7,20,5,60,1,100,6,120,3,50,3,10,5,30},arr_len=24;
    int min_sp=200,max_sp=200;


    int* narr,nlen;
    narr=sp_two(max_sp,min_sp,&nlen);
    int *road,road_len;
    road=changeArr(arr,arr_len,&road_len);
    int moves=rally(narr,nlen,road,road_len);
    printf("%d\n",moves);
    return 1;
}

/*
int rally(int* sparr,int splen,int* road,int rdlen,int cs,int cc,int moves){

    if(cc>rdlen){
//        mem[cc]=moves;
        return moves;
    }
    else if(mem[cc]!=-1) return mem[cc];
    int min_moves=10000;
//    printf("%d  - -  %d \n",cs,cc);
    for(int i=0;i<splen;++i){
        if(cs+sparr[i]<=0) continue;

        int scell=cc+1;
        int ecell=cc+1+(cs+sparr[i])/10;

        int msp=max_speed_limit(scell,ecell,road,rdlen);
        if(msp<(cs+sparr[i])) continue;

        int curr_moves=rally(sparr,splen,road,rdlen,cs+sparr[i],ecell,moves+1);
        if(min_moves>curr_moves) min_moves=curr_moves;
    }
    mem[cc]=min_moves;

    return min_moves;
}
*/

