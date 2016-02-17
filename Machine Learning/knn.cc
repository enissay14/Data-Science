#include<iostream>
#include<ctime>
#include<cstdlib>
using namespace std;
    
const int max_size=1000;
double spacesize, k, temp, res;
double searchspace[max_size];
double results[max_size];
int i=0;
int j=0;
int z=0;
int p=0;

void printlist(const double list[], const double listsize);
int main(){
    do{
        cout << "Entrer un nombre entre 0 et  1,000." << endl;
        cin >> spacesize;
        if(spacesize<=0){
            cout << "Votre noombre doit etre dans l'intervalle.  Re-essayez." << endl;
        }
        
        }while(spacesize<=0);
        
    do{
        cout << "Entrez le nombre de départ." << endl;
        cout << "(Un nombre dans votre interval)" <<  endl;
        cin >> k;
        if(k<=0 || k>spacesize){
            cout << "Votre nombre n'est pas dans l'interval. Re-essayez." << endl;
            }
        }while(k<=0 || k>spacesize);

    do{
        cout << "Entrez le nombre des plus proches que vous voulez." << endl;
        cin >> res;
        if(res<=0 || res>spacesize)
        {
            cout << "Votre nombre n'est pas dans l'interval. Re-essayez" << endl;
        }
        }while(res<=0 || res>spacesize);

    srand(time(NULL));
    for(i=0; i<spacesize; i++){
        searchspace[i]=rand()%5000;
    }	 
    for(int p=0; p<spacesize-1; p++){
    for(int j=0; j<spacesize-1; j++){
        if(searchspace[j]>searchspace[j+1]){
        temp=searchspace[j];
        searchspace[j]=searchspace[j+1];
        searchspace[j+1]=temp;
        }
        }
    }
    
    printlist(searchspace, spacesize);
    if(k==spacesize){
            results[max_size];
        }
    return 0;
}
    


void printlist(const double list[], const double listsize){
    for(i=0;i<listsize;i++) {
        cout << list[i] << endl;
    }
}