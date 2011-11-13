#include<cstdio>
#include<vector>
#include<algorithm>
using namespace std;

const int MAX_NB_NOEUDS = 10*1000, MODULE_DATE = 2, NB_ENTREES = 2;
const char REGISTRE = 'R', ENTREE = 'E', NOT = 'N', AND = 'A', OR = 'O', XOR = 'X', SORTIE = 'S';

void parcourir(int idNoeud);
int date;

struct Noeud 
{ 
   char type; // Chaque noeud a un type valant REGISTRE, ENTREE, NOT, AND, OR, XOR ou SORTIE
   bool arg[NB_ENTREES][MODULE_DATE]; // Chaque noeud a au plus 2 entrees
   int iArgCur; //indice modulaire du dernier argument recu (ne varie que pour les portes a deux entrees)
   void ajoute(bool entreeAAjouter) 
   {
      if(type == AND || type == OR || type == XOR)
         iArgCur = (iArgCur+1)%2;
      arg[iArgCur][date%2] = entreeAAjouter; 
   }
   bool sortie()
   {
      switch(type)
      {  
        case REGISTRE: return arg[iArgCur][(date+1)%2];
        case ENTREE:   return true;
        case NOT:      return !arg[iArgCur][date%2];
        case AND:      return arg[iArgCur][date%2] && arg[(iArgCur+1)%2][date%2];
        case OR:       return arg[iArgCur][date%2] || arg[(iArgCur+1)%2][date%2];
        case XOR:      return arg[iArgCur][date%2] ^  arg[(iArgCur+1)%2][date%2];
        case SORTIE:   return arg[iArgCur][date%2];
      }
   }
} noeud[MAX_NB_NOEUDS+1]; 

int nbNoeuds, duree;
vector<int> vois[MAX_NB_NOEUDS+1];
bool parcouru[MAX_NB_NOEUDS+1];
vector<int> triTopo;
vector<int> idSortie;

int main()
{
   scanf("%d", &duree);   //Lecture de la description
   scanf("%d", &nbNoeuds);       
   for(int idNoeud = 1; idNoeud <= nbNoeuds; idNoeud++)
   {
      char typeNoeud[2];
      scanf("%s", typeNoeud);
      noeud[idNoeud].type = typeNoeud[0];
      if(noeud[idNoeud].type == SORTIE)
         idSortie.push_back(idNoeud);
      int nbFils;
      scanf("%d", &nbFils);
      for(int iFils = 0; iFils < nbFils; iFils++)
      {
         int idFils;
         scanf("%d", &idFils);
         vois[idNoeud].push_back(idFils);
      }     
   }

   for(int idNoeud = 1; idNoeud <= nbNoeuds; idNoeud++)   //Tri topologique    
      parcourir(idNoeud);
   reverse(&triTopo[0], &triTopo[nbNoeuds]);


   for(date = 1; date <= duree; date++) // Calcul incremental sur les dates
   {
      for(int iNoeud = 0; iNoeud < nbNoeuds; iNoeud++)
      {
         int idNoeud = triTopo[iNoeud];
         for(int iFils = 0; iFils < (int)vois[idNoeud].size(); iFils++)
            noeud[vois[idNoeud][iFils]].ajoute(noeud[idNoeud].sortie());   
      }
      for(int iSortie = 0; iSortie < (int)idSortie.size(); iSortie++)
         printf("%d ", noeud[idSortie[iSortie]].sortie());
      printf("\n");   
   }
  
   return 0;
}

void parcourir(int idNoeud)
{
   if(parcouru[idNoeud]) 
      return;
   parcouru[idNoeud] = true;  
   for(int iFils = 0; iFils < (int)vois[idNoeud].size(); iFils++)
      if(noeud[vois[idNoeud][iFils]].type != REGISTRE)
         parcourir(vois[idNoeud][iFils]);
   triTopo.push_back(idNoeud);
}





