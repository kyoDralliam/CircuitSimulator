#include "stdio.h"

int main(int argc, char ** argv)
{
  char read[10000];
  int n, i, b;
  while (!feof(stdin))
    {
      scanf("%9999s", read);
      if (!feof(stdin) && read[0] == '1')
        {
          b = 1;
          n = 0;
          for (i = 1; read[i] == '0' || read[i] == '1'; i++)
            {
              if (read[i] == '1') n |= b;
              b <<= 1;
            }
          printf("%d\n", n);
        }
    }
  return 0;
}
