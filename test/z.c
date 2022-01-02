#include <unistd.h>
#include <stdio.h>

void print_utf8(char puf[], int len);


main()
{
  char puf[1024];
  int  len;

  len = read(0, puf, 1024);
  print_utf8(puf, len);
}



void print_utf8(char puf[], int len) 
{
  int idx = 0;

  do {
    if (puf[idx] & 0x80) 
      printf("!");
    printf("%02x ", puf[idx] & 0xFF);
    idx++;
  } while (idx < len);

  printf("\n\"");
  for (idx = 0; idx < len; idx++)
    printf("%c", puf[idx]);
  printf("\"\nLänge = %d\n",len);
}
