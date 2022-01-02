#include <stdio.h>
#include <string.h>

#include <stdio.h>
#include <unistd.h>
#include <sys/ioctl.h>


void c_aufruf(const char* ptr)
{
  printf("ptr = >%s<\nlength = %d\n", ptr, (int) strlen(ptr));
}


//--------------------------------------------------------------------------------
// #include <stdio.h>
int puts(const char *txt);


// Debug
void print_utf8_c(char* puf)
{
  int idx = 0;
  int len = strlen(puf);

  do {
    if (puf[idx] & 0x80) 
      printf("!");
    printf("%02x ", puf[idx] & 0xFF);
    idx++;
  } while (idx < len);

  printf("\nC-Seite ausgegeben; \"");
  for (idx = 0; idx < len; idx++)
    printf("%c", puf[idx]);
  printf("\", Länge = %d\n",len);
}

