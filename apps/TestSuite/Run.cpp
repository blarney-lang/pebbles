#include <stdio.h>
#include <HostLink.h>

int main(int argc, char** argv)
{
  HostLink hostLink;

  for (int i = 1; i < argc; i++) {
    char codeFile[1024];
    char dataFile[1024];
    int n = strlen(argv[i]);
    if (argv[i][n-1] == 'S' && argv[i][n-2] == '.') argv[i][n-2] = '\0';
    printf("%-16s", argv[i]);
    snprintf(codeFile, sizeof(codeFile), "%s.code.v", argv[i]);
    snprintf(dataFile, sizeof(dataFile), "%s.data.v", argv[i]);
    hostLink.boot(codeFile, dataFile);
    uint8_t result = hostLink.uart->getByte();
    if (result == 1) printf("pass\n");
    else printf("FAIL(%d)\n", result >> 1);
  }

  return 0;
}
