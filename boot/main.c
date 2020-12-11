// Boot loader
#include <pebbles.h>
#include <pebbles/io.h>

int main()
{
  volatile int x;
  x = 0xdeadbeef;
  emit(x);
  return 0;
}
