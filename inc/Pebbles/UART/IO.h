#ifndef _UART_IO_H_
#define _UART_IO_H_

#ifdef __cplusplus
extern "C" {
#endif

int putchar(int c);
int getchar();
int puts(const char* s);
int puthex(unsigned x);
int printf(const char* fmt, ...);
void assert(int cond, const char* msg);

#ifdef __cplusplus
}
#endif

#endif
