void *memcpy(void* dst, const void* src, unsigned n) {
  char *d = (char*) dst;
  const char *s = (const char *) src;
  for (unsigned i = 0; i < n; i++) d[i] = s[i];
  return dst;
}
