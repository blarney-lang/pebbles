// Basic floating point operations (no error support) in C

#include <math.h>
#include <stdint.h>

// Convert between raw bits and floats
inline float _toFloat(uint32_t a) { return *((float*) &a); }
inline uint32_t _fromFloat(float a) { return *((uint32_t*) &a); }

extern "C" {

uint32_t c_FPAddSub(int sel, uint32_t a, uint32_t b) {
  float add = _toFloat(a) + _toFloat(b);
  float sub = _toFloat(a) - _toFloat(b);
  return sel ? _fromFloat(sub) : _fromFloat(add);
}

uint32_t c_FPMul(uint32_t a, uint32_t b) {
  return _fromFloat(_toFloat(a) * _toFloat(b));
}

uint32_t c_FPDiv(uint32_t a, uint32_t b) {
  return _fromFloat(_toFloat(a) / _toFloat(b));
}

uint32_t c_FPMin(uint32_t a, uint32_t b) {
  return _toFloat(a) < _toFloat(b) ? a : b;
}

uint32_t c_FPMax(uint32_t a, uint32_t b) {
  return _toFloat(a) > _toFloat(b) ? a : b;
}

uint32_t c_FPCompareEq(uint32_t a, uint32_t b) {
  return _toFloat(a) == _toFloat(b);
}

uint32_t c_FPCompareLT(uint32_t a, uint32_t b) {
  return _toFloat(a) < _toFloat(b);
}

uint32_t c_FPCompareLTE(uint32_t a, uint32_t b) {
  return _toFloat(a) <= _toFloat(b);
}

uint32_t c_FPSqrt(uint32_t a) {
  return _fromFloat(sqrtf(_toFloat(a)));
}

uint32_t c_FPFromInt(int32_t a) {
  return _fromFloat((float) a);
}

uint32_t c_FPFromUInt(uint32_t a) {
  return _fromFloat((float) a);
}

int32_t c_FPToInt(uint32_t a) {
  return (int32_t) truncf(_toFloat(a));
}

uint32_t c_FPToUInt(uint32_t a) {
  float f = truncf(_toFloat(a));
  return f < 0.0 ? 0 : (uint32_t) f;
}

float c_FPFromInt64(int64_t a) {
  return _fromFloat((float) a);
}

int64_t c_FPToInt64(uint32_t a) {
  return (int64_t) _toFloat(a);
}

}
