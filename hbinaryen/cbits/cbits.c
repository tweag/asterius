#include <binaryen-c.h>

BinaryenExpressionRef BinaryenConstInt32(BinaryenModuleRef module, int32_t x) {
  return BinaryenConst(module, BinaryenLiteralInt32(x));
}

BinaryenExpressionRef BinaryenConstInt64(BinaryenModuleRef module, int64_t x) {
  return BinaryenConst(module, BinaryenLiteralInt64(x));
}

BinaryenExpressionRef BinaryenConstFloat32(BinaryenModuleRef module, float x) {
  return BinaryenConst(module, BinaryenLiteralFloat32(x));
}

BinaryenExpressionRef BinaryenConstFloat64(BinaryenModuleRef module, double x) {
  return BinaryenConst(module, BinaryenLiteralFloat64(x));
}

BinaryenExpressionRef BinaryenConstFloat32Bits(BinaryenModuleRef module,
                                               int32_t x) {
  return BinaryenConst(module, BinaryenLiteralFloat32Bits(x));
}

BinaryenExpressionRef BinaryenConstFloat64Bits(BinaryenModuleRef module,
                                               int64_t x) {
  return BinaryenConst(module, BinaryenLiteralFloat64Bits(x));
}

void BinaryenModuleWriteWithSourceMapMut(BinaryenModuleRef module,
                                         const char* url, char* output,
                                         size_t outputSize, char* sourceMap,
                                         size_t sourceMapSize,
                                         size_t* outputBytes,
                                         size_t* sourceMapBytes) {
  struct BinaryenBufferSizes r = BinaryenModuleWriteWithSourceMap(
      module, url, output, outputSize, sourceMap, sourceMapSize);
  *outputBytes = r.outputBytes;
  *sourceMapBytes = r.sourceMapBytes;
}

void BinaryenModuleAllocateAndWriteMut(BinaryenModuleRef module,
                                       const char* sourceMapUrl, void** binary,
                                       size_t* binaryBytes, char** sourceMap) {
  BinaryenModuleAllocateAndWriteResult r =
      BinaryenModuleAllocateAndWrite(module, sourceMapUrl);
  *binary = r.binary;
  *binaryBytes = r.binaryBytes;
  *sourceMap = r.sourceMap;
}
