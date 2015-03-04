#ifndef UTILS_BYTETOOLS_H_INCLUDED
#define UTILS_BYTETOOLS_H_INCLUDED

long VerifyData(unsigned char* DataBuff, int DataSize);
void swap32(void * inBuf, void * outBuf);
void swap16(void * inBuf, unsigned char * outBuf);
void BinToHex(unsigned char b, char * hex);
int VarToHexBytes(void * inBuf, int len, char * outHexBuf, char * delimiter, short delimiterlen);

#endif // UTILS_BYTETOOLS_H_INCLUDED
