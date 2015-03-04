#include <stdio.h>
#include <stdlib.h>
#include <winsock.h>
#include "raw/rawsocket.h"


LONG VerifyData(unsigned char* DataBuff, int DataSize)
{
    LONG Ret = 0;

    int i = 0;

    for (i = 0; i < DataSize; i++)
    {
        Ret += DataBuff[i];
    }

    return Ret;
}

void swap32(void * inBuf, void * outBuf)
{
    unsigned char * tmpOutBuf = (unsigned char *)(outBuf);
    unsigned char * tmpInBuf = (unsigned char *)(inBuf);

    tmpOutBuf[0] = tmpInBuf[3];
    tmpOutBuf[1] = tmpInBuf[2];
    tmpOutBuf[2] = tmpInBuf[1];
    tmpOutBuf[3] = tmpInBuf[0];
}

void swap16(void * inBuf, unsigned char * outBuf)
{
    unsigned char * tmpOutBuf = (unsigned char *)(outBuf);
    unsigned char * tmpInBuf = (unsigned char *)(inBuf);

    tmpOutBuf[0] = tmpInBuf[1];
    tmpOutBuf[1] = tmpInBuf[0];
}

int main()
{
    int ret;
    WINWSAStartup();
    SOCKET s = CreateTcpSocket();

    printf("这是一个纯c写(codeblocks编写)的客户端\n");
    printf("用于和diocp中的StreamCoderSERVER(samples\StreamCoderDEMO\SERVER)进行测试!\n");
    printf("该份代码可以跨平台的,不过尚未进行测试!\n");
    printf("需要运行服务端(samples\StreamCoderDEMO\SERVER),端口为:9983!\n");

    printf("======================================================\n");

    // 连接diocp echo服务器
    ret = ConnectSocket(s, "127.0.0.1", 9983);
    if (ret==SOCKET_ERROR)
    {
        PrintLastError();
        return;
    }
    printf("与服务器建立连接成功!\n");



    char * data;

    // 发送的数据
    data = "0123456789";

    int len;
    len = 10;

    WORD flag=0xD10;
    // 发送标记
    if (SendBuff(s, &flag, sizeof(WORD)) == SOCKET_ERROR)
    {
        PrintLastError();
        return;
    }
    printf("发送标记:%d\n", flag);

    LONG lvVerifyValue;
    lvVerifyValue = VerifyData(data, 10);
    // 发送验证码
    if (SendBuff(s, &lvVerifyValue, sizeof(lvVerifyValue)) == SOCKET_ERROR)
    {
        PrintLastError();
        return;
    }
    printf("发送数据校验码:%d\n", lvVerifyValue);

    unsigned char tempBuff[10];
    swap32(&len, tempBuff);
    // 发送数据长度
    if (SendBuff(s, tempBuff, 4) == SOCKET_ERROR)
    {
        PrintLastError();
        return;
    }
    printf("发送长度数据:%d\n", len);

    // 发送数据
    if (SendBuff(s, data, 10) == SOCKET_ERROR)
    {
        PrintLastError();
        return;
    }

    printf("发送数据:%d\n", len);

    printf("======================================================\n");

    printf("准进行接收数据.....");

    // 开始数据接收

    // Flag
    flag = 0;
    if (RecvBuffer(s, &flag, sizeof(WORD))== SOCKET_ERROR)
    {
        PrintLastError();
        return;
    }
    printf("接受到数据标记:%d\n", flag);

    // 验证码
    lvVerifyValue= 0;
    if (RecvBuffer(s, &lvVerifyValue, sizeof(lvVerifyValue))== SOCKET_ERROR)
    {
        PrintLastError();
        return;
    }
    printf("接受到数据校验码:%d\n", lvVerifyValue);

    int recvLen;
    // 接收数据长度
    if (RecvBuffer(s, tempBuff, sizeof(int))== SOCKET_ERROR)
    {
        PrintLastError();
        return;
    }
    swap32(tempBuff, &recvLen);
    printf("接受到数据长度:%d\n", recvLen);


    unsigned char * recvData = malloc(recvLen);



    // 接收数据
    if (RecvBuffer(s, recvData, recvLen)== SOCKET_ERROR)
    {
        PrintLastError();
        free(recvData);
        return;
    }

    printf("接收到数据:%s", recvData);

    free(recvData);


    return 0;
}
