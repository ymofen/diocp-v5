#include <stdio.h>
#include <stdlib.h>
#include <winsock.h>




int main()
{

    char ouBuf[1024];
    char xBuf[2];
    memset(ouBuf, 0, 1024);

    WORD x = 0xD10;
    VarToHexBytes(&x, 2, ouBuf, " ", 1);
    printf("0xD10 大端法:%s\r\n", ouBuf);

    memset(ouBuf, 0, 1024);
    memset(xBuf, 0, 2);
    swap16(&x, xBuf);
    VarToHexBytes(xBuf, 2, ouBuf, " ", 1);

    printf("0xD10 小端法:%s\r\n", ouBuf);


    WORD w;
    swap16(xBuf, &w);
    printf("xBuf小端法还原数据:%d\r\n", w);

    printf("sizeof(CHAR):%d\r\n", sizeof(CHAR));



    return 0;



    int ret;
    WinWSAStartup();
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
