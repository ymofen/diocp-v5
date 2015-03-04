#ifndef RAWSOCKET_H_INCLUDED
#define RAWSOCKET_H_INCLUDED



#ifdef WIN32
#include <winsock.h>
#include <Windows.h>
typedef int				socklen_t;
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/select.h>
#include <arpa/inet.h>
typedef int				SOCKET;

//#pragma region define win32 const variable in linux
#define INVALID_SOCKET	-1
#define SOCKET_ERROR	-1
//#pragma endregion
#endif


#ifdef WIN32
// 进行初始化
int WINWSAStartup()
{
    WORD wVersion;
    wVersion=MAKEWORD(2, 2);
    WSADATA wsaData;
    return WSAStartup(wVersion, &wsaData);
}
#endif

// 创建TCP的套接字句柄
SOCKET CreateTcpSocket()
{
    return socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
}

// 建立连接
int ConnectSocket(SOCKET s, const char * host, u_short port)
{
    struct sockaddr_in sa;
    sa.sin_family = AF_INET;
    sa.sin_addr.s_addr = inet_addr(host);
    sa.sin_port = htons(port);
    return connect(s, (struct sockaddr*)&sa, sizeof(sa));
}

// 测试是否有数据可以进行读取
BOOL CanRecvable(SOCKET s, UINT ims)
{
    fd_set FdSet;
	FD_ZERO(&FdSet);
	FD_SET(s, &FdSet);

    struct timeval tv;
    tv.tv_sec = (long)ims / 1000;
    tv.tv_usec = (long)ims % 1000 * 1000;
#ifdef WIN32
    return select(1, &FdSet, (fd_set *)0, (fd_set *)0, &tv) > 0;
#else
    return select(s + 1, &FdSet, (fd_set *)0, (fd_set *)0, &tv) > 0;
#endif
}

// 发送一次Buff,返回发送完成的长度
int SendBuff(SOCKET s, void* buff, int len)
{
    if (!buff || len <= 0)
    {
        return 0;
    }

    unsigned char * data = (unsigned char *)(buff);
    return send(s, data, len, 0);
}

// 发送整个Buff，直到发送完成或者产生错误
int SendEntireBuff(SOCKET s, void* buff, int len)
{
    if (!buff || len <= 0)
    {
        return 0;
    }

    unsigned char * data = (unsigned char *)(buff);

    while (1)
    {
        int ret = send(s, data, len, 0);
        if (ret == SOCKET_ERROR || ret < 0)
        {
           return SOCKET_ERROR;
        }
        else
        {
            len -= ret;
            data += ret;

            if (len < 0)
            {
                return SOCKET_ERROR;
            }
            else
            {
                if (len == 0)
                {
                    return len;
                }
            }
        }
    }
}

// 接收一次数据
int RecvBuffer(SOCKET s, char * buffer, int buflen)
{
    return recv(s, buffer, buflen, 0);
}


void PrintLastError()
{
    printf("socket error:%d", GetLastError());
}





#endif // RAWSOCKET_H_INCLUDED
