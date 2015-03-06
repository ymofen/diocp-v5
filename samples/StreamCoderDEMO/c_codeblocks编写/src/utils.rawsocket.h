#ifndef UTILS_RAWSOCKET_H_INCLUDED
#define UTILS_RAWSOCKET_H_INCLUDED

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
int WinWSAStartup();
#endif

// 创建TCP套接字
SOCKET CreateTcpSocket();

int ConnectSocket(SOCKET s, const char * host, u_short port);

int ConnectSocketTimeOut(SOCKET s, const char * host, u_short port, int ms);

// 测试是否有数据可以进行读取
BOOL CanRecvable(SOCKET s, UINT ims);

// 发送一次Buff,返回发送完成的长度
int SendBuff(SOCKET s, void* buff, int len);

// 发送整个Buff，直到发送完成或者产生错误
int SendEntireBuff(SOCKET s, void* buff, int len);


// 接收一次数据
int RecvBuffer(SOCKET s, char * buffer, int buflen);

void PrintLastError();

#endif // UTILS_RAWSOCKET_H_INCLUDED
