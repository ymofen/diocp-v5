//
//  NsNetPreBuild.h
//  ChessGame
//
//  Created by yizeng on 14-7-1.
//
//

#ifndef library_NsNetPreBuild_h
#define library_NsNetPreBuild_h

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

#endif
