//
//  uFileUpload.cpp
//  文件上传
//
//  Created by liuliu on 15-2-11.
//
//

#include <errno.h>
#include "uFileUpload.h"
#include "uSocketthread.h"
#include "NsUtils.h"

//

FileUpload::FileUpload()
{
    mState = eCLIENT_STATE_NONE;
    
    mRemoteIp = "";
    mRemotePort = 0;
    mConnectTimeOut = 0;
    
    mSocket = INVALID_SOCKET;
    
    mRecvSize = 0;
    
    Start();
    Director::getInstance()->getScheduler()->schedule(schedule_selector(FileUpload::OnTimer), this, 0, false);
}

FileUpload::~FileUpload()
{
    Director::getInstance()->getScheduler()->unschedule(schedule_selector(FileUpload::OnTimer), this);
    TerminateAndWait();
    
    ClearRequestList();
    ClearResponseList();
    
    CloseSocket(eCLIENT_STATE_NONE, false);
}

void FileUpload::SetServer(const std::string & RemoteIp, uint16_t RemotePort, int ConnectTimeOut)
{
    mRemoteIp = RemoteIp;
    mRemotePort = RemotePort;
    mConnectTimeOut = ConnectTimeOut;
}

void FileUpload::AddRequest(int UserId, UploadFileType FileType, const std::string & FileName, cocos2d::Ref * Target, SEL_UploadFile Selector)
{
    UploadRequest* Req = new UploadRequest(UserId, FileType, FileName, Target, Selector);
    
    mRequestMutex.lock();
    mRequestList.push_back(Req);
    mRequestMutex.unlock();
}

bool FileUpload::HasRequest()
{
    mRequestMutex.lock();
    bool Ret = mRequestList.size() > 0;
    mRequestMutex.unlock();
    
    return Ret;
}

UploadRequest* FileUpload::PopRequest()
{
    UploadRequest* Req = nullptr;
    
    mRequestMutex.lock();
	if (mRequestList.size() > 0)
	{
		Req = mRequestList.front();
        mRequestList.pop_front();
	}
    mRequestMutex.unlock();
    
	return Req;
}

void FileUpload::AddResponse(const UploadRequest & Req, bool Success, const std::string & SrvFileName)
{
    UploadResponse* Rep = new UploadResponse(Req, Success, SrvFileName);
    
    mResponseMutex.lock();
    mResponseList.push_back(Rep);
    mResponseMutex.unlock();
}

UploadResponse* FileUpload::PopResponse()
{
    UploadResponse* Rep = nullptr;
    
    mResponseMutex.lock();
    if (mResponseList.size() > 0)
    {
        Rep = mResponseList.front();
        mResponseList.pop_front();
    }
    mResponseMutex.unlock();
    
    return Rep;
}

void FileUpload::AddWaiting(const std::string & SrvFileName, UploadRequest* Req)
{
    mWaitingList.insert({SrvFileName, Req});
}

bool FileUpload::CanSendData() // 可写
{
    if (mState != eClient_STATE_CONNECTED)
    {
        return false;
    }
    
    fd_set FdSet;
    FD_ZERO(&FdSet);
    FD_SET(mSocket, &FdSet);
    
    timeval TimeVal;
    TimeVal.tv_sec = 0;
    TimeVal.tv_usec = 0;
    
#ifdef WIN32
    return select(1, nullptr, &FdSet, nullptr, &TimeVal) > 0;
#else
    return select(mSocket + 1, nullptr, &FdSet, nullptr, &TimeVal) > 0;
#endif
}

bool FileUpload::CanRecvData() // 可读
{
    if (mState != eClient_STATE_CONNECTED)
    {
        return false;
    }
    
    fd_set FdSet;
	FD_ZERO(&FdSet);
	FD_SET(mSocket, &FdSet);
	
    timeval TimeVal;
	TimeVal.tv_sec = 0;
	TimeVal.tv_usec = 0;
    
#ifdef WIN32
    return select(1, &FdSet, nullptr, nullptr, &TimeVal) > 0;
#else
    return select(mSocket + 1, &FdSet, nullptr, nullptr, &TimeVal) > 0;
#endif
    
    // if (FD_ISSET(mSocket, &fs) != 0)
}

void FileUpload::TryConnectServer()
{
    if (mRemoteIp.empty())
	{
		return;
	}
    
	mSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (mSocket == INVALID_SOCKET)
	{
		mState = eClient_STATE_CONNECT_FAIL;
		return;
	}
    
    const static struct timeval t = {0, 0};
    setsockopt(mSocket, SOL_SOCKET, SO_SNDTIMEO, (char *)&t, sizeof(t));
    setsockopt(mSocket, SOL_SOCKET, SO_RCVTIMEO, (char *)&t, sizeof(t));
    
    int set = 1;
    setsockopt(mSocket, SOL_SOCKET, SO_NOSIGPIPE, (void*)&set, sizeof(set));
    
    //  setsockopt(mSocket, SOL_SOCKET, SO_RCVBUF, _mReadBuffer, 0);
    //  setsockopt(mSocket, SOL_SOCKET, SO_SNDBUF, _mSendBuffer, 0);
    
    sockaddr_in sa;
    sa.sin_family = AF_INET;
    sa.sin_addr.s_addr = inet_addr(mRemoteIp.c_str());
    sa.sin_port = htons(mRemotePort);
    
    int flags = fcntl(mSocket, F_GETFL, 0);
    bool nonblock = mConnectTimeOut >= 0; // TODO: 其实始终为True
    
    if (nonblock) // 需要自定义connect超时
    {
        fcntl(mSocket, F_SETFL, flags | O_NONBLOCK);
    }
    else
    {
        fcntl(mSocket, F_SETFL, flags & ~O_NONBLOCK);
    }
    
    int ret = connect(mSocket, (sockaddr*)&sa, sizeof(sa));
    if (ret == 0)
	{
        fcntl(mSocket, F_SETFL, flags & ~O_NONBLOCK); // fixed by mazi , if (nonblock) ..
		mState = eClient_STATE_CONNECTED; // TODO: nonblock时是否会直接成功而返回0 ? 以至于未恢复~O_NONBLOCK ?
	}
	else
	{
        if (nonblock)
        {
            // TODO: errno == EINPROGRESS
            // connect调用后，无论连接是否建立立即返回-1，同时将errno（包含errno.h就可以直接使用）设置为EINPROGRESS, 表示此时tcp三次握手仍旧进行，如果errno不是EINPROGRESS，则说明连接错误。
            
            fd_set fs;
            FD_ZERO(&fs);
            FD_SET(mSocket, &fs);
            
            //  fd_set rfs = fs;
            
            //  struct timeval now;
            //  gettimeofday(&now, NULL);
            //  timeval tv;
            //  tv.tv_sec = now.tv_sec;
            //  tv.tv_usec = now.tv_usec + (uint32_t)pConnect->mtimeout * 1000;
            
            timeval tv;
            tv.tv_sec = (uint32_t)mConnectTimeOut / 1000;
            tv.tv_usec = (uint32_t)mConnectTimeOut % 1000 * 1000;
            
            int ret = 0;
#ifdef WIN32
            ret = select(1, nullptr, &fs, nullptr, &tv);
#else
            ret = select(mSocket + 1, nullptr, &fs, nullptr, &tv);
#endif
            if (ret <= 0)
            {
                CloseSocket(eClient_STATE_CONNECT_FAIL, true);
            }
            else
            {
                //#ifdef ANDROID
                fcntl(mSocket, F_SETFL, flags & ~O_NONBLOCK);
                mState = eClient_STATE_CONNECTED;
                
                /*#else
                 int error=-1, len;
                 getsockopt(mSocket, SOL_SOCKET, SO_ERROR, &error, (socklen_t *)&len);
                 if(error == 0)
                 {
                 fcntl(mSocket,F_SETFL,flags & ~O_NONBLOCK);
                 SetClientState(eClient_STATE_CONNECTED);
                 } else
                 {
                 cocos2d::log("Socket Connect Error: %d", error);
                 closeSocket();
                 SetClientState(eClient_STATE_CONNECT_FAIL);
                 }
                endif*/
            }
        }
        else
        {
            CloseSocket(eClient_STATE_CONNECT_FAIL, true);
        }
	}
}

bool FileUpload::TrySendData(void* DataBuff, int DataSize)
{
    if (mState != eClient_STATE_CONNECTED)
    {
        return false;
    }
    
    if (!DataBuff || DataSize <= 0)
    {
        return false;
    }
    
    unsigned char * Data = static_cast<unsigned char *>(DataBuff);
    
    while (true)
    {
        int Ret = send(mSocket, Data, DataSize, 0);
        if (Ret == SOCKET_ERROR || Ret < 0)
        {
            CCLOG("errno: %d, %s", errno, strerror(errno));
            
            CloseSocket(eClient_STATE_ABORT, true);
            return false;
        }
        else // TODO: 未考虑返回其他负值 (可能不会的吧)
        {
            DataSize -= Ret;
            Data += Ret;
            
            if (DataSize < 0)
            {
                CloseSocket(eClient_STATE_ABORT, true);
                return false;
            }
            else
            {
                if (DataSize == 0)
                {
                    return true;
                }
            }
        }
    }
}

bool FileUpload::TryDecodeRecvBuff(ServerResponse & SvrRep)
{
    if (mRecvSize < 10) // 2 + 4 + 4
    {
        return false;
    }
    
    // 标记
    short Flag = FromLittleEndian16(&mRecvBuff[0]);     // +2
    if (Flag != 0x0D10)
    {
        CloseSocket(eClient_STATE_DISCONNECT, true);
        return false;
    }
    
    // 校验码
    uint32_t Value = FromLittleEndian32(&mRecvBuff[2]); // +4
    
    // 流长度
    int StreamSize = FromBigEndian32(&mRecvBuff[6]);    // +4
    if (StreamSize <= 0 || StreamSize < 271) // < SizeOf(TFileHead)
    {
        CloseSocket(eClient_STATE_DISCONNECT, true);
        return false;
    }
    
    // 未收完
    if (StreamSize > mRecvSize - 10) // 2 + 4 + 4
    {
        return false;
    }
    
    // 校验之
    if (VerifyData(&mRecvBuff[10], StreamSize) != Value)
    {
        CloseSocket(eClient_STATE_DISCONNECT, true);
        return false;
    }
    
    //
    SvrRep.mFlag = FromLittleEndian16(&mRecvBuff[10]);
    SvrRep.mUserId = FromLittleEndian32(&mRecvBuff[14]);
    SvrRep.mFileType = static_cast<UploadFileType>(FromLittleEndian32(&mRecvBuff[18]));
    SvrRep.mSrvFileName = reinterpret_cast<char*>(&mRecvBuff[26]);
    
    //
    memmove(&mRecvBuff[0], &mRecvBuff[10 + StreamSize], mRecvSize - 10 - StreamSize);
    mRecvSize -= (10 + StreamSize);
    
    return true;
}

bool FileUpload::HandleRequest(UploadRequest* Req)
{
    if (mState != eClient_STATE_CONNECTED)
    {
        return false;
    }
    
    if (!Req)
    {
        return false;
    }
    
    if (!FileUtils::getInstance()->isFileExist(Req->mFileName))
    {
        return false;
    }
    
    // 扩展名
    std::string Ext = ExtractFileExt(Req->mFileName);
    if (Ext.empty())
    {
        return false;
    }
    
    // 文件内容
    auto FileData = FileUtils::getInstance()->getDataFromFile(Req->mFileName);
    if (FileData.isNull())
    {
        return false;
    }
    
    // SHA1编码
    std::string Sha1 = GetDataSha1(FileData.getBytes(), FileData.getSize());
    if (Sha1.empty())
    {
        return false;
    }
    
    // 创建数据流
    int StreamSize = 4 + 256 + 4 + 4 + 4 + FileData.getSize();
    unsigned char *StreamBuff = new unsigned char[StreamSize];
    
    ToLittleEndian32(1, &StreamBuff[0]); // 文件个数
    
    std::string SrvFileName = Sha1 + Ext; // 文件名
    memset(&StreamBuff[4], 0, 256);
    memcpy(&StreamBuff[4], SrvFileName.c_str(), SrvFileName.size());
    
    ToLittleEndian32(Req->mUserId, &StreamBuff[260]); // 用户id
    
    ToLittleEndian32(Req->mFileType, &StreamBuff[264]); // 文件类型
    
    ToLittleEndian32(FileData.getSize(), &StreamBuff[268]); // 文件大小
    
    memcpy(&StreamBuff[272], FileData.getBytes(), FileData.getSize()); // 文件内容
    
    // 标记
    unsigned char FlagBuff[2];
    ToLittleEndian16(0x0D10, FlagBuff);
    
    if (!TrySendData(FlagBuff, 2))
    {
        delete StreamBuff;
        return false;
    }
    
    // 数据流校验
    unsigned char VerifyBuff[4];
    ToLittleEndian32(VerifyData(StreamBuff, StreamSize), VerifyBuff);
    if (!TrySendData(VerifyBuff, 4))
    {
        delete StreamBuff;
        return false;
    }
    
    // 数据流长度
    unsigned char LenBuff[4];
    ToBigEndian32(StreamSize, LenBuff);
    if (!TrySendData(LenBuff, 4))
    {
        delete StreamBuff;
        return false;
    }
    
    // 数据流
    if (!TrySendData(StreamBuff, StreamSize))
    {
        delete StreamBuff;
        return false;
    }
    
    //
    AddWaiting(SrvFileName, Req);
    
    delete StreamBuff;
    return true;
}

//bool FileUpload::HandleResponse(UploadResponse * Rep)
//{
//    if (!Rep)
//    {
//        return false;
//    }
//    
//    (Rep->mRequest.mTarget->*Rep->mRequest.mSelector)(Rep);
//}

void FileUpload::ClearRequestList()
{
    mRequestMutex.lock();
    
    while (mRequestList.size())
	{
		delete mRequestList.front();
		mRequestList.pop_front();
	}
    
    mRequestMutex.unlock();
}

void FileUpload::ClearResponseList()
{
    mResponseMutex.lock();
    
    while (mResponseList.size())
	{
		delete mResponseList.front();
		mResponseList.pop_front();
	}
    
    mResponseMutex.unlock();
}

void FileUpload::ClearWaitingList(bool Notify)
{
    while (mWaitingList.size() > 0)
    {
        auto Itr = mWaitingList.begin();
        auto Req = Itr->second;
        
        if (Req)
        {
            if (Notify)
            {
                AddResponse(*Req, false, "");
            }
            
            delete Req;
        }
        
        mWaitingList.erase(Itr);
    }
}

void FileUpload::CloseSocket(eClientState State, bool Notify)
{
    mState = State;
    ClearWaitingList(Notify);
    mRecvSize = 0;
    
    if (mSocket == INVALID_SOCKET)
    {
        return;
    }
    
#ifdef WIN32
    closesocket(mSocket);
#else
    close(mSocket); // android ,ios平台
#endif
    mSocket = INVALID_SOCKET;
}

uint32_t FileUpload::VerifyData(unsigned char* DataBuff, int DataSize)
{
    uint32_t Ret = 0;
    
    for (int i = 0; i < DataSize; ++i)
    {
        Ret += DataBuff[i];
    }
    
    return Ret;
}

void FileUpload::Execute()
{
    if (HasRequest() && mState != eClient_STATE_CONNECTED) // 有上传请求 && 尚未连接
    {
        TryConnectServer();
    }
    
    if (mState == eClient_STATE_CONNECTED) // 已连接上
    {
        //if (CanSendData())
        {
            UploadRequest* Req = PopRequest();
            if (Req)
            {
                if (HandleRequest(Req))
                {
                    // Req已放入WaitingList, 暂不回收
                }
                else
                {
                    AddResponse(*Req, false, "");
                    delete Req;
                }
            }
        }
        
        if (CanRecvData() && mRecvSize < MAX_RECV_BUFF_SIZE)
        {
            int Ret = recv(mSocket, &mRecvBuff[mRecvSize], MAX_RECV_BUFF_SIZE - mRecvSize, 0);
            if (Ret == SOCKET_ERROR || Ret <= 0)
            {
                CloseSocket(eClient_STATE_ABORT, true); // TODO: 当服务器send主动关闭时 (应该至少还能recv一次, 没问题)
            }
            else
            {
                mRecvSize += Ret;
            }
        }
    }
    
    ServerResponse SrvRep;
    if (TryDecodeRecvBuff(SrvRep))
    {
        auto Itr = mWaitingList.find(SrvRep.mSrvFileName);
        if (Itr != mWaitingList.end())
        {
            auto Req = Itr->second;
            
            if (Req)
            {
                AddResponse(*Req, SrvRep.mFlag == 0x00A2, SrvRep.mSrvFileName);
                delete Req;
            }
            
            mWaitingList.erase(Itr);
        }
    }
    
    WaitFor(1);
}

void FileUpload::OnTimer(float dt)
{
    UploadResponse* Rep = PopResponse();
    if (Rep)
    {
        (Rep->mRequest.mTarget->*Rep->mRequest.mSelector)(*Rep);
        delete Rep;
    }
}

std::string FileUpload::GetFileTypeDir(UploadFileType FileType)
{
    switch (FileType) {
        case UploadFileType_Icon:
            return "avatar";
            break;
            
        case UploadFileType_Photo:
            return "photo";
            break;
            
        case UploadFileType_Video:
            return "video";
            break;
            
        case UploadFileType_Audio:
            return "audio";
            break;
            
        case UploadFileType_Show:
            return "show";
            break;
            
        default:
            return "mazi";
            break;
    }
}

std::string FileUpload::GetFileTypeName(UploadFileType FileType)
{
    switch (FileType) {
        case UploadFileType_Icon:
            return "头像";
            break;
            
        case UploadFileType_Photo:
            return "照片";
            break;
            
        case UploadFileType_Video:
            return "视频";
            break;
            
        case UploadFileType_Audio:
            return "音频";
            break;
            
        case UploadFileType_Show:
            return "秀??";
            break;
            
        default:
            return "未知资源";
            break;
    }
}

std::string FileUpload::GetFileHttpUrl(int UserId, UploadFileType FileType, const std::string & Name)
{
    return Format("http://%s:%d/upload/%d/%s/%s", cDefaultLoginServerAddr, cDefaultDownloadServerPort, UserId, GetFileTypeDir(FileType).c_str(), Name.c_str());
}

std::string FileUpload::getLocalIconPath()
{
    std::string Ret = ns::NsPlatformHelper::getExternalRootPath();
    if (Ret.empty())
    {
        CCLOG("getLocalIconPath, getExternalRootPath() fail");
        return "";
    }
    
    Ret += "/";
    Ret += FileUpload::GetFileTypeDir(UploadFileType_Icon);
    if (!CreateDir(Ret))
    {
        CCLOG("getLocalIconPath, CreateDir fail - %s", Ret.c_str());
        return "";
    }
    
    Ret += "/";
    return Ret;
}

