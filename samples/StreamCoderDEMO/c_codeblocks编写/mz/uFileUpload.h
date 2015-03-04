//
//  uFileUpload.h
//  文件上传
//
//  Created by liuliu on 15-2-11.
//
//

#ifndef __ChessGame__uFileUpload__
#define __ChessGame__uFileUpload__

#include <string>
#include <deque>
#include "CCRef.h"
#include "NsThread.h"
#include "NsTcpInterface.h"
#include "NsNetPreBuild.h"

// 文件类型
enum UploadFileType
{
    UploadFileType_Icon  = 0,
    UploadFileType_Photo = 1,
    UploadFileType_Video = 2,
    UploadFileType_Audio = 3,
    UploadFileType_Show  = 4,
};

/*
// 请求类型
enum RequestType (已废弃)
{
    RequestType_Connect, // 已废弃, 改为内部自动连接
    RequestType_Upload
};

// 请求(基类)
struct RequestHead (已废弃)
{
    RequestType mType;
};

// 请求(连接)
struct RequestConnect: public RequestHead (已废弃)
{
    std::string mRemoteIp; // [16]
    uint16_t mRemotePort;
    int mTimeOut;
    
    RequestConnect(const std::string & Ip, uint16_t Port, int TimeOut)
	{
		mType = RequestType_Connect;
        mRemoteIp = Ip;
		mRemotePort = Port;		
        mTimeOut = TimeOut;
	}
};
*/

// 上传应答
struct UploadResponse;

// 上传回调
typedef void (cocos2d::Ref::*SEL_UploadFile)(UploadResponse&);
#define uploadfile_selector(_SELECTOR) (SEL_UploadFile)(&_SELECTOR)

// 上传请求
struct UploadRequest /* (已废弃): public RequestHead */
{
    int mUserId;
    UploadFileType mFileType;
    std::string mFileName;
    cocos2d::Ref* mTarget;
    SEL_UploadFile mSelector;
    
    UploadRequest(int UserId, UploadFileType FileType, const std::string & FileName, cocos2d::Ref * Target, SEL_UploadFile Selector):
        mUserId(UserId),
        mFileType(FileType),
        mFileName(FileName),
        mTarget(Target),
        mSelector(Selector)
    {
      //mType = RequestType_Upload;
    }
};

// 上传应答
struct UploadResponse
{
    UploadRequest mRequest; // 上传请求
    bool mSuccess;          // 成功与否
    std::string mSrvFileName;
    
    UploadResponse(const UploadRequest & Request, bool Success, const std::string & SrvFileName):
        mRequest(Request),
        mSuccess(Success),
        mSrvFileName(SrvFileName)
    {
        
    }
};

// 来自服务器的应答
struct ServerResponse
{
    short mFlag;
    int mUserId;
    UploadFileType mFileType;
    std::string mSrvFileName; // 服务器文件名
};

#define MAX_RECV_BUFF_SIZE 2048

// 文件上传线程
class FileUpload: public Thread, public cocos2d::Ref
{
public:
    FileUpload();
    ~FileUpload();
    
    void SetServer(const std::string & RemoteIp, uint16_t RemotePort, int ConnectTimeOut); // 设置文件服务器 (主线程调用)
    void AddRequest(int UserId, UploadFileType FileType, const std::string & FileName, cocos2d::Ref * Target, SEL_UploadFile Selector); // 追加上传请求 (主线程调用)
    
private:
    bool HasRequest(); // 存在上传请求
    UploadRequest* PopRequest(); // 弹出第一个上传请求 (子线程调用)
    
    void AddResponse(const UploadRequest & Req, bool Success, const std::string & SrvFileName); // 追加上传应答 (子线程调用)
    UploadResponse* PopResponse(); // 弹出第一个上传应答 (主线程调用)
    
    void AddWaiting(const std::string & SrvFileName, UploadRequest* Req); // 等待应答 (可不做多线程互斥)
    
    bool CanSendData(); // 可写
    bool CanRecvData(); // 可读
    
    void TryConnectServer(); // 连接服务器 (子线程调用)
    bool TrySendData(void* DataBuff, int DataSize); // 发送数据 (子线程调用)
    bool TryDecodeRecvBuff(ServerResponse & SvrRep); // 从mRecvBuff解出第一个ServerResponse (子线程调用)
    
	bool HandleRequest(UploadRequest * Req); // 处理上传请求 (子线程调用)
  //bool HandleResponse(UploadResponse * Rep); // 处理上传应答 (主线程调用)    
    
    void ClearRequestList();
    void ClearResponseList();
    void ClearWaitingList(bool Notify); // Notify: 是否应答回调
    
    void CloseSocket(eClientState State, bool Notify); // TODO: 未考虑多线程安全, Notify: 是否应答回调(处于WaitingList中的请求)
    
    uint32_t VerifyData(unsigned char* DataBuff, int DataSize); // 校验值
        
protected:
    virtual void Execute() override; // 子线程函数
    
private:
    void OnTimer(float dt); // 主线程定时器
    
private:
    eClientState mState;
    
    std::string mRemoteIp;
    uint16_t mRemotePort;
    int mConnectTimeOut;
    
    SOCKET mSocket;
    
    std::mutex mRequestMutex;
    std::deque<UploadRequest*> mRequestList; // 请求列表
    
    std::mutex mResponseMutex;
    std::deque<UploadResponse*> mResponseList; // 应答列表
    
    std::map<std::string, UploadRequest*> mWaitingList; // 等待列表 (已请求, 尚未应答, 以服务器文件名为Key)
    
    unsigned char mRecvBuff[MAX_RECV_BUFF_SIZE];
    int mRecvSize;
    
public:
    static std::string GetFileTypeDir(UploadFileType FileType);
    static std::string GetFileTypeName(UploadFileType FileType);
    static std::string GetFileHttpUrl(int UserId, UploadFileType FileType, const std::string & Name);
    static std::string getLocalIconPath();
};

#endif /* defined(__ChessGame__uFileUpload__) */
