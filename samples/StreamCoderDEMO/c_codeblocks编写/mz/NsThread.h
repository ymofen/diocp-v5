//
//  NsThread.h
//  ChessGame
//
//  Created by yizeng on 14-7-1.
//
//

#ifndef library_NsThread_h
#define library_NsThread_h

#include <pthread.h>
#include <thread>
#include <sys/time.h>
#include "../../NsUtils.h"

#include "CCPlatformConfig.h"
#if (CC_TARGET_PLATFORM == CC_PLATFORM_ANDROID)
#include <jni.h>
#include "jni/JniHelper.h"
#endif

#define MAX_WAIT_FOR_TIME 6000

// 线程类
class Thread
{
public:
    Thread()
    {
        mAutoFree = false;
        mIsTerminated = false;
        mIsSuspended = false;
#if (CC_TARGET_PLATFORM == CC_PLATFORM_ANDROID)
        mIsJniAttach = false;
#endif
        memset(&mThread, 0, sizeof(pthread_t)); // mThread = nullptr;
        _cThreadPauseMutex = PTHREAD_MUTEX_INITIALIZER;
        _cThreadPauseCond = PTHREAD_COND_INITIALIZER;
    }
    
    // 线程句柄合法
    bool isVaildHandle()
    {
        return mThread;
    }
    
    // 创建线程 (主线程调用, stackSize单位kb)
    bool Start(bool isSuspended = false, unsigned int stackSize = 0, bool IsAutofree = false)
    {
        // 已经创建
        if (isVaildHandle())
        {
            return false;
        }
        
        // 线程属性
        int ret;
        pthread_attr_t attr;
        ret = pthread_attr_init(&attr);
        if (ret != 0)
        {
            return false;
        }
        
        // 自动释放
        mAutoFree = IsAutofree;
        if (mAutoFree)
        {
            pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED); // 分离模式
        }
        else
        {
            pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE); // 结合模式
        }
        
        // 线程堆栈
        if (stackSize > 0)
        {
            ret = pthread_attr_setstacksize(&attr, stackSize);
            if (ret != 0)
            {
                pthread_attr_destroy(&attr);
                return false;
            }
        }
        
        // 创建线程
        mIsTerminated = false;
        ret = pthread_create(&mThread, &attr, ThreadProc, this);
        pthread_attr_destroy(&attr);
        if (ret != 0)
        {
            return false;
        }
        
        // 先挂起
        if (isSuspended)
        {
            Suspend();
        }
        
        return true;
    }
    
    // 唤醒 (主线程调用)
    bool Resume()
    {
        if (mIsTerminated || !isVaildHandle())
        {
            return false;
        }
        
        // 挂起中
        if (mIsSuspended)
        {
            pthread_mutex_lock(&_cThreadPauseMutex);
            
            mIsSuspended = false;
            pthread_cond_broadcast(&_cThreadPauseCond); // TODO: 如果没有线程等待 会有问题吗 ? (如果当前没有线程等待通知，则上面两种调用实际上成为一个空操作)
            
            pthread_mutex_unlock(&_cThreadPauseMutex);
        }
        
        return true;
    }
    
    // 挂起 (主线程调用)
    bool Suspend()
    {
        if (mIsTerminated || !isVaildHandle())
        {
            return false;
        }
        
        // 未挂起
        if (!mIsSuspended)
        {
            pthread_mutex_lock(&_cThreadPauseMutex);
            
            mIsSuspended = true;
            
            pthread_mutex_unlock(&_cThreadPauseMutex);
        }
        
        return true;
    }
    
    // 结束 (主线程调用)
    void Terminate()
    {
        if (mIsTerminated || !isVaildHandle())
        {
            return;
        }
        
        Resume(); // 先唤醒
        mIsTerminated = true;
    }
    
    // 线程已结束
    bool IsTerminated()
    {
        return mIsTerminated;
    }
    
    // 线程正运行
    bool IsStarted()
    {
        return (isVaildHandle() && !mIsTerminated);
    }
    
    // 等待结束 (主线程调用)
    void WaitForTerimated()
    {
        if (isVaildHandle())
        {
            if (!mAutoFree)
            {
                pthread_join(mThread, nullptr);
            }
            
            memset(&mThread, 0, sizeof(pthread_t)); // mThread = nullptr;
            //mIsTerminated = false; // TODO: AutoFree时导致线程不结束
            //mIsSuspended = false;
        }
    }
    
    // 挂起一会 (子线程调用 单位毫秒)
    static void WaitFor(int timeout = MAX_WAIT_FOR_TIME)
    {
#ifdef WIN32
        Sleep(timeout);
#else
        pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
        pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
        pthread_mutex_lock(&mutex);
        struct timeval now;
        gettimeofday(&now, NULL);
        struct timespec outtime;
        outtime.tv_sec = now.tv_sec + (long)timeout/1000;
        outtime.tv_nsec = (now.tv_usec + (long)timeout%1000 * 1000) * 1000;
        pthread_cond_timedwait(&cond, &mutex, &outtime);
        pthread_mutex_unlock(&mutex);
#endif
    }
    
    // 关闭并且等待
    void TerminateAndWait()
    {
        Terminate();
        WaitForTerimated();
    }
    
private:
    // 解绑jvm 在子线程内部调用
    void CurrentDetachAndroid()
    {
#if (CC_TARGET_PLATFORM == CC_PLATFORM_ANDROID)
        if (!mIsJniAttach) // 未绑定
        {
            return;
        }
        
        JavaVM* vm = cocos2d::JniHelper::getJavaVM();
        if (vm)
        {
            vm->DetachCurrentThread();
        }
        
        mIsJniAttach = false;
#endif
    }
    
protected:
    virtual void Execute() { }  // 线程体 (反复被调用)
    virtual void OnTerminated() { }
    
    // 如果线程内部需要调用JNI的时候, 需在子线程内部调用这个函数，JVM绑定线程
    void CurrentAttachAndroid()
    {
#if (CC_TARGET_PLATFORM == CC_PLATFORM_ANDROID)
        if (!mThread)
        {
            return;
        }
        
        if (mIsJniAttach) // 已绑定
        {
            return;
        }
        
        JavaVM* vm = cocos2d::JniHelper::getJavaVM();
        JNIEnv* env = cocos2d::JniHelper::getEnv();
        if (env && vm)
        {
            JavaVMAttachArgs thread_args;
            thread_args.name = Format("pThread: %d", (int64_t)&mThread).c_str();
            thread_args.version = JNI_VERSION_1_4;
            thread_args.group = nullptr;
            
            vm->AttachCurrentThread(&env, (void*)&thread_args);
        }
        
        mIsJniAttach = true;
#endif
    }
    
    // 线程回调
    static void* ThreadProc(void* Obj)
    {
        Thread* ThreadObj = (Thread*)Obj;
        
        while (ThreadObj->IsStarted())
        {
            pthread_mutex_lock(&ThreadObj->_cThreadPauseMutex);
            
            while (ThreadObj->mIsSuspended)
            {
                pthread_cond_wait(&ThreadObj->_cThreadPauseCond, &ThreadObj->_cThreadPauseMutex);
            }
            
            pthread_mutex_unlock(&ThreadObj->_cThreadPauseMutex);
            
            //
            ThreadObj->Execute();
            
            if (ThreadObj->mIsTerminated)
            {
                break;
            }
        }
        
        //
        ThreadObj->OnTerminated();
        ThreadObj->CurrentDetachAndroid();
        
        if (ThreadObj->mAutoFree)
        {
            pthread_detach(pthread_self());
        }
        
        pthread_exit(0);
    }
    
protected:
    bool mAutoFree;
    volatile bool mIsTerminated;
    bool mIsSuspended;
#if (CC_TARGET_PLATFORM == CC_PLATFORM_ANDROID)
    bool mIsJniAttach;
#endif
    // 线程
    pthread_t mThread;
    pthread_mutex_t _cThreadPauseMutex;
    pthread_cond_t _cThreadPauseCond;
};

#endif

