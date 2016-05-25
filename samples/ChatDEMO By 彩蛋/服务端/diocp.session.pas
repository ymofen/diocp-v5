unit diocp.session;

interface

uses
  utils_hashs, SysUtils, Windows, Classes;

type
  TSessionItem = class(TObject)
  private
    FSessionID: string;
    // 最后交互数据的时间点
    FLastActivity: Cardinal;
  public
    constructor Create; virtual;
    procedure DoActivity;

    /// <summary>
    ///   断开 Session失效, 移除列表，准备释放SessionItem时执行
    /// </summary>
    procedure OnDisconnect; virtual;
  public
    property SessionID: string read FSessionID;
  end;

  TSessionItemClass = class of TSessionItem;

  TSessions = class(TObject)
  private
    FItemClass: TSessionItemClass;
    FList: TDHashTableSafe;
  public
    constructor Create(AItemClass: TSessionItemClass);
    destructor Destroy; override;

    /// <summary>
    ///   查找Session
    /// </summary>
    function FindSession(pvSessionID:string): TSessionItem;

    procedure GetSessionList(pvList:TList);

    /// <summary>
    ///   查找并返回Session,如果不存在则创建一个新的Session
    /// </summary>
    /// <returns> TSessionItem
    /// </returns>
    /// <param name="pvSessionID"> (string) </param>
    function CheckSession(pvSessionID:string): TSessionItem;
    /// <summary>
    /// 移除指定的会话(如会话下线时)
    /// </summary>
    /// <param name="ASID"></param>
    procedure RemoveSession(const ASID: string);
    /// <summary>
    ///   超时检测, 如果超过Timeout指定的时间还没有任何数据交换数据记录，
    ///     就进行关闭连接
    ///   使用循环检测，如果你有好的方案，欢迎提交您的宝贵意见
    /// </summary>
    procedure KickOut(pvTimeOut:Cardinal = 60000);
  end;

implementation


/// <summary>
///   计算两个TickCount时间差，避免超出49天后，溢出
///      感谢 [佛山]沧海一笑  7041779 提供
///      copy自 qsl代码 
/// </summary>
function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;

function TSessions.CheckSession(pvSessionID:string): TSessionItem;
begin
  Assert(FItemClass <> nil);
  FList.Lock;
  Result := TSessionItem(FList.ValueMap[pvSessionID]);
  if Result = nil then
  begin
    Result := FItemClass.Create;
    Result.FSessionID := pvSessionID;
    FList.ValueMap[pvSessionID] := Result;
  end;
  FList.unLock;
  Result.DoActivity;
end;

constructor TSessions.Create(AItemClass: TSessionItemClass);
begin
  inherited Create;
  FList := TDHashTableSafe.Create();
  FItemClass := AItemClass;
end;

destructor TSessions.Destroy;
begin
  FList.FreeAllDataAsObject;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TSessions.FindSession(pvSessionID:string): TSessionItem;
begin
  FList.Lock;
  Result := TSessionItem(FList.ValueMap[pvSessionID]);
  FList.unLock;
end;

procedure TSessions.GetSessionList(pvList:TList);
begin
  FList.Lock;
  FList.GetDatas(pvList);
  FList.unLock;
end;

procedure TSessions.KickOut(pvTimeOut:Cardinal = 60000);
var
  lvNowTickCount:Cardinal;
  lvBucket, lvNextBucket:PDHashData;
  I:Integer;
  lvContext : TSessionItem;
  lvDeleteList:TStrings;
  lvObj:TObject;
begin
  lvNowTickCount := GetTickCount;
  lvDeleteList := TStringList.Create;
  FList.Lock();
  try
    for I := 0 to FList.BucketSize - 1 do
    begin
      lvBucket := FList.Buckets[I];
      while lvBucket<>nil do
      begin
        lvNextBucket := lvBucket.Next;
        if lvBucket.Data <> nil then
        begin
          lvContext := TSessionItem(lvBucket.Data);
          if lvContext.FLastActivity <> 0 then
          begin
            if tick_diff(lvContext.FLastActivity, lvNowTickCount) > pvTimeOut then
            begin
              lvDeleteList.AddObject(lvContext.FSessionID, lvContext);
            end;
          end;
        end;
        lvBucket:= lvNextBucket;
      end;
    end;

    /// 清理对象
    for i := 0 to lvDeleteList.Count - 1 do
    begin
      lvObj := lvDeleteList.Objects[i];
      FList.Remove(lvDeleteList[i]);
      TSessionItem(lvObj).OnDisconnect;
      TObject(lvObj).Free;
    end;
  finally
    FList.unLock;
    lvDeleteList.Free;
  end;
end;

procedure TSessions.RemoveSession(const ASID: string);
var
  lvContext: TSessionItem;
begin
  FList.Lock;
  try
    lvContext := TSessionItem(FList.ValueMap[ASID]);
    FList.Remove(ASID);
    lvContext.OnDisconnect;
    lvContext.Free;
  finally
    FList.UnLock;
  end;
end;

constructor TSessionItem.Create;
begin
  inherited;
end;

{ TSessionItem }

procedure TSessionItem.DoActivity;
begin
  FLastActivity := GetTickCount;
end;

procedure TSessionItem.OnDisconnect;
begin
  
end;

end.
