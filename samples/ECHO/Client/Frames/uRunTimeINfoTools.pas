unit uRunTimeINfoTools;

interface

uses
  SysUtils, DateUtils;

type
  TRunTimeINfoTools = class(TObject)
  public
    class function getRunTimeINfo: String;

    class function transByteSize(pvByte:Int64):String;
  end;

const
  BytePerKB = 1024;
  BytePerMB = BytePerKB * 1024;
  BytePerGB = BytePerMB * 1024;


implementation

var
  __startTime:TDateTime;




class function TRunTimeINfoTools.getRunTimeINfo: String;
var
  lvMSec, lvRemain:Int64;
  lvDay, lvHour, lvMin, lvSec:Integer;
begin
  lvMSec := MilliSecondsBetween(Now(), __startTime);
  lvDay := Trunc(lvMSec / MSecsPerDay);
  lvRemain := lvMSec mod MSecsPerDay;

  lvHour := Trunc(lvRemain / (MSecsPerSec * 60 * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60 * 60);

  lvMin := Trunc(lvRemain / (MSecsPerSec * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60);

  lvSec := Trunc(lvRemain / (MSecsPerSec));

  if lvDay > 0 then
    Result := Result + IntToStr(lvDay) + ' d ';

  if lvHour > 0 then
    Result := Result + IntToStr(lvHour) + ' h ';

  if lvMin > 0 then
    Result := Result + IntToStr(lvMin) + ' m ';

  if lvSec > 0 then
    Result := Result + IntToStr(lvSec) + ' s ';
end;



class function TRunTimeINfoTools.transByteSize(pvByte: Int64): String;
var
  lvTB, lvGB, lvMB, lvKB:Word;
  lvRemain:Int64;
begin
  lvRemain := pvByte;

  lvTB := Trunc(lvRemain/BytePerGB/1024);
  //lvRemain := pvByte - (lvTB * BytePerGB * 1024);
  
  lvGB := Trunc(lvRemain/BytePerGB);

  lvGB := lvGB mod 1024;      // trunc TB

  lvRemain := lvRemain mod BytePerGB;

  lvMB := Trunc(lvRemain/BytePerMB);
  lvRemain := lvRemain mod BytePerMB;

  lvKB := Trunc(lvRemain/BytePerKB);
  lvRemain := lvRemain mod BytePerKB;
  Result := Format('%d TB, %d GB, %d MB, %d KB, %d B', [lvTB, lvGB, lvMB, lvKB, lvRemain]);
end;

initialization
  __startTime :=  Now();

end.
