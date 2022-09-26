(**************************************************************)
(*     Advanced Encryption Standard (AES)                     *)
(*     Interface Unit v1.3                                    *)
(*                                                            *)
(*     Copyright (c) 2002 Jorlen Young                        *)
(*                                                            *)
(* ˵����                                                     *)
(*    ���� ElASE.pas ��Ԫ��װ                                 *)
(*                                                            *)
(*    ����һ�� AES �����㷨�ı�׼�ӿڡ�                       *)
(*                                                            *)
(* ========================================================== *)
(*                                                            *)
(* ���º������� ����:                                         *)
(*                    ������      2004.12.04                  *)
(*                                                            *)
(* function StrToHex(Value: string): string;                  *)
(* function HexToStr(Value: string): string;                  *)
(* function EncryptString(Value: string; Key: string;         *)
(*   KeyBit: TKeyBit = kb128): string;                        *)
(* function DecryptString(Value: string; Key: string;         *)
(*   KeyBit: TKeyBit = kb128): string;                        *)
(* ---------------------------------------------------------- *)
(*                                                            *)
(* ���º������� ����:                                         *)
(*                    ��ͯ�� cooniur, 2005.02.03              *)
(*                                                            *)
(* function EncryptStream(Src: TStream; Key: string;          *)
(*   var Dest: TStream; KeyBit: TKeyBit = kb128): Boolean;    *)
(* function DecryptStream(Src: TStream; Key: string;          *)
(*   var Dest: TStream; KeyBit: TKeyBit = kb128): Boolean;    *)
(*                                                            *)
(* ����ʾ����                                                 *)
(* if not EncryptStream(src, key, TStream(Dest), keybit) then *)
(*   showmessage('encrypt error');                            *)
(*                                                            *)
(* if not DecryptStream(src, key, TStream(Dest), keybit) then *)
(*   showmessage('encrypt error');                            *)
(*                                                            *)
(* *** һ��Ҫ��Dest����TStream(Dest) ***                      *)
(* ========================================================== *)
(*                                                            *)
(*   ֧�� 128 / 192 / 256 λ���ܳ�                            *)
(*   Ĭ������°��� 128 λ�ܳײ���                            *)
(*                                                            *)
(**************************************************************)

{
  yangmf
    ���ݵͰ汾�ı���(2022-08-11 15:05:29)
}

unit AES;                  

interface

uses
  SysUtils, Classes, Math, ElAES, utils_strings;

const
  SDestStreamNotCreated = 'Dest stream not created.';
  SEncryptStreamError = 'Encrypt stream error.';
  SDecryptStreamError = 'Decrypt stream error.';

type
  TKeyBit = (kb128, kb192, kb256);

function StrToHex(Value: string): string;
function HexToStr(Value: string): string;

function EncryptString(Value: string; Key: string;
  KeyBit: TKeyBit = kb128): string;
function DecryptString(Value: string; Key: string;
  KeyBit: TKeyBit = kb128): string;

function EncryptStream(Src: TStream; Key: string; Dest: TStream; KeyBit:
    TKeyBit = kb128): Boolean;
function DecryptStream(Src: TStream; Key: string; Dest: TStream; KeyBit:
    TKeyBit = kb128): Boolean;

procedure EncryptFile(SourceFile, DestFile: string;
  Key: string; KeyBit: TKeyBit = kb128);
procedure DecryptFile(SourceFile, DestFile: string;
  Key: string; KeyBit: TKeyBit = kb128);
  
implementation

function StrToHex(Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
    Result := Result + IntToHex(Ord(Value[I]), 2);
end;

function HexToStr(Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    if ((I mod 2) = 1) then
      Result := Result + Chr(StrToInt('0x'+ Copy(Value, I, 2)));
  end;
end;

{  --  �ַ������ܺ��� Ĭ�ϰ��� 128 λ�ܳ׼��� --  }
function EncryptString(Value: string; Key: string;
  KeyBit: TKeyBit = kb128): string;
var
  SS, DS: TStringStream;
  Size: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
  KeyBytes:TBytes;
begin
  Result := '';
  SS := TStringStream.Create(Value);
  DS := TStringStream.Create('');
  try
    KeyBytes := utils_strings.StringToUtf8Bytes(Key, False);
    Size := SS.Size;
    DS.WriteBuffer(Size, SizeOf(Size));
    {  --  128 λ�ܳ���󳤶�Ϊ 16 ���ַ� --  }
    if KeyBit = kb128 then
    begin
      FillChar(AESKey128, SizeOf(AESKey128), 0 );
      Move(KeyBytes[0], AESKey128, Min(SizeOf(AESKey128), Length(KeyBytes)));
      EncryptAESStreamECB(SS, 0, AESKey128, DS);
    end;
    {  --  192 λ�ܳ���󳤶�Ϊ 24 ���ַ� --  }
    if KeyBit = kb192 then
    begin
      FillChar(AESKey192, SizeOf(AESKey192), 0 );
      Move(KeyBytes[0], AESKey192, Min(SizeOf(AESKey192), Length(KeyBytes)));
      EncryptAESStreamECB(SS, 0, AESKey192, DS);
    end;
    {  --  256 λ�ܳ���󳤶�Ϊ 32 ���ַ� --  }
    if KeyBit = kb256 then
    begin
      FillChar(AESKey256, SizeOf(AESKey256), 0 );
      Move(KeyBytes[0], AESKey256, Min(SizeOf(AESKey256), Length(KeyBytes)));
      EncryptAESStreamECB(SS, 0, AESKey256, DS);
    end;
    Result := StrToHex(DS.DataString);
  finally
    SS.Free;
    DS.Free;
  end;
end;

{  --  �ַ������ܺ��� Ĭ�ϰ��� 128 λ�ܳ׽��� --  }
function DecryptString(Value: string; Key: string;
  KeyBit: TKeyBit = kb128): string;
var
  SS, DS: TStringStream;
  Size: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
  KeyBytes:TBytes;
begin
  Result := '';
  SS := TStringStream.Create(HexToStr(Value));
  DS := TStringStream.Create('');
  try
    KeyBytes := utils_strings.StringToUtf8Bytes(Key, False);
    Size := SS.Size;
    SS.ReadBuffer(Size, SizeOf(Size));
    {  --  128 λ�ܳ���󳤶�Ϊ 16 ���ַ� --  }
    if KeyBit = kb128 then
    begin
      FillChar(AESKey128, SizeOf(AESKey128), 0 );
      Move(KeyBytes[0], AESKey128, Min(SizeOf(AESKey128), Length(KeyBytes)));
      DecryptAESStreamECB(SS, SS.Size - SS.Position, AESKey128, DS);
    end;
    {  --  192 λ�ܳ���󳤶�Ϊ 24 ���ַ� --  }
    if KeyBit = kb192 then
    begin
      FillChar(AESKey192, SizeOf(AESKey192), 0 );
      Move(KeyBytes[0], AESKey192, Min(SizeOf(AESKey192), Length(KeyBytes)));
      DecryptAESStreamECB(SS, SS.Size - SS.Position, AESKey192, DS);
    end;
    {  --  256 λ�ܳ���󳤶�Ϊ 32 ���ַ� --  }
    if KeyBit = kb256 then
    begin
      FillChar(AESKey256, SizeOf(AESKey256), 0 );
      Move(KeyBytes[0], AESKey256, Min(SizeOf(AESKey256), Length(KeyBytes)));
      //Move(PChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
      DecryptAESStreamECB(SS, SS.Size - SS.Position, AESKey256, DS);
    end;
    Result := DS.DataString;
  finally
    SS.Free;
    DS.Free;
  end;
end;

{ �����ܺ���, default keybit: 128bit }
function EncryptStream(Src: TStream; Key: string; Dest: TStream; KeyBit:
    TKeyBit = kb128): Boolean;
var
  Count: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
  KeyBytes:TBytes;
begin
  if Dest = nil then
  begin
    raise Exception.Create(SDestStreamNotCreated);
    Result:= False;
    Exit;
  end;

  try
    KeyBytes := utils_strings.StringToUtf8Bytes(Key, False);
    Src.Position:= 0;
    Count:= Src.Size;
    Dest.Write(Count, SizeOf(Count));
    {  --  128 λ�ܳ���󳤶�Ϊ 16 ���ַ� --  }
    if KeyBit = kb128 then
    begin
      FillChar(AESKey128, SizeOf(AESKey128), 0 );
      Move(KeyBytes[0], AESKey128, Min(SizeOf(AESKey128), Length(KeyBytes)));
      EncryptAESStreamECB(Src, 0, AESKey128, Dest);
    end;
    {  --  192 λ�ܳ���󳤶�Ϊ 24 ���ַ� --  }
    if KeyBit = kb192 then
    begin
      FillChar(AESKey192, SizeOf(AESKey192), 0 );
      Move(KeyBytes[0], AESKey192, Min(SizeOf(AESKey192), Length(KeyBytes)));
      EncryptAESStreamECB(Src, 0, AESKey192, Dest);
    end;
    {  --  256 λ�ܳ���󳤶�Ϊ 32 ���ַ� --  }
    if KeyBit = kb256 then
    begin
      FillChar(AESKey256, SizeOf(AESKey256), 0 );
      Move(KeyBytes[0], AESKey256, Min(SizeOf(AESKey256), Length(KeyBytes)));
      EncryptAESStreamECB(Src, 0, AESKey256, Dest);
    end;

    Result := True;
  except
    raise Exception.Create(SEncryptStreamError);
    Result:= False;
  end;
end;

{ �����ܺ���, default keybit: 128bit }
function DecryptStream(Src: TStream; Key: string; Dest: TStream; KeyBit:
    TKeyBit = kb128): Boolean;
var
  Count, OutPos: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
  KeyBytes:TBytes;

begin
  if Dest = nil then
  begin
    raise Exception.Create(SDestStreamNotCreated);
    Result:= False;
    Exit;
  end;

  try
    KeyBytes := utils_strings.StringToUtf8Bytes(Key, False);


    Src.Position:= 0;
    OutPos:= Dest.Position;
    Src.ReadBuffer(Count, SizeOf(Count));
    {  --  128 λ�ܳ���󳤶�Ϊ 16 ���ַ� --  }
    if KeyBit = kb128 then
    begin
      FillChar(AESKey128, SizeOf(AESKey128), 0 );
      Move(KeyBytes[0], AESKey128, Min(SizeOf(AESKey128), Length(KeyBytes)));
      DecryptAESStreamECB(Src, Src.Size - Src.Position,
        AESKey128, Dest);
    end;
    {  --  192 λ�ܳ���󳤶�Ϊ 24 ���ַ� --  }
    if KeyBit = kb192 then
    begin
      FillChar(AESKey192, SizeOf(AESKey192), 0 );
      Move(KeyBytes[0], AESKey192, Min(SizeOf(AESKey192), Length(KeyBytes)));
      DecryptAESStreamECB(Src, Src.Size - Src.Position,
        AESKey192, Dest);
    end;
    {  --  256 λ�ܳ���󳤶�Ϊ 32 ���ַ� --  }
    if KeyBit = kb256 then
    begin
      FillChar(AESKey256, SizeOf(AESKey256), 0 );
      Move(KeyBytes[0], AESKey256, Min(SizeOf(AESKey256), Length(KeyBytes)));
      DecryptAESStreamECB(Src, Src.Size - Src.Position,
        AESKey256, Dest);
    end;
    Dest.Size := OutPos + Count;
    Dest.Position := OutPos;

    Result := True;
  except
    raise Exception.Create(SDecryptStreamError);
    Result:= False;
  end;
end;

{  --  �ļ����ܺ��� Ĭ�ϰ��� 128 λ�ܳ׽��� --  }
procedure EncryptFile(SourceFile, DestFile: string;
  Key: string; KeyBit: TKeyBit = kb128);
var
  SFS, DFS: TFileStream;
  Size: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
begin
  SFS := TFileStream.Create(SourceFile, fmOpenRead);
  try
    DFS := TFileStream.Create(DestFile, fmCreate);
    try
      Size := SFS.Size;
      DFS.WriteBuffer(Size, SizeOf(Size));
      {  --  128 λ�ܳ���󳤶�Ϊ 16 ���ַ� --  }
      if KeyBit = kb128 then
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0 );
        Move(PChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAESStreamECB(SFS, 0, AESKey128, DFS);
      end;
      {  --  192 λ�ܳ���󳤶�Ϊ 24 ���ַ� --  }
      if KeyBit = kb192 then
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0 );
        Move(PChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAESStreamECB(SFS, 0, AESKey192, DFS);
      end;
      {  --  256 λ�ܳ���󳤶�Ϊ 32 ���ַ� --  }
      if KeyBit = kb256 then
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0 );
        Move(PChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAESStreamECB(SFS, 0, AESKey256, DFS);
      end;
    finally
      DFS.Free;
    end;
  finally
    SFS.Free;
  end;
end;

{  --  �ļ����ܺ��� Ĭ�ϰ��� 128 λ�ܳ׽��� --  }
procedure DecryptFile(SourceFile, DestFile: string;
  Key: string; KeyBit: TKeyBit = kb128);
var
  SFS, DFS: TFileStream;
  Size: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
begin
  SFS := TFileStream.Create(SourceFile, fmOpenRead);
  try
    SFS.ReadBuffer(Size, SizeOf(Size));
    DFS := TFileStream.Create(DestFile, fmCreate);
    try
      {  --  128 λ�ܳ���󳤶�Ϊ 16 ���ַ� --  }
      if KeyBit = kb128 then
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0 );
        Move(PChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAESStreamECB(SFS, SFS.Size - SFS.Position, AESKey128, DFS);
      end;
      {  --  192 λ�ܳ���󳤶�Ϊ 24 ���ַ� --  }
      if KeyBit = kb192 then
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0 );
        Move(PChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAESStreamECB(SFS, SFS.Size - SFS.Position, AESKey192, DFS);
      end;
      {  --  256 λ�ܳ���󳤶�Ϊ 32 ���ַ� --  }
      if KeyBit = kb256 then
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0 );
        Move(PChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAESStreamECB(SFS, SFS.Size - SFS.Position, AESKey256, DFS);
      end;
      DFS.Size := Size;
    finally
      DFS.Free;
    end;
  finally
    SFS.Free;
  end;
end;
end.
