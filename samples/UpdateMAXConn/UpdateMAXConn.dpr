program UpdateMAXConn;

{$APPTYPE CONSOLE}

uses
  SysUtils, Registry, Windows;

procedure RegUpdateMaxUserPort();
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters', True) then
    begin
      try                                                                       
        if (not reg.KeyExists('MaxUserPort')) or  (reg.ReadInteger('MaxUserPort') <> 65534) then
        begin
          reg.WriteInteger('MaxUserPort', 65534);
          reg.WriteInteger('MaxHashTableSize', 65536);
          reg.WriteInteger('MaxFreeTcbs', 16000);
          reg.WriteInteger('TcpTimedWaitDelay', 5);
        end;
      finally
        reg.CloseKey;
      end;           
    end;
  finally
    reg.Free;
  end;
end;

var
  s:string;
begin
  try
    RegUpdateMaxUserPort;
    Writeln('update succ.., visit www.diocp.org!');
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  Readln(s);
end.
