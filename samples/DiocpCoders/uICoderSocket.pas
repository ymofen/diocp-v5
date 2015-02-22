unit uICoderSocket;

interface

type
  ICoderSocket = interface
    ['{6C90309D-C0AA-40B9-9DAE-8C801A1DF99B}']
    function sendBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;
    function recvBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;
    
    procedure closeSocket; stdcall;
  end;

implementation

end.
