unit uMyTypes;

interface

uses
  SysUtils;

type
  {$if CompilerVersion <= 23}
     NativeUInt = Cardinal;
  {$ifend}

  {$if CompilerVersion <= 18.5}
     TBytes = array of Byte;
  {$else}
     TBytes = SysUtils.TBytes;
  {$ifend}

  ///
  MyInteger = Integer;


implementation

end.
