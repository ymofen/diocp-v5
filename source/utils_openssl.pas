unit utils_openssl;

interface

uses
  Windows, SyncObjs;

const
  SSLEAY_DLL = 'ssleay32.dll';
  LIBEAY_DLL = 'libeay32.dll';
  
  SSL_FILETYPE_PEM = 1;
  EVP_PKEY_RSA = 6;
  BIO_FLAGS_READ                          = 1;
  BIO_FLAGS_WRITE                         = 2;
  BIO_FLAGS_IO_SPECIAL                    = 4;
  BIO_FLAGS_RWS                           = (BIO_FLAGS_READ or
                                             BIO_FLAGS_WRITE or
                                             BIO_FLAGS_IO_SPECIAL);
  BIO_FLAGS_SHOULD_RETRY                  = 8;

  BIO_CTRL_EOF     = 2;
  BIO_CTRL_INFO		 = 3;
  BIO_CTRL_PENDING = 10;
  
  SSL_ERROR_NONE                              = 0;
  SSL_ERROR_SSL                               = 1;
  SSL_ERROR_WANT_READ                         = 2;
  SSL_ERROR_WANT_WRITE                        = 3;
  SSL_ERROR_WANT_X509_LOOKUP                  = 4;
  SSL_ERROR_SYSCALL                           = 5;
  SSL_ERROR_ZERO_RETURN                       = 6;
  SSL_ERROR_WANT_CONNECT                      = 7;
  SSL_ERROR_WANT_ACCEPT                       = 8;

  SSL_ST_CONNECT                              = $1000;
  SSL_ST_ACCEPT                               = $2000;
  SSL_ST_MASK                                 = $0FFF;
  SSL_ST_INIT                                 = (SSL_ST_CONNECT or SSL_ST_ACCEPT);
  SSL_ST_BEFORE                               = $4000;
  SSL_ST_OK                                   = $03;
  SSL_ST_RENEGOTIATE                          = ($04 or SSL_ST_INIT);

  CRYPTO_LOCK		= 1;
  CRYPTO_UNLOCK	= 2;
  CRYPTO_READ   = 4;
  CRYPTO_WRITE  = 8;
  
type
 { ctype }
  StringA = AnsiString;
  PCharA = PAnsiChar;
  PCharW = PWideChar;
  qword = int64;  // Keep h2pas "uses ctypes" headers working with delphi.
  ptruint = cardinal;
  pptruint = ^ptruint;

  { the following type definitions are compiler dependant }
  { and system dependant                                  }

  cint8                  = shortint;           pcint8                 = ^cint8;
  cuint8                 = byte;               pcuint8                = ^cuint8;
  cchar                  = cint8;              pcchar                 = ^cchar;
  cschar                 = cint8;              pcschar                = ^cschar;
  cuchar                 = cuint8;             pcuchar                = ^cuchar;

  cint16                 = smallint;           pcint16                = ^cint16;
  cuint16                = word;               pcuint16               = ^cuint16;
  cshort                 = cint16;             pcshort                = ^cshort;
  csshort                = cint16;             pcsshort               = ^csshort;
  cushort                = cuint16;            pcushort               = ^cushort;

  cint32                 = longint;            pcint32                = ^cint32;
  cuint32                = longword;           pcuint32               = ^cuint32;

  cint64                 = int64;              pcint64                = ^cint64;
  cuint64                = qword;              pcuint64               = ^cuint64;
  clonglong              = cint64;             pclonglong             = ^clonglong;
  cslonglong             = cint64;             pcslonglong            = ^cslonglong;
  culonglong             = cuint64;            pculonglong            = ^culonglong;

  cbool                  = longbool;           pcbool                 = ^cbool;
{$if defined(cpu64) and not(defined(win64) and defined(cpux86_64))}
  cint                   = cint32;             pcint                  = ^cint;              { minimum range is : 32-bit    }
  csint                  = cint32;             pcsint                 = ^csint;             { minimum range is : 32-bit    }
  cuint                  = cuint32;            pcuint                 = ^cuint;             { minimum range is : 32-bit    }
  clong                  = int64;              pclong                 = ^clong;
  cslong                 = int64;              pcslong                = ^cslong;
  culong                 = qword;              pculong                = ^culong;
{$elseif defined(cpu16)}
  { 16-bit int sizes checked against Borland C++ 3.1 and Open Watcom 1.9 }
  cint                   = cint16;             pcint                  = ^cint;
  csint                  = cint16;             pcsint                 = ^csint;
  cuint                  = cuint16;            pcuint                 = ^cuint;
  clong                  = longint;            pclong                 = ^clong;
  cslong                 = longint;            pcslong                = ^cslong;
  culong                 = cardinal;           pculong                = ^culong;
{$else}
  cint                   = cint32;             pcint                  = ^cint;              { minimum range is : 32-bit    }
  csint                  = cint32;             pcsint                 = ^csint;             { minimum range is : 32-bit    }
  cuint                  = cuint32;            pcuint                 = ^cuint;             { minimum range is : 32-bit    }
  clong                  = longint;            pclong                 = ^clong;
  cslong                 = longint;            pcslong                = ^cslong;
  culong                 = cardinal;           pculong                = ^culong;
{$ifend}


const
  SSL_CipherList :StringA = 'ECDHE-ECDSA-AES128-GCM-SHA256:' +
    'ECDHE-RSA-AES128-GCM-SHA256:' +
    'ECDHE-RSA-AES256-GCM-SHA384:' +
    'ECDHE-ECDSA-AES256-GCM-SHA384:' +
    'DHE-RSA-AES128-GCM-SHA256:' +
    'ECDHE-RSA-AES128-SHA256:' +
    'DHE-RSA-AES128-SHA256:' +
    'ECDHE-RSA-AES256-SHA384:' +
    'DHE-RSA-AES256-SHA384:' +
    'ECDHE-RSA-AES256-SHA256:' +
    'DHE-RSA-AES256-SHA256:' +
    'HIGH:' +
    '!aNULL:' +
    '!eNULL:' +
    '!EXPORT:' +
    '!DES:' +
    '!RC4:' +
    '!MD5:' +
    '!PSK:' +
    '!SRP:' +
    '!CAMELLIA';

type
  SSL_Ptr         = Pointer;
  PSSL_CTX        = Pointer;
  PSSL            = Pointer;
  PSSL_METHOD     = Pointer;
  PSSL_SESSION    = Pointer;
  PPSSL_SESSION   =^PSSL_SESSION;
  PSSL_CIPHER	  = Pointer;
  Pevp_pkey_st    = Pointer;
  PSTACK          = Pointer;
  PPSTACK         =^PSTACK;
  PCRYPTO_EX_DATA = Pointer;
  PLHASH	  = Pointer;

  PBIO            = Pointer;
  PBIO_METHOD     = Pointer;


var
  SSL_library_init: function:cInt; cdecl;
  SSL_load_error_strings: procedure; cdecl;
  SSL_CTX_new: function(meth: PSSL_METHOD): PSSL_CTX; cdecl;
  SSL_CTX_free : procedure(arg0: PSSL_CTX) cdecl;
  SSL_CTX_set_cipher_list: function(arg0: PSSL_CTX; str: PCharA):cInt; cdecl;
  SSL_CTX_set_default_passwd_cb_userdata: procedure(ctx: PSSL_CTX; u: SSL_Ptr); cdecl;
  SSL_CTX_load_verify_locations: function(ctx: PSSL_CTX; const CAfile: PCharA; const CApath: PCharA):cInt; cdecl;
  SSL_CTX_use_certificate_file: function(ctx: PSSL_CTX; const _file: PCharA; _type: cInt):cInt; cdecl;
  SSL_CTX_use_RSAPrivateKey_file : function(ctx: PSSL_CTX; const _file: PCharA; _type: cInt):cInt; cdecl;
  SSL_CTX_check_private_key : function(ctx: PSSL_CTX):cInt; cdecl;
  SSL_set_fd : function(s: PSSL; fd: Integer):Integer cdecl;
  SSL_MethodV23:  function:PSSL_METHOD; cdecl;

  SSL_new : function(ctx: PSSL_CTX):PSSL; cdecl;
  SSL_free : procedure(ssl: PSSL); cdecl;
  SSL_accept : function(ssl: PSSL):cInt; cdecl;
  SSL_read: function(s: PSSL; buf: Pointer; num: Integer): Integer; cdecl;
  SSL_write: function(s: PSSL; const buf: Pointer; num: Integer): Integer; cdecl;
  SSL_state: function(s: PSSL): Integer; cdecl;
  SSL_set_bio: procedure(s: PSSL; rbio, wbio: PBIO); cdecl;
  SSL_set_accept_state: procedure(s: PSSL); cdecl;
  SSL_do_handshake: function(S: PSSL): Integer; cdecl;
  SSL_get_error: function(s: PSSL; ret_code: Integer): Integer; cdecl;

  BIO_new: function (BioMethods: PBIO_METHOD): PBIO; cdecl;
  BIO_ctrl: function (bp: PBIO; cmd: Integer; larg: Longint; parg: Pointer): Longint; cdecl;

  BIO_new_mem_buf:function (buf: Pointer; len: Integer): PBIO; cdecl;
  BIO_free: function (b: PBIO): Integer; cdecl;
  BIO_s_mem: function : PBIO_METHOD; cdecl;
  BIO_read: function (b: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl;
  BIO_write:function (b: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl;

  CRYPTO_num_locks: function: cInt; cdecl;
  CRYPTO_set_locking_callback:procedure(cb: SSL_Ptr); cdecl;


  ssl_isready:Boolean;

function BIO_get_flags(b: PBIO): Integer;

// BIO中数据长度
function BIO_pending(b:PBIO): Integer;
function SSL_IsFatalErr(pvErr:Integer): Boolean;

implementation

var
  __libeay_handle: THandle;
  __ssleay_handle: THandle;
  __sslLocks: array of TCriticalSection;

procedure __ssl_lock_callback(Mode, N: Integer;
  const _File: PCharA; Line: Integer); cdecl;
begin
	if(mode and CRYPTO_LOCK <> 0) then
    __sslLocks[N].Enter
	else
    __sslLocks[N].Leave;
end;

procedure __ssl_initial();
var
  lvN, i:Integer;
begin
  SSL_library_init();
  SSL_load_error_strings();

  // 锁，如果不处理，多并发时会出现av错误
  lvN := CRYPTO_num_locks();
	if(lvN > 0) then
  begin
    SetLength(__sslLocks, lvN);
    for i := Low(__sslLocks) to High(__sslLocks) do
      __sslLocks[I] := TCriticalSection.Create;
	end;

	CRYPTO_set_locking_callback(@__ssl_lock_callback);

end;

procedure __ssl_finalization();
var
  lvN, i:Integer;
begin
  for i := Low(__sslLocks) to High(__sslLocks) do
    __sslLocks[I].Free;    
  SetLength(__sslLocks, 0);
end;

function BIO_get_flags(b: PBIO): Integer;
begin
  // This is a hack : BIO structure has not been defined. But I know
  // flags member is the 6th field in the structure (index is 5)
  // This could change when OpenSSL is updated. Check "struct bio_st".
  Result := PInteger(PCharA(b) + 3 * SizeOf(Pointer) + 2 * SizeOf(Integer))^;
end;

function LoadSsl: Boolean;
begin
  result := False;
  if __libeay_handle = 0 then __libeay_handle := LoadLibrary(LIBEAY_DLL);
  If __ssleay_handle = 0 Then __ssleay_handle := LoadLibrary(SSLEAY_DLL);

  if __ssleay_handle = 0 then exit;

  @SSL_library_init := GetProcAddress(__ssleay_handle, 'SSL_library_init');
  @SSL_load_error_strings := GetProcAddress(__ssleay_handle, 'SSL_load_error_strings');
  @SSL_CTX_new := GetProcAddress(__ssleay_handle, 'SSL_CTX_new');
  @SSL_CTX_free := GetProcAddress(__ssleay_handle, 'SSL_CTX_free');
  @SSL_CTX_set_cipher_list := GetProcAddress(__ssleay_handle, 'SSL_CTX_set_cipher_list');
  @SSL_CTX_set_default_passwd_cb_userdata := GetProcAddress(__ssleay_handle, 'SSL_CTX_set_default_passwd_cb_userdata');
  @SSL_CTX_load_verify_locations := GetProcAddress(__ssleay_handle, 'SSL_CTX_load_verify_locations');
  @SSL_CTX_use_certificate_file := GetProcAddress(__ssleay_handle, 'SSL_CTX_use_certificate_file');
  @SSL_CTX_use_RSAPrivateKey_file := GetProcAddress(__ssleay_handle, 'SSL_CTX_use_RSAPrivateKey_file');
  @SSL_CTX_check_private_key := GetProcAddress(__ssleay_handle, 'SSL_CTX_check_private_key');
  
  @SSL_set_fd := GetProcAddress(__ssleay_handle, 'SSL_set_fd');

  @SSL_new := GetProcAddress(__ssleay_handle, 'SSL_new');
  @SSL_free := GetProcAddress(__ssleay_handle, 'SSL_free');
  @SSL_accept := GetProcAddress(__ssleay_handle, 'SSL_accept');
  @SSL_read := GetProcAddress(__ssleay_handle, 'SSL_read');
  @SSL_write := GetProcAddress(__ssleay_handle, 'SSL_write');
  @SSL_state := GetProcAddress(__ssleay_handle, 'SSL_state');
  @SSL_MethodV23 := GetProcAddress(__ssleay_handle, 'SSLv23_method');
  @SSL_set_bio := GetProcAddress(__ssleay_handle, 'SSL_set_bio');
  @SSL_set_accept_state := GetProcAddress(__ssleay_handle, 'SSL_set_accept_state');
  @SSL_do_handshake := GetProcAddress(__ssleay_handle, 'SSL_do_handshake');
  @SSL_get_error := GetProcAddress(__ssleay_handle, 'SSL_get_error');


  if __libeay_handle = 0 then exit;
  @BIO_new := GetProcAddress(__libeay_handle, 'BIO_new');
  @BIO_ctrl := GetProcAddress(__libeay_handle, 'BIO_ctrl');
  @BIO_new_mem_buf := GetProcAddress(__libeay_handle, 'BIO_new_mem_buf');
  @BIO_free := GetProcAddress(__libeay_handle, 'BIO_free');
  @BIO_s_mem := GetProcAddress(__libeay_handle, 'BIO_s_mem');
  @BIO_read := GetProcAddress(__libeay_handle, 'BIO_read');
  @BIO_write := GetProcAddress(__libeay_handle, 'BIO_write');

  @CRYPTO_num_locks  := GetProcAddress(__libeay_handle, 'CRYPTO_num_locks');
  @CRYPTO_set_locking_callback  := GetProcAddress(__libeay_handle, 'CRYPTO_set_locking_callback');
  Result := True;
end;

function SSL_IsFatalErr(pvErr:Integer): Boolean;
begin
	case pvErr of
		SSL_ERROR_NONE,
		SSL_ERROR_WANT_READ,
		SSL_ERROR_WANT_WRITE,
		SSL_ERROR_WANT_CONNECT,
		SSL_ERROR_WANT_ACCEPT: Result := False;
  else
    Result := True;
	end;
end;

function BIO_pending(b:PBIO): Integer;
begin
   Result := BIO_ctrl(b, BIO_CTRL_PENDING, 0, nil);
end;



initialization
  ssl_isready := LoadSsl;
  if ssl_isready then
  begin
    __ssl_initial();

  end;

finalization
  __ssl_finalization;
  if __libeay_handle > 0 then
    FreeLibrary(__libeay_handle);
  if __ssleay_handle > 0 then
    FreeLibrary(__ssleay_handle);

  

end.
