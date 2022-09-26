(*
 *	 Unit owner: d10.�����
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 ����
 *
 *)
 
unit diocp_coder_baseObject;

interface

uses
  diocp_tcp_server, utils_buffer, utils_queues, utils_BufferPool;

type
{$if CompilerVersion< 18.5}
  TBytes = array of Byte;
{$IFEND}

  TIOCPDecoder = class(TObject)
  public
    /// <summary>
    ///   �����յ�������,����н��յ�����,���ø÷���,���н���
    /// </summary>
    /// <returns>
    ///   ���ؽ���õĶ���
    /// </returns>
    /// <param name="inBuf"> ���յ��������� </param>
    function Decode(const inBuf: TBufferLink; pvContext: TObject): TObject;
        virtual; abstract;
  end;

  TIOCPDecoderClass = class of TIOCPDecoder;

  TIOCPEncoder = class(TObject)
  public
    /// <summary>
    ///   ����Ҫ���͵Ķ���
    /// </summary>
    /// <param name="pvDataObject"> Ҫ���б���Ķ��� </param>
    /// <param name="ouBuf"> ����õ����� </param>
    procedure Encode(pvDataObject: TObject; const ouBuf: TBufferLink); virtual;
        abstract;
  end;

  TIOCPEncoderClass = class of TIOCPEncoder;



  TDiocpContextCoderExchange = class(TObject)
  private
    FContext: TObject;
  public
    constructor Create; virtual;
    procedure CleanUp; virtual;
    property Context: TObject read FContext write FContext;
  end;

  TDiocpContextCoderExchangeClass = class of TDiocpContextCoderExchange;

  TDiocpEncoder = class(TObject)
  public

    /// <summary>
    ///   ����Ҫ���͵Ķ���
    /// </summary>
    /// <param name="pvDataObject"> Ҫ���б���Ķ��� </param>
    /// <param name="pvBufWriter"> ����д�� </param>
    procedure Encode(const pvExchange: TDiocpContextCoderExchange; const
        pvDataObject: Pointer; const pvBufWriter: TBlockBuffer); virtual; abstract;
  end;

  
  TDiocpEncoderClass = class of TDiocpEncoder;


  /// <summary>
  ///  ������
  /// </summary>
  TDiocpDecoder = class(TObject)
  public

    /// <summary>
    ///   ��ȡ����õ�����
    /// </summary>
    function GetData(const pvExchange: TDiocpContextCoderExchange; const pvCopy:
        Boolean): Pointer; virtual; abstract;


    /// <summary>
    ///   �ͷŽ���õ�����
    /// </summary>
    procedure ReleaseData(const pvExchange: TDiocpContextCoderExchange; const
        pvData: Pointer; const pvCopy: Boolean); virtual; abstract;

    /// <summary>
    ///   ��������
    /// </summary>
    procedure SetRecvBuffer(const pvExchange: TDiocpContextCoderExchange;
      const buf:Pointer; len:Cardinal); virtual; abstract;

    /// <summary>
    ///   �����յ�������,����н��յ�����,���ø÷���,���н���
    /// </summary>
    /// <returns>
    ///   0����Ҫ���������
    ///   1: ����ɹ�
    ///  -1: ����ʧ��
    /// </returns>
    /// <param name="inBuf"> ���յ��������� </param>
    function Decode(const pvExchange: TDiocpContextCoderExchange): Integer;  virtual; abstract;
  end;

  TDiocpDecoderClass = class of TDiocpDecoder;

implementation


{ TDiocpEncoder }

procedure TDiocpContextCoderExchange.CleanUp;
begin
  
end;

constructor TDiocpContextCoderExchange.Create;
begin
  inherited;
end;

end.
