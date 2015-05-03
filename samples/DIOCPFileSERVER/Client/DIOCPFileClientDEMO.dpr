program DIOCPFileClientDEMO;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uICoderSocket in '..\..\DiocpCoders\uICoderSocket.pas',
  IdTCPClientCoderImpl in '..\..\DiocpCoders\IdTCPClientCoderImpl.pas',
  uDTcpClientCoderImpl in '..\..\DiocpCoders\uDTcpClientCoderImpl.pas',
  uStreamCoderSocket in '..\..\DiocpCoders\uStreamCoderSocket.pas',
  DiocpFileOperator in 'service\DiocpFileOperator.pas',
  SimpleMsgPack in 'service\SimpleMsgPack.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
//  /// <summary>
//  ///   diocpÔ¶³ÌÎÄ¼þ´æ´¢
//  /// </summary>
//  beanFactory.RegisterBean('diocpRemoteFile', TDIOCPFileAccessImpl);
//
//  beanFactory.RegisterBean('diocpRemoteFileDEMO', TfrmMain);
end.
