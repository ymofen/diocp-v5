unit utils_link;

interface

type
  PDListNode = ^TDListNode;
  TDListNode = record
    Prev,
    Next: PDListNode;
    Data: Pointer;
    RefCount: Integer;
  end;

  PDList = ^TDLink;
  TDLink = object
  private
    FNode: array[0..1] of TDListNode;
    FHead: PDListNode;
    FTail: PDListNode;
    FFetchNode: PDListNode;
    FCount: Integer;
  public
    procedure AddNode(Node: PDListNode);
    procedure DelNode(Node: PDListNode);
    function Fetch(var Data: Pointer): Boolean;
    procedure First;
    procedure Init;
    procedure Free;
  public
    property Count: Integer read FCount;
    property Head: PDListNode read FHead;
    property Tail: PDListNode read FTail;
  end;

implementation

{ TDLink }
procedure TDLink.Init;
begin
  FHead := @FNode[0];
  FTail := @FNode[1];

  FHead.Next := FTail;
  FTail.Prev:= FHead;
end;

procedure TDLink.AddNode(Node: PDListNode);
begin
  FHead.Next.Prev := Node;
  Node.Next := FHead.Next;
  Node.Prev := FHead;
  FHead.Next := Node;

  Inc(FCount);
  Inc(Node.RefCount);
  Assert(Node.RefCount = 1);
end;

procedure TDLink.DelNode(Node: PDListNode);
begin
  if FCount = 0 then
    Exit;

  Node.Prev.Next := Node.Next;
  Node.Next.Prev := Node.Prev;

  Dec(FCount);
  Dec(Node.RefCount);
  Assert(Node.RefCount = 0);
end;

function TDLink.Fetch(var Data: Pointer): Boolean;
begin
  Result := False;
  if FFetchNode.Next <> FTail then begin
    FFetchNode := FFetchNode.Next;
    Data := FFetchNode.Data;
    Result := True;
  end;
end;

procedure TDLink.First;
begin
  FFetchNode := FHead;
end;

procedure TDLink.Free;
begin
end;


end.
